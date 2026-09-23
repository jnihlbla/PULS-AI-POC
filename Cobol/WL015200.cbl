000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL015200.                                                
000300 AUTHOR.         SUBBARAO PARUCHURI V.                                    
000400 DATE-WRITTEN.   04/08/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       'CARPARTS.LDC.TREATRETPERMIT'                            
000800*                                                                         
000900*                                                                         
001000*    FUNCTION:                                                            
001100*        VISA RETURTILLSTÅND.                                             
001200*        ANVÄNDS FÖR BEHANDLING AV RETURTILLSTÅND.                        
001300*                                                                         
001400*        WL015200 PROGRAM IS A REPLICA OF W4073700 PROGRAM                
001500*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
001600*                                                                         
001700*    E-TRACKER: 4230251 2007-05  RETURNS FOR LDC WEB                      
001800*               5708363 2007-10  RÄTTA SKROTORDER/R32                     
001900*               8200058 2012-07  MANAGEMENT SCRAPPING FOLLOW UP           
002000*               10296404 2017-01 RETURNS FROM CA TO US                    
002100*               10302968 2017-07 GENERIC SOLUTION IDFTG                   
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSACTION: WL0152U                                             
002500*        REQUEST:     WL0152I1                                            
002600*                                                                         
002700*    OUTDATA.                                                             
002800*        RESPONSE:    WL0152O1                                            
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     SKIP2                                                                
003300 INPUT-OUTPUT SECTION.                                                    
003400                                                                          
003500 FILE-CONTROL.                                                            
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200 77  IDPGM                       PIC X(08)   VALUE 'WL015200'.            
004300                                                                          
004400*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004500 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004600 77  KDRC-DISPLAY                PIC Z(5).                                
004700                                                                          
004800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005000                                                                          
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  YES                         PIC X       VALUE 'Y'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400 77  NOO                         PIC X       VALUE 'N'.                   
005500 77  W-BLI                       PIC X(3)    VALUE 'BLI'.                 
005600 77  W-BIN                       PIC X(3)    VALUE 'BIN'.                 
005700 77  W-SCR                       PIC X(3)    VALUE 'SCR'.                 
005800 77  W-DEV                       PIC X(3)    VALUE 'DEV'.                 
005900 77  W-QDE                       PIC X(3)    VALUE 'QDE'.                 
006000 77  INFO-1                      PIC X(15)   VALUE SPACE.                 
006100 77  INFO-2                      PIC X(15)   VALUE SPACE.                 
006200 77  INFO-3                      PIC X(15)   VALUE SPACE.                 
006300 77  INFO-4                      PIC X(15)   VALUE SPACE.                 
006400 77  INFO-5                      PIC X(15)   VALUE SPACE.                 
006500 77  INFO-6                      PIC X(15)   VALUE SPACE.                 
006600 77  INFO-7                      PIC X(15)   VALUE SPACE.                 
006700 77  INFO-8                      PIC X(15)   VALUE SPACE.                 
006800 77  INFO-1-ANTAL                PIC 9(7)    VALUE ZERO.                  
006900 77  INFO-2-ANTAL                PIC 9(7)    VALUE ZERO.                  
007000 77  INFO-3-ANTAL                PIC 9(7)    VALUE ZERO.                  
007100 77  INFO-4-ANTAL                PIC 9(7)    VALUE ZERO.                  
007200 77  INFO-5-ANTAL                PIC 9(7)    VALUE ZERO.                  
007300 77  INFO-6-ANTAL                PIC 9(7)    VALUE ZERO.                  
007400 77  INFO-7-ANTAL                PIC 9(7)    VALUE ZERO.                  
007500 77  INFO-8-ANTAL                PIC 9(7)    VALUE ZERO.                  
007600                                                                          
007700*    --- INDEX FÖR BLÄDDRINGSRADER                                        
007800 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
007900 77  MSG-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
008000 77  4794-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
008100 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP SYNC.        
008200 77  4797-IX2                    PIC S9(5)  VALUE +0    COMP SYNC.        
008300 77  SPAR4797-IX                 PIC S9(5)  VALUE +0    COMP SYNC.        
008400 77  4797-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
008500 77  4797-MAX-INDX               PIC S9(4)  VALUE +16   COMP SYNC.        
008600 77  MAX-TRANS-IX                PIC S9(4)  VALUE +0    COMP SYNC.        
008700 77  ORAD-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
008800 77  ORAD-IX-MAX                 PIC S9(4)  VALUE +5    COMP SYNC.        
008900 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
009000                                                                          
009100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
009200 77  WS-IDDISTR                  PIC  X(4)  VALUE SPACE.                  
009300 77  WS-IDKUNDNR                 PIC  X(6)  VALUE SPACE.                  
009400 77  WS-IDRAPPNR                 PIC  X(6)  VALUE SPACE.                  
009500 77  WS-IDARTNR-NUM              PIC  9(9)  VALUE ZERO.                   
009600 77  WS-FLTOT                    PIC  X(1)  VALUE SPACE.                  
009700 77  WS-KDCMDVAL                 PIC  X(3)  VALUE SPACE.                  
009800 77  WS-KVANTAL                  PIC  9(6)  VALUE ZERO.                   
009900 77  WS-ADGANG                   PIC  9(2)  VALUE ZERO.                   
010000 77  WS-KDCMD-NUM                PIC  9(4).                               
010100                                                                          
010200 77  W-IDPERSON                  PIC  9(3)  VALUE ZERO.                   
010300 77  W-ANM-MOT                   PIC  X(1)  VALUE '5'.                    
010400 77  W-ANM-PAAB                  PIC  X(1)  VALUE '6'.                    
010500 77  W-ANM-KLAR                  PIC  X(1)  VALUE '7'.                    
010600 77  W-KVLEVANM-KVAR             PIC S9(7)  VALUE 0   COMP-3.             
010700 77  W-KVAVV-KVANT-R32           PIC S9(7)  VALUE 0   COMP-3.             
010800 77  W-KVRETINL-R32              PIC S9(7)  VALUE 0   COMP-3.             
010900 77  W-KVRETINL-R32-SKR          PIC S9(7)  VALUE 0   COMP-3.             
011000 77  W-KVRADER-BEH               PIC S9(3)  VALUE 0   COMP-3.             
011100 77  W-KVANTAL                   PIC S9(7)  VALUE 0   COMP-3.             
011200 77  W-IDILIST                   PIC  9(5)  VALUE 0.                      
011300 77  W-KDCMDVAL-NUM              PIC  9(3)  VALUE 0.                      
011400                                                                          
011500 77  W-TIKLOCK-R32               PIC  9(8)  VALUE ZERO.                   
011600 77  W-TIKLOCK-ORDER             PIC  9(8)  VALUE ZERO.                   
011700                                                                          
011800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
011900 77  RKOD-ABEND-MED-DUMP         PIC S9(4) VALUE +1000 COMP SYNC.         
012000                                                                          
012100 77  WS-IDELMT-ERROR             PIC X(16)   VALUE SPACE.                 
012200 77  WS-IDMSG-ERROR              PIC X(03)   VALUE SPACE.                 
012300 77  WS-IDMSG-INFO               PIC X(03)   VALUE SPACE.                 
012400                                                                          
012500 77  WS-REC-LIMIT                PIC X       VALUE 'N'.                   
012600     88  REC-LIMIT                           VALUE 'J'.                   
012700                                                                          
012800 77  WS-COUNT                    PIC 9(03)   VALUE ZERO.                  
012900                                                                          
013000 77  WS-INDX-REC                 PIC S9(4)  VALUE +0    COMP SYNC.        
013100                                                                          
013200 77    W-UPDATE-SW                 PIC X       VALUE 'N'.                 
013300     88  W-UPDATE-OK                         VALUE 'J'.                   
013400                                                                          
013500 77  WS-IDSKYLT-CN               PIC X(3)  VALUE 'RCN'.                   
013600 77  WS-IDSKYLT-GB               PIC X(3)  VALUE 'GB '.                   
013700 77  WS-CP-UTF8                  PIC X(4)  VALUE 'UTF8'.                  
013800 77  WS-CP-278                   PIC X(3)  VALUE '278'.                   
013900                                                                          
014000 01  W-SKROT-FAELT.                                                       
014100     05  W-KVSKROT-6-X.                                                   
014200         10  W-KVSKROT-6         PIC 9(6).                                
014300                                                                          
014400     05  W-IDORDNR-X.                                                     
014500       07 FILLER          PIC 9(2).                                       
014600       07 W-IDORDNR.                                                      
014700         10  W-IDORDNR-VV    PIC 9(2).                                    
014800         10  W-IDORDNR-LLL   PIC 9(3).                                    
014900                                                                          
015000 01  WS-IDDISTR-X.                                                        
015100     05 WS-IDDISTR-N         PIC 9(4).                                    
015200                                                                          
015300 01  WS-IDKUNDNR-X.                                                       
015400     05 WS-IDKUNDNR-N        PIC 9(6).                                    
015500                                                                          
015600 77  TEST-KDCMDVAL               PIC X(4)    VALUE SPACE.                 
015700     88  GODK-KDCMDVAL                       VALUE 'INL '                 
015800                                                   'SKR '                 
015900                                                   'ANT '                 
016000                                                   'KVA '                 
016100                                                   'ILI '                 
016200                                                   'BIN '                 
016300                                                   'SCR '                 
016400                                                   'DEV '                 
016500                                                   'QDE '                 
016600                                                   'BLI '.                
016700                                                                          
016800 77  SW-IDANSTNR                 PIC X       VALUE 'N'.                   
016900     88  IDANSTNR-IFYLLT                     VALUE 'J'.                   
017000                                                                          
017100 77  SW-ALLT-INLAGT              PIC X       VALUE 'N'.                   
017200     88  ALLT-INLAGT                         VALUE 'J'.                   
017300                                                                          
017400 77  SW-ALLT-ANT-AVV             PIC X       VALUE 'N'.                   
017500     88  ALLT-ANT-AVV                        VALUE 'J'.                   
017600                                                                          
017700 77  SW-ALLT-SKROT               PIC X       VALUE 'N'.                   
017800     88  ALLT-SKROT                          VALUE 'J'.                   
017900                                                                          
018000 77  SW-ALLT-TILL-ILI            PIC X       VALUE 'N'.                   
018100     88  ALLT-TILL-ILI                       VALUE 'J'.                   
018200                                                                          
018300 77  SW-RAD-CMD                  PIC X       VALUE 'N'.                   
018400     88  RAD-CMD                             VALUE 'J'.                   
018500                                                                          
018600 77  SW-ILI                      PIC X       VALUE 'J'.                   
018700     88  FOERSTA-ILI                         VALUE 'J'.                   
018800                                                                          
018900 77  SW-KLART                    PIC X       VALUE 'J'.                   
019000     88  KLART                               VALUE 'J'.                   
019100     88  EJ-KLART                            VALUE 'N'.                   
019200                                                                          
019300 77  TRANS-OHUVUD-DAM-SKAPAD-SW  PIC X       VALUE 'N'.                   
019400     88  TRANS-OHUVUD-DAM-SKAPAD             VALUE 'J'.                   
019500                                                                          
019600 77  ALLT-SKR-ORDER-SKAPAD-SW    PIC X       VALUE 'N'.                   
019700     88  ALLT-SKR-ORDER-SKAPAD               VALUE 'J'.                   
019800                                                                          
019900 77  UPPD-VALDA-RADER-SW         PIC X       VALUE 'N'.                   
020000     88  UPPD-VALDA-RADER                    VALUE 'J'.                   
020100                                                                          
020200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
020300     88  INDATA-OK                           VALUE 'J'.                   
020400     88  INDATA-FEL                          VALUE 'N'.                   
020500                                                                          
020600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
020700     88  NYCKLAR-OK                          VALUE 'J'.                   
020800     88  NYCKLAR-FEL                         VALUE 'N'.                   
020900                                                                          
021000 77  KEYS-SW                   PIC X      VALUE 'J'.                      
021100     88  KEYS-OK                          VALUE 'J'.                      
021200     88  KEYS-WRONG                       VALUE 'N'.                      
021300 77  DATE-INTERVAL-SW            PIC X       VALUE 'J'.                   
021400     88  W-DATE-INTERVAL-OK                  VALUE 'J'.                   
021500     88  W-DATE-INTERVAL-EJ                  VALUE 'N'.                   
021600                                                                          
021700 01  WS-DAREGDAT                 PIC 9(8)    VALUE ZERO.                  
021800 01  FILLER REDEFINES WS-DAREGDAT.                                        
021900     03  WS-SEKEL-D              PIC 9(2).                                
022000     03  WS-AAMMDD               PIC 9(6).                                
022100                                                                          
022200 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
022300 01  FILLER REDEFINES DAGENS-DATUM.                                       
022400     03  DAGENS-DATUM-SEKEL      PIC 9(2).                                
022500     03  DAGENS-DATUM-AAMMDD     PIC 9(6).                                
022600                                                                          
022700     EJECT                                                                
022800                                                                          
022900*    --- VALID DC CODES                                                   
023000*01  -COPY WWDC99                                                         
023100     EJECT                                                                
023200                                                                          
023300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
023400 01  GENERAL-SUBPROGRAMS.                                                 
023500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
023600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
023700     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
023800     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
023900     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
024000     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
024100     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
024200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
024300     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
024400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
024500     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
024600     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
024700     SKIP3                                                                
024800*    --- PARAMETERS TO ABEND                                              
024900                                                                          
025000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
025100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
025200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
025300*                                                                         
025400 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
025500*01  -COPY WTRAUTF8                                                       
025600     EJECT                                                                
025700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
025800     SKIP3                                                                
025900*01  -COPY WZ01SUB                                                        
026000     EJECT                                                                
026100 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH'.            
026200*01  -COPY WZ01AUTH                                                       
026300     EJECT                                                                
026400 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
026500*01  -COPY WMSGCONV                                                       
026600     EJECT                                                                
026700 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
026800     SKIP3                                                                
026900 01  REQU-AREA.                                                           
027000*    03  -COPY WZ01REQ2                                                   
027100*    03  -COPY WL0152I1                                                   
027200     EJECT                                                                
027300 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
027400     SKIP3                                                                
027500 01  RESP-AREA.                                                           
027600*    03  -COPY WZ01RES2                                                   
027700*    03  -COPY WL0152O1                                                   
027800     EJECT                                                                
027900*    ---  LÄNKAREA TILL W418OKOD                                          
028000 01  FILLER                      PIC X(16)   VALUE 'W418OKOD'.            
028100                                                                          
028200*01 -COPY W418OKOD           -PRE OKOD-.                                  
028300     EJECT                                                                
028400 01  FILLER                      PIC X(16)   VALUE 'DAT-AREA'.            
028500     SKIP3                                                                
028600 01  DAT-IO-AREA.                                                         
028700*    03  -COPY WDATAREA                                                   
028800     EJECT                                                                
028900*    --- PARAMETRAR TILL WDAGKONV                                         
029000*01  -COPY WDAGAREA                                                       
029100     EJECT                                                                
029200 01  MESSAGE-CODES.                                                       
029300     03  ERR-FORBIDDEN-UPDATE     PIC X(3)   VALUE '007'.                 
029400     03  ERR-PF11-AND-NO-DATA     PIC X(3)   VALUE '014'.                 
029500     03  ERR-FLERA-FUNKTIONER     PIC X(3)   VALUE '187'.                 
029600     03  ERR-RAPPORTERING-STARTAD PIC X(3)   VALUE '237'.                 
029700     03  ERR-RAPPORTERING-KLAR    PIC X(3)   VALUE '238'.                 
029800     03  ERR-UNAUTHORIZED        PIC X(3)    VALUE '00A'.                 
029900     03  INF-UPDATE-DONE          PIC X(3)   VALUE '001'.                 
030000     03  ERR-WRONG-KEY            PIC X(3)   VALUE '022'.                 
030100     03  SYSTEM-ERROR             PIC X(3)   VALUE '099'.                 
030200     03  NUM-OF-LINES             PIC X(3)   VALUE '028'.                 
030300     03  ERR-LAGER-SAKNAS         PIC X(3)   VALUE '706'.                 
030400     EJECT                                                                
030500 01  KONTROLL-SIFFRA.                                                     
030600     03  REK-IDARTNR             PIC 9(9)    VALUE 0.                     
030700     03  REK-LNGD                PIC 9(1)    VALUE 9.                     
030800     03  REK-REKSIFFR            PIC 9(1)    VALUE 0.                     
030900                                                                          
031000     EJECT                                                                
031100*    --- FÄLT FÖR HOPP TILL ANDRA BILDER                                  
031200   77  SW-STARTA-ANNAN-BILD        PIC X       VALUE 'N'.                 
031300     88  STARTA-ANNAN-BILD                     VALUE 'J'.                 
031400                                                                          
031500 01  BILD-HOPP-AREOR.                                                     
031600                                                                          
031700   03 FILLER             PIC X(16)   VALUE 'P-TO-P-AREA'.                 
031800   03      P-TO-P-SW.                                                     
031900                                                                          
032000     05  P-TO-P-KVLL             PIC S9(4) VALUE +0   COMP SYNC.          
032100     05  P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
032200     05  P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
032300     05  P-TO-P-KDTRANS          PIC X(8).                                
032400     05  P-TO-P-IDTRANS          PIC X(4).                                
032500     05  P-TO-P-KDMFSFOR         PIC X(1).                                
032600     05  P-TO-P-DATA             PIC X(100) VALUE ALL '+'.                
032700                                                                          
032800     EJECT                                                                
032900 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA4'.        
033000*  AREA FÖR DISPATCHEN                                                    
033100 01  P-TO-P-AREA4.                                                        
033200     03  P-TO-P4-LL              PIC S9(4)            COMP SYNC.          
033300     03  P-TO-P4-Z1              PIC  X(1)   VALUE LOW-VALUE.             
033400     03  P-TO-P4-Z2              PIC  X(1)   VALUE LOW-VALUE.             
033500     03  P-TO-P4-TRANSKOD        PIC  X(7).                               
033600     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
033700     03  P-TO-P4-FROM-MID        PIC  X(4).                               
033800     03  P-TO-P4-KDMFSFOR        PIC  X(1).                               
033900     03  P-TO-P4-DATA            PIC  X(1000).                            
034000                                                                          
034100     EJECT                                                                
034200 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA5'.        
034300*  AREA FÖR DISPATCHEN                                                    
034400 01  P-TO-P-AREA5.                                                        
034500     03  P-TO-P5-LL              PIC S9(4)            COMP SYNC.          
034600     03  P-TO-P5-Z1              PIC  X(1)   VALUE LOW-VALUE.             
034700     03  P-TO-P5-Z2              PIC  X(1)   VALUE LOW-VALUE.             
034800     03  P-TO-P5-TRANSKOD        PIC  X(7).                               
034900     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
035000     03  P-TO-P5-FROM-MID        PIC  X(4).                               
035100     03  P-TO-P5-KDMFSFOR        PIC  X(1).                               
035200     03  P-TO-P5-DATA            PIC  X(1000).                            
035300                                                                          
035400     EJECT                                                                
035500******************************************************************        
035600*  SPAR-AREA R32-MID FÖR DISPATCHEN                                       
035700 01 SPAR-AREA-R32.                                                        
035800    03 SPAR4797-MID-R32-POST   OCCURS 500 TIMES.                          
035900      05  SPAR4797-MID-IDDISTR       PIC 9(4)  VALUE ZERO.                
036000      05  SPAR4797-MID-IDKUNDNR      PIC 9(6)  VALUE ZERO.                
036100      05  SPAR4797-MID-IDRAPPNR      PIC 9(7)  VALUE ZERO.                
036200      05  SPAR4797-MID-IDARTNR       PIC 9(8)  VALUE ZERO.                
036300      05  SPAR4797-MID-IDRADNR       PIC 9(4)  VALUE ZERO.                
036400      05  SPAR4797-MID-KVRETINL      PIC 9(6)  VALUE ZERO.                
036500      05  SPAR4797-MID-KVAVV-KVANT   PIC 9(6)  VALUE ZERO.                
036600      05  SPAR4797-MID-KVRETINL-TRP  PIC 9(6)  VALUE ZERO.                
036700      05  SPAR4797-MID-KVRETINL-SKR  PIC 9(6)  VALUE ZERO.                
036800                                                                          
036900******************************************************************        
037000     EJECT                                                                
037100*                                                                         
037200*    --- AREOR FÖR W006KOM SUBMODUL                                       
037300*                                                                         
037400 01  FILLER                      PIC X(16)   VALUE 'MSG-KOM-AREA'.        
037500*01  -COPY WMSGKOM                                                        
037600     EJECT                                                                
037700*                                                                         
037800 01  FILLER                      PIC X(16)   VALUE 'W4I25101'.            
037900 01  FILLER.                                                              
038000   03  OHUV-KOM-AREA.                                                     
038100*    05      -COPY W4I25101   -PRE OHUV-                                  
038200     EJECT                                                                
038300 01  FILLER                      PIC X(16)   VALUE 'W4I25201'.            
038400 01  FILLER.                                                              
038500   03  ORAD-KOM-AREA.                                                     
038600*    05      -COPY W4I25201   -PRE ORAD-                                  
038700     EJECT                                                                
038800*                                                                         
038900 01  FILLER                      PIC X(16)   VALUE 'W4I79701'.            
039000 01  FILLER.                                                              
039100    03 R32-KOM-AREA.                                                      
039200      05    -COPY W4I79701 -PRE MOD4797-                                  
039300     EJECT                                                                
039400                                                                          
039500     SKIP2                                                                
039600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
039700*                                                                         
039800     EJECT                                                                
039900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
040000     SKIP3                                                                
040100 01  W-MINKEY-X.                                                          
040200     03  W-MINKEY-IDTRANS          PIC X(4)    VALUE '4737'.              
040300     03  W-MINKEY-WDA211KY-ENTER.                                         
040400         05  W-MINKEY-IDARTNR      PIC S9(9)   COMP-3 VALUE ZERO.         
040500         05  W-MINKEY-IDRADNR      PIC S9(5)   COMP-3 VALUE ZERO.         
040600     03  W-MINKEY-WDA211KY-NEXT.                                          
040700         05  W-MINKEY-IDARTNR-NEXT PIC S9(9)   COMP-3 VALUE ZERO.         
040800         05  W-MINKEY-IDRADNR-NEXT PIC S9(5)   COMP-3 VALUE ZERO.         
040900     SKIP3                                                                
041000                                                                          
041100 01  NYCKLAR-TILL-DLI.                                                    
041200                                                                          
041300     03  W-IDLEVANM-X.                                                    
041400         05  W-IDDISTR           PIC S9(5)   COMP-3 VALUE ZERO.           
041500         05  W-IDKUNDNR          PIC S9(7)   COMP-3 VALUE ZERO.           
041600         05  W-IDRAPPNR          PIC  9(7)          VALUE ZERO.           
041700                                                                          
041800     03  W-WDA211KY-X.                                                    
041900         05  W-IDARTNR-A2        PIC S9(9)   COMP-3 VALUE ZERO.           
042000         05  W-IDRADNR-A2        PIC S9(5)   COMP-3 VALUE ZERO.           
042100                                                                          
042200     03  W-WDA211KY-MIN-X.                                                
042300         05  W-IDARTNR-A2-MIN    PIC S9(9)   COMP-3 VALUE ZERO.           
042400         05  W-IDRADNR-A2-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
042500                                                                          
042600     03  W-WDA2E1KY-MIN-X.                                                
042700         05  W-IDDC-E1-MIN       PIC  X(2)          VALUE SPACE.          
042800         05  W-IDILIST-E1-MIN    PIC  9(5)          VALUE ZERO.           
042900         05  FILLER              PIC X(29)          VALUE SPACE.          
043000                                                                          
043100     03  W-WDA2E1KY-MAX-X.                                                
043200         05  W-IDDC-E1-MAX       PIC  X(2)          VALUE SPACE.          
043300         05  W-IDILIST-E1-MAX    PIC  9(5)          VALUE ZERO.           
043400         05  FILLER              PIC X(29)          VALUE SPACE.          
043500                                                                          
043600     03  W-WDA3FSEQ-X.                                                    
043700         05  W-IDDC-FSEQ         PIC  X(2)          VALUE SPACE.          
043800         05  W-IDDISTR-FSEQ      PIC S9(5)   COMP-3 VALUE ZERO.           
043900         05  W-IDKUNDNR-FSEQ     PIC S9(7)   COMP-3 VALUE ZERO.           
044000         05  W-IDRAPPNR-FSEQ     PIC  9(7)   VALUE ZERO.                  
044100                                                                          
044200     03  W-WDA3FSEQ-MIN-X.                                                
044300         05  W-IDDC-FSEQ-MIN     PIC  X(2)          VALUE SPACE.          
044400         05  W-IDDISTR-FSEQ-MIN  PIC S9(5)   COMP-3 VALUE ZERO.           
044500         05  W-IDKUNDNR-FSEQ-MIN PIC S9(7)   COMP-3 VALUE ZERO.           
044600         05  W-IDRAPPNR-FSEQ-MIN PIC  9(7)   VALUE ZERO.                  
044700                                                                          
044800     03  W-WDA3FSEQ-MAX-X.                                                
044900         05  W-IDDC-FSEQ-MAX     PIC  X(2)          VALUE SPACE.          
045000         05  W-IDDISTR-FSEQ-MAX  PIC S9(5)   COMP-3 VALUE ZERO.           
045100         05  W-IDKUNDNR-FSEQ-MAX PIC S9(7)   COMP-3 VALUE ZERO.           
045200         05  W-IDRAPPNR-FSEQ-MAX PIC  9(7)   VALUE ZERO.                  
045300                                                                          
045400     03  W-IDARTNR-X.                                                     
045500         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
045600     03  W-KDKVAINF-X.                                                    
045700         05  W-KDKVAINF          PIC  X(1)   VALUE 'R'.                   
045800                                                                          
045900     03  W-IDDC-X.                                                        
046000         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
046100                                                                          
046200     03  W-IDSKYLT-X.                                                     
046300         05  W-IDSKYLT           PIC  X(3)   VALUE SPACE.                 
046400                                                                          
046500     03  W-IDFTG-X.                                                       
046600         05  W-IDFTG             PIC  9(2)   VALUE ZERO.                  
046700                                                                          
046800     03  W-WDGXKEY-4111-X.                                                
046900         05  FILLER              PIC  X(4)   VALUE '4111'.                
047000         05  W-IDRT-4111         PIC  X(3)   VALUE 'CDC'.                 
047100         05  FILLER              PIC  X(23)  VALUE LOW-VALUE.             
047200                                                                          
047300     03  W-IDDC-B6-X.                                                     
047400         05 W-IDDC-B6                  PIC X(2).                          
047500                                                                          
047600     SKIP2                                                                
047700*    --- STATUS-KOD FRÅN IMS                                              
047800 01  STATUS-WS                   PIC XX.                                  
047900     88  STATUS-OK                           VALUE '  '.                  
048000     88  SEGMENT-FINNS                       VALUE '  '.                  
048100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
048200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
048300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
048400     88  TRANSKOD-FEL                        VALUE 'A1'.                  
048500     88  SECURITY-FEL                        VALUE 'A4'.                  
048600     SKIP2                                                                
048700 01  GODK-STATUSKODER.                                                    
048800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
048900     SKIP3                                                                
049000 01  SSA1                        PIC X(192).                              
049100 01  SSA2                        PIC X(64).                               
049200     EJECT                                                                
049300*    --- IMS FUNKTIONSKODER                                               
049400*01  -COPY W0003                                                          
049500     EJECT                                                                
049600*    ---  DLI INPUT-OUTPUT AREA                                           
049700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
049800     SKIP3                                                                
049900 01  DLI-IO-AREA.                                                         
050000     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
050100     SKIP3                                                                
050200     03  WLKREE01 REDEFINES IO-AREA.                                      
050300*        05  -COPY WDA201                                                 
050400     EJECT                                                                
050500     03  WLKREE11 REDEFINES IO-AREA.                                      
050600*        05  -COPY WDA211                                                 
050700     EJECT                                                                
050800     03  WLKREJ01 REDEFINES IO-AREA.                                      
050900*        05  -COPY WDA2E1                                                 
051000     EJECT                                                                
051100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
051200     SKIP3                                                                
051300 01  DLI-IO-AREA2.                                                        
051400     03  IO-AREA2                PIC X(1200)  VALUE SPACE.                
051500     SKIP3                                                                
051600     03  WLKREE21 REDEFINES IO-AREA2.                                     
051700*        05  -COPY WDA221                                                 
051800     SKIP2                                                                
051900 01  DLI-IO-AREA4.                                                        
052000     03  IO-AREA4                PIC X(32)  VALUE SPACE.                  
052100     SKIP3                                                                
052200     03  WL411111 REDEFINES IO-AREA4.                                     
052300*        05  -COPY WDGX4112                                               
052400     SKIP2                                                                
052500 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDA301'.        
052600 01  DLI-IO-WDA301.                                                       
052700*    03  -COPY WDA301                                                     
052800     SKIP2                                                                
052900 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK611'.        
053000 01  DLI-IO-WDK611.                                                       
053100*    03  -COPY WDK611                                                     
053200     SKIP2                                                                
053300 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK711'.        
053400 01  DLI-IO-WDK711.                                                       
053500*    03  -COPY WDK711                                                     
053600     SKIP2                                                                
053700 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDD311'.        
053800 01  DLI-IO-WDD311.                                                       
053900*    03  -COPY WDD311                                                     
054000     SKIP2                                                                
054100 01  FILLER                      PIC X(16)   VALUE 'WDB601 AREA'.         
054200 01   DLI-IO-AREA-B601.                                                   
054300*     03  -COPY WDB601                                                    
054400 01  FILLER                      PIC X(16) VALUE 'W6D211   AREA'.         
054500 01   DLI-IO-AREA-6D21.                                                   
054600*     03  -COPY W6D211                                                    
054700     EJECT                                                                
054800 LINKAGE SECTION.                                                         
054900 01  MSG-PCB                     PIC X.                                   
055000     EJECT                                                                
055100*01  -COPY W0009   -PRE DISP-                                             
055200     EJECT                                                                
055300*01  -COPY W0008   -PRE USEA-                                             
055400     05  FILLER                  PIC X.                                   
055500     EJECT                                                                
055600 01  ATAB-PCB                    PIC X.                                   
055700     EJECT                                                                
055800*01  -COPY W0008  -PRE KREE-                                              
055900     05  FILLER                  PIC X.                                   
056000     EJECT                                                                
056100*01  -COPY W0008  -PRE KREJ-                                              
056200     05  FILLER                  PIC X.                                   
056300     EJECT                                                                
056400*01  -COPY W0008  -PRE RETA-                                              
056500     05  FILLER                  PIC X.                                   
056600     EJECT                                                                
056700*01  -COPY W0008  -PRE BENA-                                              
056800     05  FILLER                  PIC X.                                   
056900     EJECT                                                                
057000*01  -COPY W0008  -PRE 4111-                                              
057100     05  FILLER                  PIC X.                                   
057200     EJECT                                                                
057300*01  -COPY W0008  -PRE ARTS-                                              
057400     05  FILLER                  PIC X.                                   
057500     EJECT                                                                
057600*01  -COPY W0008  -PRE KOMA-                                              
057700     05  FILLER                  PIC X.                                   
057800     EJECT                                                                
057900*01  -COPY W0008  -PRE WDB6-                                              
058000     05  FILLER                  PIC X.                                   
058100*01  -COPY W0008  -PRE ARTC-                                              
058200     05  FILLER                  PIC X.                                   
058300*01  -COPY W0008   -PRE KVAH-                                             
058400     05  FILLER                  PIC X.                                   
058500     EJECT                                                                
058600 PROCEDURE DIVISION  USING MSG-PCB  DISP-PCB ATAB-PCB                     
058700                           KREE-PCB KREJ-PCB RETA-PCB                     
058800                           BENA-PCB 4111-PCB                              
058900                           ARTS-PCB KOMA-PCB WDB6-PCB                     
059000                           ARTC-PCB KVAH-PCB.                             
059100                                                                          
059200 MAIN SECTION.                                                            
059300     ENTRY 'DLITCBL' USING MSG-PCB  DISP-PCB ATAB-PCB                     
059400                           KREE-PCB KREJ-PCB RETA-PCB                     
059500                           BENA-PCB 4111-PCB                              
059600                           ARTS-PCB KOMA-PCB WDB6-PCB                     
059700                           ARTC-PCB KVAH-PCB.                             
059800                                                                          
059900                                                                          
060000     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
060100     IF SUB-KDRC = 0                                                      
060200       IF REQU-KDPGMACT = 'S' OR 'E' OR 'C'                               
060300         PERFORM A-INIT                                                   
060400         PERFORM B-KOLLA-NYCKLAR                                          
060500         IF NYCKLAR-OK                                                    
060600           IF REQU-KDPGMACT =  'E'                                        
060700             PERFORM G-KOLLA-INPUT                                        
060800           END-IF                                                         
060900                                                                          
061000           IF REQU-KDPGMACT =  'E'                                        
061100             IF INDATA-OK                                                 
061200               PERFORM H-UPPDATERA-SKRIV-UT                               
061300             END-IF                                                       
061400           END-IF                                                         
061500           IF INDATA-OK                                                   
061600             PERFORM F-LAES-VISA-INFO                                     
061700           END-IF                                                         
061800         END-IF                                                           
061900       ELSE                                                               
062000         MOVE SYSTEM-ERROR      TO RESP-IDMSG-ERROR                       
062100       END-IF                                                             
062200                                                                          
062300       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
062400       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
062500       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
062600                                                                          
062700       IF WS-IDMSG-ERROR NOT = SPACE                                      
062800         IF SUB-KDTRANS(1:6) = 'WLA152'                                   
062900           MOVE LOW-VALUES       TO RESP-AREA                             
063000         ELSE                                                             
063100           MOVE ALL '+'          TO RESP-AREA                             
063200           MOVE +1               TO INDX                                  
063300           PERFORM UNTIL INDX > MAX-INDX                                  
063400                                                                          
063500*      -- BEART SKA VARA PLUS '+' I UNICODE                               
063600             MOVE ALL X'2B'      TO RESP-BEART(INDX)                      
063700                                                                          
063800             ADD +1              TO INDX                                  
063900           END-PERFORM                                                    
064000         END-IF                                                           
064100                                                                          
064200         MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                        
064300         MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                       
064400         MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                         
064500         MOVE 001              TO RESP-IDRESVER                           
064600         IF REQU-KDPGMACT = 'S' OR 'C'                                    
064700           MOVE ZERO             TO RESP-KVRADER-MAX1                     
064800         ELSE                                                             
064900           IF REQU-KVRADER-MAX1 NUMERIC AND                               
065000              WS-IDMSG-ERROR NOT = '099'                                  
065100             MOVE REQU-KVRADER-MAX1 TO RESP-KVRADER-MAX1                  
065200           ELSE                                                           
065300             MOVE ZERO             TO RESP-KVRADER-MAX1                   
065400           END-IF                                                         
065500         END-IF                                                           
065600       END-IF                                                             
065700       IF SUB-KDTRANS(1:6) = 'WLA152'                                     
065800         PERFORM S11-MSG-CONV                                             
065900       END-IF                                                             
066000       PERFORM S02-RETURN-RESPONSE                                        
066100     END-IF                                                               
066200                                                                          
066300     MOVE ZERO TO RETURN-CODE                                             
066400     GOBACK                                                               
066500     .                                                                    
066600     EJECT                                                                
066700 A-INIT SECTION.                                                          
066800                                                                          
066900     MOVE LOW-VALUE             TO W-WDA211KY-MIN-X                       
067000                                   W-WDA2E1KY-MIN-X                       
067100                                   W-WDA3FSEQ-MIN-X                       
067200                                                                          
067300     MOVE HIGH-VALUE            TO W-WDA2E1KY-MAX-X                       
067400                                   W-WDA3FSEQ-MAX-X                       
067500                                                                          
067600     MOVE 'IDAG ' TO DAT-KDDATFORM                                        
067700     MOVE ZERO    TO DAT-I-TIDATUM                                        
067800                     DAT-O-TIDATUM                                        
067900     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
068000                         DAT-O-TIDATUM DAT-KDSVAR                         
068100     IF DAT-KDSVAR-FEL                                                    
068200        MOVE 'FEL FRÅN DATKONV' TO FELTEXT                                
068300        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
068400     END-IF                                                               
068500                                                                          
068600     IF SUB-KDTRANS(1:6) = 'WLA152'                                       
068700       MOVE LOW-VALUES           TO RESP-AREA                             
068800     ELSE                                                                 
068900       MOVE ALL '+'              TO RESP-AREA                             
069000     END-IF                                                               
069100     MOVE SPACE      TO RESP-IDMSG-ERROR                                  
069200                        RESP-IDMSG-INFO                                   
069300                        RESP-IDELMT-ERROR                                 
069400     MOVE 001        TO RESP-IDRESVER                                     
069500     MOVE ZERO       TO RESP-KVRADER-MAX1                                 
069600     MOVE SPACE      TO RESP-WL0152O1                                     
069700     MOVE +1         TO INDX                                              
069800     PERFORM UNTIL INDX        > MAX-INDX                                 
069900                                                                          
070000*    -- BEART SKA VARA SPACE I UNICODE                                    
070100       MOVE ALL X'20'  TO RESP-BEART(INDX)                                
070200                                                                          
070300       ADD +1               TO INDX                                       
070400     END-PERFORM                                                          
070500     IF SUB-KDTRANS(1:6) = 'WLA152'                                       
070600       MOVE 001                  TO AUTH-KDCALL                           
070700       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
070800                                    REQU-WZ01REQ2                         
070900       IF AUTH-KDRC > 0                                                   
071000         MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                      
071100         MOVE NOO                TO KEYS-SW                               
071200       END-IF                                                             
071300       MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY) TO                        
071400                                 REQU-IDDC-KEY                            
071500       MOVE FUNCTION UPPER-CASE (REQU-FLTOT-KEY) TO                       
071600                                 REQU-FLTOT-KEY                           
071700       MOVE FUNCTION UPPER-CASE (REQU-IDRT-KEY) TO                        
071800                                 REQU-IDRT-KEY                            
071900       MOVE FUNCTION UPPER-CASE (REQU-FLILI) TO                           
072000                                 REQU-FLILI                               
072100       MOVE FUNCTION UPPER-CASE (REQU-FLKLAR) TO                          
072200                                 REQU-FLKLAR                              
072300       MOVE FUNCTION UPPER-CASE (REQU-FLSKROT) TO                         
072400                                 REQU-FLSKROT                             
072500       MOVE FUNCTION UPPER-CASE (REQU-FLSKRIV) TO                         
072600                                 REQU-FLSKRIV                             
072700       MOVE FUNCTION UPPER-CASE (REQU-FLANTAVV) TO                        
072800                                 REQU-FLANTAVV                            
072900       MOVE +1 TO INDX                                                    
073000       PERFORM UNTIL INDX > MAX-INDX                                      
073100        MOVE FUNCTION UPPER-CASE (REQU-KDCMD(INDX)) TO                    
073200                                  REQU-KDCMD(INDX)                        
073300        ADD +1 TO INDX                                                    
073400       END-PERFORM                                                        
073500     END-IF                                                               
073600                                                                          
073700     IF REQU-KDPGMACT = 'C' OR 'E'                                        
073800       IF REQU-IDANSTNR-UPD   NOT = ALL '+'  AND                          
073900          REQU-IDANSTNR-UPD NUMERIC                                       
074000         MOVE REQU-IDANSTNR-UPD TO RESP-IDANSTNR-UPD                      
074100       END-IF                                                             
074200     END-IF                                                               
074300                                                                          
074400     ACCEPT W-TIKLOCK-R32      FROM TIME                                  
074500     ACCEPT W-TIKLOCK-ORDER    FROM TIME                                  
074600                                                                          
074700     MOVE NEJ     TO  ALLT-SKR-ORDER-SKAPAD-SW                            
074800     MOVE NEJ     TO  UPPD-VALDA-RADER-SW                                 
074900                                                                          
075000     .                                                                    
075100     EJECT                                                                
075200 B-KOLLA-NYCKLAR SECTION.                                                 
075300                                                                          
075400*    MOVE 'GB'                  TO W-IDSKYLT                              
075500                                                                          
075600     MOVE JA TO NYCKLAR-SW                                                
075700                                                                          
075800     PERFORM BA-KOLLA-IDDISTR                                             
075900     PERFORM BB-KOLLA-IDKUNDNR                                            
076000     PERFORM BC-KOLLA-IDRAPPNR                                            
076100     PERFORM BD-KOLLA-IDARTNR                                             
076200     PERFORM BE-KOLLA-FLTOT                                               
076300     MOVE REQU-IDDC-KEY        TO W-IDDC-E1-MIN                           
076400                                  W-IDDC-E1-MAX                           
076500                                  W-IDDC-FSEQ                             
076600                                  W-IDDC-FSEQ-MIN                         
076700                                  W-IDDC-FSEQ-MAX                         
076800                                  RESP-IDDC-KEY                           
076900                                  WS-IDDC                                 
077000     MOVE REQU-IDRT-KEY        TO W-IDRT-4111                             
077100                                                                          
077200*    FIX TO MAKE IT POSSIBLE FOR A SPECIFIC USER TO HANDLE                
077300*    RETURNS FROM CA (FTG=54) TO US (DC=44, FTG=53)                       
077400*    IF REQU-IDUSER = 'PHCA4G1'                                           
077500*      MOVE '54'               TO W-IDFTG                                 
077600*    END FIX                                                              
077700*    ELSE                                                                 
077800     IF REQU-IDFTG-KEY NOT NUMERIC                                        
077900       MOVE NEJ                    TO NYCKLAR-SW                          
078000       MOVE 'IDFTG'                TO RESP-IDELMT-ERROR                   
078100       MOVE '023'                  TO RESP-IDMSG-ERROR                    
078200     ELSE                                                                 
078300       MOVE REQU-IDFTG-KEY         TO W-IDFTG                             
078400     END-IF                                                               
078500                                                                          
078600     IF NYCKLAR-OK                                                        
078700       IF REQU-IDDISTR-KEY NUMERIC                                        
078800         MOVE REQU-IDDISTR-KEY     TO RESP-IDDISTR-KEY                    
078900         INSPECT RESP-IDDISTR-KEY  REPLACING LEADING ZERO BY SPACE        
079000       END-IF                                                             
079100                                                                          
079200       IF REQU-IDKUNDNR-KEY  NUMERIC                                      
079300         MOVE REQU-IDKUNDNR-KEY    TO RESP-IDKUNDNR-KEY                   
079400         INSPECT RESP-IDKUNDNR-KEY REPLACING LEADING ZERO BY SPACE        
079500       END-IF                                                             
079600                                                                          
079700       IF REQU-IDRAPPNR-KEY  NUMERIC                                      
079800         MOVE REQU-IDRAPPNR-KEY    TO RESP-IDRAPPNR-KEY                   
079900         INSPECT RESP-IDRAPPNR-KEY REPLACING LEADING ZERO BY SPACE        
080000       END-IF                                                             
080100                                                                          
080200       IF W-IDARTNR NUMERIC                                               
080300         MOVE W-IDARTNR            TO RESP-IDARTNR-KEY                    
080400         INSPECT RESP-IDARTNR-KEY  REPLACING LEADING ZERO BY SPACE        
080500       END-IF                                                             
080600                                                                          
080700       MOVE WS-FLTOT               TO RESP-FLTOT-KEY                      
080800     END-IF                                                               
080900     .                                                                    
081000     EJECT                                                                
081100                                                                          
081200                                                                          
081300 BA-KOLLA-IDDISTR  SECTION.                                               
081400                                                                          
081500     IF REQU-IDDISTR-KEY NUMERIC AND REQU-IDDISTR-KEY > ZERO              
081600       MOVE REQU-IDDISTR-KEY    TO W-IDDISTR                              
081700     ELSE                                                                 
081800       MOVE NEJ                 TO NYCKLAR-SW                             
081900       MOVE '023'                TO RESP-IDMSG-ERROR                      
082000       MOVE 'IDDISTR'            TO RESP-IDELMT-ERROR                     
082100     END-IF                                                               
082200                                                                          
082300     .                                                                    
082400     EJECT                                                                
082500                                                                          
082600 BB-KOLLA-IDKUNDNR   SECTION.                                             
082700                                                                          
082800     IF REQU-IDKUNDNR-KEY       NUMERIC                                   
082900       MOVE REQU-IDKUNDNR-KEY   TO W-IDKUNDNR                             
083000     ELSE                                                                 
083100       MOVE NEJ                 TO NYCKLAR-SW                             
083200       MOVE '023'                TO RESP-IDMSG-ERROR                      
083300       MOVE 'IDDISTR'            TO RESP-IDELMT-ERROR                     
083400     END-IF                                                               
083500                                                                          
083600     .                                                                    
083700     EJECT                                                                
083800 BC-KOLLA-IDRAPPNR   SECTION.                                             
083900                                                                          
084000     IF REQU-IDRAPPNR-KEY       NUMERIC                                   
084100       MOVE REQU-IDRAPPNR-KEY   TO W-IDRAPPNR                             
084200     ELSE                                                                 
084300       MOVE NEJ                 TO NYCKLAR-SW                             
084400       MOVE '023'                TO RESP-IDMSG-ERROR                      
084500       MOVE 'IDRAPPNR'           TO RESP-IDELMT-ERROR                     
084600     END-IF                                                               
084700                                                                          
084800     .                                                                    
084900     EJECT                                                                
085000 BD-KOLLA-IDARTNR   SECTION.                                              
085100                                                                          
085200     IF REQU-IDARTNR-KEY NOT = ALL '+'                                    
085300       IF REQU-IDARTNR-KEY    NUMERIC AND REQU-IDARTNR-KEY > ZERO         
085400         MOVE REQU-IDARTNR-KEY    TO W-IDARTNR-A2-MIN                     
085500                                     W-IDARTNR-A2                         
085600                                     W-IDARTNR                            
085700       END-IF                                                             
085800     END-IF                                                               
085900                                                                          
086000     .                                                                    
086100     EJECT                                                                
086200 BE-KOLLA-FLTOT      SECTION.                                             
086300                                                                          
086400                                                                          
086500     MOVE REQU-FLTOT-KEY TO WS-FLTOT                                      
086600     IF WS-FLTOT = JA OR YES                                              
086700       CONTINUE                                                           
086800     ELSE                                                                 
086900       MOVE NEJ    TO WS-FLTOT                                            
087000     END-IF                                                               
087100                                                                          
087200     .                                                                    
087300     EJECT                                                                
087400 F-LAES-VISA-INFO SECTION.                                                
087500                                                                          
087600     PERFORM IMS-GHU-WLKREE01                                             
087700                                                                          
087800     IF SEGMENT-SAKNAS                                                    
087900        MOVE 'IDLEVANM'         TO RESP-IDELMT-ERROR                      
088000        MOVE '025'              TO RESP-IDMSG-ERROR                       
088100     ELSE                                                                 
088200       PERFORM FA-REDIGERA-ANM-UPPGIFTER                                  
088300       PERFORM FF-LAES-NAESTA-RETILLRAD                                   
088400       PERFORM FB-FIXA-ENTER-KEY                                          
088500       MOVE +1                  TO INDX                                   
088600                                                                          
088700       PERFORM UNTIL INDX        > MAX-INDX                               
088800         IF SEGMENT-FINNS                                                 
088900                                                                          
089000            COMPUTE W-KVLEVANM-KVAR   =  LEV-KVLEVANM-BEKR -              
089100                                         LEV-KVRETINL -                   
089200                                         LEV-KVAVV-KVANT -                
089300                                         LEV-KVRETINL-SKR -               
089400                                         LEV-KVAVV-KVAL -                 
089500                                         LEV-KVANTAL-ILI                  
089600                                                                          
089700            IF WS-FLTOT = JA OR YES   OR                                  
089800              W-KVLEVANM-KVAR > ZERO                                      
089900              PERFORM FC-REDIGERA-RAD-UPPGIFTER                           
090000              ADD +1               TO INDX                                
090100                                      WS-COUNT                            
090200            END-IF                                                        
090300                                                                          
090400            PERFORM FF-LAES-NAESTA-RETILLRAD                              
090500         ELSE                                                             
090600            ADD +1               TO INDX                                  
090700         END-IF                                                           
090800       END-PERFORM                                                        
090900                                                                          
091000       IF REQU-KDPGMACT = 'S' OR 'C'                                      
091100        MOVE 'N'              TO       RESP-FLILI                         
091200        MOVE 'N'              TO       RESP-FLKLAR                        
091300        MOVE 'N'              TO       RESP-FLSKROT                       
091400        MOVE 'N'              TO       RESP-FLANTAVV                      
091500        MOVE 'N'              TO       RESP-FLSKRIV                       
091600      ELSE                                                                
091700        MOVE REQU-FLILI       TO       RESP-FLILI                         
091800        MOVE REQU-FLKLAR      TO       RESP-FLKLAR                        
091900        MOVE REQU-FLSKROT     TO       RESP-FLSKROT                       
092000        MOVE REQU-FLANTAVV    TO       RESP-FLANTAVV                      
092100        MOVE REQU-FLSKRIV     TO       RESP-FLSKRIV                       
092200      END-IF                                                              
092300                                                                          
092400                                                                          
092500       IF WS-COUNT < 501                                                  
092600         CONTINUE                                                         
092700       ELSE                                                               
092800         MOVE NUM-OF-LINES       TO RESP-IDMSG-ERROR                      
092900       END-IF                                                             
093000                                                                          
093100       MOVE WS-COUNT             TO RESP-KVRADER-MAX1                     
093200                                                                          
093300       PERFORM FE-FIXA-NEXT-KEY                                           
093400                                                                          
093500     END-IF                                                               
093600     .                                                                    
093700     EJECT                                                                
093800                                                                          
093900 FA-REDIGERA-ANM-UPPGIFTER SECTION.                                       
094000                                                                          
094100     MOVE ANM-KVRADER-RT     TO RESP-KVRADER-TOT                          
094200     MOVE ANM-KVRADER-OBEH   TO RESP-KVRADER-OBEH                         
094300     MOVE ANM-DARETILL (3:6) TO RESP-TIRETILL                             
094400                                                                          
094500     IF ANM-KDLEVANM         =  W-ANM-MOT  OR                             
094600        ANM-KDLEVANM         =  W-ANM-PAAB OR                             
094700        ANM-KDLEVANM         =  W-ANM-KLAR                                
094800        PERFORM FAA-LAES-RETUR-UPPGIFTER                                  
094900     END-IF                                                               
095000     .                                                                    
095100     EJECT                                                                
095200                                                                          
095300 FAA-LAES-RETUR-UPPGIFTER SECTION.                                        
095400                                                                          
095500     MOVE ANM-IDDISTR     TO W-IDDISTR-FSEQ-MIN                           
095600                             W-IDDISTR-FSEQ-MAX                           
095700     MOVE ANM-IDKUNDNR    TO W-IDKUNDNR-FSEQ-MIN                          
095800                             W-IDKUNDNR-FSEQ-MAX                          
095900     MOVE ANM-IDRAPPNR    TO W-IDRAPPNR-FSEQ-MIN                          
096000                             W-IDRAPPNR-FSEQ-MAX                          
096100                                                                          
096200     PERFORM IMS-GU-WLRETA01                                              
096300                                                                          
096400     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
096500                                  OR RET-TIINLMOT > ZERO                  
096600         PERFORM IMS-GN-WLRETA01                                          
096700     END-PERFORM                                                          
096800     IF SEGMENT-SAKNAS OR SEGMENT-SLUT                                    
096900      CONTINUE                                                            
097000     ELSE                                                                 
097100         MOVE RET-TIINLMOT      TO RESP-TIINLMOT                          
097200     END-IF                                                               
097300                                                                          
097400     .                                                                    
097500     EJECT                                                                
097600 FB-FIXA-ENTER-KEY        SECTION.                                        
097700                                                                          
097800     IF SEGMENT-FINNS                                                     
097900        MOVE '4737'                 TO W-MINKEY-IDTRANS                   
098000        MOVE LEV-IDARTNR            TO W-MINKEY-IDARTNR                   
098100        MOVE LEV-IDRADNR            TO W-MINKEY-IDRADNR                   
098200     ELSE                                                                 
098300        MOVE ZERO                   TO W-MINKEY-IDARTNR                   
098400                                       W-MINKEY-IDRADNR                   
098500     END-IF                                                               
098600                                                                          
098700     .                                                                    
098800     EJECT                                                                
098900                                                                          
099000 FC-REDIGERA-RAD-UPPGIFTER  SECTION.                                      
099100                                                                          
099200                                                                          
099300     MOVE W-KVLEVANM-KVAR      TO RESP-KVANTAL-KVAR (INDX)                
099400                                                                          
099500     MOVE LEV-IDARTNR          TO RESP-IDARTNR  (INDX)                    
099600     MOVE LEV-KDANMORS         TO RESP-KDANMORS (INDX)                    
099700     MOVE LEV-IDRADNR          TO RESP-IDRADNR  (INDX)                    
099800     IF LEV-IDILIST            >  ZERO                                    
099900        MOVE LEV-IDILIST       TO RESP-IDILIST  (INDX)                    
100000     ELSE                                                                 
100100        MOVE ZERO              TO RESP-IDILIST  (INDX)                    
100200     END-IF                                                               
100300                                                                          
100400     PERFORM FCA-FIXA-ART-UPPGIFTER                                       
100500     PERFORM FCB-KOLLA-OM-TEXTINFO                                        
100600     .                                                                    
100700     EJECT                                                                
100800                                                                          
100900 FCA-FIXA-ART-UPPGIFTER  SECTION.                                         
101000                                                                          
101100     MOVE LEV-IDARTNR          TO W-IDARTNR                               
101200                                                                          
101300     MOVE REQU-IDDC-KEY TO W-IDDC                                         
101400                           W-IDDC-B6                                      
101500     IF DCS-CDC                                                           
101600      PERFORM IMS-GU-WLARTC11                                             
101700     ELSE                                                                 
101800      PERFORM IMS-GU-WLARTS11                                             
101900     END-IF                                                               
102000     IF SEGMENT-FINNS                                                     
102100      IF DCS-CDC                                                          
102200        MOVE CLAG-ADLAGOMR     TO RESP-ADLAGOMR (INDX)                    
102300        MOVE CLAG-ADGANG       TO WS-ADGANG                               
102400        MOVE WS-ADGANG         TO RESP-ADGANG   (INDX)                    
102500        MOVE CLAG-ADPLATS      TO RESP-ADPLATS  (INDX)                    
102600      ELSE                                                                
102700        MOVE SLAG-ADLAGOMR     TO RESP-ADLAGOMR (INDX)                    
102800        MOVE SLAG-ADGANG       TO WS-ADGANG                               
102900        MOVE WS-ADGANG         TO RESP-ADGANG   (INDX)                    
103000        MOVE SLAG-ADPLATS      TO RESP-ADPLATS  (INDX)                    
103100      END-IF                                                              
103200     END-IF                                                               
103300                                                                          
103400     PERFORM IMS-GU-WDB601                                                
103500                                                                          
103600     MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
103700     IF DCS-UNICODE-IDSKYLT                                               
103800        MOVE 'UTF8'             TO TRAUTF8-KDCP                           
103900     ELSE                                                                 
104000        MOVE '278 '             TO TRAUTF8-KDCP                           
104100     END-IF                                                               
104200                                                                          
104300     PERFORM IMS-GU-WLBENA11                                              
104400     IF SEGMENT-FINNS                                                     
104500        MOVE TEXT-BEART        TO TRAUTF8-TECONV-FROM                     
104600     ELSE                                                                 
104700        MOVE SPACE             TO TRAUTF8-TECONV-FROM                     
104800        MOVE WS-CP-278         TO TRAUTF8-KDCP                            
104900     END-IF                                                               
105000     IF TRAUTF8-TECONV-FROM = SPACES                                      
105100      MOVE 'GB'  TO W-IDSKYLT                                             
105200      MOVE '278' TO TRAUTF8-KDCP                                          
105300      PERFORM IMS-GU-WLBENA11                                             
105400      MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
105500     END-IF                                                               
105600                                                                          
105700*    -- STRIP SPACE OR CONVERT TO UNICODE                                 
105800     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
105900                                                                          
106000*    -- MOVE CONVERTED DESCRIPTION TO THE RESPONSE                        
106100     MOVE TRAUTF8-TECONV-TO   TO RESP-BEART (INDX)                        
106200                                                                          
106300     .                                                                    
106400     EJECT                                                                
106500                                                                          
106600 FCB-KOLLA-OM-TEXTINFO   SECTION.                                         
106700                                                                          
106800     MOVE LEV-IDARTNR           TO W-IDARTNR-A2                           
106900     MOVE LEV-IDRADNR           TO W-IDRADNR-A2                           
107000     PERFORM IMS-GNP-WLKREE21                                             
107100     IF SEGMENT-FINNS                                                     
107200          MOVE YES                TO RESP-FLTEXT (INDX)                   
107300          IF TXT-TEANMNOT-REG(1) = SPACE AND                              
107400             TXT-TEANMNOT-REG(2) = SPACE AND                              
107500             TXT-TEANMNOT-REG(3) = SPACE AND                              
107600             TXT-TEANMNOT-ADM(1) = SPACE AND                              
107700             TXT-TEANMNOT-ADM(2) = SPACE AND                              
107800             TXT-TEANMNOT-ADM(3) = SPACE                                  
107900            CONTINUE                                                      
108000          ELSE                                                            
108100            IF TXT-TEANMNOT-REG(1) NOT  = SPACE                           
108200              MOVE TXT-TEANMNOT-REG(1)  TO                                
108300                               RESP-TEANMNOT-REG (INDX,1)                 
108400            END-IF                                                        
108500            IF TXT-TEANMNOT-REG(2) NOT  = SPACE                           
108600              MOVE TXT-TEANMNOT-REG(2)  TO                                
108700                               RESP-TEANMNOT-REG (INDX,2)                 
108800            END-IF                                                        
108900            IF TXT-TEANMNOT-REG(3) NOT  = SPACE                           
109000              MOVE TXT-TEANMNOT-REG(3)  TO                                
109100                               RESP-TEANMNOT-REG (INDX,3)                 
109200            END-IF                                                        
109300            IF TXT-TEANMNOT-ADM(1) NOT  = SPACE                           
109400              MOVE TXT-TEANMNOT-ADM(1)  TO                                
109500                                 RESP-TEANMNOT-ADM (INDX,1)               
109600            END-IF                                                        
109700            IF TXT-TEANMNOT-ADM(2) NOT  = SPACE                           
109800              MOVE TXT-TEANMNOT-ADM(2)  TO                                
109900                                 RESP-TEANMNOT-ADM (INDX,2)               
110000            END-IF                                                        
110100            IF TXT-TEANMNOT-ADM(3) NOT  = SPACE                           
110200              MOVE TXT-TEANMNOT-ADM(3)  TO                                
110300                                 RESP-TEANMNOT-ADM (INDX,3)               
110400            END-IF                                                        
110500          END-IF                                                          
110600     END-IF                                                               
110700     PERFORM IMS-GU-W6KVAH11                                              
110800     IF SEGMENT-FINNS                                                     
110900      PERFORM S05-DATE-INTERVAL                                           
111000      IF W-DATE-INTERVAL-OK                                               
111100       IF INFO-TEKVAINF-EXT(1) NOT = SPACE                                
111200         MOVE INFO-TEKVAINF-EXT(1)  TO                                    
111300                RESP-TEKVAINF-EXT (INDX,1)                                
111400       END-IF                                                             
111500       IF INFO-TEKVAINF-EXT(2) NOT = SPACE                                
111600         MOVE INFO-TEKVAINF-EXT(2)  TO                                    
111700                RESP-TEKVAINF-EXT (INDX,2)                                
111800       END-IF                                                             
111900       IF INFO-TEKVAINF-EXT(3) NOT = SPACE                                
112000         MOVE INFO-TEKVAINF-EXT(3)  TO                                    
112100                RESP-TEKVAINF-EXT (INDX,3)                                
112200       END-IF                                                             
112300       IF INFO-TEKVAINF-EXT(4) NOT = SPACE                                
112400         MOVE INFO-TEKVAINF-EXT(4)  TO                                    
112500                RESP-TEKVAINF-EXT (INDX,4)                                
112600       END-IF                                                             
112700       IF INFO-TEKVAINF-EXT(5) NOT = SPACE                                
112800         MOVE INFO-TEKVAINF-EXT(5)  TO                                    
112900                RESP-TEKVAINF-EXT (INDX,5)                                
113000       END-IF                                                             
113100       IF INFO-TEKVAINF-EXT(6) NOT = SPACE                                
113200         MOVE INFO-TEKVAINF-EXT(6)  TO                                    
113300                RESP-TEKVAINF-EXT (INDX,6)                                
113400       END-IF                                                             
113500       IF INFO-TEKVAINF-EXT(7) NOT = SPACE                                
113600         MOVE INFO-TEKVAINF-EXT(7)  TO                                    
113700                RESP-TEKVAINF-EXT (INDX,7)                                
113800       END-IF                                                             
113900      END-IF                                                              
114000     END-IF                                                               
114100     .                                                                    
114200     EJECT                                                                
114300                                                                          
114400 FE-FIXA-NEXT-KEY        SECTION.                                         
114500                                                                          
114600     IF SEGMENT-FINNS                                                     
114700                                                                          
114800        MOVE '4737'                 TO W-MINKEY-IDTRANS                   
114900        MOVE LEV-IDARTNR            TO W-MINKEY-IDARTNR-NEXT              
115000        MOVE LEV-IDRADNR            TO W-MINKEY-IDRADNR-NEXT              
115100     ELSE                                                                 
115200        MOVE ZERO                   TO W-MINKEY-IDARTNR-NEXT              
115300                                       W-MINKEY-IDRADNR-NEXT              
115400     END-IF                                                               
115500                                                                          
115600     .                                                                    
115700     EJECT                                                                
115800 FF-LAES-NAESTA-RETILLRAD SECTION.                                        
115900                                                                          
116000     MOVE NEJ                    TO OKOD-FL-RETILL                        
116100                                    OKOD-FL-INTERNUPPACKNING              
116200     PERFORM IMS-GNP-WLKREE11                                             
116300     PERFORM UNTIL OKOD-FL-RETILL = 'J' OR SEGMENT-SAKNAS                 
116400                OR OKOD-FL-INTERNUPPACKNING = 'J'                         
116500        IF LEV-KDKREBEH(1:1) = 'Y'   OR                                   
116600           LEV-KDKREBEH(1:1) = 'J'   OR                                   
116700           LEV-KDKREBEH(1:1) = 'C'   OR                                   
116800           LEV-KDKREBEH      = 'D01' OR                                   
116900           LEV-KDKREBEH      = 'D02' OR                                   
117000           LEV-KDKREBEH      = 'D03'                                      
117100*--ANROPA KONTROLL AV ORSAKSKODER                                         
117200            MOVE LEV-KDANMORS   TO OKOD-KDANMORS                          
117300            CALL W418OKOD USING OKOD-W418OKOD                             
117400        END-IF                                                            
117500        IF OKOD-FL-RETILL = 'J' OR                                        
117600           OKOD-FL-INTERNUPPACKNING = 'J'                                 
117700           CONTINUE                                                       
117800        ELSE                                                              
117900          PERFORM IMS-GNP-WLKREE11                                        
118000        END-IF                                                            
118100     END-PERFORM                                                          
118200                                                                          
118300     .                                                                    
118400     EJECT                                                                
118500 G-KOLLA-INPUT SECTION.                                                   
118600                                                                          
118700     MOVE JA                      TO INDATA-SW                            
118800                                                                          
118900     PERFORM GA-FORMELL-KONTROLL                                          
119000     IF INDATA-OK                                                         
119100        PERFORM GB-LOGISK-KONTROLL                                        
119200     END-IF                                                               
119300                                                                          
119400     .                                                                    
119500     EJECT                                                                
119600 GA-FORMELL-KONTROLL SECTION.                                             
119700                                                                          
119800     IF REQU-INPUT               = ALL '+'                                
119900       MOVE ERR-PF11-AND-NO-DATA TO RESP-IDMSG-ERROR                      
120000       MOVE NEJ                  TO INDATA-SW                             
120100     ELSE                                                                 
120200       PERFORM GAB-KOLLA-IDANSTNR                                         
120300       PERFORM GAC-KOLLA-RADINFO                                          
120400       PERFORM GAD-KOLLA-FLKLAR                                           
120500       PERFORM GAG-KOLLA-FLSKROT                                          
120600       PERFORM GAH-KOLLA-FLANTAVV                                         
120700       PERFORM GAE-KOLLA-FLILI                                            
120800       PERFORM GAF-KOLLA-RELATION                                         
120900     END-IF                                                               
121000                                                                          
121100     .                                                                    
121200     EJECT                                                                
121300                                                                          
121400 GAB-KOLLA-IDANSTNR   SECTION.                                            
121500                                                                          
121600     MOVE NEJ                        TO SW-IDANSTNR                       
121700     IF REQU-IDANSTNR-UPD             NOT = ALL '+'                       
121800        IF REQU-IDANSTNR-UPD NUMERIC                                      
121900            MOVE REQU-IDANSTNR-UPD    TO RESP-IDANSTNR-UPD                
122000           MOVE JA                   TO SW-IDANSTNR                       
122100        ELSE                                                              
122200           MOVE NEJ                  TO INDATA-SW                         
122300           MOVE 'IDANSTNR' TO RESP-IDELMT-ERROR                           
122400           MOVE '024'      TO RESP-IDMSG-ERROR                            
122500        END-IF                                                            
122600     ELSE                                                                 
122700        MOVE NEJ                  TO INDATA-SW                            
122800        MOVE 'IDANSTNR' TO RESP-IDELMT-ERROR                              
122900        MOVE '026'      TO RESP-IDMSG-ERROR                               
123000     END-IF                                                               
123100                                                                          
123200     .                                                                    
123300     EJECT                                                                
123400                                                                          
123500 GAC-KOLLA-RADINFO    SECTION.                                            
123600                                                                          
123700     IF REQU-KVRADER-MAX1 NUMERIC AND REQU-KVRADER-MAX1 > 0               
123800       MOVE REQU-KVRADER-MAX1    TO WS-INDX-REC                           
123900       MOVE NEJ                  TO WS-REC-LIMIT                          
124000       MOVE +1                   TO INDX                                  
124100                                                                          
124200       PERFORM UNTIL INDX >  MAX-INDX OR REC-LIMIT                        
124300         IF (REQU-KDCMD(INDX) NOT = ALL '+'  AND SPACE) OR                
124400            (REQU-IDILIST-BEF(INDX) NOT = ALL '+' AND                     
124500                                          SPACE   AND                     
124600                                          ZERO)                           
124700           MOVE JA               TO SW-RAD-CMD                            
124800                                                                          
124900           IF (REQU-KDCMD(INDX) NOT = ALL '+'  AND SPACE) AND             
125000              (REQU-IDILIST-BEF(INDX) NOT = ALL '+' AND                   
125100                                            SPACE   AND                   
125200                                            ZERO)                         
125300             MOVE NEJ            TO INDATA-SW                             
125400             MOVE 'IDILIST'      TO RESP-IDELMT-ERROR                     
125500             MOVE '023'          TO RESP-IDMSG-ERROR                      
125600                                    RESP-IDMSG-ERROR-LINE (INDX)          
125700           ELSE                                                           
125800             IF (REQU-KDCMD(INDX) = ALL '+'  OR SPACE) AND                
125900                (REQU-IDILIST-BEF(INDX) NOT NUMERIC)                      
126000               MOVE NEJ          TO INDATA-SW                             
126100               MOVE 'IDILIST'    TO RESP-IDELMT-ERROR                     
126200               MOVE '025'        TO RESP-IDMSG-ERROR                      
126300                                    RESP-IDMSG-ERROR-LINE (INDX)          
126400             END-IF                                                       
126500           END-IF                                                         
126600                                                                          
126700           IF REQU-KDCMD(INDX) NOT = ALL '+' AND SPACE                    
126800             MOVE REQU-KDCMD(INDX) TO TEST-KDCMDVAL                       
126900                                                                          
127000             IF GODK-KDCMDVAL                                             
127100               CONTINUE                                                   
127200             ELSE                                                         
127300               MOVE NEJ          TO INDATA-SW                             
127400               MOVE 'KDCMDVAL'   TO RESP-IDELMT-ERROR                     
127500               MOVE '023'        TO RESP-IDMSG-ERROR                      
127600                                    RESP-IDMSG-ERROR-LINE (INDX)          
127700             END-IF                                                       
127800           END-IF                                                         
127900                                                                          
128000           IF INDATA-OK                                                   
128100             IF REQU-IDILIST-BEF(INDX) NUMERIC AND                        
128200                REQU-IDILIST-BEF(INDX) > ZERO                             
128300               MOVE REQU-IDILIST-BEF(INDX)                                
128400                                 TO WS-KDCMD-NUM                          
128500               MOVE WS-KDCMD-NUM TO REQU-KDCMD(INDX)                      
128600             END-IF                                                       
128700           END-IF                                                         
128800                                                                          
128900           IF REQU-KVANTAL(INDX) NOT = ALL '+'                            
129000             IF REQU-KVANTAL(INDX) NUMERIC                                
129100               CONTINUE                                                   
129200             ELSE                                                         
129300               MOVE 'KVANTAL'    TO RESP-IDELMT-ERROR                     
129400               MOVE '024'        TO RESP-IDMSG-ERROR                      
129500                                    RESP-IDMSG-ERROR-LINE (INDX)          
129600               MOVE NEJ          TO INDATA-SW                             
129700             END-IF                                                       
129800           END-IF                                                         
129900         END-IF                                                           
130000                                                                          
130100         IF INDX = WS-INDX-REC                                            
130200           MOVE JA               TO WS-REC-LIMIT                          
130300         ELSE                                                             
130400           ADD +1                TO INDX                                  
130500         END-IF                                                           
130600       END-PERFORM                                                        
130700                                                                          
130800     ELSE                                                                 
130900       MOVE NEJ                  TO INDATA-SW                             
131000       IF REQU-KVRADER-MAX1 = 0                                           
131100         MOVE 'KVRADER'          TO RESP-IDELMT-ERROR                     
131200         MOVE '126'              TO RESP-IDMSG-ERROR                      
131300       ELSE                                                               
131400         MOVE 'KVRADER'          TO RESP-IDELMT-ERROR                     
131500         MOVE '024'              TO RESP-IDMSG-ERROR                      
131600       END-IF                                                             
131700     END-IF                                                               
131800     .                                                                    
131900     EJECT                                                                
132000                                                                          
132100 GAD-KOLLA-FLKLAR     SECTION.                                            
132200                                                                          
132300     MOVE NEJ                         TO SW-ALLT-INLAGT                   
132400                                                                          
132500     IF REQU-FLKLAR                    NOT = ALL '+'                      
132600        IF REQU-FLKLAR                 =  JA OR YES OR NEJ                
132700           IF REQU-FLKLAR              =  JA OR YES                       
132800               MOVE JA                TO SW-ALLT-INLAGT                   
132900           END-IF                                                         
133000        ELSE                                                              
133100           MOVE NEJ                   TO INDATA-SW                        
133200           MOVE '239'      TO RESP-IDMSG-ERROR                            
133300        END-IF                                                            
133400     END-IF                                                               
133500                                                                          
133600     .                                                                    
133700     EJECT                                                                
133800                                                                          
133900 GAE-KOLLA-FLILI      SECTION.                                            
134000                                                                          
134100     MOVE NEJ                         TO SW-ALLT-TILL-ILI                 
134200                                                                          
134300     IF REQU-FLILI                     NOT = ALL '+'                      
134400        IF REQU-FLILI                  =  JA OR YES OR NEJ                
134500           IF REQU-FLILI               =  JA OR YES                       
134600               MOVE JA                TO SW-ALLT-TILL-ILI                 
134700           END-IF                                                         
134800        ELSE                                                              
134900           MOVE NEJ                   TO INDATA-SW                        
135000           MOVE '240'                 TO RESP-IDMSG-ERROR                 
135100        END-IF                                                            
135200     END-IF                                                               
135300                                                                          
135400     .                                                                    
135500     EJECT                                                                
135600                                                                          
135700 GAF-KOLLA-RELATION SECTION.                                              
135800                                                                          
135900     IF REQU-FLSKRIV = 'Y'                                                
136000       IF RAD-CMD     OR                                                  
136100          ALLT-INLAGT OR                                                  
136200          ALLT-SKROT  OR                                                  
136300          ALLT-ANT-AVV   OR                                               
136400          ALLT-TILL-ILI                                                   
136500         MOVE NEJ TO INDATA-SW                                            
136600         MOVE ERR-FLERA-FUNKTIONER TO RESP-IDMSG-ERROR                    
136700       END-IF                                                             
136800     ELSE                                                                 
136900       IF (RAD-CMD AND ALLT-INLAGT)     OR                                
137000          (RAD-CMD AND ALLT-TILL-ILI)   OR                                
137100          (RAD-CMD AND ALLT-SKROT)      OR                                
137200          (RAD-CMD AND ALLT-ANT-AVV)    OR                                
137300          (ALLT-TILL-ILI AND ALLT-INLAGT)    OR                           
137400          (ALLT-TILL-ILI AND ALLT-SKROT)     OR                           
137500          (ALLT-TILL-ILI AND ALLT-ANT-AVV)   OR                           
137600          (ALLT-INLAGT AND ALLT-SKROT)       OR                           
137700          (ALLT-INLAGT AND ALLT-ANT-AVV)     OR                           
137800          (ALLT-SKROT AND ALLT-ANT-AVV)                                   
137900         MOVE NEJ TO INDATA-SW                                            
138000         MOVE ERR-FLERA-FUNKTIONER TO RESP-IDMSG-ERROR                    
138100       END-IF                                                             
138200     END-IF                                                               
138300     .                                                                    
138400     EJECT                                                                
138500 GAG-KOLLA-FLSKROT    SECTION.                                            
138600                                                                          
138700     MOVE NEJ                         TO SW-ALLT-SKROT                    
138800                                                                          
138900     IF REQU-FLSKROT                  NOT = ALL '+'                       
139000        IF REQU-FLSKROT               =  JA OR YES OR NEJ                 
139100           IF REQU-FLSKROT            =  JA OR YES                        
139200               MOVE JA                TO SW-ALLT-SKROT                    
139300           END-IF                                                         
139400        ELSE                                                              
139500           MOVE NEJ                   TO INDATA-SW                        
139600           MOVE '241'                 TO RESP-IDMSG-ERROR                 
139700        END-IF                                                            
139800     END-IF                                                               
139900                                                                          
140000     .                                                                    
140100     EJECT                                                                
140200 GAH-KOLLA-FLANTAVV   SECTION.                                            
140300                                                                          
140400     MOVE NEJ                         TO SW-ALLT-ANT-AVV                  
140500                                                                          
140600     IF REQU-FLANTAVV                 NOT = ALL '+'                       
140700        IF REQU-FLANTAVV              =  JA OR YES OR NEJ                 
140800           IF REQU-FLANTAVV           =  JA OR YES                        
140900               MOVE JA                TO SW-ALLT-ANT-AVV                  
141000           END-IF                                                         
141100        ELSE                                                              
141200           MOVE NEJ                   TO INDATA-SW                        
141300           MOVE '242'                 TO RESP-IDMSG-ERROR                 
141400        END-IF                                                            
141500     END-IF                                                               
141600                                                                          
141700     .                                                                    
141800     EJECT                                                                
141900 GB-LOGISK-KONTROLL SECTION.                                              
142000                                                                          
142100     IF REQU-FLSKRIV = 'Y'                                                
142200        CONTINUE                                                          
142300     ELSE                                                                 
142400        PERFORM GBA-KOLLA-KDLEVANM-IDDC                                   
142500        IF INDATA-OK                                                      
142600          PERFORM GBF-KOLLA-MOTTAGET                                      
142700        END-IF                                                            
142800     END-IF                                                               
142900                                                                          
143000     IF INDATA-OK                                                         
143100        IF REQU-FLSKRIV = 'Y'                                             
143200           CONTINUE                                                       
143300        ELSE                                                              
143400           IF RAD-CMD                                                     
143500             PERFORM GBC-KOLLA-RADINFO                                    
143600           ELSE                                                           
143700             IF ALLT-INLAGT OR ALLT-SKROT OR ALLT-ANT-AVV                 
143800               PERFORM GBD-KOLLA-ALLT-INLAGT                              
143900             ELSE                                                         
144000               PERFORM GBE-KOLLA-ALLT-ILI                                 
144100             END-IF                                                       
144200           END-IF                                                         
144300        END-IF                                                            
144400     END-IF                                                               
144500                                                                          
144600     .                                                                    
144700     EJECT                                                                
144800                                                                          
144900 GBA-KOLLA-KDLEVANM-IDDC          SECTION.                                
145000                                                                          
145100     PERFORM IMS-GHU-WLKREE01                                             
145200     IF SEGMENT-FINNS                                                     
145300         IF ANM-KDLEVANM              =  W-ANM-MOT OR                     
145400                                         W-ANM-PAAB                       
145500            CONTINUE                                                      
145600         ELSE                                                             
145700            MOVE ERR-FORBIDDEN-UPDATE TO RESP-IDMSG-ERROR                 
145800            MOVE NEJ                  TO INDATA-SW                        
145900         END-IF                                                           
146000         PERFORM HS0-LAES-WLKREE11                                        
146100         IF SEGMENT-FINNS                                                 
146200           IF LEV-IDDC-RET = REQU-IDDC-KEY                                
146300             CONTINUE                                                     
146400           ELSE                                                           
146500             MOVE ERR-FORBIDDEN-UPDATE    TO RESP-IDMSG-ERROR             
146600             MOVE NEJ                     TO INDATA-SW                    
146700           END-IF                                                         
146800         END-IF                                                           
146900     ELSE                                                                 
147000         MOVE ERR-FORBIDDEN-UPDATE    TO RESP-IDMSG-ERROR                 
147100         MOVE NEJ                     TO INDATA-SW                        
147200     END-IF                                                               
147300     .                                                                    
147400     EJECT                                                                
147500                                                                          
147600 GBC-KOLLA-RADINFO    SECTION.                                            
147700                                                                          
147800     MOVE +1                           TO INDX                            
147900     MOVE NEJ                          TO WS-REC-LIMIT                    
148000                                                                          
148100     PERFORM UNTIL INDX                >  MAX-INDX OR REC-LIMIT           
148200        IF REQU-KDCMD(INDX)             NOT = ALL '+'  AND SPACE          
148300                                                                          
148400           IF REQU-IDARTNR (INDX) IS NOT NUMERIC                          
148500               MOVE ZERO TO REQU-IDARTNR (INDX)                           
148600           END-IF                                                         
148700                                                                          
148800           IF REQU-IDRADNR (INDX) IS NOT NUMERIC                          
148900               MOVE ZERO TO REQU-IDRADNR (INDX)                           
149000           END-IF                                                         
149100                                                                          
149200           IF REQU-KDCMD(INDX)          NUMERIC OR                        
149300              REQU-KDCMD(INDX)          = W-BLI                           
149400              PERFORM GBCA-KOLLA-IDILIST                                  
149500           ELSE                                                           
149600              PERFORM GBCB-KOLLA-RAD-OK                                   
149700           END-IF                                                         
149800                                                                          
149900        END-IF                                                            
150000                                                                          
150100       IF INDX = WS-INDX-REC                                              
150200          MOVE JA TO WS-REC-LIMIT                                         
150300       ELSE                                                               
150400          ADD +1                       TO INDX                            
150500       END-IF                                                             
150600     END-PERFORM                                                          
150700     .                                                                    
150800     EJECT                                                                
150900                                                                          
151000                                                                          
151100 GBCA-KOLLA-IDILIST    SECTION.                                           
151200                                                                          
151300     MOVE REQU-IDARTNR (INDX)    TO W-IDARTNR-A2                          
151400     MOVE REQU-IDRADNR (INDX)    TO W-IDRADNR-A2                          
151500                                                                          
151600     PERFORM IMS-GNP-WLKREE11-UNIK                                        
151700                                                                          
151800     IF SEGMENT-FINNS                                                     
151900        IF REQU-KDCMD(INDX)                  = W-BLI                      
152000          IF LEV-IDILIST                  = ZERO                          
152100            IF REQU-KVANTAL(INDX) NUMERIC                                 
152200              MOVE REQU-KVANTAL (INDX)  TO WS-KVANTAL                     
152300            ELSE                                                          
152400              MOVE ZERO                 TO WS-KVANTAL                     
152500            END-IF                                                        
152600            IF WS-KVANTAL                >                                
152700              (LEV-KVLEVANM-BEKR -                                        
152800               LEV-KVRETINL -                                             
152900               LEV-KVAVV-KVANT -                                          
153000               LEV-KVRETINL-SKR -                                         
153100               LEV-KVAVV-KVAL -                                           
153200               LEV-KVANTAL-ILI)                                           
153300              MOVE NEJ                TO INDATA-SW                        
153400              MOVE '023'              TO RESP-IDMSG-ERROR                 
153500                                     RESP-IDMSG-ERROR-LINE (INDX)         
153600              MOVE 'KVANTAL'          TO RESP-IDELMT-ERROR                
153700            END-IF                                                        
153800          END-IF                                                          
153900          IF LEV-IDILIST                  = ZERO AND                      
154000            (LEV-KVLEVANM-BEKR            >                               
154100             LEV-KVRETINL                 +                               
154200             LEV-KVRETINL-SKR             +                               
154300             LEV-KVAVV-KVANT              +                               
154400             LEV-KVAVV-KVAL)                                              
154500             CONTINUE                                                     
154600          ELSE                                                            
154700              MOVE NEJ                TO INDATA-SW                        
154800              MOVE '023'              TO RESP-IDMSG-ERROR                 
154900                                   RESP-IDMSG-ERROR-LINE (INDX)           
155000              MOVE 'KDCMD'            TO RESP-IDELMT-ERROR                
155100          END-IF                                                          
155200        END-IF                                                            
155300                                                                          
155400        IF REQU-KDCMD(INDX)     NUMERIC   AND                             
155500           LEV-IDILIST > ZERO                                             
155600                                                                          
155700          MOVE REQU-KDCMD(INDX)     TO W-KDCMDVAL-NUM                     
155800                                                                          
155900          IF W-KDCMDVAL-NUM = LEV-IDILIST                                 
156000            IF REQU-KVANTAL(INDX) NUMERIC                                 
156100              MOVE REQU-KVANTAL (INDX)  TO WS-KVANTAL                     
156200            ELSE                                                          
156300              MOVE ZERO                 TO WS-KVANTAL                     
156400            END-IF                                                        
156500            IF WS-KVANTAL                >                                
156600              (LEV-KVLEVANM-BEKR -                                        
156700               LEV-KVRETINL -                                             
156800               LEV-KVAVV-KVANT -                                          
156900               LEV-KVRETINL-SKR -                                         
157000               LEV-KVAVV-KVAL -                                           
157100               LEV-KVANTAL-ILI)                                           
157200              MOVE NEJ                TO INDATA-SW                        
157300              MOVE '023'              TO RESP-IDMSG-ERROR                 
157400                                     RESP-IDMSG-ERROR-LINE (INDX)         
157500              MOVE 'KVANTAL'          TO RESP-IDELMT-ERROR                
157600            END-IF                                                        
157700                                                                          
157800            IF INDATA-OK                                                  
157900              IF ( W-KDCMDVAL-NUM = LEV-IDILIST )  AND                    
158000                ((LEV-KVLEVANM-BEKR        >                              
158100                  LEV-KVRETINL             +                              
158200                  LEV-KVRETINL-SKR         +                              
158300                  LEV-KVAVV-KVANT          +                              
158400                  LEV-KVAVV-KVAL           +                              
158500                  LEV-KVANTAL-ILI))                                       
158600                                                                          
158700                MOVE REQU-KDCMD(INDX) TO W-IDILIST-E1-MIN                 
158800                                             W-IDILIST-E1-MAX             
158900                                                                          
159000                PERFORM IMS-GU-WLKREJ01                                   
159100                IF SEGMENT-SAKNAS OR SEQE-TIUTSKR > ZERO                  
159200                   MOVE NEJ           TO INDATA-SW                        
159300                   MOVE '023'         TO RESP-IDMSG-ERROR                 
159400                                    RESP-IDMSG-ERROR-LINE (INDX)          
159500                  MOVE 'KDCMDVAL'     TO RESP-IDELMT-ERROR                
159600                END-IF                                                    
159700              ELSE                                                        
159800                MOVE NEJ                TO INDATA-SW                      
159900                MOVE '023'              TO RESP-IDMSG-ERROR               
160000                                      RESP-IDMSG-ERROR-LINE (INDX)        
160100                MOVE 'KDCMDVAL'         TO RESP-IDELMT-ERROR              
160200              END-IF                                                      
160300            END-IF                                                        
160400          ELSE                                                            
160500            MOVE NEJ                TO INDATA-SW                          
160600            MOVE '023'              TO RESP-IDMSG-ERROR                   
160700                                     RESP-IDMSG-ERROR-LINE (INDX)         
160800            MOVE 'KDCMDVAL'         TO RESP-IDELMT-ERROR                  
160900          END-IF                                                          
161000        END-IF                                                            
161100                                                                          
161200        IF REQU-KDCMD (INDX)     NUMERIC   AND                            
161300           LEV-IDILIST = ZERO                                             
161400                                                                          
161500          IF REQU-KVANTAL(INDX) NUMERIC                                   
161600            MOVE REQU-KVANTAL (INDX)  TO WS-KVANTAL                       
161700          ELSE                                                            
161800            MOVE ZERO                 TO WS-KVANTAL                       
161900          END-IF                                                          
162000          IF WS-KVANTAL                >                                  
162100            (LEV-KVLEVANM-BEKR -                                          
162200             LEV-KVRETINL -                                               
162300             LEV-KVAVV-KVANT -                                            
162400             LEV-KVRETINL-SKR -                                           
162500             LEV-KVAVV-KVAL)                                              
162600            MOVE NEJ                TO INDATA-SW                          
162700            MOVE '023'              TO RESP-IDMSG-ERROR                   
162800                                  RESP-IDMSG-ERROR-LINE (INDX)            
162900            MOVE 'KVANTAL'          TO RESP-IDELMT-ERROR                  
163000          END-IF                                                          
163100                                                                          
163200          IF LEV-KVLEVANM-BEKR            >                               
163300             LEV-KVRETINL                 +                               
163400             LEV-KVRETINL-SKR             +                               
163500             LEV-KVAVV-KVANT              +                               
163600             LEV-KVAVV-KVAL                                               
163700                                                                          
163800             MOVE REQU-KDCMD(INDX)     TO W-IDILIST-E1-MIN                
163900                                         W-IDILIST-E1-MAX                 
164000                                                                          
164100             PERFORM IMS-GU-WLKREJ01                                      
164200             IF SEGMENT-SAKNAS OR SEQE-TIUTSKR > ZERO                     
164300                MOVE NEJ               TO INDATA-SW                       
164400                MOVE '023'             TO RESP-IDMSG-ERROR                
164500                                    RESP-IDMSG-ERROR-LINE (INDX)          
164600                MOVE 'KDCMDVAL'        TO RESP-IDELMT-ERROR               
164700             END-IF                                                       
164800          ELSE                                                            
164900            MOVE NEJ                    TO INDATA-SW                      
165000            MOVE '023'              TO RESP-IDMSG-ERROR                   
165100                                   RESP-IDMSG-ERROR-LINE (INDX)           
165200            MOVE 'KDCMDVAL'         TO RESP-IDELMT-ERROR                  
165300          END-IF                                                          
165400        END-IF                                                            
165500     ELSE                                                                 
165600        MOVE NEJ                        TO INDATA-SW                      
165700        MOVE '023'                      TO RESP-IDMSG-ERROR               
165800                                      RESP-IDMSG-ERROR-LINE (INDX)        
165900        MOVE 'KDCMDVAL'                 TO RESP-IDELMT-ERROR              
166000     END-IF                                                               
166100     .                                                                    
166200     EJECT                                                                
166300                                                                          
166400 GBCB-KOLLA-RAD-OK     SECTION.                                           
166500                                                                          
166600     MOVE REQU-IDARTNR (INDX)    TO W-IDARTNR-A2                          
166700     MOVE REQU-IDRADNR (INDX)    TO W-IDRADNR-A2                          
166800                                                                          
166900     PERFORM IMS-GNP-WLKREE11-UNIK                                        
167000                                                                          
167100     IF SEGMENT-FINNS                                                     
167200        IF LEV-KVLEVANM-BEKR            =                                 
167300           LEV-KVRETINL                 +                                 
167400           LEV-KVRETINL-SKR             +                                 
167500           LEV-KVAVV-KVANT              +                                 
167600           LEV-KVAVV-KVAL               +                                 
167700           LEV-KVANTAL-ILI                                                
167800            MOVE NEJ                    TO INDATA-SW                      
167900            MOVE '987'                  TO RESP-IDMSG-ERROR               
168000                                      RESP-IDMSG-ERROR-LINE (INDX)        
168100            MOVE 'KDCMDVAL'             TO RESP-IDELMT-ERROR              
168200        ELSE                                                              
168300          IF LEV-IDILIST > ZERO                                           
168400            IF REQU-KVANTAL(INDX) NUMERIC                                 
168500              MOVE REQU-KVANTAL (INDX)  TO WS-KVANTAL                     
168600            ELSE                                                          
168700              MOVE ZERO                 TO WS-KVANTAL                     
168800            END-IF                                                        
168900            IF (LEV-KVLEVANM-BEKR            -                            
169000                LEV-KVRETINL                 -                            
169100                LEV-KVRETINL-SKR             -                            
169200                LEV-KVAVV-KVANT              -                            
169300                LEV-KVAVV-KVAL               -                            
169400                LEV-KVANTAL-ILI)     <   WS-KVANTAL                       
169500              MOVE NEJ                 TO INDATA-SW                       
169600              MOVE '023'               TO RESP-IDMSG-ERROR                
169700                                      RESP-IDMSG-ERROR-LINE (INDX)        
169800              MOVE 'KVANTAL'           TO RESP-IDELMT-ERROR               
169900            END-IF                                                        
170000          ELSE                                                            
170100            IF REQU-KVANTAL(INDX) NUMERIC                                 
170200              MOVE REQU-KVANTAL (INDX)  TO WS-KVANTAL                     
170300            ELSE                                                          
170400              MOVE ZERO                 TO WS-KVANTAL                     
170500            END-IF                                                        
170600            IF (LEV-KVLEVANM-BEKR            -                            
170700                LEV-KVRETINL                 -                            
170800                LEV-KVRETINL-SKR             -                            
170900                LEV-KVAVV-KVANT              -                            
171000                LEV-KVAVV-KVAL)     <   WS-KVANTAL                        
171100              MOVE NEJ                 TO INDATA-SW                       
171200              MOVE '023'               TO RESP-IDMSG-ERROR                
171300                                      RESP-IDMSG-ERROR-LINE (INDX)        
171400              MOVE 'KVANTAL'           TO RESP-IDELMT-ERROR               
171500            END-IF                                                        
171600          END-IF                                                          
171700        END-IF                                                            
171800        IF REQU-KDCMD (INDX) = W-DEV OR W-QDE                             
171900          IF LEV-KDANMORS = 97                                            
172000            MOVE NEJ                 TO INDATA-SW                         
172100            MOVE '988'               TO RESP-IDMSG-ERROR                  
172200                                     RESP-IDMSG-ERROR-LINE (INDX)         
172300            MOVE 'KDCMDVAL'          TO RESP-IDELMT-ERROR                 
172400          END-IF                                                          
172500        END-IF                                                            
172600*---    KOLLA ATT LAGERPLATS FINNS OM ÅTG = INL/BIN                       
172700        IF REQU-KDCMD (INDX) = W-BIN                                      
172800          IF LEV-IDARTNR NOT = 100                                        
172900            PERFORM GBCBA-KOLLA-LAGERPLATS                                
173000          END-IF                                                          
173100        END-IF                                                            
173200     ELSE                                                                 
173300        MOVE NEJ                        TO INDATA-SW                      
173400        MOVE '989'                      TO RESP-IDMSG-ERROR               
173500                                    RESP-IDMSG-ERROR-LINE (INDX)          
173600        MOVE 'KDCMDVAL'               TO RESP-IDELMT-ERROR                
173700     END-IF                                                               
173800     .                                                                    
173900                                                                          
174000 GBCBA-KOLLA-LAGERPLATS  SECTION.                                         
174100                                                                          
174200     MOVE LEV-IDARTNR          TO W-IDARTNR                               
174300                                                                          
174400     IF DCS-IDDC NOT = REQU-IDDC-KEY                                      
174500        MOVE REQU-IDDC-KEY TO W-IDDC-B6                                   
174600        PERFORM IMS-GU-WDB601                                             
174700     END-IF                                                               
174800     IF DCS-CDC                                                           
174900       MOVE REQU-IDDC-KEY TO W-IDDC                                       
175000       PERFORM IMS-GU-WLARTC11                                            
175100       IF SEGMENT-FINNS                                                   
175200         IF CLAG-ADLAGOMR > ZERO OR                                       
175300           CLAG-ADGANG   > ZERO OR                                        
175400           CLAG-ADPLATS  > ZERO                                           
175500           CONTINUE                                                       
175600         ELSE                                                             
175700            MOVE NEJ                    TO INDATA-SW                      
175800            MOVE ERR-LAGER-SAKNAS      TO RESP-IDMSG-ERROR                
175900                                      RESP-IDMSG-ERROR-LINE (INDX)        
176000            MOVE 'KDCMDVAL'             TO RESP-IDELMT-ERROR              
176100         END-IF                                                           
176200       ELSE                                                               
176300         MOVE NEJ                       TO INDATA-SW                      
176400         MOVE ERR-LAGER-SAKNAS          TO RESP-IDMSG-ERROR               
176500                                      RESP-IDMSG-ERROR-LINE (INDX)        
176600         MOVE 'KDCMDVAL'                TO RESP-IDELMT-ERROR              
176700       END-IF                                                             
176800     ELSE                                                                 
176900       MOVE REQU-IDDC-KEY TO W-IDDC                                       
177000       PERFORM IMS-GU-WLARTS11                                            
177100       IF SEGMENT-FINNS                                                   
177200         IF SLAG-ADLAGOMR > ZERO OR                                       
177300           SLAG-ADGANG   > ZERO OR                                        
177400           SLAG-ADPLATS  > ZERO                                           
177500           CONTINUE                                                       
177600         ELSE                                                             
177700           MOVE NEJ                     TO INDATA-SW                      
177800           MOVE ERR-LAGER-SAKNAS        TO RESP-IDMSG-ERROR               
177900                                     RESP-IDMSG-ERROR-LINE (INDX)         
178000           MOVE 'KDCMDVAL'              TO RESP-IDELMT-ERROR              
178100         END-IF                                                           
178200       ELSE                                                               
178300         MOVE NEJ                       TO INDATA-SW                      
178400         MOVE ERR-LAGER-SAKNAS          TO RESP-IDMSG-ERROR               
178500                                   RESP-IDMSG-ERROR-LINE (INDX)           
178600         MOVE 'KDCMDVAL'                TO RESP-IDELMT-ERROR              
178700       END-IF                                                             
178800     END-IF                                                               
178900                                                                          
179000     .                                                                    
179100     EJECT                                                                
179200                                                                          
179300 GBD-KOLLA-ALLT-INLAGT SECTION.                                           
179400                                                                          
179500     PERFORM IMS-GHU-WLKREE01                                             
179600                                                                          
179700     PERFORM HS0-LAES-WLKREE11                                            
179800     PERFORM UNTIL SEGMENT-SAKNAS OR INDATA-FEL                           
179900       IF LEV-IDILIST > ZERO                                              
180000         MOVE NEJ                        TO INDATA-SW                     
180100         MOVE ERR-RAPPORTERING-STARTAD   TO RESP-IDMSG-ERROR              
180200       END-IF                                                             
180300       IF LEV-KVLEVANM-BEKR NOT =                                         
180400          LEV-KVAVV-KVAL        +                                         
180500          LEV-KVAVV-KVANT       +                                         
180600          LEV-KVRETINL          +                                         
180700          LEV-KVRETINL-SKR                                                
180800         MOVE NEJ     TO SW-KLART                                         
180900       END-IF                                                             
181000       IF INDATA-OK                                                       
181100         IF LEV-IDARTNR NOT = 100                                         
181200           PERFORM GBDA-KOLLA-LAGERPLATS                                  
181300         END-IF                                                           
181400       END-IF                                                             
181500       PERFORM HS0-LAES-WLKREE11                                          
181600     END-PERFORM                                                          
181700     IF KLART                                                             
181800       MOVE NEJ                        TO INDATA-SW                       
181900       MOVE ERR-RAPPORTERING-KLAR      TO RESP-IDMSG-ERROR                
182000     END-IF                                                               
182100     .                                                                    
182200 GBDA-KOLLA-LAGERPLATS  SECTION.                                          
182300                                                                          
182400     MOVE LEV-IDARTNR          TO W-IDARTNR                               
182500                                                                          
182600     IF DCS-IDDC NOT = REQU-IDDC-KEY                                      
182700        MOVE REQU-IDDC-KEY TO W-IDDC-B6                                   
182800        PERFORM IMS-GU-WDB601                                             
182900     END-IF                                                               
183000     IF DCS-CDC                                                           
183100       PERFORM IMS-GU-WLARTC11                                            
183200       MOVE REQU-IDDC-KEY TO W-IDDC                                       
183300       IF SEGMENT-FINNS                                                   
183400         IF CLAG-ADLAGOMR > ZERO OR                                       
183500           CLAG-ADGANG   > ZERO OR                                        
183600           CLAG-ADPLATS  > ZERO                                           
183700           CONTINUE                                                       
183800         ELSE                                                             
183900           MOVE NEJ               TO INDATA-SW                            
184000           MOVE ERR-LAGER-SAKNAS      TO RESP-IDMSG-ERROR                 
184100         END-IF                                                           
184200       ELSE                                                               
184300         MOVE NEJ                   TO INDATA-SW                          
184400         MOVE ERR-LAGER-SAKNAS      TO RESP-IDMSG-ERROR                   
184500       END-IF                                                             
184600     ELSE                                                                 
184700       MOVE REQU-IDDC-KEY TO W-IDDC                                       
184800       PERFORM IMS-GU-WLARTS11                                            
184900       IF SEGMENT-FINNS                                                   
185000         IF SLAG-ADLAGOMR > ZERO OR                                       
185100           SLAG-ADGANG   > ZERO OR                                        
185200           SLAG-ADPLATS  > ZERO                                           
185300           CONTINUE                                                       
185400         ELSE                                                             
185500           MOVE NEJ               TO INDATA-SW                            
185600           MOVE ERR-LAGER-SAKNAS      TO RESP-IDMSG-ERROR                 
185700         END-IF                                                           
185800       ELSE                                                               
185900         MOVE NEJ                   TO INDATA-SW                          
186000         MOVE ERR-LAGER-SAKNAS      TO RESP-IDMSG-ERROR                   
186100       END-IF                                                             
186200     END-IF                                                               
186300                                                                          
186400     .                                                                    
186500     EJECT                                                                
186600 GBE-KOLLA-ALLT-ILI    SECTION.                                           
186700                                                                          
186800     PERFORM IMS-GHU-WLKREE01                                             
186900                                                                          
187000     PERFORM HS0-LAES-WLKREE11                                            
187100     PERFORM UNTIL SEGMENT-SAKNAS OR EJ-KLART                             
187200       IF LEV-IDILIST = ZERO AND                                          
187300         (LEV-KVLEVANM-BEKR NOT =                                         
187400          LEV-KVAVV-KVAL        +                                         
187500          LEV-KVAVV-KVANT       +                                         
187600          LEV-KVRETINL          +                                         
187700          LEV-KVRETINL-SKR)                                               
187800         MOVE NEJ     TO SW-KLART                                         
187900       END-IF                                                             
188000       PERFORM HS0-LAES-WLKREE11                                          
188100     END-PERFORM                                                          
188200     IF KLART                                                             
188300       MOVE NEJ                        TO INDATA-SW                       
188400       MOVE ERR-RAPPORTERING-KLAR      TO RESP-IDMSG-ERROR                
188500     END-IF                                                               
188600     .                                                                    
188700     EJECT                                                                
188800 GBF-KOLLA-MOTTAGET       SECTION.                                        
188900                                                                          
189000     MOVE W-IDDISTR       TO W-IDDISTR-FSEQ-MIN                           
189100                             W-IDDISTR-FSEQ-MAX                           
189200     MOVE W-IDKUNDNR      TO W-IDKUNDNR-FSEQ-MIN                          
189300                             W-IDKUNDNR-FSEQ-MAX                          
189400     MOVE W-IDRAPPNR      TO W-IDRAPPNR-FSEQ-MIN                          
189500                             W-IDRAPPNR-FSEQ-MAX                          
189600                                                                          
189700     PERFORM IMS-GU-WLRETA01                                              
189800                                                                          
189900     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
190000                                  OR RET-TIINLMOT > ZERO                  
190100         PERFORM IMS-GN-WLRETA01                                          
190200     END-PERFORM                                                          
190300     IF SEGMENT-SAKNAS OR SEGMENT-SLUT                                    
190400         MOVE NEJ TO INDATA-SW                                            
190500         MOVE '025'                      TO RESP-IDMSG-ERROR              
190600         MOVE 'IDRETILL'                 TO RESP-IDELMT-ERROR             
190700     END-IF                                                               
190800     .                                                                    
190900     EJECT                                                                
191000 H-UPPDATERA-SKRIV-UT SECTION.                                            
191100                                                                          
191200     MOVE +1                     TO 4797-INDX                             
191300                                    4797-IX2                              
191400     MOVE NEJ                    TO W-UPDATE-SW                           
191500                                                                          
191600     IF REQU-FLSKRIV = 'Y'                                                
191700       PERFORM HA-SKRIV-UT-TILLSTAND                                      
191800     ELSE                                                                 
191900       IF ALLT-INLAGT                                                     
192000         PERFORM HB-UPPDATERA-ALLT-INLAGT                                 
192100         MOVE JA               TO  W-UPDATE-SW                            
192200       ELSE                                                               
192300         IF ALLT-ANT-AVV                                                  
192400           PERFORM HF-UPPDATERA-ALLT-ANT-AVV                              
192500           MOVE JA             TO  W-UPDATE-SW                            
192600         ELSE                                                             
192700           IF ALLT-TILL-ILI                                               
192800             PERFORM HC-UPPDATERA-ALLT-TILL-ILI                           
192900             MOVE JA          TO  W-UPDATE-SW                             
193000           ELSE                                                           
193100             IF ALLT-SKROT                                                
193200               PERFORM HE-UPPDATERA-ALLT-SKROT                            
193300               MOVE 'ALLT SKROT' TO INFO-1                                
193400               ADD +1 TO INFO-1-ANTAL                                     
193500               MOVE JA        TO  W-UPDATE-SW                             
193600             ELSE                                                         
193700               PERFORM HD-UPPDATERA-VALDA-RADER                           
193800             END-IF                                                       
193900           END-IF                                                         
194000         END-IF                                                           
194100       END-IF                                                             
194200                                                                          
194300       PERFORM IMS-GHU-WLKREE01                                           
194400                                                                          
194500       COMPUTE ANM-KVRADER-OBEH       =  ANM-KVRADER-OBEH -               
194600                                         W-KVRADER-BEH                    
194700                                                                          
194800       MOVE W-ANM-PAAB                TO ANM-KDLEVANM                     
194900                                                                          
195000       PERFORM IMS-REPL-WLKREE                                            
195100                                                                          
195200       IF W-UPDATE-OK                                                     
195300         MOVE '001'               TO RESP-IDMSG-INFO                      
195400       ELSE                                                               
195500         MOVE '004'               TO RESP-IDMSG-INFO                      
195600       END-IF                                                             
195700                                                                          
195800     END-IF                                                               
195900                                                                          
196000     IF ALLT-SKR-ORDER-SKAPAD                                             
196100       PERFORM HG-STARTA-R32-RAPP-SKROT                                   
196200     END-IF                                                               
196300                                                                          
196400     IF UPPD-VALDA-RADER                                                  
196500       PERFORM HG-STARTA-R32-RAPP-SKROT                                   
196600     END-IF                                                               
196700                                                                          
196800     IF 4797-INDX              >  +1                                      
196900        PERFORM S03-STARTA-R32-RAPPORTERING                               
197000     END-IF                                                               
197100                                                                          
197200     .                                                                    
197300     EJECT                                                                
197400                                                                          
197500 HA-SKRIV-UT-TILLSTAND SECTION.                                           
197600                                                                          
197700     MOVE +1                   TO 4794-IX                                 
197800                                                                          
197900     MOVE REQU-IDDISTR-KEY     TO RESP-IDDISTR-REP   (4794-IX)            
198000     MOVE REQU-IDKUNDNR-KEY    TO RESP-IDKUNDNR-REP  (4794-IX)            
198100     MOVE REQU-IDRAPPNR-KEY    TO RESP-IDRAPPNR-REP  (4794-IX)            
198200                                                                          
198300     PERFORM IMS-GHU-WLKREE01                                             
198400     IF SEGMENT-FINNS AND                                                 
198500        ANM-KDLEVANM           = W-ANM-MOT                                
198600         MOVE W-ANM-PAAB       TO ANM-KDLEVANM                            
198700         PERFORM IMS-REPL-WLKREE                                          
198800     END-IF                                                               
198900                                                                          
199000     PERFORM HAB-STARTA-4794                                              
199100     .                                                                    
199200     EJECT                                                                
199300                                                                          
199400 HAB-STARTA-4794  SECTION.                                                
199500                                                                          
199600                                                                          
199700     MOVE 'WL0152'              TO RESP-IDPGM-REP                         
199800     .                                                                    
199900     EJECT                                                                
200000                                                                          
200100 HB-UPPDATERA-ALLT-INLAGT  SECTION.                                       
200200                                                                          
200300     MOVE ZERO                     TO W-KVRADER-BEH                       
200400     PERFORM IMS-GHU-WLKREE01                                             
200500                                                                          
200600     PERFORM HS0-LAES-WLKREE11                                            
200700     PERFORM UNTIL SEGMENT-SAKNAS OR MAX-TRANS-IX > 6                     
200800                                                                          
200900        COMPUTE W-KVLEVANM-KVAR    =  LEV-KVLEVANM-BEKR -                 
201000                                      LEV-KVRETINL -                      
201100                                      LEV-KVAVV-KVANT -                   
201200                                      LEV-KVRETINL-SKR -                  
201300                                      LEV-KVAVV-KVAL                      
201400                                                                          
201500        IF W-KVLEVANM-KVAR         >  ZERO                                
201600          MOVE W-KVLEVANM-KVAR   TO W-KVRETINL-R32                        
201700          MOVE ZERO              TO W-KVAVV-KVANT-R32                     
201800                                    W-KVRETINL-R32-SKR                    
201900          IF REQU-IDANSTNR-UPD NUMERIC                                    
202000            MOVE REQU-IDANSTNR-UPD   TO LEV-IDANSTNR-RET                  
202100          END-IF                                                          
202200          COMPUTE LEV-KVRETINL       =  LEV-KVRETINL +                    
202300                                        W-KVLEVANM-KVAR                   
202400                                                                          
202500          ACCEPT LEV-TIINLINL FROM DATE                                   
202600                                                                          
202700          COMPUTE W-KVLEVANM-KVAR    =  LEV-KVLEVANM-BEKR -               
202800                                        LEV-KVRETINL -                    
202900                                        LEV-KVAVV-KVANT -                 
203000                                        LEV-KVRETINL-SKR -                
203100                                        LEV-KVAVV-KVAL                    
203200                                                                          
203300          ADD +1                  TO W-KVRADER-BEH                        
203400                                                                          
203500          PERFORM IMS-REPL-WLKREE                                         
203600          PERFORM S04-FYLL-I-R32-MID                                      
203700        END-IF                                                            
203800                                                                          
203900        PERFORM HS0-LAES-WLKREE11                                         
204000     END-PERFORM                                                          
204100                                                                          
204200     .                                                                    
204300     EJECT                                                                
204400                                                                          
204500 HC-UPPDATERA-ALLT-TILL-ILI SECTION.                                      
204600                                                                          
204700     MOVE ZERO                     TO W-KVRADER-BEH                       
204800     PERFORM S01-LAES-HOEGSTA-IDILIST                                     
204900                                                                          
205000     PERFORM IMS-GHU-WLKREE01                                             
205100                                                                          
205200     PERFORM HS0-LAES-WLKREE11                                            
205300     PERFORM UNTIL SEGMENT-SAKNAS                                         
205400                                                                          
205500        IF (LEV-IDILIST > ZERO) OR                                        
205600           (LEV-KVLEVANM-BEKR     =                                       
205700            LEV-KVAVV-KVAL        +                                       
205800            LEV-KVAVV-KVANT       +                                       
205900            LEV-KVRETINL          +                                       
206000            LEV-KVRETINL-SKR)                                             
206100          CONTINUE                                                        
206200        ELSE                                                              
206300          COMPUTE W-KVLEVANM-KVAR    =  LEV-KVLEVANM-BEKR -               
206400                                        LEV-KVRETINL -                    
206500                                        LEV-KVAVV-KVANT -                 
206600                                        LEV-KVRETINL-SKR -                
206700                                        LEV-KVAVV-KVAL                    
206800                                                                          
206900          COMPUTE LEV-KVANTAL-ILI   =  LEV-KVANTAL-ILI +                  
207000                                        W-KVLEVANM-KVAR                   
207100                                                                          
207200          IF REQU-IDANSTNR-UPD NUMERIC                                    
207300            MOVE REQU-IDANSTNR-UPD       TO LEV-IDANSTNR-RET              
207400          END-IF                                                          
207500                                                                          
207600          MOVE REQU-IDDC-KEY           TO W-IDDC                          
207700          MOVE LEV-IDARTNR             TO W-IDARTNR                       
207800          PERFORM IMS-GU-WLARTS11                                         
207900                                                                          
208000          MOVE W-IDILIST               TO LEV-IDILIST                     
208100          ACCEPT LEV-TIUPPDAT-ILI FROM DATE                               
208200          MOVE SLAG-ADLAGOMR           TO LEV-ADLAGOMR                    
208300          MOVE SLAG-ADGANG             TO LEV-ADGANG                      
208400          MOVE SLAG-ADPLATS            TO LEV-ADPLATS                     
208500                                                                          
208600          PERFORM IMS-REPL-WLKREE                                         
208700        END-IF                                                            
208800        PERFORM HS0-LAES-WLKREE11                                         
208900     END-PERFORM                                                          
209000                                                                          
209100     .                                                                    
209200     EJECT                                                                
209300                                                                          
209400 HD-UPPDATERA-VALDA-RADER  SECTION.                                       
209500                                                                          
209600     MOVE JA                       TO UPPD-VALDA-RADER-SW                 
209700     MOVE ZERO                     TO W-KVRADER-BEH                       
209800     MOVE NEJ                      TO WS-REC-LIMIT                        
209900     MOVE +1                       TO INDX                                
210000     PERFORM IMS-GHU-WLKREE01                                             
210100     PERFORM UNTIL INDX            >  MAX-INDX OR REC-LIMIT               
210200                                                                          
210300        IF REQU-KDCMD(INDX)         = ALL '+' OR SPACE                    
210400           CONTINUE                                                       
210500        ELSE                                                              
210600                                                                          
210700           MOVE REQU-IDARTNR (INDX) TO W-IDARTNR-A2                       
210800                                      W-IDARTNR-A2-MIN                    
210900                                      W-IDARTNR                           
211000           MOVE REQU-IDRADNR (INDX) TO W-IDRADNR-A2                       
211100                                      W-IDRADNR-A2-MIN                    
211200                                                                          
211300           PERFORM IMS-GHNP-WLKREE11                                      
211400           COMPUTE W-KVLEVANM-KVAR = LEV-KVLEVANM-BEKR -                  
211500                                     LEV-KVRETINL -                       
211600                                     LEV-KVAVV-KVANT -                    
211700                                     LEV-KVRETINL-SKR -                   
211800                                     LEV-KVAVV-KVAL -                     
211900                                     LEV-KVANTAL-ILI                      
212000                                                                          
212100           EVALUATE REQU-KDCMD(INDX)                                      
212200                                                                          
212300                                                                          
212400               WHEN W-BIN                                                 
212500                 PERFORM HDA-UPPDATERA-INL                                
212600                 MOVE JA           TO  W-UPDATE-SW                        
212700               WHEN W-SCR                                                 
212800                 PERFORM HDB-UPPDATERA-SKR                                
212900                 MOVE JA           TO  W-UPDATE-SW                        
213000               WHEN W-DEV                                                 
213100                 PERFORM HDC-UPPDATERA-ANT                                
213200                 MOVE JA           TO  W-UPDATE-SW                        
213300               WHEN W-QDE                                                 
213400                 PERFORM HDD-UPPDATERA-KVA                                
213500                 MOVE JA           TO  W-UPDATE-SW                        
213600               WHEN W-BLI                                                 
213700                 PERFORM HDE-UPPDATERA-ILI                                
213800                 MOVE JA           TO  W-UPDATE-SW                        
213900                                                                          
214000               WHEN OTHER                                                 
214100** KDCMDVAL ÄR NUMERISKT OCH INNEHÅLLER ETT ILISTE NR                     
214200                 PERFORM HDF-UPPDATERA-SPEC-ILI                           
214300                 MOVE JA           TO  W-UPDATE-SW                        
214400                                                                          
214500           END-EVALUATE                                                   
214600                                                                          
214700           IF REQU-KDCMD(INDX)         = W-BLI OR                         
214800              REQU-KDCMD(INDX)         NUMERIC                            
214900               CONTINUE                                                   
215000           ELSE                                                           
215100              COMPUTE W-KVLEVANM-KVAR = LEV-KVLEVANM-BEKR -               
215200                                        LEV-KVRETINL -                    
215300                                        LEV-KVAVV-KVANT -                 
215400                                        LEV-KVRETINL-SKR -                
215500                                        LEV-KVAVV-KVAL                    
215600                                                                          
215700              IF W-KVLEVANM-KVAR       = ZERO                             
215800                 ACCEPT LEV-TIINLINL FROM DATE                            
215900                 ADD +1                TO W-KVRADER-BEH                   
216000              END-IF                                                      
216100           END-IF                                                         
216200                                                                          
216300           IF LEV-KVAVV-KVANT > ZERO AND LEV-KVAVV-KVAL > ZERO            
216400              MOVE 'D03'               TO LEV-KDKREBEH                    
216500           ELSE                                                           
216600             IF LEV-KVAVV-KVANT > ZERO                                    
216700                MOVE 'D01'             TO LEV-KDKREBEH                    
216800             ELSE                                                         
216900                IF LEV-KVAVV-KVAL > ZERO                                  
217000                   MOVE 'D02'          TO LEV-KDKREBEH                    
217100                END-IF                                                    
217200             END-IF                                                       
217300           END-IF                                                         
217400           PERFORM IMS-REPL-WLKREE                                        
217500        END-IF                                                            
217600                                                                          
217700       IF INDX = WS-INDX-REC                                              
217800          MOVE JA TO WS-REC-LIMIT                                         
217900       ELSE                                                               
218000          ADD +1                       TO INDX                            
218100       END-IF                                                             
218200                                                                          
218300     END-PERFORM                                                          
218400                                                                          
218500     IF ORAD-IX > +1                                                      
218600       MOVE 'J'              TO ORAD-MID-FLSLUT                           
218700       MOVE ORAD-KOM-AREA    TO P-TO-P4-DATA                              
218800       PERFORM S02-CALL-W006KOM                                           
218900     END-IF                                                               
219000     .                                                                    
219100     EJECT                                                                
219200                                                                          
219300 HDA-UPPDATERA-INL    SECTION.                                            
219400                                                                          
219500     IF REQU-IDANSTNR-UPD NUMERIC                                         
219600       MOVE REQU-IDANSTNR-UPD        TO LEV-IDANSTNR-RET                  
219700     END-IF                                                               
219800     IF REQU-KVANTAL(INDX)         NUMERIC                                
219900         MOVE REQU-KVANTAL(INDX)   TO W-KVANTAL                           
220000                                      W-KVRETINL-R32                      
220100         MOVE ZERO                 TO W-KVAVV-KVANT-R32                   
220200                                      W-KVRETINL-R32-SKR                  
220300         COMPUTE LEV-KVRETINL      = LEV-KVRETINL +                       
220400                                     W-KVANTAL                            
220500     ELSE                                                                 
220600         COMPUTE LEV-KVRETINL      = LEV-KVRETINL +                       
220700                                     W-KVLEVANM-KVAR                      
220800         MOVE W-KVLEVANM-KVAR TO W-KVRETINL-R32                           
220900         MOVE ZERO            TO W-KVAVV-KVANT-R32                        
221000                                 W-KVRETINL-R32-SKR                       
221100     END-IF                                                               
221200                                                                          
221300     PERFORM S11-SPARA-R32-MID                                            
221400                                                                          
221500     .                                                                    
221600     EJECT                                                                
221700                                                                          
221800 HDB-UPPDATERA-SKR  SECTION.                                              
221900                                                                          
222000     IF REQU-IDANSTNR-UPD NUMERIC                                         
222100       MOVE REQU-IDANSTNR-UPD        TO LEV-IDANSTNR-RET                  
222200     END-IF                                                               
222300     MOVE LEV-IDDC-RET           TO W-IDDC-B6                             
222400     PERFORM IMS-GU-WDB601                                                
222500                                                                          
222600     IF REQU-KVANTAL(INDX)         NUMERIC                                
222700       IF  SEGMENT-FINNS                                                  
222800       AND DCS-KDSKRMET = 1                                               
222900         MOVE REQU-KVANTAL(INDX)   TO W-KVANTAL                           
223000                                      W-KVRETINL-R32                      
223100         MOVE ZERO                 TO W-KVRETINL-R32-SKR                  
223200       ELSE                                                               
223300         MOVE REQU-KVANTAL(INDX)   TO W-KVANTAL                           
223400                                      W-KVRETINL-R32-SKR                  
223500         MOVE ZERO                 TO W-KVRETINL-R32                      
223600       END-IF                                                             
223700       MOVE ZERO                 TO W-KVAVV-KVANT-R32                     
223800       COMPUTE LEV-KVRETINL-SKR  = LEV-KVRETINL-SKR +                     
223900                                   W-KVANTAL                              
224000     ELSE                                                                 
224100       IF  SEGMENT-FINNS                                                  
224200       AND DCS-KDSKRMET = 1                                               
224300         COMPUTE LEV-KVRETINL-SKR  = LEV-KVRETINL-SKR +                   
224400                                     W-KVLEVANM-KVAR                      
224500         MOVE W-KVLEVANM-KVAR    TO  W-KVRETINL-R32                       
224600         MOVE ZERO               TO W-KVAVV-KVANT-R32                     
224700                                    W-KVRETINL-R32-SKR                    
224800       ELSE                                                               
224900         COMPUTE LEV-KVRETINL-SKR  = LEV-KVRETINL-SKR +                   
225000                                     W-KVLEVANM-KVAR                      
225100         MOVE W-KVLEVANM-KVAR    TO  W-KVRETINL-R32-SKR                   
225200         MOVE ZERO               TO W-KVAVV-KVANT-R32                     
225300                                    W-KVRETINL-R32                        
225400       END-IF                                                             
225500     END-IF                                                               
225600                                                                          
225700     PERFORM S11-SPARA-R32-MID                                            
225800                                                                          
225900     IF LEV-IDARTNR NOT = 100                                             
226000       IF  SEGMENT-FINNS                                                  
226100       AND DCS-KDSKRMET = 1                                               
226200                                                                          
226300         IF  TRANS-OHUVUD-DAM-SKAPAD                                      
226400             CONTINUE                                                     
226500         ELSE                                                             
226600             PERFORM S08-SKAPA-TRANS-ORDERHUVUD                           
226700                                                                          
226800             PERFORM S09-SKAPA-HUVUD-ORDERRADER                           
226900                                                                          
227000             MOVE 1           TO ORAD-IX                                  
227100         END-IF                                                           
227200                                                                          
227300         IF ORAD-IX > ORAD-IX-MAX                                         
227400           MOVE ORAD-KOM-AREA    TO P-TO-P4-DATA                          
227500           PERFORM S02-CALL-W006KOM                                       
227600           PERFORM S09-SKAPA-HUVUD-ORDERRADER                             
227700           MOVE 1               TO ORAD-IX                                
227800                                                                          
227900           PERFORM S10-EDIT-TRANS-ORDERRADER                              
228000           ADD +1               TO ORAD-IX                                
228100         ELSE                                                             
228200           PERFORM S10-EDIT-TRANS-ORDERRADER                              
228300           ADD +1               TO ORAD-IX                                
228400         END-IF                                                           
228500                                                                          
228600       END-IF                                                             
228700     END-IF                                                               
228800     .                                                                    
228900     EJECT                                                                
229000                                                                          
229100 HDC-UPPDATERA-ANT   SECTION.                                             
229200                                                                          
229300                                                                          
229400     IF REQU-IDANSTNR-UPD NUMERIC                                         
229500       MOVE REQU-IDANSTNR-UPD          TO LEV-IDANSTNR-RET                
229600     END-IF                                                               
229700     IF REQU-KVANTAL(INDX)           NUMERIC                              
229800         MOVE REQU-KVANTAL(INDX)     TO W-KVANTAL                         
229900                                     W-KVAVV-KVANT-R32                    
230000         MOVE ZERO                   TO W-KVRETINL-R32                    
230100                                        W-KVRETINL-R32-SKR                
230200         COMPUTE LEV-KVAVV-KVANT     = LEV-KVAVV-KVANT +                  
230300                                       W-KVANTAL                          
230400     ELSE                                                                 
230500         COMPUTE LEV-KVAVV-KVANT     = LEV-KVAVV-KVANT +                  
230600                                       W-KVLEVANM-KVAR                    
230700         MOVE W-KVLEVANM-KVAR    TO  W-KVAVV-KVANT-R32                    
230800         MOVE ZERO               TO W-KVRETINL-R32                        
230900                                    W-KVRETINL-R32-SKR                    
231000     END-IF                                                               
231100                                                                          
231200     PERFORM S11-SPARA-R32-MID                                            
231300     .                                                                    
231400     EJECT                                                                
231500                                                                          
231600 HDD-UPPDATERA-KVA   SECTION.                                             
231700                                                                          
231800     IF REQU-IDANSTNR-UPD NUMERIC                                         
231900       MOVE REQU-IDANSTNR-UPD          TO LEV-IDANSTNR-RET                
232000     END-IF                                                               
232100     IF REQU-KVANTAL(INDX)           NUMERIC                              
232200         MOVE REQU-KVANTAL(INDX)     TO W-KVANTAL                         
232300                                        W-KVAVV-KVANT-R32                 
232400         MOVE ZERO                   TO W-KVRETINL-R32                    
232500                                        W-KVRETINL-R32-SKR                
232600         COMPUTE LEV-KVAVV-KVAL      = LEV-KVAVV-KVAL  +                  
232700                                       W-KVANTAL                          
232800     ELSE                                                                 
232900         COMPUTE LEV-KVAVV-KVAL      = LEV-KVAVV-KVAL  +                  
233000                                       W-KVLEVANM-KVAR                    
233100         MOVE W-KVLEVANM-KVAR    TO  W-KVAVV-KVANT-R32                    
233200         MOVE ZERO               TO W-KVRETINL-R32                        
233300                                    W-KVRETINL-R32-SKR                    
233400     END-IF                                                               
233500                                                                          
233600     PERFORM S11-SPARA-R32-MID                                            
233700     .                                                                    
233800     EJECT                                                                
233900                                                                          
234000 HDE-UPPDATERA-ILI    SECTION.                                            
234100                                                                          
234200     IF REQU-IDANSTNR-UPD NUMERIC                                         
234300       MOVE REQU-IDANSTNR-UPD          TO LEV-IDANSTNR-RET                
234400     END-IF                                                               
234500                                                                          
234600     IF FOERSTA-ILI                                                       
234700       PERFORM S01-LAES-HOEGSTA-IDILIST                                   
234800       MOVE NEJ      TO SW-ILI                                            
234900     END-IF                                                               
235000                                                                          
235100     MOVE W-IDILIST                    TO LEV-IDILIST                     
235200     ACCEPT LEV-TIUPPDAT-ILI  FROM DATE                                   
235300                                                                          
235400     MOVE REQU-IDDC-KEY TO W-IDDC                                         
235500     IF DCS-CDC                                                           
235600      PERFORM IMS-GU-WLARTC11                                             
235700      MOVE CLAG-ADLAGOMR              TO LEV-ADLAGOMR                     
235800      IF LEV-ADLAGOMR = 22                                                
235900       MOVE 21 TO LEV-ADLAGOMR                                            
236000      END-IF                                                              
236100      IF LEV-ADLAGOMR = 31                                                
236200       MOVE 30 TO LEV-ADLAGOMR                                            
236300      END-IF                                                              
236400      MOVE CLAG-ADGANG                TO LEV-ADGANG                       
236500      MOVE CLAG-ADPLATS               TO LEV-ADPLATS                      
236600     ELSE                                                                 
236700      PERFORM IMS-GU-WLARTS11                                             
236800      MOVE SLAG-ADLAGOMR              TO LEV-ADLAGOMR                     
236900      MOVE SLAG-ADGANG                TO LEV-ADGANG                       
237000      MOVE SLAG-ADPLATS               TO LEV-ADPLATS                      
237100                                                                          
237200      IF REQU-KVANTAL(INDX)           NUMERIC                             
237300          MOVE REQU-KVANTAL(INDX)     TO W-KVANTAL                        
237400          COMPUTE LEV-KVANTAL-ILI     = LEV-KVANTAL-ILI +                 
237500                                        W-KVANTAL                         
237600      ELSE                                                                
237700          COMPUTE LEV-KVANTAL-ILI     = LEV-KVANTAL-ILI +                 
237800                                        W-KVLEVANM-KVAR                   
237900      END-IF                                                              
238000     END-IF                                                               
238100     .                                                                    
238200     EJECT                                                                
238300                                                                          
238400 HDF-UPPDATERA-SPEC-ILI   SECTION.                                        
238500                                                                          
238600     IF LEV-IDILIST = ZERO                                                
238700       MOVE REQU-KDCMD (INDX)          TO LEV-IDILIST                     
238800       ACCEPT LEV-TIUPPDAT-ILI FROM DATE                                  
238900                                                                          
239000       IF REQU-IDANSTNR-UPD NUMERIC                                       
239100         MOVE REQU-IDANSTNR-UPD        TO LEV-IDANSTNR-RET                
239200       END-IF                                                             
239300                                                                          
239400        MOVE REQU-IDDC-KEY TO W-IDDC                                      
239500        IF DCS-CDC                                                        
239600          PERFORM IMS-GU-WLARTC11                                         
239700          MOVE CLAG-ADLAGOMR              TO LEV-ADLAGOMR                 
239800          IF LEV-ADLAGOMR = 22                                            
239900            MOVE 21                       TO LEV-ADLAGOMR                 
240000          END-IF                                                          
240100          IF LEV-ADLAGOMR = 31                                            
240200            MOVE 30                       TO LEV-ADLAGOMR                 
240300          END-IF                                                          
240400          MOVE CLAG-ADGANG                TO LEV-ADGANG                   
240500          MOVE CLAG-ADPLATS               TO LEV-ADPLATS                  
240600        ELSE                                                              
240700          PERFORM IMS-GU-WLARTS11                                         
240800          MOVE SLAG-ADLAGOMR              TO LEV-ADLAGOMR                 
240900          MOVE SLAG-ADGANG                TO LEV-ADGANG                   
241000          MOVE SLAG-ADPLATS               TO LEV-ADPLATS                  
241100        END-IF                                                            
241200     END-IF                                                               
241300                                                                          
241400     IF REQU-KVANTAL(INDX)           NUMERIC                              
241500         MOVE REQU-KVANTAL(INDX)     TO W-KVANTAL                         
241600         COMPUTE LEV-KVANTAL-ILI     = LEV-KVANTAL-ILI +                  
241700                                       W-KVANTAL                          
241800     ELSE                                                                 
241900         COMPUTE LEV-KVANTAL-ILI     = LEV-KVANTAL-ILI +                  
242000                                       W-KVLEVANM-KVAR                    
242100     END-IF                                                               
242200     .                                                                    
242300     EJECT                                                                
242400                                                                          
242500 HE-UPPDATERA-ALLT-SKROT   SECTION.                                       
242600                                                                          
242700     MOVE ZERO                     TO W-KVRADER-BEH                       
242800     PERFORM IMS-GHU-WLKREE01                                             
242900                                                                          
243000     PERFORM HS0-LAES-WLKREE11                                            
243100     PERFORM UNTIL SEGMENT-SAKNAS OR MAX-TRANS-IX > 10                    
243200        MOVE 'I PERFORM ' TO INFO-2                                       
243300          ADD +1 TO INFO-2-ANTAL                                          
243400                                                                          
243500        COMPUTE W-KVLEVANM-KVAR    =  LEV-KVLEVANM-BEKR -                 
243600                                      LEV-KVRETINL -                      
243700                                      LEV-KVAVV-KVANT -                   
243800                                      LEV-KVRETINL-SKR -                  
243900                                      LEV-KVAVV-KVAL                      
244000                                                                          
244100        IF W-KVLEVANM-KVAR         >  ZERO                                
244200          MOVE 'LEVANM > 0 '     TO INFO-3                                
244300          ADD +1 TO INFO-3-ANTAL                                          
244400          MOVE LEV-IDDC-RET      TO W-IDDC-B6                             
244500          PERFORM IMS-GU-WDB601                                           
244600                                                                          
244700          IF  SEGMENT-FINNS                                               
244800          AND DCS-KDSKRMET = 1                                            
244900            MOVE W-KVLEVANM-KVAR   TO W-KVRETINL-R32                      
245000            MOVE ZERO              TO W-KVRETINL-R32-SKR                  
245100          ELSE                                                            
245200            MOVE ZERO              TO W-KVRETINL-R32                      
245300            MOVE W-KVLEVANM-KVAR   TO W-KVRETINL-R32-SKR                  
245400          END-IF                                                          
245500          MOVE ZERO              TO W-KVAVV-KVANT-R32                     
245600          IF REQU-IDANSTNR-UPD NUMERIC                                    
245700            MOVE REQU-IDANSTNR-UPD   TO LEV-IDANSTNR-RET                  
245800          END-IF                                                          
245900          COMPUTE LEV-KVRETINL-SKR   =  LEV-KVRETINL-SKR +                
246000                                        W-KVLEVANM-KVAR                   
246100                                                                          
246200          ACCEPT LEV-TIINLINL FROM DATE                                   
246300                                                                          
246400          COMPUTE W-KVLEVANM-KVAR    =  LEV-KVLEVANM-BEKR -               
246500                                        LEV-KVRETINL -                    
246600                                        LEV-KVAVV-KVANT -                 
246700                                        LEV-KVRETINL-SKR -                
246800                                        LEV-KVAVV-KVAL                    
246900                                                                          
247000          ADD +1                  TO W-KVRADER-BEH                        
247100                                                                          
247200          MOVE LEV-IDARTNR        TO W-IDARTNR                            
247300          PERFORM IMS-REPL-WLKREE                                         
247400                                                                          
247500          PERFORM IMS-GU-WDB601                                           
247600          IF  SEGMENT-FINNS                                               
247700          AND DCS-KDSKRMET = 1                                            
247800            PERFORM S11-SPARA-R32-MID                                     
247900            MOVE JA   TO ALLT-SKR-ORDER-SKAPAD-SW                         
248000          ELSE                                                            
248100            PERFORM S04-FYLL-I-R32-MID                                    
248200          END-IF                                                          
248300                                                                          
248400          IF LEV-IDARTNR NOT = 100                                        
248500            PERFORM IMS-GU-WDB601                                         
248600            IF  SEGMENT-FINNS                                             
248700            AND DCS-KDSKRMET = 1                                          
248800                                                                          
248900              MOVE 'I SKROT SEC'     TO INFO-4                            
249000              ADD +1 TO INFO-4-ANTAL                                      
249100              IF  TRANS-OHUVUD-DAM-SKAPAD                                 
249200                  CONTINUE                                                
249300                MOVE 'ORD HUV SKAPAD'  TO INFO-5                          
249400                ADD +1 TO INFO-5-ANTAL                                    
249500              ELSE                                                        
249600                PERFORM S08-SKAPA-TRANS-ORDERHUVUD                        
249700                                                                          
249800                PERFORM S09-SKAPA-HUVUD-ORDERRADER                        
249900                                                                          
250000                MOVE 1           TO ORAD-IX                               
250100                MOVE 'SKAPA ORD HUV '  TO INFO-6                          
250200                ADD +1 TO INFO-6-ANTAL                                    
250300              END-IF                                                      
250400                                                                          
250500              IF ORAD-IX > ORAD-IX-MAX                                    
250600                                                                          
250700                MOVE ORAD-KOM-AREA    TO P-TO-P4-DATA                     
250800                PERFORM S02-CALL-W006KOM                                  
250900                PERFORM S09-SKAPA-HUVUD-ORDERRADER                        
251000                MOVE 1               TO ORAD-IX                           
251100                ADD +1               TO MAX-TRANS-IX                      
251200                                                                          
251300                PERFORM S10-EDIT-TRANS-ORDERRADER                         
251400                ADD +1               TO ORAD-IX                           
251500              ELSE                                                        
251600                PERFORM S10-EDIT-TRANS-ORDERRADER                         
251700                ADD +1               TO ORAD-IX                           
251800              END-IF                                                      
251900                                                                          
252000            END-IF                                                        
252100          END-IF                                                          
252200        END-IF                                                            
252300                                                                          
252400        PERFORM HS0-LAES-WLKREE11                                         
252500     END-PERFORM                                                          
252600                                                                          
252700     IF ORAD-IX > +1                                                      
252800          MOVE 'ORAD-IX > 0   '  TO INFO-7                                
252900          ADD +1 TO INFO-7-ANTAL                                          
253000       MOVE 'J'              TO ORAD-MID-FLSLUT                           
253100       MOVE ORAD-KOM-AREA    TO P-TO-P4-DATA                              
253200       PERFORM S02-CALL-W006KOM                                           
253300     END-IF                                                               
253400                                                                          
253500     .                                                                    
253600     EJECT                                                                
253700                                                                          
253800 HF-UPPDATERA-ALLT-ANT-AVV    SECTION.                                    
253900                                                                          
254000     MOVE ZERO                   TO W-KVRADER-BEH                         
254100     PERFORM IMS-GHU-WLKREE01                                             
254200                                                                          
254300     PERFORM HS0-LAES-WLKREE11                                            
254400     PERFORM UNTIL SEGMENT-SAKNAS OR MAX-TRANS-IX > 6                     
254500                                                                          
254600        COMPUTE W-KVLEVANM-KVAR    =  LEV-KVLEVANM-BEKR -                 
254700                                      LEV-KVRETINL -                      
254800                                      LEV-KVAVV-KVANT -                   
254900                                      LEV-KVRETINL-SKR -                  
255000                                      LEV-KVAVV-KVAL                      
255100                                                                          
255200        IF W-KVLEVANM-KVAR         >  ZERO                                
255300          MOVE W-KVLEVANM-KVAR   TO W-KVAVV-KVANT-R32                     
255400          MOVE ZERO              TO W-KVRETINL-R32                        
255500                                    W-KVRETINL-R32-SKR                    
255600          IF REQU-IDANSTNR-UPD NUMERIC                                    
255700            MOVE REQU-IDANSTNR-UPD   TO LEV-IDANSTNR-RET                  
255800          END-IF                                                          
255900          COMPUTE LEV-KVAVV-KVANT    =  LEV-KVAVV-KVANT +                 
256000                                        W-KVLEVANM-KVAR                   
256100                                                                          
256200          ACCEPT LEV-TIINLINL FROM DATE                                   
256300                                                                          
256400          COMPUTE W-KVLEVANM-KVAR    =  LEV-KVLEVANM-BEKR -               
256500                                        LEV-KVRETINL -                    
256600                                        LEV-KVAVV-KVANT -                 
256700                                        LEV-KVRETINL-SKR -                
256800                                        LEV-KVAVV-KVAL                    
256900                                                                          
257000          ADD +1                  TO W-KVRADER-BEH                        
257100                                                                          
257200          IF LEV-KVAVV-KVANT > ZERO AND LEV-KVAVV-KVAL > ZERO             
257300             MOVE 'D03'               TO LEV-KDKREBEH                     
257400          ELSE                                                            
257500            IF LEV-KVAVV-KVANT > ZERO                                     
257600               MOVE 'D01'             TO LEV-KDKREBEH                     
257700            ELSE                                                          
257800               IF LEV-KVAVV-KVAL > ZERO                                   
257900                  MOVE 'D02'          TO LEV-KDKREBEH                     
258000               END-IF                                                     
258100            END-IF                                                        
258200          END-IF                                                          
258300                                                                          
258400          PERFORM IMS-REPL-WLKREE                                         
258500          PERFORM S04-FYLL-I-R32-MID                                      
258600        END-IF                                                            
258700                                                                          
258800        PERFORM HS0-LAES-WLKREE11                                         
258900     END-PERFORM                                                          
259000                                                                          
259100     .                                                                    
259200     EJECT                                                                
259300 HG-STARTA-R32-RAPP-SKROT  SECTION.                                       
259400                                                                          
259500     MOVE +1      TO 4797-INDX                                            
259600                     SPAR4797-IX                                          
259700                                                                          
259800     PERFORM UNTIL SPAR4797-IX = 4797-IX2                                 
259900                                                                          
260000       MOVE SPAR4797-MID-IDDISTR (SPAR4797-IX)                            
260100                            TO MOD4797-MID-IDDISTR     (4797-INDX)        
260200       MOVE SPAR4797-MID-IDKUNDNR (SPAR4797-IX)                           
260300                            TO MOD4797-MID-IDKUNDNR    (4797-INDX)        
260400       MOVE SPAR4797-MID-IDRAPPNR (SPAR4797-IX)                           
260500                            TO MOD4797-MID-IDRAPPNR    (4797-INDX)        
260600       MOVE SPAR4797-MID-IDARTNR (SPAR4797-IX)                            
260700                            TO MOD4797-MID-IDARTNR     (4797-INDX)        
260800       MOVE SPAR4797-MID-IDRADNR (SPAR4797-IX)                            
260900                            TO MOD4797-MID-IDRADNR     (4797-INDX)        
261000       MOVE SPAR4797-MID-KVRETINL (SPAR4797-IX)                           
261100                            TO MOD4797-MID-KVRETINL    (4797-INDX)        
261200       MOVE SPAR4797-MID-KVAVV-KVANT (SPAR4797-IX)                        
261300                            TO MOD4797-MID-KVAVV-KVANT (4797-INDX)        
261400                                                                          
261500       MOVE ZERO            TO MOD4797-MID-KVRETINL-TRP(4797-INDX)        
261600                                                                          
261700       MOVE SPAR4797-MID-KVRETINL-SKR (SPAR4797-IX)                       
261800                            TO MOD4797-MID-KVRETINL-SKR(4797-INDX)        
261900                                                                          
262000       ADD +1               TO 4797-INDX                                  
262100                                                                          
262200       IF 4797-INDX              >  4797-MAX-INDX                         
262300         PERFORM S03-STARTA-R32-RAPPORTERING                              
262400         MOVE +1                TO 4797-INDX                              
262500       END-IF                                                             
262600                                                                          
262700        ADD +1              TO SPAR4797-IX                                
262800     END-PERFORM                                                          
262900                                                                          
263000     .                                                                    
263100     EJECT                                                                
263200                                                                          
263300 HS0-LAES-WLKREE11          SECTION.                                      
263400                                                                          
263500     MOVE NEJ                    TO OKOD-FL-RETILL                        
263600                                    OKOD-FL-INTERNUPPACKNING              
263700     PERFORM IMS-GHNP-WLKREE11-OKVAL                                      
263800     PERFORM UNTIL OKOD-FL-RETILL = 'J' OR SEGMENT-SAKNAS                 
263900                OR OKOD-FL-INTERNUPPACKNING = 'J'                         
264000        IF LEV-KDKREBEH(1:1) = 'Y'   OR                                   
264100           LEV-KDKREBEH(1:1) = 'J'   OR                                   
264200           LEV-KDKREBEH(1:1) = 'C'   OR                                   
264300           LEV-KDKREBEH      = 'D01' OR                                   
264400           LEV-KDKREBEH      = 'D02' OR                                   
264500           LEV-KDKREBEH      = 'D03'                                      
264600*--ANROPA KONTROLL AV ORSAKSKODER                                         
264700            MOVE LEV-KDANMORS   TO OKOD-KDANMORS                          
264800            CALL W418OKOD USING OKOD-W418OKOD                             
264900        END-IF                                                            
265000        IF OKOD-FL-RETILL = 'J' OR                                        
265100           OKOD-FL-INTERNUPPACKNING = 'J'                                 
265200           CONTINUE                                                       
265300        ELSE                                                              
265400          PERFORM IMS-GHNP-WLKREE11-OKVAL                                 
265500        END-IF                                                            
265600     END-PERFORM                                                          
265700                                                                          
265800     .                                                                    
265900     EJECT                                                                
266000 S01-LAES-HOEGSTA-IDILIST  SECTION.                                       
266100                                                                          
266200     PERFORM IMS-GHU-WL411111                                             
266300     MOVE 4112-IDILIST TO W-IDILIST                                       
266400     IF W-IDILIST = 999                                                   
266500       MOVE +1           TO W-IDILIST                                     
266600     ELSE                                                                 
266700       ADD  +1           TO W-IDILIST                                     
266800     END-IF                                                               
266900     MOVE W-IDILIST    TO 4112-IDILIST                                    
267000     PERFORM IMS-REPL-WL4111                                              
267100                                                                          
267200     MOVE W-IDILIST            TO RESP-IDILIST-NY                         
267300     .                                                                    
267400     EJECT                                                                
267500                                                                          
267600 S02-CALL-W006KOM        SECTION.                                         
267700                                                                          
267800     CALL W006KOM         USING MSG-PCB                                   
267900                                DISP-PCB                                  
268000                                KOMA-PCB                                  
268100                                MSG-KOM-WMSGKOM                           
268200                                P-TO-P-AREA4                              
268300                                                                          
268400          MOVE 'S02 KOM       '  TO INFO-8                                
268500          ADD +1 TO INFO-8-ANTAL                                          
268600     .                                                                    
268700     EJECT                                                                
268800                                                                          
268900 S03-STARTA-R32-RAPPORTERING     SECTION.                                 
269000                                                                          
269100     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
269200     COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
269300     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
269400     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
269500     MOVE SPACE                TO MSG-KOM-KDTRANS                         
269600     MOVE 'W4I79701'           TO MSG-KOM-IDCPYTXT                        
269700     MOVE 'INLEVRET'           TO MSG-KOM-IDSNDNOD                        
269800     MOVE 'WL015200'           TO MSG-KOM-IDSNDJOB                        
269900     ACCEPT MSG-KOM-TIREGDAT   FROM DATE                                  
270000     ADD +1                    TO W-TIKLOCK-R32                           
270100     MOVE W-TIKLOCK-R32        TO MSG-KOM-TIKLOCK                         
270200                                                                          
270300     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
270400                                                                          
270500     COMPUTE P-TO-P5-LL        =  LNG-P-TO-P-PREFIX +                     
270600                                  LENGTH OF MOD4797-MID-W4I79701          
270700                                                                          
270800     MOVE 'W4T797X '           TO P-TO-P5-TRANSKOD                        
270900     MOVE 'L152'               TO P-TO-P5-FROM-MID                        
271000     MOVE '2'                  TO P-TO-P5-KDMFSFOR                        
271100                                                                          
271200     COMPUTE MOD4797-MID-KVPOST  = 4797-INDX - 1                          
271300                                                                          
271400     MOVE R32-KOM-AREA   TO P-TO-P5-DATA                                  
271500                                                                          
271600     CALL W006KOM         USING MSG-PCB                                   
271700                                DISP-PCB                                  
271800                                KOMA-PCB                                  
271900                                MSG-KOM-WMSGKOM                           
272000                                P-TO-P-AREA5                              
272100                                                                          
272200                                                                          
272300     .                                                                    
272400     EJECT                                                                
272500                                                                          
272600 S04-FYLL-I-R32-MID SECTION.                                              
272700                                                                          
272800     MOVE REQU-IDDISTR-KEY  TO MOD4797-MID-IDDISTR     (4797-INDX)        
272900     MOVE REQU-IDKUNDNR-KEY TO MOD4797-MID-IDKUNDNR    (4797-INDX)        
273000     MOVE REQU-IDRAPPNR-KEY TO MOD4797-MID-IDRAPPNR    (4797-INDX)        
273100     MOVE LEV-IDARTNR       TO MOD4797-MID-IDARTNR     (4797-INDX)        
273200     MOVE LEV-IDRADNR       TO MOD4797-MID-IDRADNR     (4797-INDX)        
273300     MOVE W-KVRETINL-R32    TO MOD4797-MID-KVRETINL    (4797-INDX)        
273400     MOVE W-KVAVV-KVANT-R32 TO MOD4797-MID-KVAVV-KVANT (4797-INDX)        
273500     MOVE ZERO              TO MOD4797-MID-KVRETINL-TRP(4797-INDX)        
273600     MOVE W-KVRETINL-R32-SKR                                              
273700                            TO MOD4797-MID-KVRETINL-SKR(4797-INDX)        
273800                                                                          
273900     ADD +1                    TO 4797-INDX                               
274000                                                                          
274100     IF 4797-INDX              >  4797-MAX-INDX                           
274200        PERFORM S03-STARTA-R32-RAPPORTERING                               
274300        MOVE +1                TO 4797-INDX                               
274400        ADD  +1                TO MAX-TRANS-IX                            
274500     END-IF                                                               
274600     .                                                                    
274700     EJECT                                                                
274800 S05-DATE-INTERVAL SECTION.                                               
274900                                                                          
275000      COMPUTE WS-DAREGDAT = 99999999 - INFO-DAREGDAT-9KOMPL               
275100      MOVE WS-SEKEL-D               TO DAG-TISEKEL-FOM                    
275200      MOVE WS-AAMMDD                TO DAG-TIAAMMDD-FOM                   
275300      MOVE DAGENS-DATUM-SEKEL       TO DAG-TISEKEL-TOM                    
275400      MOVE DAGENS-DATUM-AAMMDD      TO DAG-TIAAMMDD-TOM                   
275500      MOVE '001'                    TO DAG-KDCALL                         
275600      CALL WDAGKONV USING DAG-KDCALL, DAG-DATUM-AREA,                     
275700                          DAG-KDSVAR                                      
275800      IF DAG-KDSVAR = SPACE                                               
275900         IF DAG-KVKALDAG < 00090                                          
276000            SET W-DATE-INTERVAL-OK  TO TRUE                               
276100         ELSE                                                             
276200            SET W-DATE-INTERVAL-EJ  TO TRUE                               
276300         END-IF                                                           
276400      END-IF                                                              
276500     .                                                                    
276600     EJECT                                                                
276700 S07-LAES-HOEGSTA-IDORDNR  SECTION.                                       
276800                                                                          
276900     PERFORM IMS-GHU-WL411111                                             
277000     MOVE 4112-IDORDNR7 TO W-IDORDNR-X                                    
277100     IF W-IDORDNR-VV = DAT-TIVV                                           
277200       ADD  +1           TO W-IDORDNR-LLL                                 
277300     ELSE                                                                 
277400       MOVE DAT-TIVV     TO W-IDORDNR-VV                                  
277500       MOVE +1           TO W-IDORDNR-LLL                                 
277600     END-IF                                                               
277700     MOVE W-IDORDNR-X  TO 4112-IDORDNR7                                   
277800     PERFORM IMS-REPL-WL4111                                              
277900     .                                                                    
278000     EJECT                                                                
278100                                                                          
278200 S08-SKAPA-TRANS-ORDERHUVUD SECTION.                                      
278300                                                                          
278400     MOVE JA              TO TRANS-OHUVUD-DAM-SKAPAD-SW                   
278500                                                                          
278600     MOVE SPACE           TO MSG-KOM-WMSGKOM                              
278700     MOVE +54             TO MSG-KOM-KVLL                                 
278800     MOVE LOW-VALUE       TO MSG-KOM-KDZ1                                 
278900     MOVE LOW-VALUE       TO MSG-KOM-KDZ2                                 
279000     MOVE SPACE           TO MSG-KOM-KDTRANS                              
279100     MOVE 'W4I25101'      TO MSG-KOM-IDCPYTXT                             
279200     MOVE 'RET-RET '      TO MSG-KOM-IDSNDNOD                             
279300     MOVE 'WL015200'      TO MSG-KOM-IDSNDJOB                             
279400                                                                          
279500     ACCEPT MSG-KOM-TIREGDAT  FROM DATE                                   
279600                                                                          
279700     MOVE W-TIKLOCK-ORDER TO MSG-KOM-TIKLOCK                              
279800                                                                          
279900     MOVE SPACE           TO MSG-KOM-IDMFSMED                             
280000                             MSG-KOM-KDSVAR                               
280100                                                                          
280200     COMPUTE P-TO-P4-LL   =  LNG-P-TO-P-PREFIX +                          
280300                             LENGTH OF OHUV-MID-W4I25101                  
280400                                                                          
280500     MOVE LOW-VALUE              TO P-TO-P4-Z1                            
280600     MOVE LOW-VALUE              TO P-TO-P4-Z2                            
280700     MOVE 'W4T251X'              TO P-TO-P4-TRANSKOD                      
280800     MOVE '4251'                 TO P-TO-P4-FROM-MID                      
280900     MOVE '2'                    TO P-TO-P4-KDMFSFOR                      
281000                                                                          
281100                                                                          
281200     MOVE SPACE                  TO OHUV-KOM-AREA                         
281300                                                                          
281400     MOVE 'W407'                 TO OHUV-MID-IDSYSTEM                     
281500                                                                          
281600     MOVE LEV-IDDC-RET           TO W-IDDC-B6                             
281700     PERFORM IMS-GU-WDB601                                                
281800     IF  SEGMENT-FINNS                                                    
281900         MOVE DCS-IDDISTR-RSKROT     TO WS-IDDISTR-N                      
282000         MOVE WS-IDDISTR-X           TO OHUV-MID-IDDISTR                  
282100         MOVE DCS-IDKUNDNR-RSKROT    TO WS-IDKUNDNR-N                     
282200         MOVE WS-IDKUNDNR-X          TO OHUV-MID-IDKUNDNR                 
282300     END-IF                                                               
282400                                                                          
282500     PERFORM S07-LAES-HOEGSTA-IDORDNR                                     
282600     MOVE W-IDORDNR-X           TO OHUV-MID-IDORDNR                       
282700     MOVE '1'                   TO OHUV-MID-KDORDKL                       
282800                                                                          
282900     MOVE SPACE           TO OHUV-MID-KDFRAKT                             
283000                             OHUV-MID-TIRFS                               
283100     MOVE SPACE           TO OHUV-MID-BEKUNDRF                            
283200     MOVE 'N'             TO OHUV-MID-KDFAKTYP                            
283300     MOVE NEJ             TO OHUV-MID-FLRESTN                             
283400     MOVE SPACE           TO OHUV-MID-KDTPOTYP                            
283500                             OHUV-MID-TITPO                               
283600                             OHUV-MID-BELAGINS                            
283700                             OHUV-MID-BEGMT                               
283800                             OHUV-MID-ADGMT-GATA                          
283900                             OHUV-MID-ADGMT-PADR                          
284000                             OHUV-MID-KDROPACK                            
284100                             OHUV-MID-BEVARREF                            
284200                             OHUV-MID-KDTULLVE                            
284300                             OHUV-MID-KDNOTES                             
284400*SAP EJ KONTO HÄR I DETTA FALLET, ENL. BOSSE H. 981026.EFTERSOM           
284500*GULL-BRITT EJ HAR SVARAT,IFALL HON VILL HA ETT EGET ANALYSNR FÖR         
284600*DESSA SKROTNINGAR, HAMNAR ALLT PÅ 'ALLMÄNNA SKROTNINGAR' MED             
284700*INTERNTABELL. DETTA GÄLLER FÖR DC11.                                     
284800     MOVE ZERO            TO OHUV-MID-IDKONTO                             
284900     MOVE SPACE           TO OHUV-MID-IDANALYS                            
285000     MOVE SPACE           TO OHUV-MID-IDKST                               
285100*** OVAN SKALL BELASTA RETURAVD. ENL. G-B STARMAN 980904, MEN SAP         
285200*** SKALL EJ HA KOSTN.STÄLLE ENL TUULA 981026.                            
285300     MOVE JA              TO OHUV-MID-FLAUTFAK                            
285400     MOVE JA              TO OHUV-MID-FLAUTPAC                            
285500     MOVE NEJ             TO OHUV-MID-FLEMBORD                            
285600     MOVE NEJ             TO OHUV-MID-FLOVRLEV                            
285700     MOVE REQU-IDFTG-KEY  TO OHUV-MID-IDFTG                               
285800     MOVE SPACE           TO OHUV-MID-IDKAMPRF                            
285900                             OHUV-MID-ADBET                               
286000                             OHUV-MID-BEBET                               
286100                             OHUV-MID-IDSKYLT                             
286200                             OHUV-MID-FLLSBOK                             
286300                             OHUV-MID-IDBILREG                            
286400                             OHUV-MID-IDVIN                               
286500                             OHUV-MID-IDCISNR                             
286600     MOVE REQU-IDDC-KEY   TO OHUV-MID-IDDC                                
286700     MOVE SPACE           TO OHUV-MID-KDORDTYP-LDC                        
286800     MOVE ZERO            TO OHUV-MID-TIREPDAT                            
286900     MOVE NEJ             TO OHUV-MID-FLFORBI                             
287000                             OHUV-MID-FLORDTIL                            
287100     MOVE ZERO            TO OHUV-MID-IDDEPT                              
287200                             OHUV-MID-IDGROSS                             
287300                                                                          
287400     MOVE OHUV-KOM-AREA   TO P-TO-P4-DATA                                 
287500                                                                          
287600     PERFORM S02-CALL-W006KOM                                             
287700     .                                                                    
287800     EJECT                                                                
287900 S09-SKAPA-HUVUD-ORDERRADER SECTION.                                      
288000                                                                          
288100     COMPUTE P-TO-P4-LL =  LNG-P-TO-P-PREFIX +                            
288200                          LENGTH OF ORAD-MID-W4I25201                     
288300                                                                          
288400     MOVE LOW-VALUE        TO P-TO-P4-Z1                                  
288500     MOVE LOW-VALUE        TO P-TO-P4-Z2                                  
288600     MOVE 'W4T252X'        TO P-TO-P4-TRANSKOD                            
288700     MOVE '4252'           TO P-TO-P4-FROM-MID                            
288800     MOVE '2'              TO P-TO-P4-KDMFSFOR                            
288900                                                                          
289000     MOVE SPACE            TO ORAD-KOM-AREA                               
289100                                                                          
289200     MOVE 'W407'           TO ORAD-MID-IDSYSTEM                           
289300                                                                          
289400     MOVE LEV-IDDC-RET     TO W-IDDC-B6                                   
289500     PERFORM IMS-GU-WDB601                                                
289600     IF  SEGMENT-FINNS                                                    
289700         MOVE DCS-IDDISTR-RSKROT     TO WS-IDDISTR-N                      
289800         MOVE WS-IDDISTR-X           TO ORAD-MID-IDDISTR                  
289900         MOVE DCS-IDKUNDNR-RSKROT    TO WS-IDKUNDNR-N                     
290000         MOVE WS-IDKUNDNR-X          TO ORAD-MID-IDKUNDNR                 
290100     END-IF                                                               
290200                                                                          
290300     MOVE W-IDORDNR-X      TO ORAD-MID-IDORDNR                            
290400     MOVE SPACE            TO ORAD-MID-BEVOLREF                           
290500     MOVE 'N'              TO ORAD-MID-FLSLUT                             
290600     MOVE SPACE            TO ORAD-MID-IDKUNDRF-RO                        
290700                                                                          
290800     .                                                                    
290900     EJECT                                                                
291000 S10-EDIT-TRANS-ORDERRADER SECTION.                                       
291100                                                                          
291200     MOVE W-IDARTNR       TO ORAD-MID-IDARTNR      (ORAD-IX)              
291300                             REK-IDARTNR                                  
291400     MOVE 9               TO REK-LNGD                                     
291500     MOVE 0               TO REK-REKSIFFR                                 
291600                                                                          
291700     CALL W009KSIF        USING REK-IDARTNR                               
291800                                REK-LNGD                                  
291900                                REK-REKSIFFR                              
292000                                                                          
292100     MOVE REK-REKSIFFR     TO ORAD-MID-REKSIFFR  (ORAD-IX)                
292200     MOVE W-KVRETINL-R32   TO W-KVSKROT-6                                 
292300     MOVE W-KVSKROT-6-X    TO ORAD-MID-KVBEART   (ORAD-IX)                
292400     MOVE SPACE            TO ORAD-MID-PRARTNTO  (ORAD-IX)                
292500                              ORAD-MID-TITPO     (ORAD-IX)                
292600                              ORAD-MID-FLRESTN   (ORAD-IX)                
292700                              ORAD-MID-KDKVBRYT  (ORAD-IX)                
292800                              ORAD-MID-FLINVEST  (ORAD-IX)                
292900     MOVE ZERO             TO ORAD-MID-KDVRINFO  (ORAD-IX)                
293000**** MAN FLYTTAR EJ KONTO PÅ RADNIVÅ TILL SAP.                            
293100     MOVE ZERO             TO ORAD-MID-IDKONTO   (ORAD-IX)                
293200     MOVE SPACE            TO ORAD-MID-IDKST     (ORAD-IX)                
293300     MOVE SPACE            TO ORAD-MID-BERADREF  (ORAD-IX)                
293400                              ORAD-MID-IDBIL     (ORAD-IX)                
293500     MOVE 2                TO ORAD-MID-KDDSP     (ORAD-IX)                
293600     MOVE NEJ              TO ORAD-MID-FLSLATT   (ORAD-IX)                
293700     MOVE SPACE            TO ORAD-MID-IDKUNDRF-WIP (ORAD-IX)             
293800                                                                          
293900     IF DCS-FLWEBDC = JA                                                  
294000       MOVE 'RETSCRAP'     TO ORAD-MID-BERADREF  (ORAD-IX)                
294100     END-IF                                                               
294200     .                                                                    
294300     EJECT                                                                
294400 S11-SPARA-R32-MID  SECTION.                                              
294500                                                                          
294600     MOVE REQU-IDDISTR-KEY  TO SPAR4797-MID-IDDISTR    (4797-IX2)         
294700     MOVE REQU-IDKUNDNR-KEY TO SPAR4797-MID-IDKUNDNR   (4797-IX2)         
294800     MOVE REQU-IDRAPPNR-KEY TO SPAR4797-MID-IDRAPPNR   (4797-IX2)         
294900     MOVE LEV-IDARTNR       TO SPAR4797-MID-IDARTNR    (4797-IX2)         
295000     MOVE LEV-IDRADNR       TO SPAR4797-MID-IDRADNR    (4797-IX2)         
295100     MOVE W-KVRETINL-R32    TO SPAR4797-MID-KVRETINL   (4797-IX2)         
295200     MOVE W-KVAVV-KVANT-R32                                               
295300                           TO SPAR4797-MID-KVAVV-KVANT (4797-IX2)         
295400     MOVE ZERO                                                            
295500                           TO SPAR4797-MID-KVRETINL-TRP(4797-IX2)         
295600     MOVE W-KVRETINL-R32-SKR                                              
295700                           TO SPAR4797-MID-KVRETINL-SKR(4797-IX2)         
295800                                                                          
295900     ADD +1                 TO 4797-IX2                                   
296000                                                                          
296100     .                                                                    
296200     EJECT                                                                
296300                                                                          
296400*    --- DISPATCHER SECTIONS                                              
296500 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
296600                                                                          
296700     MOVE 'GETARG'               TO SUB-KDFUNC                            
296800     MOVE 'CARPARTS.LDC.TREATRETPERMIT'      TO SUB-ADDISPABS             
296900                                                                          
297000*    MOVE MAX-INDX (500) TO REQU-KVRADER-MAX1 SO THAT THE                 
297100*    LENGTH IS CALCULATED CORRECTLY TO BE ABLE TO FETCH ALL               
297200*    POSSIBLE INPUT                                                       
297300     MOVE MAX-INDX               TO REQU-KVRADER-MAX1                     
297400     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
297500                                                                          
297600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
297700                                                                          
297800     IF SUB-KDRC > 0                                                      
297900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
298000       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
298100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
298200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
298300     END-IF                                                               
298400     .                                                                    
298500     SKIP3                                                                
298600 S02-RETURN-RESPONSE SECTION.                                             
298700                                                                          
298800     MOVE 'RETURN'                   TO SUB-KDFUNC                        
298900     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
299000                                                                          
299100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
299200                                                                          
299300     IF SUB-KDRC > 0                                                      
299400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
299500       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
299600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
299700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
299800     END-IF                                                               
299900     .                                                                    
300000     EJECT                                                                
300100 S11-MSG-CONV SECTION.                                                    
300200     MOVE SPACES                  TO RESP-MESSAGES (1)                    
300300                                     RESP-MESSAGES (2)                    
300400     MOVE 1                       TO MSG-IX                               
300500*    REQUEST OK                                                           
300600     MOVE 200                     TO RESP-KDSTATUS-API                    
300700     IF RESP-IDMSG-INFO > SPACE                                           
300800       MOVE SPACES                TO MSG-CONV-AREA                        
300900       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
301000       CALL WMSGCONV           USING MSG-CONV-AREA                        
301100       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
301200       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
301300       ADD 1                      TO MSG-IX                               
301400     END-IF                                                               
301500     IF RESP-IDMSG-ERROR > SPACE                                          
301600*      BAD REQUEST                                                        
301700       MOVE 400                   TO RESP-KDSTATUS-API                    
301800       MOVE SPACES                TO MSG-CONV-AREA                        
301900       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
302000       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
302100       CALL WMSGCONV           USING MSG-CONV-AREA                        
302200       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
302300       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
302400     END-IF                                                               
302500     .                                                                    
302600* --- IMS SEKTIONER ---                                                   
302700                                                                          
302800*    SKIP3                                                                
302900 IMS-GHU-WLKREE01       SECTION.                                          
303000                                                                          
303100     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X                            
303200                    '&IDFTG    =' W-IDFTG-X    ')'                        
303300          DELIMITED BY SIZE INTO SSA1                                     
303400     MOVE '  GE'           TO GODK-STATUSKODER                            
303500     CALL CBLTDLI USING GHU KREE-PCB DLI-IO-AREA SSA1                     
303600     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
303700     PERFORM IMS-STATUSKONTROLL                                           
303800     .                                                                    
303900                                                                          
304000 IMS-REPL-WLKREE        SECTION.                                          
304100                                                                          
304200     MOVE '    '           TO GODK-STATUSKODER                            
304300     CALL CBLTDLI USING REPL KREE-PCB DLI-IO-AREA                         
304400     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
304500     PERFORM IMS-STATUSKONTROLL                                           
304600     .                                                                    
304700     EJECT                                                                
304800                                                                          
304900                                                                          
305000 IMS-GNP-WLKREE11       SECTION.                                          
305100                                                                          
305200     STRING 'WLKREE11(WDA211KY>=' W-WDA211KY-MIN-X ')'                    
305300          DELIMITED BY SIZE INTO SSA1                                     
305400     MOVE '  GE'           TO GODK-STATUSKODER                            
305500     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA SSA1                     
305600     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
305700     PERFORM IMS-STATUSKONTROLL                                           
305800     .                                                                    
305900                                                                          
306000 IMS-GNP-WLKREE11-UNIK  SECTION.                                          
306100                                                                          
306200     STRING 'WLKREE11*F(WDA211KY =' W-WDA211KY-X ')'                      
306300          DELIMITED BY SIZE INTO SSA1                                     
306400     MOVE '  GE'           TO GODK-STATUSKODER                            
306500     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA SSA1                     
306600     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
306700     PERFORM IMS-STATUSKONTROLL                                           
306800     .                                                                    
306900                                                                          
307000 IMS-GHNP-WLKREE11      SECTION.                                          
307100                                                                          
307200     STRING 'WLKREE11*F(WDA211KY =' W-WDA211KY-X ')'                      
307300          DELIMITED BY SIZE INTO SSA1                                     
307400     MOVE '  '           TO GODK-STATUSKODER                              
307500     CALL CBLTDLI USING GHNP KREE-PCB DLI-IO-AREA SSA1                    
307600     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
307700     PERFORM IMS-STATUSKONTROLL                                           
307800     .                                                                    
307900                                                                          
308000 IMS-GHNP-WLKREE11-OKVAL    SECTION.                                      
308100                                                                          
308200     MOVE 'WLKREE11'       TO SSA1                                        
308300     MOVE '  GE'           TO GODK-STATUSKODER                            
308400     CALL CBLTDLI USING GHNP KREE-PCB DLI-IO-AREA SSA1                    
308500     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
308600     PERFORM IMS-STATUSKONTROLL                                           
308700     .                                                                    
308800                                                                          
308900 IMS-GNP-WLKREE21       SECTION.                                          
309000                                                                          
309100     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
309200          DELIMITED BY SIZE INTO SSA1                                     
309300     MOVE 'WLKREE21'       TO SSA2                                        
309400     MOVE '  GE'           TO GODK-STATUSKODER                            
309500     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA2 SSA1 SSA2               
309600     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
309700     PERFORM IMS-STATUSKONTROLL                                           
309800     .                                                                    
309900                                                                          
310000 IMS-GU-WLKREJ01       SECTION.                                           
310100                                                                          
310200     STRING 'WLKREJ01(WDA2E1KY>=' W-WDA2E1KY-MIN-X                        
310300                    '&WDA2E1KY<=' W-WDA2E1KY-MAX-X ')'                    
310400          DELIMITED BY SIZE INTO SSA1                                     
310500     MOVE '  GE'           TO GODK-STATUSKODER                            
310600     CALL CBLTDLI USING GU KREJ-PCB DLI-IO-AREA SSA1                      
310700     MOVE KREJ-STATUS-CODE TO STATUS-WS                                   
310800     PERFORM IMS-STATUSKONTROLL                                           
310900     .                                                                    
311000                                                                          
311100 IMS-GU-WLRETA01       SECTION.                                           
311200                                                                          
311300     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
311400                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
311500          DELIMITED BY SIZE INTO SSA1                                     
311600     MOVE '  GE'           TO GODK-STATUSKODER                            
311700     CALL CBLTDLI USING GU RETA-PCB RET-WDA301 SSA1                       
311800     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
311900     PERFORM IMS-STATUSKONTROLL                                           
312000     .                                                                    
312100                                                                          
312200 IMS-GN-WLRETA01       SECTION.                                           
312300                                                                          
312400     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
312500                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
312600          DELIMITED BY SIZE INTO SSA1                                     
312700     MOVE '  GEGB'         TO GODK-STATUSKODER                            
312800     CALL CBLTDLI USING GN RETA-PCB RET-WDA301 SSA1                       
312900     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
313000     PERFORM IMS-STATUSKONTROLL                                           
313100     .                                                                    
313200                                                                          
313300 IMS-GU-WLARTC11     SECTION.                                             
313400                                                                          
313500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
313600          DELIMITED BY SIZE INTO SSA1                                     
313700     MOVE 'WLARTC11'   TO  SSA2                                           
313800     MOVE '  GE' TO GODK-STATUSKODER                                      
313900     CALL CBLTDLI USING GU ARTC-PCB CLAG-WDK611 SSA1 SSA2                 
314000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
314100     PERFORM IMS-STATUSKONTROLL                                           
314200     .                                                                    
314300                                                                          
314400 IMS-GU-WLARTS11     SECTION.                                             
314500                                                                          
314600     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
314700          DELIMITED BY SIZE INTO SSA1                                     
314800     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
314900          DELIMITED BY SIZE INTO SSA2                                     
315000     MOVE '  GE' TO GODK-STATUSKODER                                      
315100     CALL CBLTDLI USING GU ARTS-PCB SLAG-WDK711 SSA1 SSA2                 
315200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
315300     PERFORM IMS-STATUSKONTROLL                                           
315400     .                                                                    
315500                                                                          
315600 IMS-GU-WLBENA11     SECTION.                                             
315700                                                                          
315800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
315900            DELIMITED BY SIZE INTO SSA1                                   
316000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
316100            DELIMITED BY SIZE INTO SSA2                                   
316200     MOVE '  ' TO GODK-STATUSKODER                                        
316300     CALL CBLTDLI USING GU  BENA-PCB TEXT-WDD311 SSA1 SSA2                
316400     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
316500     PERFORM IMS-STATUSKONTROLL                                           
316600     .                                                                    
316700     EJECT                                                                
316800 IMS-GHU-WL411111    SECTION.                                             
316900                                                                          
317000     STRING 'WL411101(WDGXKEY  =' W-WDGXKEY-4111-X ')'                    
317100            DELIMITED BY SIZE INTO SSA1                                   
317200     MOVE 'WL411111 ' TO SSA2                                             
317300     MOVE '  ' TO GODK-STATUSKODER                                        
317400     CALL CBLTDLI USING GHU 4111-PCB DLI-IO-AREA4 SSA1 SSA2               
317500     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
317600     PERFORM IMS-STATUSKONTROLL                                           
317700     .                                                                    
317800     EJECT                                                                
317900 IMS-REPL-WL4111    SECTION.                                              
318000                                                                          
318100     MOVE '  ' TO GODK-STATUSKODER                                        
318200     CALL CBLTDLI USING REPL 4111-PCB DLI-IO-AREA4                        
318300     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
318400     PERFORM IMS-STATUSKONTROLL                                           
318500     .                                                                    
318600     EJECT                                                                
318700 IMS-GU-WDB601    SECTION.                                                
318800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
318900          DELIMITED BY SIZE INTO SSA1                                     
319000     MOVE '  GE' TO GODK-STATUSKODER                                      
319100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
319200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
319300     PERFORM IMS-STATUSKONTROLL                                           
319400     .                                                                    
319500     EJECT                                                                
319600 IMS-GU-W6KVAH11     SECTION.                                             
319700                                                                          
319800     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
319900          DELIMITED BY SIZE INTO SSA1                                     
320000     STRING 'W6KVAH11(KDKVAINF =' W-KDKVAINF-X ')'                        
320100          DELIMITED BY SIZE INTO SSA2                                     
320200     MOVE '  GE' TO GODK-STATUSKODER                                      
320300     CALL CBLTDLI USING GU  KVAH-PCB DLI-IO-AREA-6D21 SSA1 SSA2           
320400     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
320500     PERFORM IMS-STATUSKONTROLL                                           
320600     .                                                                    
320700     EJECT                                                                
320800 IMS-STATUSKONTROLL SECTION.                                              
320900                                                                          
321000     SET STATUS-IX TO 1                                                   
321100     SEARCH GODK-STATUS                                                   
321200       AT END                                                             
321300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
321400         DELIMITED BY SIZE INTO FELTEXT                                   
321500         CALL FELLOG                                                      
321600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
321700         CONTINUE                                                         
321800     END-SEARCH                                                           
321900     .                                                                    
