000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL010400.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   03/11/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAMN:CARPARTS.LDC.WL0104                                             
000800*    WEB-LDC: WL010400 PROGRAM IS A REPLICA OF W6030400 PROGRAM           
000900*             AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                    
001000*                                                                         
001100*    FUNKTION:                                                            
001200*        LAGERDATA REGISTRERING LDC.                                      
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: WL0104U                                             
001600*        REQUEST:     WZ01REQU                                            
001700*                     WL0104I1                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        RESPONSE:    WZ01RESP                                            
002100*                     WL0104O1                                            
002200*                                                                         
002300*    PROGRAMMET UPPDATERAR WLARTC (WDK6)                                  
002400*                          WDK7                                           
002500*                          WLLOCB (WDJ9)                                  
002600*                          WLBENA (WDD3)                                  
002700*                                                                         
002800*               STARTAR EV DISPATCH FÖR UPPDATERING AV ADRESS PÅ          
002900*               INLEVERANSREGISTRET.                                      
003000*                                                                         
003100*    e-tracker: 7450328  2008-höst  vohf                                  
003200*    e-tracker:10254592  2015       Decomission VOHF                      
003300 ENVIRONMENT DIVISION.                                                    
003400     SKIP2                                                                
003500 INPUT-OUTPUT SECTION.                                                    
003600 FILE-CONTROL.                                                            
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300*    -- CHECKED BY WY2000                                                 
004400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004500 77  KDRC-DISPLAY                PIC Z(5).                                
004600 77  IDPGM                       PIC X(08)   VALUE 'WL010400'.            
004700 77  WS-ADRESS                   PIC X(50)                                
004800       VALUE 'CARPARTS.LDC.STOCKDATAREPORTING'.                           
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100 77  NOO                         PIC X       VALUE 'N'.                   
005200 77  WS-UPD                      PIC X       VALUE 'N'.                   
005300 77  WS-UPD-VOLUME               PIC X       VALUE 'N'.                   
005400 77  WS-UPD-LOCATION             PIC X       VALUE 'N'.                   
005600 77  TAB-IX                      PIC S9(3)   VALUE ZERO COMP-3.           
005700 77  TAB-IX-MAX                  PIC S9(3)   VALUE +5   COMP-3.           
005800 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
005900 77  WS-VKART                    PIC 9(7)        VALUE ZERO.              
006000 77  WS-VKART-DEC                PIC 9(7)V9(2)   VALUE ZERO.              
006100 77  WS-VLARTNTO                 PIC S9(8)V9(1)  VALUE ZERO.              
006200 77  LOGG-DATUM                  PIC S9(8)       VALUE ZERO.              
006300 77  LOGG-TID                    PIC S9(7)       VALUE ZERO.              
006400 77  PRIME-LOCATION              PIC X           VALUE 'P'.               
006500 77  WS-RESP-VKART               PIC Z(6)9.9(2)  VALUE ZERO.              
006600 77  WS-RESP-VLARTNTO            PIC Z(7)9.9     VALUE ZERO.              
006700 77  WS-RED-ADLAGOMR             PIC 9(2)        VALUE ZERO.              
006800 77  WS-RED-ADGANG               PIC 9(2)        VALUE ZERO.              
006900 77  WS-RED-ADPLATS              PIC 9(5)        VALUE ZERO.              
007000 77  MSG-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
007100                                                                          
007200 01  WS-VLARTNTO-DISP-NUM        PIC 9(8)V9(1).                           
007300 01  WS-VLARTNTO-DISP-XX  REDEFINES WS-VLARTNTO-DISP-NUM.                 
007400     05 WS-VLARTNTO-DISP-ALFA    PIC X(9).                                
007500                                                                          
007600 77  WS-ADLAGOMR                 PIC 9(2).                                
007700 77  WS-ADPLATS                  PIC 9(5).                                
007800 77  DAGENS-DATUM                PIC 9(6)          VALUE ZERO.            
007900 77  DAGENS-TID                  PIC 9(8)          VALUE ZERO.            
008000 77  LNG-P-TO-P-PREFIX           PIC S9(4)   VALUE +17  COMP SYNC.        
008100 77  WS-ADLAGOMR-WDJ9            PIC 9(2)          VALUE ZERO.            
008200 77  WS-ADGANG-WDJ9              PIC 9(2)          VALUE ZERO.            
008300 77  WS-ADPLATS-WDJ9             PIC 9(5)          VALUE ZERO.            
008400                                                                          
008500 77  WS-ADLAGOMR-CMP             PIC S9(3)  COMP-3.                       
008600 77  WS-ADGANG-CMP               PIC S9(3)  COMP-3.                       
008700 77  WS-ADPLATS-CMP              PIC S9(5)  COMP-3.                       
008800 01  INDATA-SW                   PIC X       VALUE 'J'.                   
008900     88  INDATA-OK                           VALUE 'J'.                   
009000     88  INDATA-FEL                          VALUE 'N'.                   
009100 77  KEYS-SW                   PIC X      VALUE 'J'.                      
009200     88  KEYS-OK                          VALUE 'J'.                      
009300     88  KEYS-WRONG                       VALUE 'N'.                      
009400                                                                          
009500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009600     88  NYCKLAR-OK                          VALUE 'J'.                   
009700     88  NYCKLAR-FEL                         VALUE 'N'.                   
009800                                                                          
009900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010200     EJECT                                                                
010300 01  GENERELLA-SUBPROGRAM.                                                
010400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010600     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
010700     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
010800     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
010900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011100     03  WWOMVAND                PIC X(8)    VALUE 'WWOMVAND'.            
011200     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
011300     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
011400     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
011500     03  W488PRMA                PIC X(8)    VALUE 'W488PRMA'.            
011600     EJECT                                                                
011700*   -COPY WWDC99                                                          
011800     EJECT                                                                
011900*   -COPY WWDC99               -PRE REF-                                  
012000     EJECT                                                                
012100*   -COPY WWLNDKON                                                        
012200     EJECT                                                                
012300 01  FILLER                      PIC X(16)  VALUE 'WWOMVAND '.            
012400*   -COPY WWOMVAND                                                        
012500     EJECT                                                                
012600 01  MESSAGE-CODES.                                                       
012700     03  UPDATE-DONE             PIC X(3)  VALUE '001'.                   
012800     03  NO-DATA-ENTERED         PIC X(3)  VALUE '014'.                   
012900     03  INVALID-KEY-FIELDS      PIC X(3)  VALUE '022'.                   
013000     03  IS-INVALID              PIC X(3)  VALUE '023'.                   
013100     03  SYSTEM-ERROR            PIC X(3)  VALUE '099'.                   
013200     03  NOT-FOUND               PIC X(3)  VALUE '025'.                   
013300     03  ZERO-NOT-ALLOWED        PIC X(3)  VALUE '168'.                   
013400     03  INF-SEE-SCREEN-6306     PIC X(3)  VALUE '101'.                   
013500     03  GOODS-ADDRESS-MISSING   PIC X(3)  VALUE '102'.                   
013600     03  ERR-UNAUTHORIZED        PIC X(3)  VALUE '00A'.                   
013700     EJECT                                                                
013800*01  -COPY WDECAREA                                                       
013900     EJECT                                                                
014000 01  FILLER                      PIC X(16)   VALUE 'W488PRMA'.            
014100     SKIP3                                                                
014200*01  -COPY W488PRMA                                                       
014300     EJECT                                                                
014400 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
014500     SKIP3                                                                
014600*01  -COPY WZ01SUB                                                        
014700     EJECT                                                                
014800 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH'.            
014900*01  -COPY WZ01AUTH                                                       
015000     EJECT                                                                
015100 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
015200*01  -COPY WMSGCONV                                                       
015300     EJECT                                                                
015400 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
015500     SKIP3                                                                
015600 01  REQU-AREA.                                                           
015700*    03  -COPY WZ01REQ2                                                   
015800*    03  -COPY WL0104I1                                                   
015900     EJECT                                                                
016000 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
016100     SKIP3                                                                
016200 01  RESP-AREA.                                                           
016300*    03  -COPY WZ01RES2                                                   
016400*    03  -COPY WL0104O1                                                   
016500     EJECT                                                                
016600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016700*01  -COPY WMSGAREA                                                       
016800     EJECT                                                                
016900*01  -COPY WMFSAREA                                                       
017000     EJECT                                                                
017100 01  FILLER                  PIC X(16)   VALUE 'KOM-IO-AREA'.             
017200     SKIP3                                                                
017300 01  FILLER                  PIC X(16)  VALUE 'KOM-MSG-IO-AREA '.         
017400 01  KOM-MSG-IO-AREA.                                                     
017500*03  -COPY WMSGKOM                                                        
017600                                                                          
017700     EJECT                                                                
017800 01  FILLER                  PIC X(16)  VALUE 'WTRAUTF8-AREA   '.         
017900*01  -COPY WTRAUTF8                                                       
018000                                                                          
018100 01  WS-IDSKYLT-GB           PIC X(3) VALUE 'GB '.                        
018200 01  WS-IDSKYLT-CN           PIC X(3) VALUE 'RCN'.                        
018300                                                                          
018400     EJECT                                                                
018500 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW'.           
018600 01      P-TO-P-SW.                                                       
018700  02     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.           
018800  02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
018900  02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
019000  02     P-TO-P-KDTRANS          PIC X(8).                                
019100  02     P-TO-P-IDTRANS          PIC X(4).                                
019200  02     P-TO-P-KDMFSFOR         PIC X(1).                                
019300  02     P-TO-P-DATA             PIC X(1000).                             
019400     EJECT                                                                
019500 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW2'.          
019600 01  P-TO-P-SW2.                                                          
019700     02     P-TO-P2-KVLL             PIC S9(4) COMP SYNC.                 
019800     02     P-TO-P2-KDZ1             PIC X(1)  VALUE LOW-VALUE.           
019900     02     P-TO-P2-KDZ2             PIC X(1)  VALUE LOW-VALUE.           
020000     02     P-TO-P2-KDTRANS          PIC X(8).                            
020100     02     P-TO-P2-IDTRANS          PIC X(4).                            
020200     02     P-TO-P2-KDMFSFOR         PIC X(1).                            
020300     02     MID -COPY W4I28901   -PRE P-TO-P2-                            
020400     EJECT                                                                
020500 01      FILLER                  PIC X(32)   VALUE SPACE.                 
020600 01      FILLER                  PIC X(24)   VALUE                        
020700                                 'MOD619A-MID-W6I19A01'.                  
020800     -COPY W6I19A01 -PRE MOD619A-                                         
020900     EJECT                                                                
021000 01      FILLER                  PIC X(24)   VALUE                        
021100                                 'MOD619B-MID-W6I19B01'.                  
021200     SKIP2                                                                
021300     -COPY W6I19B01 -PRE MOD619B-                                         
021400     EJECT                                                                
021500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021700     SKIP3                                                                
021800 01  NYCKLAR-TILL-DLI.                                                    
021900     03  W-IDARTNR-X.                                                     
022000         05  W-IDARTNR            PIC S9(9)   VALUE ZERO COMP-3.          
022100     03  W-IDDC-X.                                                        
022200         05  W-IDDC               PIC X(2)    VALUE SPACE.                
022300     03  W-IDSKYLT-X.                                                     
022400         05  W-IDSKYLT            PIC X(3)    VALUE SPACES.               
022500     03  W-KDSEGKEY-X.                                                    
022600         05  W-KDSEGKEY           PIC X(1)    VALUE '1'.                  
022700     03  W-IDLAND-X.                                                      
022800         05  W-IDLAND             PIC X(2)    VALUE SPACE.                
022900     03  WDK7A1KY-MIN-X.                                                  
023000         05 W-IDDC-MIN            PIC X(2)           VALUE SPACE.         
023100         05 W-ADART-MIN.                                                  
023200            07  W-ADLAGOMR-MIN    PIC S9(3)  COMP-3  VALUE ZERO.          
023300            07  W-ADGANG-MIN      PIC S9(3)  COMP-3  VALUE ZERO.          
023400            07  W-ADPLATS-MIN     PIC S9(5)  COMP-3  VALUE ZERO.          
023500         05 W-IDARTNR-MIN         PIC S9(9)  COMP-3  VALUE ZERO.          
023600     03  WDK7A1KY-MAX-X.                                                  
023700         05  W-IDDC-MAX           PIC X(2)           VALUE SPACE.         
023800         05  W-ADART-MAX.                                                 
023900            07  W-ADLAGOMR-MAX    PIC S9(3)  COMP-3  VALUE ZERO.          
024000            07  W-ADGANG-MAX      PIC S9(3)  COMP-3  VALUE ZERO.          
024100            07  W-ADPLATS-MAX     PIC S9(5)  COMP-3  VALUE ZERO.          
024200         05 W-IDARTNR-MAX         PIC S9(9)  COMP-3  VALUE ZERO.          
024300                                                                          
024400     03  W-IDDC-MIN-X.                                                    
024500         05  W-IDDC-MIN-1        PIC X(2)    VALUE LOW-VALUE.             
024600                                                                          
024700     03  W-IDDC-MAX-X.                                                    
024800         05  W-IDDC-MAX-1        PIC X(2)    VALUE HIGH-VALUE.            
024900                                                                          
025000     03  W-IDDC-REF-MIN-X.                                                
025100         05  W-IDDC-REF-MIN      PIC X(2)    VALUE LOW-VALUE.             
025200                                                                          
025300     03  W-IDDC-REF-MAX-X.                                                
025400         05  W-IDDC-REF-MAX      PIC X(2)    VALUE HIGH-VALUE.            
025500     03  W-IDDC-B6-X.                                                     
025600         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
025700                                                                          
025800     EJECT                                                                
025900*    --- STATUS-KOD FRÅN IMS                                              
026000 01  STATUS-WS                   PIC XX.                                  
026100     88  SEGMENT-FINNS                       VALUE '  '.                  
026200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
026300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026400     SKIP2                                                                
026500 01  GODK-STATUSKODER.                                                    
026600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026700     SKIP3                                                                
026800 01  FILLER                  PIC X(16)  VALUE 'SSA1            '.         
026900 01  SSA1                    PIC X(64).                                   
027000 01  FILLER                  PIC X(16)  VALUE 'SSA2            '.         
027100 01  SSA2                    PIC X(64).                                   
027200 01  FILLER                  PIC X(16)  VALUE 'SSA3            '.         
027300 01  SSA3                    PIC X(32).                                   
027400     EJECT                                                                
027500*    --- IMS FUNKTIONSKODER                                               
027600*01  -COPY W0003                                                          
027700     EJECT                                                                
027800*    ---  DLI INPUT-OUTPUT AREOR                                          
027900 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-AREA-WDK6'.         
028000 01  DLI-IO-AREA-WDK6.                                                    
028100     03  IO-AREA-WDK6       PIC X(900)  VALUE SPACE.                      
028200     03  WLARTC01 REDEFINES IO-AREA-WDK6.                                 
028300*        05  -COPY WDK601                                                 
028400     EJECT                                                                
028500     03  WLARTC11 REDEFINES IO-AREA-WDK6.                                 
028600*        05  -COPY WDK611                                                 
028700     EJECT                                                                
028800 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDK711'.            
028900 01  DLI-IO-AREA-WDK711.                                                  
029000*    03  -COPY WDK711                                                     
029100     EJECT                                                                
029200 01  FILLER                 PIC X(16) VALUE 'DLI-IO-WDK712'.              
029300 01  DLI-IO-AREA-WDK712.                                                  
029400*    03  -COPY WDK712                                                     
029500     EJECT                                                                
029600 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDK7A1'.            
029700 01  DLI-IO-AREA-WDK7A.                                                   
029800*    03  -COPY WDK7A1                                                     
029900     EJECT                                                                
030000 01  FILLER                PIC X(16) VALUE 'DLI-IO-WLLOCB01'.             
030100 01  DLI-IO-WLLOCB01.                                                     
030200*    03  -COPY WDJ901    -PRE LOCB-                                       
030300     EJECT                                                                
030400 01  FILLER                PIC X(16) VALUE 'DLI-IO-WLLOCB11'.             
030500 01  DLI-IO-WLLOCB11.                                                     
030600*    03  -COPY WDJ911    -PRE LOCB-                                       
030700     EJECT                                                                
030800 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDD311'.               
030900 01  DLI-IO-WLBENA11.                                                     
031000*    03  -COPY WDD311    -PRE BENA-                                       
031100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
031200 01   DLI-IO-AREA-B601.                                                   
031300*     03  -COPY WDB601                                                    
031400     EJECT                                                                
031500 LINKAGE SECTION.                                                         
031600*01  -COPY W0009   -PRE MSG-                                              
031700     EJECT                                                                
031800*01  -COPY W0009   -PRE DISP-                                             
031900     EJECT                                                                
032000*01  -COPY W0009   -PRE 4289-                                             
032100     EJECT                                                                
032200*01  -COPY W0009   -PRE SYNQ-                                             
032300     EJECT                                                                
032400 01  ATAB-PCB                    PIC X.                                   
032500     EJECT                                                                
032600*01  -COPY W0008   -PRE ARTC-                                             
032700     05  FILLER                  PIC X.                                   
032800     EJECT                                                                
032900*01  -COPY W0008   -PRE WDK7-                                             
033000     05  FILLER                  PIC X.                                   
033100     EJECT                                                                
033200*01  -COPY W0008   -PRE LOCB-                                             
033300     05  FILLER                  PIC X.                                   
033400     EJECT                                                                
033500*01  -COPY W0008   -PRE WDK7A-                                            
033600     05  FILLER                  PIC X.                                   
033700     EJECT                                                                
033800*01  -COPY W0008   -PRE BENA-                                             
033900     05  FILLER                  PIC X.                                   
034000     EJECT                                                                
034100*01  -COPY W0008  -PRE WDB6-                                              
034200     05  FILLER                  PIC X.                                   
034300     EJECT                                                                
034400*    PCB'ER FÖR SUBPGM                                                    
034500 01  KOM-KOMA-PCB                PIC X.                                   
034600 01  SYNQ-ATAB-PCB               PIC X.                                   
034700 01  SYNQ-WDK6-PCB               PIC X.                                   
034800 01  SYNQ-WDD3-PCB               PIC X.                                   
034900     EJECT                                                                
035000 PROCEDURE DIVISION USING MSG-PCB DISP-PCB 4289-PCB SYNQ-PCB              
035100                          ATAB-PCB                                        
035200                          ARTC-PCB WDK7-PCB LOCB-PCB KOM-KOMA-PCB         
035300                          WDK7A-PCB BENA-PCB WDB6-PCB                     
035400                          SYNQ-ATAB-PCB SYNQ-WDK6-PCB                     
035500                          SYNQ-WDD3-PCB.                                  
035600 MAIN SECTION.                                                            
035700     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB 4289-PCB SYNQ-PCB             
035800                          ATAB-PCB                                        
035900                          ARTC-PCB WDK7-PCB LOCB-PCB KOM-KOMA-PCB         
036000                          WDK7A-PCB BENA-PCB WDB6-PCB                     
036100                          SYNQ-ATAB-PCB SYNQ-WDK6-PCB                     
036200                          SYNQ-WDD3-PCB.                                  
036300                                                                          
036400     PERFORM S01-HAEMTA-ANROPSDATA                                        
036500     IF SUB-KDRC = 0                                                      
036600        PERFORM A-INIT                                                    
036700        PERFORM B-KOLLA-NYCKLAR                                           
036800        IF NYCKLAR-OK                                                     
036900           IF REQU-KDPGMACT = 'U' OR REQU-UPDATE                          
037000              PERFORM G-KOLLA-INPUT                                       
037100              IF INDATA-OK                                                
037200                 PERFORM H-UPPDATERA                                      
037300              END-IF                                                      
037400           END-IF                                                         
037500           IF INDATA-OK                                                   
037600              PERFORM F-LAES-VISA-INFO                                    
037700           END-IF                                                         
037800        END-IF                                                            
037900        IF SUB-KDTRANS(1:6) = 'WLA104'                                    
038000          PERFORM S11-MSG-CONV                                            
038100        END-IF                                                            
038200        PERFORM S02-RETURNERA-SVAR                                        
038300     END-IF                                                               
038400                                                                          
038500     MOVE ZERO TO RETURN-CODE                                             
038600     GOBACK                                                               
038700     .                                                                    
038800     EJECT                                                                
038900 A-INIT SECTION.                                                          
039000                                                                          
039100     ACCEPT DAGENS-DATUM FROM DATE                                        
039200     ACCEPT DAGENS-TID   FROM TIME                                        
039300                                                                          
039400     MOVE ALL '+' TO RESP-AREA                                            
039500     MOVE SPACE   TO RESP-AREA                                            
039600     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
039700                     RESP-IDMSG-INFO                                      
039800                     RESP-IDELMT-ERROR                                    
039900     MOVE '001'   TO RESP-IDRESVER                                        
040000     MOVE LOW-VALUE  TO W-ADART-MIN                                       
040100     MOVE HIGH-VALUE TO W-ADART-MAX                                       
040200     MOVE ZERO       TO W-IDARTNR-MIN                                     
040300     MOVE +999999999 TO W-IDARTNR-MAX                                     
040400     IF SUB-KDTRANS(1:6) = 'WLA104'                                       
040500       MOVE 001                  TO AUTH-KDCALL                           
040600       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
040700                                    REQU-WZ01REQ2                         
040800       IF AUTH-KDRC > 0                                                   
040900         MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                      
041000         MOVE NOO                TO KEYS-SW                               
041100       END-IF                                                             
041200       MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY) TO                        
041300                                 REQU-IDDC-KEY                            
041400       MOVE FUNCTION UPPER-CASE (REQU-ADLAGOMR) TO                        
041500                                 REQU-ADLAGOMR                            
041600       MOVE FUNCTION UPPER-CASE (REQU-ADGANG) TO                          
041700                                 REQU-ADGANG                              
041800       MOVE FUNCTION UPPER-CASE (REQU-ADPLATS) TO                         
041900                                 REQU-ADPLATS                             
042000       MOVE FUNCTION UPPER-CASE (REQU-VKART2) TO                          
042100                                 REQU-VKART2                              
042200       MOVE FUNCTION UPPER-CASE (REQU-VLARTNTO) TO                        
042300                                 REQU-VLARTNTO                            
042400       MOVE FUNCTION UPPER-CASE (REQU-KDMATT) TO                          
042500                                 REQU-KDMATT                              
042600     END-IF                                                               
042700     MOVE REQU-IDDC-KEY TO W-IDDC-B6                                      
042800     PERFORM IMS-GU-WDB601                                                
042900     .                                                                    
043000     EJECT                                                                
043100 B-KOLLA-NYCKLAR SECTION.                                                 
043200                                                                          
043300     MOVE JA TO NYCKLAR-SW                                                
043400                                                                          
043500***  KONTROLL AV REQU-KDPGMACT                                            
043600     IF REQU-UPDATE OR                                                    
043700        REQU-KDPGMACT = 'S' OR 'U'                                        
043800        CONTINUE                                                          
043900     ELSE                                                                 
044000        MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                             
044100        MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                            
044200        MOVE NEJ TO NYCKLAR-SW                                            
044300     END-IF                                                               
044400                                                                          
044500***  KONTROLL AV IDARTNR-KEY                                              
044600     IF REQU-IDARTNR-KEY = ALL '+'                                        
044700        MOVE 'IDARTNR' TO RESP-IDELMT-ERROR                               
044800        MOVE NEJ       TO NYCKLAR-SW                                      
044900     ELSE                                                                 
045000        INSPECT REQU-IDARTNR-KEY REPLACING LEADING SPACE BY ZERO          
045100        IF REQU-IDARTNR-KEY NUMERIC                                       
045200           MOVE REQU-IDARTNR-KEY TO W-IDARTNR                             
045300                                    WS-IDARTNR                            
045400        ELSE                                                              
045500           MOVE 'IDARTNR' TO RESP-IDELMT-ERROR                            
045600           MOVE NEJ       TO NYCKLAR-SW                                   
045700        END-IF                                                            
045800     END-IF                                                               
045900                                                                          
046000***  KONTROLL IDDC-KEY                                                    
046100     MOVE REQU-IDARTNR-KEY TO RESP-IDARTNR-KEY                            
046200     MOVE REQU-IDDC-KEY    TO RESP-IDDC-KEY                               
046300                              W-IDDC                                      
046400                              WS-IDDC                                     
046500                              W-IDDC-MIN                                  
046600                              W-IDDC-MAX                                  
046700                                                                          
046800     IF NYCKLAR-FEL                                                       
046900        IF RESP-IDELMT-ERROR = 'KDPGMACT'                                 
047000           MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                          
047100        ELSE                                                              
047200           MOVE INVALID-KEY-FIELDS TO RESP-IDMSG-ERROR                    
047300        END-IF                                                            
047400     END-IF                                                               
047500     .                                                                    
047600     EJECT                                                                
047700 F-LAES-VISA-INFO SECTION.                                                
047800                                                                          
047900     PERFORM IMS-GU-ARTC11                                                
048000                                                                          
048100     IF SEGMENT-SAKNAS                                                    
048200        MOVE 'IDARTNR'        TO RESP-IDELMT-ERROR                        
048300        MOVE NOT-FOUND        TO RESP-IDMSG-ERROR                         
048400     ELSE                                                                 
048500        IF REQU-KDMATT = 'U'                                              
048600           COMPUTE WS-RESP-VKART ROUNDED =                                
048700                                   CLAG-VKART * CONV-GR-TO-OZ             
048800                                                                          
048900           COMPUTE WS-RESP-VLARTNTO ROUNDED =                             
049000                                   CLAG-VLARTNTO * CONV-CM3-TO-IN3        
049100                                                                          
049200           MOVE '    OZ'             TO RESP-BESORT-VKART                 
049300           MOVE 'CU.IN.'             TO RESP-BESORT-VLART                 
049400        ELSE                                                              
049500           MOVE CLAG-VKART           TO WS-RESP-VKART                     
049600           MOVE CLAG-VLARTNTO        TO WS-RESP-VLARTNTO                  
049700           MOVE '     G'             TO RESP-BESORT-VKART                 
049800           MOVE '   CM3'             TO RESP-BESORT-VLART                 
049900        END-IF                                                            
050000                                                                          
050100        MOVE WS-RESP-VKART    TO RESP-VKART2                              
050200        MOVE WS-RESP-VLARTNTO TO RESP-VLARTNTO                            
050300        MOVE CLAG-KDVSOP      TO RESP-KDVSOP                              
050400                                                                          
050500        PERFORM IMS-GHU-WDK711                                            
050600        IF SEGMENT-SAKNAS                                                 
050700           MOVE GOODS-ADDRESS-MISSING TO RESP-IDMSG-ERROR                 
050800        ELSE                                                              
050900           MOVE SLAG-ADLAGOMR TO WS-RED-ADLAGOMR                          
051000           MOVE WS-RED-ADLAGOMR TO RESP-ADLAGOMR                          
051100                                W-ADLAGOMR-MIN                            
051200                                W-ADLAGOMR-MAX                            
051300           MOVE SLAG-ADGANG   TO WS-RED-ADGANG                            
051400           MOVE WS-RED-ADGANG TO RESP-ADGANG                              
051500                                 W-ADGANG-MIN                             
051600                                 W-ADGANG-MAX                             
051700           MOVE SLAG-ADPLATS   TO WS-RED-ADPLATS                          
051800           MOVE WS-RED-ADPLATS TO RESP-ADPLATS                            
051900                                W-ADPLATS-MIN                             
052000                                W-ADPLATS-MAX                             
052100           IF SLAG-IDDC-REF = SPACE                                       
052200             MOVE 'O'          TO RESP-KDDIAVAR                           
052300           ELSE                                                           
052400             MOVE 'C'          TO RESP-KDDIAVAR                           
052500           END-IF                                                         
052600                                                                          
052700           IF NDC-CN OR LDC-CN OR NDC-US                                  
052800              IF NDC-US                                                   
052900                 MOVE WC-LAND-US TO W-IDLAND                              
053000              ELSE                                                        
053100                 MOVE WC-LAND-CN TO W-IDLAND                              
053200              END-IF                                                      
053300              PERFORM IMS-GHU-WDK712                                      
053400              IF LART-VKART > 0                                           
053500                IF REQU-KDMATT = 'U'                                      
053600                   COMPUTE WS-RESP-VKART ROUNDED =                        
053700                                   LART-VKART * CONV-GR-TO-OZ             
053800                ELSE                                                      
053900                   MOVE LART-VKART TO WS-RESP-VKART                       
054000                END-IF                                                    
054100                MOVE WS-RESP-VKART TO RESP-VKART2                         
054200              END-IF                                                      
054300              IF LART-VLARTNTO > 0                                        
054400                IF REQU-KDMATT = 'U'                                      
054500                  COMPUTE WS-RESP-VLARTNTO ROUNDED =                      
054600                                   LART-VLARTNTO * CONV-CM3-TO-IN3        
054700                ELSE                                                      
054800                  MOVE LART-VLARTNTO  TO WS-RESP-VLARTNTO                 
054900                END-IF                                                    
055000                MOVE WS-RESP-VLARTNTO TO RESP-VLARTNTO                    
055100              END-IF                                                      
055200           END-IF                                                         
055300                                                                          
055400           PERFORM FA-LAS-ADRESS                                          
055500        END-IF                                                            
055600                                                                          
055700*       -- READ CHINESE OR ENGLISH BEART                                  
055800*       -- ENGLISH WILL BE TRANSLATED TO UNICODE                          
055900        MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                           
056000        IF DCS-UNICODE-IDSKYLT                                            
056100          MOVE 'UTF8'             TO TRAUTF8-KDCP                         
056200        ELSE                                                              
056300          MOVE '278 '             TO TRAUTF8-KDCP                         
056400        END-IF                                                            
056500                                                                          
056600        PERFORM IMS-GET-BENA11                                            
056700        IF SEGMENT-FINNS                                                  
056800          MOVE BENA-TEXT-BEART  TO TRAUTF8-TECONV-FROM                    
056900        END-IF                                                            
057000        IF TRAUTF8-TECONV-FROM = SPACES                                   
057100         MOVE 'GB'  TO W-IDSKYLT                                          
057200         MOVE '278' TO TRAUTF8-KDCP                                       
057300         PERFORM IMS-GET-BENA11                                           
057400         MOVE BENA-TEXT-BEART    TO TRAUTF8-TECONV-FROM                   
057500        END-IF                                                            
057600                                                                          
057700*       -- STRIP SPACE OR CONVERT TO UNICODE                              
057800        CALL WTRAUTF8 USING TRAUTF8-AREA                                  
057900                                                                          
058000        MOVE TRAUTF8-TECONV-TO   TO RESP-BEART                            
058100     END-IF                                                               
058200     .                                                                    
058300     EJECT                                                                
058400 FA-LAS-ADRESS SECTION.                                                   
058500                                                                          
058600     MOVE +1 TO TAB-IX                                                    
058700     PERFORM IMS-GU-WDK7A                                                 
058800     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
058900        IF SEGMENT-FINNS                                                  
059000           IF SEQA-IDARTNR = W-IDARTNR                                    
059100              CONTINUE                                                    
059200           ELSE                                                           
059300              MOVE SEQA-IDARTNR TO RESP-IDARTNR(TAB-IX)                   
059400              ADD +1 TO TAB-IX                                            
059500           END-IF                                                         
059600        ELSE                                                              
059700           ADD +1 TO TAB-IX                                               
059800        END-IF                                                            
059900        PERFORM IMS-GN-WDK7A                                              
060000     END-PERFORM                                                          
060100                                                                          
060200     IF SEGMENT-FINNS                                                     
060300        MOVE INF-SEE-SCREEN-6306 TO RESP-IDMSG-INFO                       
060400     END-IF                                                               
060500     .                                                                    
060600     EJECT                                                                
060700 G-KOLLA-INPUT SECTION.                                                   
060800                                                                          
060900                                                                          
061000     MOVE JA TO INDATA-SW                                                 
061100                                                                          
061200     IF REQU-ADLAGOMR  = ALL '+'                                          
061300     AND REQU-ADGANG   = ALL '+'                                          
061400     AND REQU-ADPLATS  = ALL '+'                                          
061500     AND REQU-VKART2   = ALL '+'                                          
061600     AND REQU-VLARTNTO = ALL '+'                                          
061700     AND REQU-KDVSOP   = ALL '+'                                          
061800*eT  AND REQU-VKART2   = ALL '+' Dessa två fält uppdateras inte           
061900*eT  AND REQU-VLARTNTO = ALL '+' längre från HTML. eT=3797315             
062000*eT                                                                       
062100*eT     Kod i JAVA Mid-Mod handler WL0104MM och MID-WL0104i1              
062200*eT     bör justeras vid annat tillfälle. /mvh Conny E                    
062300        MOVE NO-DATA-ENTERED TO RESP-IDMSG-INFO                           
062400        MOVE NEJ TO INDATA-SW                                             
062500     ELSE                                                                 
              IF REQU-ADLAGOMR = ALL '+'                                        
              OR REQU-ADGANG   = ALL '+'                                        
              OR REQU-ADPLATS  = ALL '+'                                        
               MOVE NO-DATA-ENTERED TO RESP-IDMSG-INFO                          
               MOVE NEJ TO INDATA-SW                                            
              END-IF                                                            
062600        IF REQU-ADLAGOMR NOT = ALL '+'                                    
062700           MOVE REQU-ADLAGOMR TO RESP-ADLAGOMR                            
062800           INSPECT REQU-ADLAGOMR REPLACING LEADING SPACE BY ZERO          
062900           IF REQU-ADLAGOMR NOT NUMERIC                                   
063000              MOVE 'ADLAGOMR' TO RESP-IDELMT-ERROR                        
063100              MOVE NEJ TO INDATA-SW                                       
063200           END-IF                                                         
063300        END-IF                                                            
063400        IF REQU-ADGANG NOT = ALL '+'                                      
063500           MOVE REQU-ADGANG TO RESP-ADGANG                                
063600           INSPECT REQU-ADGANG REPLACING LEADING SPACE BY ZERO            
063700           IF REQU-ADGANG NOT NUMERIC                                     
063800              MOVE 'ADGANG' TO RESP-IDELMT-ERROR                          
063900              MOVE NEJ TO INDATA-SW                                       
064000           END-IF                                                         
064100        END-IF                                                            
064200        IF REQU-ADPLATS NOT = ALL '+'                                     
064300           MOVE REQU-ADPLATS TO RESP-ADPLATS                              
064400           INSPECT REQU-ADPLATS REPLACING LEADING SPACE BY ZERO           
064500           IF REQU-ADPLATS NOT NUMERIC                                    
064600              MOVE 'ADPLATS' TO RESP-IDELMT-ERROR                         
064700              MOVE NEJ TO INDATA-SW                                       
064800           END-IF                                                         
064900        END-IF                                                            
065000                                                                          
065100*      VALIDATION FOR WEIGHT OF PART NUMBER                               
065200       IF REQU-VKART2         NOT = ALL '+'                               
065300         MOVE REQU-VKART2 TO RESP-VKART2                                  
065400         INSPECT REQU-VKART2  REPLACING LEADING SPACE BY ZERO             
065500         MOVE REQU-VKART2           TO DEC-IDFRIDATA                      
065600         MOVE  7                    TO DEC-KVHELTAL                       
065700         MOVE  2                    TO DEC-KVDECIMAL                      
065800         CALL WDECEDIT              USING DEC-WDECAREA                    
065900         IF DEC-KDSVAR-OK                                                 
066000           MOVE DEC-IDEDITDATA      TO WS-VKART-DEC                       
066100           IF WS-VKART-DEC  NOT NUMERIC                                   
066200             MOVE 'VKART'             TO RESP-IDELMT-ERROR                
066300             MOVE NEJ                 TO INDATA-SW                        
066400           ELSE                                                           
066500             IF REQU-KDMATT = 'U'                                         
066600               COMPUTE WS-VKART ROUNDED =                                 
066700                            WS-VKART-DEC * CONV-OZ-TO-GR                  
066800             ELSE                                                         
066900               MOVE WS-VKART-DEC      TO WS-VKART                         
067000             END-IF                                                       
067100*            9999999 = MAX-VÄRDE VIKT (GRAM)                              
067200             IF WS-VKART > 9999999                                        
067300               MOVE 'VKART'           TO RESP-IDELMT-ERROR                
067400               MOVE NEJ               TO INDATA-SW                        
067500             END-IF                                                       
067600           END-IF                                                         
067700         END-IF                                                           
067800       END-IF                                                             
067900                                                                          
068000*      VALIDATION FOR VOLUME OF PART NUMBER                               
068100       IF REQU-VLARTNTO          NOT = ALL '+'                            
068200         MOVE REQU-VLARTNTO TO RESP-VLARTNTO                              
068300         INSPECT REQU-VLARTNTO   REPLACING LEADING SPACE BY ZERO          
068400         MOVE REQU-VLARTNTO         TO DEC-IDFRIDATA                      
068500         MOVE  8                    TO DEC-KVHELTAL                       
068600         MOVE  1                    TO DEC-KVDECIMAL                      
068700         CALL WDECEDIT              USING DEC-WDECAREA                    
068800         IF DEC-KDSVAR-OK                                                 
068900           MOVE DEC-IDEDITDATA      TO WS-VLARTNTO                        
069000           IF WS-VLARTNTO NOT NUMERIC                                     
069100             MOVE 'VLARTNTO'        TO RESP-IDELMT-ERROR                  
069200             MOVE NEJ               TO INDATA-SW                          
069300           ELSE                                                           
069400             IF REQU-KDMATT = 'U'                                         
069500               COMPUTE WS-VLARTNTO ROUNDED =                              
069600                              WS-VLARTNTO * CONV-IN3-TO-CM3               
069700             END-IF                                                       
069800             MOVE WS-VLARTNTO       TO WS-VLARTNTO-DISP-NUM               
069900*            99999999.9 = MAX-VÄRDE VOLYM (CM3)                           
070000             IF WS-VLARTNTO > 99999999.9                                  
070100               MOVE 'VLARTNTO'        TO RESP-IDELMT-ERROR                
070200               MOVE NEJ               TO INDATA-SW                        
070300             END-IF                                                       
070400           END-IF                                                         
070500         ELSE                                                             
070600           MOVE 'VLARTNTO'        TO RESP-IDELMT-ERROR                    
070700           MOVE NEJ               TO INDATA-SW                            
070800         END-IF                                                           
070900       END-IF                                                             
071000                                                                          
071100*      VALIDATION FOR KDVSOP OF PART NUMBER                               
071200       IF REQU-KDVSOP            NOT = ALL '+'                            
071300         INSPECT REQU-KDVSOP     REPLACING LEADING SPACE BY ZERO          
071400         IF REQU-KDVSOP NOT NUMERIC                                       
071500           MOVE 'KDVSOP'         TO RESP-IDELMT-ERROR                     
071600           MOVE NEJ              TO INDATA-SW                             
071700         END-IF                                                           
071800       END-IF                                                             
071900                                                                          
072000*eT=3797315. Vikt & Volym kan inte längre Nollas här                      
072100        IF INDATA-FEL                                                     
072200           MOVE IS-INVALID TO RESP-IDMSG-ERROR                            
072300        ELSE                                                              
072400           PERFORM IMS-GU-ARTC01                                          
072500           IF SEGMENT-FINNS                                               
072600              IF REQU-ADLAGOMR NOT = ALL '+'                              
072700              OR REQU-ADPLATS NOT = ALL '+'                               
072800                 PERFORM IMS-GHU-WDK711                                   
072900                 IF SEGMENT-FINNS                                         
073000                    IF SLAG-KVLS > 0                                      
073100                       MOVE REQU-ADLAGOMR TO WS-ADLAGOMR                  
073200                       MOVE REQU-ADPLATS TO WS-ADPLATS                    
073300                       IF WS-ADLAGOMR = 0                                 
073400                       OR WS-ADPLATS = 0                                  
073500                          MOVE NEJ TO INDATA-SW                           
073600                          IF WS-ADLAGOMR = 0                              
073700                             MOVE ZERO-NOT-ALLOWED TO                     
073800                                      RESP-IDMSG-ERROR                    
073900                             MOVE NEJ TO INDATA-SW                        
074000                          END-IF                                          
074100                          IF WS-ADPLATS = 0                               
074200                             MOVE ZERO-NOT-ALLOWED TO                     
074300                                      RESP-IDMSG-ERROR                    
074400                             MOVE NEJ TO INDATA-SW                        
074500                          END-IF                                          
074600                       END-IF                                             
074700                    END-IF                                                
074800                 ELSE                                                     
074900                   MOVE NEJ TO INDATA-SW                                  
075000                   MOVE 'IDARTNR' TO RESP-IDELMT-ERROR                    
075100                   MOVE NOT-FOUND TO RESP-IDMSG-ERROR                     
075200                 END-IF                                                   
075300              END-IF                                                      
075400           ELSE                                                           
075500              MOVE NEJ TO INDATA-SW                                       
075600              MOVE 'IDARTNR' TO RESP-IDELMT-ERROR                         
075700              MOVE NOT-FOUND TO RESP-IDMSG-ERROR                          
075800           END-IF                                                         
075900        END-IF                                                            
076000     END-IF                                                               
076100     .                                                                    
076200     EJECT                                                                
076300 H-UPPDATERA SECTION.                                                     
076400                                                                          
076500     MOVE NEJ TO WS-UPD-LOCATION                                          
076600     IF REQU-ADLAGOMR NOT = ALL '+'                                       
076700     OR REQU-ADGANG   NOT = ALL '+'                                       
076800     OR REQU-ADPLATS  NOT = ALL '+'                                       
076900        MOVE JA TO WS-UPD-LOCATION                                        
077000        PERFORM IMS-GHU-WDK711                                            
077100        MOVE REQU-ADLAGOMR TO WS-ADLAGOMR-CMP                             
077200        MOVE REQU-ADGANG   TO WS-ADGANG-CMP                               
077300        MOVE REQU-ADPLATS    TO WS-ADPLATS-CMP                            
077400                                                                          
077500        IF WS-ADLAGOMR-CMP = SLAG-ADLAGOMR                                
077600        AND WS-ADGANG-CMP  = SLAG-ADGANG                                  
077700        AND WS-ADPLATS-CMP = SLAG-ADPLATS                                 
077800         MOVE NEJ TO WS-UPD-LOCATION                                      
077900        END-IF                                                            
078000        IF SEGMENT-FINNS                                                  
078100           IF REQU-ADLAGOMR NOT = ALL '+'                                 
078200              MOVE REQU-ADLAGOMR    TO SLAG-ADLAGOMR                      
078300                                       WS-ADLAGOMR-WDJ9                   
078400           ELSE                                                           
078500              MOVE SLAG-ADLAGOMR    TO WS-ADLAGOMR-WDJ9                   
078600           END-IF                                                         
078700                                                                          
078800           IF REQU-ADGANG  NOT = ALL '+'                                  
078900              MOVE REQU-ADGANG      TO SLAG-ADGANG                        
079000                                       WS-ADGANG-WDJ9                     
079100           ELSE                                                           
079200              MOVE SLAG-ADGANG      TO WS-ADGANG-WDJ9                     
079300           END-IF                                                         
079400                                                                          
079500           IF REQU-ADPLATS NOT = ALL '+'                                  
079600              MOVE REQU-ADPLATS     TO SLAG-ADPLATS                       
079700                                       WS-ADPLATS-WDJ9                    
079800           ELSE                                                           
079900              MOVE SLAG-ADPLATS     TO WS-ADPLATS-WDJ9                    
080000           END-IF                                                         
080100                                                                          
080200           PERFORM IMS-REPL-WDK711                                        
080300                                                                          
080400           IF NDC-CN OR LDC-CN OR NDC-US                                  
080500             IF (NDC-CN OR NDC-US OR LDC-CN)                              
080600             AND SLAG-IDDC-REF = SPACES                                   
080800                   IF NDC-US                                              
080900                      MOVE WC-LAND-US     TO LART-IDLANDX2                
081000                                             W-IDLAND                     
081100                   ELSE                                                   
081200                      MOVE WC-LAND-CN     TO LART-IDLANDX2                
081300                                             W-IDLAND                     
081400                   END-IF                                                 
081500                   PERFORM IMS-GHU-WDK712                                 
081600                   IF WS-VKART NOT = LART-VKART AND                       
081700                      WS-VKART NOT = 0                                    
081800                      MOVE WS-VKART    TO LART-VKART                      
081900                      MOVE NEJ TO LART-FLMSKUPD                           
082000                      MOVE JA TO WS-UPD                                   
082100                   END-IF                                                 
082200                   IF WS-VLARTNTO-DISP-NUM NOT = LART-VLARTNTO AND        
082300                      WS-VLARTNTO-DISP-NUM NOT = 0                        
082400                      MOVE WS-VLARTNTO-DISP-NUM TO LART-VLARTNTO          
082500                      MOVE NEJ TO LART-FLMSKUPD                           
082600                      MOVE JA  TO WS-UPD                                  
082700                   END-IF                                                 
082800                   IF WS-UPD = JA                                         
082900                      PERFORM IMS-REPL-WDK712                             
083000                   END-IF                                                 
083100** UPDATE COUNTRY SEGMENT - START                                         
083200                   IF NDC-CN                                              
083300                      MOVE '40'        TO W-IDDC-MIN-1                    
083400                                          W-IDDC-MAX-1                    
083500                      MOVE 'A'         TO W-IDDC-MIN-1(2:1)               
083600                      MOVE '9'         TO W-IDDC-MAX-1(2:1)               
083700                      MOVE '70'        TO W-IDDC-REF-MIN                  
083800                                          W-IDDC-REF-MAX                  
083900                      MOVE 'A'         TO W-IDDC-REF-MIN(2:1)             
084000                      MOVE '9'         TO W-IDDC-REF-MAX(2:1)             
084100                   END-IF                                                 
084200                                                                          
084300                   IF NDC-US                                              
084400                         MOVE '70'     TO W-IDDC-MIN-1                    
084500                                          W-IDDC-MAX-1                    
084600                         MOVE 'A'      TO W-IDDC-MIN-1(2:1)               
084700                         MOVE '9'      TO W-IDDC-MAX-1(2:1)               
084800                                                                          
084900                         MOVE '40'     TO W-IDDC-REF-MIN                  
085000                                          W-IDDC-REF-MAX                  
085100                         MOVE 'A'      TO W-IDDC-REF-MIN(2:1)             
085200                         MOVE '9'      TO W-IDDC-REF-MAX(2:1)             
085300                   END-IF                                                 
085400                                                                          
085500                   PERFORM IMS-GU-WDK711-REF                              
085600                   IF SEGMENT-FINNS                                       
085700                      IF NDC-US                                           
085800                         MOVE WC-LAND-CN     TO LART-IDLANDX2             
085900                                                W-IDLAND                  
086000                      ELSE                                                
086100                         MOVE WC-LAND-US     TO LART-IDLANDX2             
086200                                                W-IDLAND                  
086300                      END-IF                                              
086400                      PERFORM IMS-GHU-WDK712                              
086500                      IF WS-VKART NOT = LART-VKART AND                    
086600                         WS-VKART NOT = 0                                 
086700                         MOVE WS-VKART    TO LART-VKART                   
086800                         MOVE NEJ TO LART-FLMSKUPD                        
086900                         MOVE JA TO WS-UPD                                
087000                      END-IF                                              
087100                      IF WS-VLARTNTO-DISP-NUM NOT = LART-VLARTNTO         
087200                        AND WS-VLARTNTO-DISP-NUM NOT = 0                  
087300                        MOVE WS-VLARTNTO-DISP-NUM TO LART-VLARTNTO        
087400                        MOVE NEJ TO LART-FLMSKUPD                         
087500                        MOVE JA TO WS-UPD                                 
087600                      END-IF                                              
087700                      IF WS-UPD = JA                                      
087800                        PERFORM IMS-REPL-WDK712                           
087900                      END-IF                                              
088000                   END-IF                                                 
088100** UPDATE COUNTRY SEGMENT - END                                           
088200                                                                          
088300             END-IF                                                       
088400           END-IF                                                         
088500                                                                          
088600           PERFORM IMS-GU-LOCB01                                          
088700           IF SEGMENT-SAKNAS                                              
088800              MOVE W-IDARTNR TO LOCB-ART-IDARTNR                          
088900              PERFORM IMS-ISRT-LOCB01                                     
089000              PERFORM IMS-GU-LOCB01                                       
089100           END-IF                                                         
089200           IF SEGMENT-FINNS                                               
089300             PERFORM UNTIL SEGMENT-SAKNAS OR LOCB-HIST-KDLOC = 'P'        
089400                PERFORM IMS-GHNP-LOCB11                                   
089500                IF SEGMENT-FINNS AND LOCB-HIST-KDLOC = 'P'                
089600                   MOVE FUNCTION CURRENT-DATE(1:8) TO                     
089700                                   LOCB-HIST-DASTODAT                     
089800                   MOVE REQU-IDUSER TO LOCB-HIST-IDUSER-STO               
089900                   PERFORM IMS-REPL-LOCB11                                
090000                END-IF                                                    
090100             END-PERFORM                                                  
090200             MOVE FUNCTION CURRENT-DATE(1:8)  TO LOGG-DATUM               
090300             MOVE FUNCTION CURRENT-DATE(9:6)  TO LOGG-TID                 
090400             COMPUTE LOCB-HIST-DASTADAT-9KOMPL = 99999999 -               
090500                                    LOGG-DATUM                            
090600             COMPUTE LOCB-HIST-TISTATID-9KOMPL = 999999 - LOGG-TID        
090700             MOVE REQU-IDDC-KEY      TO LOCB-HIST-IDDC                    
090800             MOVE PRIME-LOCATION     TO LOCB-HIST-KDLOC                   
090900             MOVE WS-ADLAGOMR-WDJ9   TO LOCB-HIST-ADLAGOMR                
091000             MOVE WS-ADGANG-WDJ9     TO LOCB-HIST-ADGANG                  
091100             MOVE WS-ADPLATS-WDJ9    TO LOCB-HIST-ADPLATS                 
091200             MOVE REQU-IDUSER        TO LOCB-HIST-IDUSER                  
091300             MOVE SPACE              TO LOCB-HIST-IDUSER-STO              
091400             MOVE ZERO               TO LOCB-HIST-DASTODAT                
091500                                                                          
091600             PERFORM IMS-ISRT-LOCB11                                      
091700          END-IF                                                          
091800        END-IF                                                            
091900                                                                          
092000     END-IF                                                               
092100                                                                          
092200     IF REQU-ADLAGOMR  NOT = ALL '+'                                      
092300     OR  REQU-ADGANG   NOT = ALL '+'                                      
092400     OR  REQU-ADPLATS  NOT = ALL '+'                                      
092500     OR  REQU-VKART2   NOT = ALL '+'                                      
092600     OR  REQU-VLARTNTO NOT = ALL '+'                                      
092700       PERFORM HA-STARTA-W40289                                           
092800       PERFORM I-STARTA-DISPATCHEN                                        
092900     END-IF                                                               
093000                                                                          
093100     IF SLAG-IDDC-REF = SPACES                                            
093200       PERFORM IMS-GHU-ARTC11                                             
093300       IF SEGMENT-FINNS                                                   
093400         MOVE NEJ TO WS-UPD                                               
093500                     WS-UPD-VOLUME                                        
093600                                                                          
093700         MOVE CLAG-IDDC-REF             TO REF-WS-IDDC                    
093800         IF REQU-KDVSOP NOT = CLAG-KDVSOP                                 
093900           MOVE REQU-KDVSOP             TO CLAG-KDVSOP                    
094000           MOVE REQU-IDUSER  TO CLAG-IDUSER-VUPD                          
094100           MOVE DAGENS-DATUM TO CLAG-TIUPPDAT-VUPD                        
094200           MOVE JA TO WS-UPD                                              
094300         END-IF                                                           
094400         IF ((NDC-CN OR LDC-CN) AND REF-NDC-CN)                           
094500         OR (NDC-US AND REF-NDC-US)                                       
094600         OR ((NDC-CN OR LDC-CN OR NDC-US) AND (CLAG-VKART = 0 OR          
094700              CLAG-VLARTNTO = 0))                                         
094800           IF WS-VKART NOT = CLAG-VKART AND                               
094900              WS-VKART NOT = 0                                            
095000              MOVE REQU-IDUSER  TO CLAG-IDUSER-VUPD                       
095100              MOVE DAGENS-DATUM TO CLAG-TIUPPDAT-VUPD                     
095200              MOVE WS-VKART     TO CLAG-VKART                             
095300              MOVE JA TO WS-UPD                                           
095400              IF CLAG-VKART-NTO = ZERO OR                                 
095500                 CLAG-VKART-NTO > CLAG-VKART                              
095600                MOVE CLAG-VKART TO CLAG-VKART-NTO                         
095700                MOVE '4'        TO CLAG-KDUVKNTO                          
095800              END-IF                                                      
095900           END-IF                                                         
096000           IF WS-VLARTNTO-DISP-NUM NOT = CLAG-VLARTNTO AND                
096100              WS-VLARTNTO-DISP-NUM NOT = 0                                
096200              MOVE REQU-IDUSER  TO CLAG-IDUSER-VUPD                       
096300              MOVE DAGENS-DATUM TO CLAG-TIUPPDAT-VUPD                     
096400              IF CLAG-VLARTNTO = ZERO                                     
096500                MOVE 002        TO SYNQ-KDCALL                            
096600              ELSE                                                        
096700                MOVE 003        TO SYNQ-KDCALL                            
096800              END-IF                                                      
096900              MOVE WS-VLARTNTO-DISP-NUM TO CLAG-VLARTNTO                  
097000              MOVE JA TO WS-UPD                                           
097100                         WS-UPD-VOLUME                                    
097200           END-IF                                                         
097300         ELSE                                                             
097400         IF NOT (NDC-CN OR LDC-CN OR NDC-US)                              
097500           IF WS-VKART NOT = CLAG-VKART AND                               
097600              WS-VKART NOT = 0                                            
097700              MOVE REQU-IDUSER  TO CLAG-IDUSER-VUPD                       
097800              MOVE DAGENS-DATUM TO CLAG-TIUPPDAT-VUPD                     
097900              MOVE WS-VKART     TO CLAG-VKART                             
098400              MOVE JA TO WS-UPD                                           
098000              IF CLAG-VKART-NTO = ZERO OR                                 
098100                 CLAG-VKART-NTO > CLAG-VKART                              
098200                MOVE CLAG-VKART TO CLAG-VKART-NTO                         
098300                MOVE '4'        TO CLAG-KDUVKNTO                          
098500              END-IF                                                      
098600           END-IF                                                         
098700           IF WS-VLARTNTO-DISP-NUM NOT = CLAG-VLARTNTO AND                
098800              WS-VLARTNTO-DISP-NUM NOT = 0                                
098900              MOVE REQU-IDUSER  TO CLAG-IDUSER-VUPD                       
099000              MOVE DAGENS-DATUM TO CLAG-TIUPPDAT-VUPD                     
099100              IF CLAG-VLARTNTO = ZERO                                     
099200                MOVE 002        TO SYNQ-KDCALL                            
099300              ELSE                                                        
099400                MOVE 003        TO SYNQ-KDCALL                            
099500              END-IF                                                      
099600              MOVE WS-VLARTNTO-DISP-NUM TO CLAG-VLARTNTO                  
099700              MOVE JA TO WS-UPD                                           
099800                         WS-UPD-VOLUME                                    
099900           END-IF                                                         
100000         END-IF                                                           
100100         END-IF                                                           
100200         IF WS-UPD = JA                                                   
100300           PERFORM IMS-REPL-ARTC11                                        
100400           IF WS-UPD-VOLUME = JA                                          
100500             MOVE W-IDARTNR         TO SYNQ-IDARTNR                       
100600             CALL W488PRMA USING  SYNQ-W488PRMA                           
100700             SYNQ-ATAB-PCB SYNQ-WDK6-PCB SYNQ-WDD3-PCB                    
100800           END-IF                                                         
100900         END-IF                                                           
101000       END-IF                                                             
101100     END-IF                                                               
101200                                                                          
101300     MOVE UPDATE-DONE TO RESP-IDMSG-INFO                                  
101400     .                                                                    
101500     EJECT                                                                
101600 HA-STARTA-W40289 SECTION.                                                
101700                                                                          
101800     MOVE W-IDARTNR            TO P-TO-P2-MID-IDARTNR-IN                  
101900     MOVE REQU-IDDC-KEY        TO P-TO-P2-MID-IDDC-IN                     
102000     IF WS-UPD-LOCATION = JA                                              
102100      MOVE WS-ADLAGOMR-WDJ9     TO P-TO-P2-MID-ADLAGOMR-IN                
102200      MOVE WS-ADGANG-WDJ9       TO P-TO-P2-MID-ADGANG-IN                  
102300      MOVE WS-ADPLATS-WDJ9      TO P-TO-P2-MID-ADPLATS-IN                 
102400     ELSE                                                                 
102500      MOVE ZERO              TO P-TO-P2-MID-ADLAGOMR-IN                   
102600      MOVE ZERO              TO P-TO-P2-MID-ADGANG-IN                     
102700      MOVE ZERO              TO P-TO-P2-MID-ADPLATS-IN                    
102710      MOVE ZERO              TO P-TO-P2-MID-IDDC-IN                       
102800     END-IF                                                               
102900     IF WS-VKART > 0 AND WS-UPD = JA                                      
103000     MOVE WS-VKART             TO P-TO-P2-MID-VKART-IN                    
103200     END-IF                                                               
103300     IF WS-VLARTNTO > 0 AND WS-UPD = JA                                   
103400     MOVE WS-VLARTNTO          TO P-TO-P2-MID-VLARTNTO-IN                 
103600     END-IF                                                               
103700     MOVE ZERO                 TO P-TO-P2-MID-IDDISTR-IN                  
103800                                  P-TO-P2-MID-IDKUNDNR-IN                 
103900                                  P-TO-P2-MID-IDORDNR5-IN                 
104000                                  P-TO-P2-MID-IDORDER-IN                  
104100                                  P-TO-P2-MID-IDLOPNR-IN                  
104200                                                                          
104300     COMPUTE P-TO-P2-KVLL   =  LENGTH OF P-TO-P2-MID-W4I28901 + 25        
104400     END-COMPUTE                                                          
104500                                                                          
104600     MOVE 'W4T289X '           TO P-TO-P2-KDTRANS                         
104700     MOVE 'L104'               TO P-TO-P2-IDTRANS                         
104800     MOVE '2'                  TO P-TO-P2-KDMFSFOR                        
104900                                                                          
105000     PERFORM IMS-PURG-4289                                                
105100     .                                                                    
105200     EJECT                                                                
105300 I-STARTA-DISPATCHEN   SECTION.                                           
105400                                                                          
105500     MOVE SPACE                 TO MSG-KOM-WMSGKOM                        
105600     MOVE +54                   TO MSG-KOM-KVLL                           
105700     MOVE LOW-VALUE             TO MSG-KOM-KDZ1                           
105800     MOVE LOW-VALUE             TO MSG-KOM-KDZ2                           
105900     MOVE SPACE                 TO MSG-KOM-KDTRANS                        
106000     MOVE 'W6I19B01'            TO MSG-KOM-IDCPYTXT                       
106100     MOVE 'INLEV   '            TO MSG-KOM-IDSNDNOD                       
106200     MOVE 'WL010400'            TO MSG-KOM-IDSNDJOB                       
106300     MOVE DAGENS-DATUM          TO MSG-KOM-TIREGDAT                       
106400     MOVE DAGENS-TID            TO MSG-KOM-TIKLOCK                        
106500     MOVE SPACE                 TO MSG-KOM-IDMFSMED                       
106600                                                                          
106700     MOVE ALL '+'               TO MOD619B-MID-W6I19B01                   
106800     MOVE W-IDARTNR             TO MOD619B-MID-IDARTNR                    
106900     MOVE REQU-IDDC-KEY         TO MOD619B-MID-IDDC                       
107000     MOVE REQU-ADLAGOMR         TO MOD619B-MID-ADLAGOMR                   
107100     MOVE REQU-ADGANG           TO MOD619B-MID-ADGANG                     
107200     MOVE REQU-ADPLATS          TO MOD619B-MID-ADPLATS                    
107300     MOVE WS-VKART              TO MOD619B-MID-VKART                      
107400     MOVE WS-VLARTNTO-DISP-ALFA TO MOD619B-MID-VLARTNTO                   
107500     COMPUTE P-TO-P-KVLL        =  LNG-P-TO-P-PREFIX + 87                 
107600     MOVE 'W6T19BX '            TO P-TO-P-KDTRANS                         
107700     MOVE 'L104'                TO P-TO-P-IDTRANS                         
107800     MOVE '2'                   TO P-TO-P-KDMFSFOR                        
107900     MOVE MOD619B-MID-W6I19B01  TO P-TO-P-DATA                            
108000                                                                          
108100     CALL W006KOM USING MSG-PCB                                           
108200                        DISP-PCB                                          
108300                        KOM-KOMA-PCB                                      
108400                        MSG-KOM-WMSGKOM                                   
108500                        P-TO-P-SW                                         
108600     .                                                                    
108700     EJECT                                                                
108800 S01-HAEMTA-ANROPSDATA SECTION.                                           
108900                                                                          
109000     MOVE 'GETARG'               TO SUB-KDFUNC                            
109100     MOVE WS-ADRESS              TO SUB-ADDISPABS                         
109200     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
109300                                                                          
109400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
109500                                                                          
109600     IF SUB-KDRC > 0                                                      
109700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
109800       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
109900       DELIMITED BY SIZE INTO FELTEXT                                     
110000       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
110100     END-IF                                                               
110200     .                                                                    
110300     SKIP3                                                                
110400 S02-RETURNERA-SVAR SECTION.                                              
110500                                                                          
110600     MOVE 'RETURN'                   TO SUB-KDFUNC                        
110700     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
110800                                                                          
110900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
111000                                                                          
111100     IF SUB-KDRC > 0                                                      
111200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
111300       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
111400       DELIMITED BY SIZE INTO FELTEXT                                     
111500       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
111600     END-IF                                                               
111700     .                                                                    
111800     EJECT                                                                
111900 S11-MSG-CONV SECTION.                                                    
112000     MOVE SPACES                  TO RESP-MESSAGES (1)                    
112100                                     RESP-MESSAGES (2)                    
112200     MOVE 1                       TO MSG-IX                               
112300*    REQUEST OK                                                           
112400     MOVE 200                     TO RESP-KDSTATUS-API                    
112500     IF RESP-IDMSG-INFO > SPACE                                           
112600       MOVE SPACES                TO MSG-CONV-AREA                        
112700       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
112800       CALL WMSGCONV           USING MSG-CONV-AREA                        
112900       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
113000       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
113100       ADD 1                      TO MSG-IX                               
113200     END-IF                                                               
113300     IF RESP-IDMSG-ERROR > SPACE                                          
113400*      BAD REQUEST                                                        
113500       MOVE 400                   TO RESP-KDSTATUS-API                    
113600       MOVE SPACES                TO MSG-CONV-AREA                        
113700       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
113800       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
113900       CALL WMSGCONV           USING MSG-CONV-AREA                        
114000       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
114100       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
114200     END-IF                                                               
114300     .                                                                    
114400     EJECT                                                                
114500* --- IMS SEKTIONER ---                                                   
114600     SKIP3                                                                
114700 IMS-PURG-4289 SECTION.                                                   
114800     MOVE LOW-VALUE TO P-TO-P2-KDZ1 P-TO-P2-KDZ2                          
114900     MOVE SPACE TO GODK-STATUSKODER                                       
115000     CALL  CBLTDLI  USING PURG 4289-PCB P-TO-P-SW2                        
115100     MOVE 4289-STATUS-CODE TO STATUS-WS                                   
115200     PERFORM IMS-STATUSKONTROLL                                           
115300     .                                                                    
115400     SKIP3                                                                
115500 IMS-GU-ARTC01 SECTION.                                                   
115600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
115700          DELIMITED BY SIZE INTO SSA1                                     
115800     MOVE '  GE' TO GODK-STATUSKODER                                      
115900     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-WDK6 SSA1                 
116000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
116100     PERFORM IMS-STATUSKONTROLL                                           
116200     .                                                                    
116300     SKIP3                                                                
116400 IMS-GU-ARTC11 SECTION.                                                   
116500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
116600          DELIMITED BY SIZE INTO SSA1                                     
116700     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
116800          DELIMITED BY SIZE INTO SSA2                                     
116900     MOVE '  GE' TO GODK-STATUSKODER                                      
117000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-WDK6 SSA1 SSA2            
117100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
117200     PERFORM IMS-STATUSKONTROLL                                           
117300     .                                                                    
117400     EJECT                                                                
117500 IMS-GHU-ARTC11 SECTION.                                                  
117600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
117700          DELIMITED BY SIZE INTO SSA1                                     
117800     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
117900          DELIMITED BY SIZE INTO SSA2                                     
118000     MOVE '  GE' TO GODK-STATUSKODER                                      
118100     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA-WDK6 SSA1 SSA2           
118200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
118300     PERFORM IMS-STATUSKONTROLL                                           
118400     .                                                                    
118500     EJECT                                                                
118600 IMS-REPL-ARTC11 SECTION.                                                 
118700     MOVE '  ' TO GODK-STATUSKODER                                        
118800     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-WDK6                    
118900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
119000     PERFORM IMS-STATUSKONTROLL                                           
119100     .                                                                    
119200     EJECT                                                                
119300 IMS-GHU-WDK711 SECTION.                                                  
119400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
119500          DELIMITED BY SIZE INTO SSA1                                     
119600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
119700          DELIMITED BY SIZE INTO SSA2                                     
119800     MOVE '  GE' TO GODK-STATUSKODER                                      
119900     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2         
120000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
120100     PERFORM IMS-STATUSKONTROLL                                           
120200     .                                                                    
120300     SKIP3                                                                
120400 IMS-GU-WDK711 SECTION.                                                   
120500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
120600          DELIMITED BY SIZE INTO SSA1                                     
120700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
120800          DELIMITED BY SIZE INTO SSA2                                     
120900     MOVE '  GE' TO GODK-STATUSKODER                                      
121000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2          
121100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
121200     PERFORM IMS-STATUSKONTROLL                                           
121300     .                                                                    
121400     SKIP3                                                                
121500 IMS-REPL-WDK711 SECTION.                                                 
121600     MOVE '  ' TO GODK-STATUSKODER                                        
121700     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-WDK711                  
121800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
121900     PERFORM IMS-STATUSKONTROLL                                           
122000     .                                                                    
122100     EJECT                                                                
122200 IMS-GU-WDK711-REF SECTION.                                               
122300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
122400          DELIMITED BY SIZE INTO SSA1                                     
122500     STRING 'WDK711  (IDDC    >=' W-IDDC-MIN-X                            
122600                    '&IDDC    <=' W-IDDC-MAX-X                            
122700                    '&IDDCREF >=' W-IDDC-REF-MIN-X                        
122800                    '&IDDCREF <=' W-IDDC-REF-MAX-X ')'                    
122900          DELIMITED BY SIZE INTO SSA2                                     
123000     MOVE '  GE' TO GODK-STATUSKODER                                      
123100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2          
123200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
123300     PERFORM IMS-STATUSKONTROLL                                           
123400     .                                                                    
123500     EJECT                                                                
123600 IMS-GU-LOCB01 SECTION.                                                   
123700     STRING 'WLLOCB01(IDARTNR  =' W-IDARTNR-X ')'                         
123800          DELIMITED BY SIZE INTO SSA1                                     
123900     MOVE '  GE' TO GODK-STATUSKODER                                      
124000     CALL CBLTDLI USING GU LOCB-PCB DLI-IO-WLLOCB01 SSA1                  
124100     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
124200     PERFORM IMS-STATUSKONTROLL                                           
124300     .                                                                    
124400     SKIP3                                                                
124500 IMS-ISRT-LOCB01 SECTION.                                                 
124600     MOVE 'WLLOCB01 ' TO SSA1                                             
124700     MOVE '  ' TO GODK-STATUSKODER                                        
124800     CALL CBLTDLI USING ISRT LOCB-PCB DLI-IO-WLLOCB01 SSA1                
124900     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
125000     PERFORM IMS-STATUSKONTROLL                                           
125100     .                                                                    
125200     SKIP3                                                                
125300 IMS-GHNP-LOCB11 SECTION.                                                 
125400     STRING 'WLLOCB11(IDDC     =' W-IDDC-X ')'                            
125500             DELIMITED BY SIZE INTO SSA1                                  
125600     MOVE '  GE' TO GODK-STATUSKODER                                      
125700     CALL CBLTDLI USING GHNP LOCB-PCB DLI-IO-WLLOCB11 SSA1                
125800     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
125900     PERFORM IMS-STATUSKONTROLL                                           
126000     .                                                                    
126100     SKIP3                                                                
126200 IMS-REPL-LOCB11 SECTION.                                                 
126300     MOVE '  ' TO GODK-STATUSKODER                                        
126400     CALL CBLTDLI USING REPL LOCB-PCB DLI-IO-WLLOCB11                     
126500     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
126600     PERFORM IMS-STATUSKONTROLL                                           
126700     .                                                                    
126800     EJECT                                                                
126900 IMS-ISRT-LOCB11 SECTION.                                                 
127000     STRING 'WLLOCB01(IDARTNR  =' W-IDARTNR-X ')'                         
127100          DELIMITED BY SIZE INTO SSA1                                     
127200     MOVE 'WLLOCB11 ' TO SSA2                                             
127300     MOVE '  II' TO GODK-STATUSKODER                                      
127400     CALL CBLTDLI USING ISRT LOCB-PCB DLI-IO-WLLOCB11 SSA1 SSA2           
127500     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
127600     PERFORM IMS-STATUSKONTROLL                                           
127700     .                                                                    
127800     SKIP3                                                                
127900 IMS-GU-WDK7A SECTION.                                                    
128000     STRING 'WDK7A1  (WDK7A1KY=>' WDK7A1KY-MIN-X                          
128100                    '&WDK7A1KY=<' WDK7A1KY-MAX-X ')'                      
128200          DELIMITED BY SIZE INTO SSA1                                     
128300     MOVE '  GE' TO GODK-STATUSKODER                                      
128400     CALL CBLTDLI USING GU WDK7A-PCB DLI-IO-AREA-WDK7A SSA1               
128500     MOVE WDK7A-STATUS-CODE TO STATUS-WS                                  
128600     PERFORM IMS-STATUSKONTROLL                                           
128700     .                                                                    
128800     SKIP3                                                                
128900 IMS-GN-WDK7A SECTION.                                                    
129000     STRING 'WDK7A1  (WDK7A1KY=>' WDK7A1KY-MIN-X                          
129100                    '&WDK7A1KY=<' WDK7A1KY-MAX-X ')'                      
129200          DELIMITED BY SIZE INTO SSA1                                     
129300     MOVE '  GE' TO GODK-STATUSKODER                                      
129400     CALL CBLTDLI USING GN WDK7A-PCB DLI-IO-AREA-WDK7A SSA1               
129500     MOVE WDK7A-STATUS-CODE TO STATUS-WS                                  
129600     PERFORM IMS-STATUSKONTROLL                                           
129700     .                                                                    
129800     EJECT                                                                
129900 IMS-GU-WDB601 SECTION.                                                   
130000                                                                          
130100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
130200          DELIMITED BY SIZE INTO SSA1                                     
130300     MOVE '  ' TO GODK-STATUSKODER                                        
130400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
130500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
130600     PERFORM IMS-STATUSKONTROLL                                           
130700     .                                                                    
130800     SKIP3                                                                
130900 IMS-GET-BENA11 SECTION.                                                  
131000     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
131100             DELIMITED BY SIZE INTO SSA1                                  
131200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
131300              DELIMITED BY SIZE INTO SSA2                                 
131400     MOVE '  GE' TO GODK-STATUSKODER                                      
131500     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA11 SSA1 SSA2             
131600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
131700     PERFORM IMS-STATUSKONTROLL                                           
131800     .                                                                    
131900     EJECT                                                                
132000 IMS-GHU-WDK712 SECTION.                                                  
132100     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
132200          DELIMITED BY SIZE INTO SSA1                                     
132300     STRING 'WDK712  (IDLAND  = ' W-IDLAND-X ')'                          
132400          DELIMITED BY SIZE INTO SSA2                                     
132500     MOVE '    ' TO GODK-STATUSKODER                                      
132600     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK712 SSA1 SSA2         
132700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
132800     PERFORM IMS-STATUSKONTROLL                                           
132900     .                                                                    
133000     EJECT                                                                
133100 IMS-REPL-WDK712 SECTION.                                                 
133200     MOVE '  ' TO GODK-STATUSKODER                                        
133300     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-WDK712                  
133400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
133500     PERFORM IMS-STATUSKONTROLL                                           
133600     .                                                                    
133700     EJECT                                                                
133800 IMS-STATUSKONTROLL SECTION.                                              
133900                                                                          
134000     SET STATUS-IX TO 1                                                   
134100     SEARCH GODK-STATUS                                                   
134200       AT END                                                             
134300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
134400         DELIMITED BY SIZE INTO FELTEXT                                   
134500         CALL FELLOG                                                      
134600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
134700         CONTINUE                                                         
134800     END-SEARCH                                                           
134900     .                                                                    
