000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4026400.                                                
000400 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000500 DATE-WRITTEN.   91/04/15.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*                                                                         
001100*        MHA 4264-BILDEN KAN MAN FRÅGA PÅ OCH ÄNDRA                       
001200*        PROFORMA-ORDERHUVUDEN.                                           
001300*        ETT ANTAL SUBPROGRAM ANROPAS FÖR FORMELL OCH LOGISK              
001400*        KONTROLL, LÄSNING AV KUNDREGISTER,                               
001500*        UPPDATERING AV PROFORMA-HUVUD SAMT HANTERING AV                  
001600*        WOPS-UPPGIFTER.                                                  
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W4T264                                              
002000*                     W4T264U                                             
002100*        MID:         W4I26401                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W4O26401                                            
002500*                                                                         
002600*  BASER:                                                                 
002700*        FYSISKT  LOGISKT    COPYTEXT    PREFIX (COPYTEXT)                
002800*  WDE8  WDE801   WLPROC01   WDE801      PHUV-                            
002900*                                                                         
003000                                                                          
003100     EJECT                                                                
003200     SKIP1                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     SKIP2                                                                
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003700*    -COPY WY2000W1                                                       
003800     SKIP3                                                                
003900 77  IDPGM                       PIC X(08)   VALUE 'W4026400'.            
004000 77  FELTEXT-VID-CALL-ABEND      PIC X(64)   VALUE SPACE.                 
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004400 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004500 77  ARBTAB-IX                   PIC S9(9)   VALUE +0   COMP SYNC.        
004600 77  ARBTAB-IX-MAX-99            PIC S9(9)   VALUE +99  COMP SYNC.        
004700*77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +513 COMP SYNC.        
004800 77  RKOD-ABEND-KREG             PIC S9(4)   VALUE +33  COMP SYNC.        
004900                                                                          
005000 77  WS-TIORDDAT                 PIC 9(6).                                
005100 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
005200 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
005300 77  WS-IDORDNR7                 PIC X(7)    VALUE SPACE.                 
005400 77  WS-KDFAKTYP                 PIC X(1)    VALUE SPACE.                 
005500 77  WS-FLBORT                   PIC X(1)    VALUE SPACE.                 
005600 77  WS-REOMRTAL-SPAR            PIC S9(2)V9(3) COMP-3.                   
005700 77  WS-VARNINGS-TEXT            PIC X(60)   VALUE                        
005800     'TRYCK PF11 FÖR ANNULLERING, TRYCK ENTER FÖR ATT ÅTERGÅ'.            
005900 01  WS-TALLY                    PIC S9(1)   VALUE +0.                    
006000 01  WS-TALLY-NUM      REDEFINES WS-TALLY                                 
006100                                 PIC 9(1).                                
006200 01  WS-BEKUNDRF                 PIC X(15)   VALUE SPACE.                 
006300 01  WS-IDFTG                    PIC X(2)    VALUE SPACE.                 
006400 01  WS-IDFTG-NUM      REDEFINES WS-IDFTG                                 
006500                                 PIC 9(2).                                
006600 01  WS-REOMRTAL                 PIC X(5)    VALUE SPACE.                 
006700 01  WS-REOMRTAL-NUM    REDEFINES WS-REOMRTAL                             
006800                                 PIC 9(1).9(3).                           
006900 01  WS-BEVARREF                 PIC X(10)   VALUE SPACE.                 
007000 01  WS-IDKONTO                  PIC X(10)   VALUE SPACE.                 
007100 01  WS-IDKONTO-NUM    REDEFINES WS-IDKONTO                               
007200                                 PIC 9(10).                               
007300 01  WS-SPRAK                    PIC X(3)    VALUE SPACE.                 
007400 01  WS-TIFORDAT                 PIC X(6)    VALUE SPACE.                 
007500 01  WS-TIFORDAT-NUM   REDEFINES WS-TIFORDAT                              
007600                                 PIC 9(6).                                
007700 01  WS-IDKST                    PIC X(10)   VALUE SPACE.                 
008000 01  WS-KDFRAKT                  PIC X(2)    VALUE SPACE.                 
008100 01  WS-KDFRAKT-NUM    REDEFINES WS-KDFRAKT                               
008200                                 PIC 9(2).                                
008300 01  WS-IDANALYS                 PIC X(12)   VALUE SPACE.                 
008400 01  WS-BEGMT-RAD1               PIC X(35)   VALUE SPACE.                 
008500 01  WS-BEGMT-RAD2               PIC X(35)   VALUE SPACE.                 
008600 01  WS-ADGMT-GATA               PIC X(35)   VALUE SPACE.                 
008700 01  WS-ADGMT-PADR               PIC X(35)   VALUE SPACE.                 
008800 01  WS-ADGMT-LAND               PIC X(35)   VALUE SPACE.                 
008900 01  WS-BEBETRAD-1               PIC X(35)   VALUE SPACE.                 
009000 01  WS-BEBETRAD-2               PIC X(35)   VALUE SPACE.                 
009100 01  WS-ADBETRAD-1               PIC X(35)   VALUE SPACE.                 
009200 01  WS-ADBETRAD-2               PIC X(35)   VALUE SPACE.                 
009300 01  WS-ADBETRAD-3               PIC X(35)   VALUE SPACE.                 
009400                                                                          
009500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009600     88  NYCKLAR-OK                          VALUE 'J'.                   
009700     88  NYCKLAR-FEL                         VALUE 'N'.                   
009800                                                                          
009900 77  ALLT-SW                     PIC X       VALUE 'J'.                   
010000     88  ALLT-OK                             VALUE 'J'.                   
010100                                                                          
010200 77  SW-REOMRTAL                 PIC X       VALUE 'N'.                   
010300     88  NY-OMRAKNING                        VALUE 'J'.                   
010400 77  SW-ORDERDELAR-FINNS         PIC X       VALUE 'J'.                   
010500 77  SW-DATA-FORANDRAT           PIC X       VALUE 'N'.                   
010600 77  SW-GOR-LOGISK-KONTROLL      PIC X       VALUE 'N'.                   
010700                                                                          
010800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010900     88  EGEN-MID                            VALUE '4264'.                
011000     88  GODK-MID                     VALUE '4262' '4263'                 
011100                                            '4264' '4265' '4266'          
011200                                            '4267' '4268'.                
011300*    --- VALID IDDC CODES                                                 
011400*                                                                         
011500*01  -COPY WWDCKONS                                                       
011600     EJECT                                                                
011700*                                                                         
011800*    ----DIST79-DEALER-PRICE-----                                         
011900*01  -COPY WWDIST79                                                       
012000     SKIP2                                                                
012100 01  MAX-FORF-DATUM              PIC 9(6)    VALUE ZERO.                  
012200 01  FILLER REDEFINES MAX-FORF-DATUM.                                     
012300     03  MAX-AR                  PIC 9(2).                                
012400     03  MAX-MAN                 PIC 9(2).                                
012500     03  FILLER                  PIC 9(2).                                
012600*                                                                         
012700 01  DAGENS-DATUM.                                                        
012800*                                                                         
012900     03  DAGENS-DAT-AAMMDD       PIC 9(6)    VALUE ZERO.                  
013000*                                                                         
013100 01  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
013200 01  FILLER REDEFINES DAGENS-TID.                                         
013300     03 WS-DAGENS-TID-HHMM       PIC 9(4).                                
013400     03 FILLER                   PIC 9(4).                                
013500*                                                                         
013600 01  GENERELLA-SUBPROGRAM.                                                
013700*                                                                         
013800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
013900     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
014000     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
014100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
014400*                                                                         
014500 01  GEMENSAMMA-SUBPROGRAM.                                               
014600*                                                                         
014700     03  W411OHLK                PIC X(8)    VALUE 'W411OHLK'.            
014800*            LOGISKA KONTROLLER                                           
014900     03  W411OHFK                PIC X(8)    VALUE 'W411OHFK'.            
015000*            FORMELLA KONTROLLER                                          
015100     03  W411KREG                PIC X(8)    VALUE 'W411KREG'.            
015200*            LÄSNING AV KUNDREGISTRET                                     
015300     EJECT                                                                
015400*                                                                         
015500 01  FELMEDDELANDE-AREA.                                                  
015600     03  FILLER                  PIC X(16)   VALUE 'FELMEDD AREA'.        
015700     03  FELM-ORADER-SAKNAS-F-CL-028                                      
015800                                 PIC X(3)           VALUE '028'.          
015900     03  FELM-ORDERN-ANNULL-052                                           
016000                                 PIC X(3)           VALUE '052'.          
016100     03  FELM-OTILL-UPPDAT-007                                            
016200                                 PIC X(3)           VALUE '007'.          
016300     03  FELM-KUNDUPPG-SAKNAS-063                                         
016400                                 PIC X(3)           VALUE '063'.          
016500     03  FELM-TRP-KAN-EJ-SATTAS-064                                       
016600                                 PIC X(3)           VALUE '064'.          
016700     03  FELM-FK-KAN-EJ-ANDRAS-068                                        
016800                                 PIC X(3)           VALUE '068'.          
016900     03  FELM-UPPDAT-UTFORD-101  PIC X(3)           VALUE '101'.          
017000     03  FELM-PF11-FOR-UPPD-407  PIC X(3)           VALUE '407'.          
017100     03  FELM-OBEHORIG-ANV-405   PIC X(3)           VALUE '405'.          
017200     03  FELM-UPPLYSTA-FALT-FEL-001                                       
017300                                 PIC X(3)           VALUE '001'.          
017400     03  FELM-INGET-ANDRAT-414   PIC X(3)           VALUE '414'.          
017500     03  FELM-ORDER-SAKNAS-701   PIC X(3)           VALUE '701'.          
017600     03  FELM-TOO-MANY-FUNCTIONS PIC X(3)           VALUE '097'.          
017700     EJECT                                                                
017800*                                                                         
017900 01  KONSTANT-AREA.                                                       
018000     03  FILLER                  PIC  X(16) VALUE 'KONSTANT AREA'.        
018100     03  K-KDORDKL-0             PIC S9(1)  COMP-3 VALUE +0.              
018200     03  K-KDORDKL-1             PIC S9(1)  COMP-3 VALUE +1.              
018300     03  K-KDORDKL-2             PIC S9(1)  COMP-3 VALUE +2.              
018400     03  K-KDORDKL-3             PIC S9(1)  COMP-3 VALUE +3.              
018500     03  K-KDORDKL-4             PIC S9(1)  COMP-3 VALUE +4.              
018600     03  K-KDTPOTYP-0            PIC S9(1)  COMP-3 VALUE +0.              
018700     03  K-KDODELSTA-R           PIC  X(1)         VALUE 'R'.             
018800     03  K-KDTRPKAT-A            PIC  X(1)         VALUE 'A'.             
018900     03  K-KDTRPKAT-B            PIC  X(1)         VALUE 'B'.             
019000     03  K-KDTRPKAT-C            PIC  X(1)         VALUE 'C'.             
019100     03  K-TRAN-KDSVAR-0-OK      PIC  X(1)         VALUE '0'.             
019200     03  K-KDORDBEH-ANDRING-OH-9 PIC  X(1)         VALUE '9'.             
019300     03  K-IDKUNDNR-UT-NOLL      PIC  X(6)         VALUE '     0'.        
019400     SKIP2                                                                
019500*                                                                         
019600 01  SPAR-AREA.                                                           
019700     03  FILLER                  PIC X(16)      VALUE 'SPAR AREA'.        
019800     03  SPAR-KVRADER            PIC S9(5)   COMP-3 VALUE +0.             
019900     03  SPAR-SUORDV             PIC S9(9)V9(2)                           
020000                                             COMP-3 VALUE +0.             
020100     03  SPAR-SUORDV-LOC         PIC S9(9)V9(2)                           
020200                                             COMP-3 VALUE +0.             
020300     03  SPAR-SUORDV-LOCPREL     PIC S9(9)V9(2)                           
020400                                             COMP-3 VALUE +0.             
020500*                                                                         
020600     03  SPAR-OHUV-IDORDNR7      PIC  9(7)          VALUE ZERO.           
020700     EJECT                                                                
020800*                                                                         
020900 01  HELP-AREA.                                                           
021000     03  FILLER                  PIC X(16)      VALUE 'HELP AREA'.        
021100     03  HELP-KVRADER            PIC S9(5)   COMP-3 VALUE +0.             
021200     03  HELP-SUORDV             PIC S9(9)V9(2)                           
021300                                             COMP-3 VALUE +0.             
021400     03  HELP-KVRADER-OPACK      PIC  9(5)          VALUE ZERO.           
021500     03  HELP-SUORDV-OPACK       PIC  9(9)V9(2)     VALUE ZERO.           
021600     03  HELP-IDORDER            PIC  9(7)          VALUE ZERO.           
021700*                                                                         
021800     03  HELP-IDFTG              PIC  9(2)          VALUE ZERO.           
021900     03  HELP-IDFTG-ALFA      REDEFINES HELP-IDFTG                        
022000                                 PIC  X(2).                               
022100     03  HELP-IDKONTO            PIC  9(10)         VALUE ZERO.           
022200     03  HELP-IDKONTO-ALFA    REDEFINES HELP-IDKONTO                      
022300                                 PIC  X(10).                              
022400     03  HELP-IDANALYS           PIC  X(12)         VALUE SPACE.          
022800     03  HELP-REOMRTAL           PIC  9.9(3)        VALUE ZERO.           
022900     03  HELP-REOMRTAL-ALFA   REDEFINES HELP-REOMRTAL                     
023000                                 PIC  X(5).                               
023100     03  HELP-TIFORDAT           PIC  9(6)          VALUE ZERO.           
023200     03  HELP-TIFORDAT-ALFA   REDEFINES HELP-TIFORDAT                     
023300                                 PIC  X(6).                               
023400     03  HELP-KDORDKL            PIC  9(2)          VALUE ZERO.           
023500     03  HELP-KDORDKL-ALFA    REDEFINES HELP-KDORDKL                      
023600                                 PIC  X(2).                               
023700     03  HELP-KDFRAKT            PIC  9(2)          VALUE ZERO.           
023800     03  HELP-KDFRAKT-ALFA    REDEFINES HELP-KDFRAKT                      
023900                                 PIC  X(2).                               
024000     EJECT                                                                
024100 01  MESSAGE-CODES.                                                       
024200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
024300     SKIP3                                                                
024400*                                                                         
024500*                 WMEDKONV                                                
024600*                                                                         
024700*   -COPY WMEDAREA                                                        
024800     EJECT                                                                
024900*                                                                         
025000*                 WDECEDIT                                                
025100*                                                                         
025200*   -COPY WDECAREA                                                        
025300     EJECT                                                                
025400*                                                                         
025500*                 SKÄRMHANTERING                                          
025600*                                                                         
025700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
025800     SKIP3                                                                
025900*01  MID -COPY W4I26401                                                   
026000     EJECT                                                                
026100 01  FILLER                      PIC X(16)   VALUE 'MOD/MID-AREA'.        
026200     SKIP3                                                                
026300 01  ALT1-P-TO-P-SW.                                                      
026400     03   FILLER                 PIC S9(4)  COMP SYNC VALUE 66.           
026500     03   FILLER                 PIC  X(2)  VALUE LOW-VALUE.              
026600     03   FILLER                 PIC  X(8)  VALUE 'W4T296X '.             
026700     03   FILLER                 PIC  X(4)  VALUE '4264'.                 
026800     03   ALT1-KDMFSFOR          PIC  X(1).                               
026900*    03   FILLER  -COPY W4I29601 -PRE MOD-                                
027000     EJECT                                                                
027100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
027200     SKIP3                                                                
027300*01  -COPY WMSGAREA                                                       
027400     EJECT                                                                
027500     03  MOD REDEFINES MSG-AREA.                                          
027600*      05  -COPY W4O26401                                                 
027700     EJECT                                                                
027800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
027900     SKIP3                                                                
028000*01  -COPY WMFSAREA                                                       
028100     EJECT                                                                
028200*                                                                         
028300*                 SUBPROGRAM                                              
028400*                                                                         
028500 01  FILLER                      PIC  X(16) VALUE                         
028600                                               'FORMELLA KONTR  '.        
028700*                                                                         
028800*01 -COPY W411OHFK                                                        
028900     EJECT                                                                
029000*                                                                         
029100 01  FILLER                      PIC  X(16) VALUE                         
029200                                               'LOGISKA KONTR   '.        
029300*                                                                         
029400*01 -COPY W411OHLK                                                        
029500     EJECT                                                                
029600*                                                                         
029700 01  FILLER                      PIC  X(16) VALUE                         
029800                                               'SAP KONTR   '.            
029900*                                                                         
030000*                                                                         
030100 01  FILLER                      PIC  X(16) VALUE                         
030200                                               'LÄSNING KUNDREG '.        
030300*                                                                         
030400*01 -COPY W411KREG                                                        
030500     EJECT                                                                
030600*                                                                         
030700*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
030800*                                                                         
030900 01  FILLER                       PIC X(16)   VALUE 'IMS-WS'.             
031000                                                                          
031100 01  NYCKLAR-TILL-DLI.                                                    
031200*                                                                         
031300     03  W-IDGMTREF-X.                                                    
031400         05  W-WDE8-IDDISTR       PIC S9(5)   COMP-3 VALUE +0.            
031500         05  W-WDE8-IDKUNDNR      PIC S9(7)   COMP-3 VALUE +0.            
031600         05  W-WDE8-IDKUNDRF.                                             
031700             07  W-WDE8-IDORDNR7  PIC  9(7)          VALUE ZERO.          
031800             07  FILLER           PIC  X(3)          VALUE SPACE.         
031900*                                                                         
032000     EJECT                                                                
032100*    --- STATUS-KOD FRÅN IMS                                              
032200 01  STATUS-WS                    PIC X(2).                               
032300     88  SEGMENT-FINNS                       VALUE '  '.                  
032400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
032500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
032600     88  ANNAT-SEGMENT                       VALUE 'GK'.                  
032700     SKIP2                                                                
032800 01  GODK-STATUSKODER.                                                    
032900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
033000     SKIP3                                                                
033100 01  SSA1                        PIC X(128).                              
033200     SKIP3                                                                
033300*    --- IMS FUNKTIONSKODER                                               
033400*01  -COPY W0003                                                          
033500     EJECT                                                                
033600*    ---  DLI INPUT-OUTPUT AREA                                           
033700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
033800     SKIP3                                                                
033900 01  DLI-IO-AREA.                                                         
034000     03  IO-AREA                 PIC X(2000)  VALUE SPACE.                
034100     03  WLPROC01 REDEFINES IO-AREA.                                      
034200*        05  -COPY WDE801                                                 
034300     EJECT                                                                
034400 LINKAGE SECTION.                                                         
034500*01  -COPY W0009      -PRE MSG-                                           
034600     EJECT                                                                
034700*01  -COPY W0009      -PRE ALT1-                                          
034800     EJECT                                                                
034900*01  -COPY W0008      -PRE PROC-                                          
035000     05  FILLER                  PIC X.                                   
035100     EJECT                                                                
035200 01  OHLK-WDM2-PCB               PIC X.                                   
035300 01  OHLK-XXKP-PCB               PIC X.                                   
035400 01  KREG-GMTA-PCB               PIC X.                                   
035500 01  KREG-GMTB-PCB               PIC X.                                   
035600 01  KREG-GMTC-PCB               PIC X.                                   
035700 01  KREG-BETC-PCB               PIC X.                                   
035800 01  SAP-SAPC-PCB                PIC X.                                   
035900 01  WDB6-PCB                    PIC X.                                   
036000     EJECT                                                                
036100 PROCEDURE DIVISION  USING MSG-PCB                                        
036200                           ALT1-PCB                                       
036300                           PROC-PCB                                       
036400                           OHLK-WDM2-PCB                                  
036500                           OHLK-XXKP-PCB                                  
036600                           KREG-GMTA-PCB                                  
036700                           KREG-GMTB-PCB                                  
036800                           KREG-GMTC-PCB                                  
036900                           KREG-BETC-PCB                                  
037000                           SAP-SAPC-PCB                                   
037100                           WDB6-PCB.                                      
037200 MAIN SECTION.                                                            
037300     ENTRY 'DLITCBL' USING MSG-PCB                                        
037400                           ALT1-PCB                                       
037500                           PROC-PCB                                       
037600                           OHLK-WDM2-PCB                                  
037700                           OHLK-XXKP-PCB                                  
037800                           KREG-GMTA-PCB                                  
037900                           KREG-GMTB-PCB                                  
038000                           KREG-GMTC-PCB                                  
038100                           KREG-BETC-PCB                                  
038200                           SAP-SAPC-PCB                                   
038300                           WDB6-PCB.                                      
038400     SKIP2                                                                
038600     PERFORM IMS-GET-MSG                                                  
038700     IF SEGMENT-FINNS                                                     
038800        MOVE JA TO ALLT-SW                                                
038900        PERFORM A-INIT                                                    
039000        PERFORM B-KOLLA-NYCKLAR                                           
039100        IF NYCKLAR-OK AND                                                 
039200           ALLT-OK                                                        
039300           PERFORM F-LAES-VISA-INFO                                       
039400           IF ALLT-OK  AND                                                
039500              EGEN-MID                                                    
039600              PERFORM G-KONTROLLERA-USERS-AVSIKT                          
039700              IF MFS-UPDATE AND                                           
039800                 ALLT-OK                                                  
039900*-- TAS BORT EFTER TEST                                                   
040000                  MOVE PHUV-TIFORDAT     TO TMP1-YYMMDD                   
040100                  MOVE DAGENS-DAT-AAMMDD TO TMP2-YYMMDD                   
040200                  PERFORM WY2000P1                                        
040300                  IF PHUV-TIORDDAT > +0 OR                                
040400                     PHUV-FLBORT = JA OR                                  
040500                     TMP1-YYMMDD   <= TMP2-YYMMDD                         
040600                    MOVE JA             TO ALLT-SW                        
040700                    MOVE SPACE    TO MED-IDMFSFEL                         
040800*                   PERFORM MFS-RENSA-FAELT-UT                            
040900                    MOVE FELM-OTILL-UPPDAT-007 TO                         
041000                                       MED-IDMFSFEL                       
041100                     CALL WMEDKONV USING MED-WMEDAREA                     
041200                     MOVE MED-MFSFEL TO MOD-TEMFSFEL                      
041300                     MOVE SPACE      TO MFS-KDTRTYP                       
041400                     MOVE ' '        TO MFS-IDPFK                         
041500                     MOVE NEJ        TO MOD-FLBORT                        
041600                     PERFORM F-LAES-VISA-INFO                             
041700                  ELSE                                                    
041800*-- TAS BORT EFTER TEST                                                   
041900                    PERFORM H-KOLLA-INDATA-UPPDATERA-PH                   
042000                  END-IF                                                  
042100              END-IF                                                      
042200           END-IF                                                         
042300        END-IF                                                            
042400        COMPUTE MSG-KVLL = LENGTH OF MOD-W4O26401 + 4                     
042500        PERFORM IMS-INSERT-MSG                                            
042600     END-IF                                                               
042700                                                                          
042800     MOVE   ZERO TO RETURN-CODE                                           
042900*    CALL ABEND USING RKOD-ABEND-KREG                                     
043000     GOBACK                                                               
043100     .                                                                    
043200     EJECT                                                                
043300 A-INIT SECTION.                                                          
043400                                                                          
043500     IF MSG-DUBBLA-TRANSKODER                                             
043600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I26401                 
043700       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
043800       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
043900     ELSE                                                                 
044000       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I26401                 
044100       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
044200       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
044300     END-IF                                                               
044400                                                                          
044500     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
044600     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
044700     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
044800                                                                          
044900     MOVE LOW-VALUE                       TO MSG-AREA                     
045000     MOVE 'W4O26401'                      TO MFS-IDMOD                    
045100     MOVE '4264'                          TO MOD-IDTRANS                  
045200     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
045300                                             MOD-TEMFSINF                 
045400                                                                          
045500     IF NOT EGEN-MID                                                      
045600       MOVE SPACE                         TO MFS-KDTRTYP                  
045700       MOVE '7'                           TO MFS-IDPFK                    
045800     END-IF                                                               
045900                                                                          
046000     IF ENGLISH-TEXT                                                      
046100       MOVE +2                            TO SPRAK-IX                     
046200       MOVE 'GB '                         TO MED-IDSKYLT                  
046300     ELSE                                                                 
046400       MOVE +1                            TO SPRAK-IX                     
046500       MOVE 'S  '                         TO MED-IDSKYLT                  
046600     END-IF                                                               
046700                                                                          
046800     ACCEPT DAGENS-DATUM FROM DATE                                        
046900     ACCEPT DAGENS-TID   FROM TIME                                        
047000     .                                                                    
047100     EJECT                                                                
047200 B-KOLLA-NYCKLAR SECTION.                                                 
047300                                                                          
047400     MOVE    JA               TO    NYCKLAR-SW                            
047500     MOVE    MFS-RENSA-FAELT  TO    MOD-IDDISTR-IN                        
047600                                    MOD-IDKUNDNR-IN                       
047700                                    MOD-IDORDNR7-IN                       
047800                                                                          
047900     PERFORM BA-KONTROLLERA-IDDISTR                                       
048000     PERFORM BB-KONTROLLERA-IDKUNDNR                                      
048100     PERFORM BC-KONTROLLERA-IDKUNDRF                                      
048200                                                                          
048300     IF NYCKLAR-FEL                                                       
048400       MOVE    ERR-WRONG-KEY  TO    MED-IDMFSFEL                          
048500       CALL    WMEDKONV       USING MED-WMEDAREA                          
048600       MOVE    MED-MFSFEL     TO    MOD-TEMFSFEL                          
048700       PERFORM MFS-RENSA-FAELT-UT                                         
048800                                                                          
048900       IF NOT GODK-MID                                                    
049000         MOVE MFS-RENSA-FAELT TO    MOD-IDDISTR-UT                        
049100                                    MOD-IDKUNDNR-UT                       
049200                                    MOD-IDORDNR7-UT                       
049300       END-IF                                                             
049400     ELSE                                                                 
049500       IF NOT EGEN-MID                                                    
049600         PERFORM BE-INITIERA-UPPDATERINGS-FALT                            
049700       ELSE                                                               
049800         PERFORM BF-KOLLA-UPPDATERINGS-FALT                               
049900       END-IF                                                             
050000                                                                          
050100       IF NOT ALLT-OK                                                     
050200         CALL    WMEDKONV     USING MED-WMEDAREA                          
050300         MOVE    MED-MFSFEL   TO    MOD-TEMFSFEL                          
050400         PERFORM MFS-ROR-EJ-FAELT-UT                                      
050500       END-IF                                                             
050600     END-IF                                                               
050700     .                                                                    
050800     EJECT                                                                
050900 BA-KONTROLLERA-IDDISTR SECTION.                                          
051000                                                                          
051100     IF MID-IDDISTR-IN  = ALL '+'                                         
051200       MOVE    MID-IDDISTR-UT  TO        WS-IDDISTR                       
051300       INSPECT WS-IDDISTR      REPLACING LEADING SPACE BY ZERO            
051400     ELSE                                                                 
051500       MOVE    MID-IDDISTR-IN  TO        WS-IDDISTR                       
051600       MOVE    '7'             TO        MFS-IDPFK                        
051700       MOVE    SPACE           TO        MFS-KDTRTYP                      
051800     END-IF                                                               
051900                                                                          
052000     MOVE      WS-IDDISTR      TO        MOD-IDDISTR-UT                   
052100     INSPECT   MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE            
052200                                                                          
052300     IF WS-IDDISTR NUMERIC AND WS-IDDISTR > ZERO                          
052400       MOVE    WS-IDDISTR      TO        W-WDE8-IDDISTR                   
052410       MOVE WS-IDDISTR         TO        DIST79-IDDISTR                   
052420       IF DIST79-DEALER-PRICE                                             
052430          IF ENGLISH-TEXT                                                 
052440             MOVE 'DEALPRICE'  TO        MOD-TEDDI                        
052450          ELSE                                                            
052460             MOVE '  ÅF PRIS'  TO        MOD-TEDDI                        
052470          END-IF                                                          
052480       ELSE                                                               
052490          MOVE SPACES          TO        MOD-TEDDI                        
052491       END-IF                                                             
052500     ELSE                                                                 
052600       MOVE    NEJ             TO        NYCKLAR-SW                       
052700     END-IF                                                               
052800                                                                          
053900     .                                                                    
054000     EJECT                                                                
054100 BB-KONTROLLERA-IDKUNDNR SECTION.                                         
054200                                                                          
054300     IF MID-IDKUNDNR-IN = ALL '+'                                         
054400       MOVE    MID-IDKUNDNR-UT    TO        WS-IDKUNDNR                   
054500       INSPECT WS-IDKUNDNR        REPLACING LEADING SPACE BY ZERO         
054600     ELSE                                                                 
054700       MOVE    MID-IDKUNDNR-IN    TO        WS-IDKUNDNR                   
054800       MOVE    '7'                TO        MFS-IDPFK                     
054900       MOVE    SPACE              TO        MFS-KDTRTYP                   
055000     END-IF                                                               
055100                                                                          
055200     IF WS-IDKUNDNR = ZERO                                                
055300       MOVE    K-IDKUNDNR-UT-NOLL TO        MOD-IDKUNDNR-UT               
055400     ELSE                                                                 
055500       MOVE    WS-IDKUNDNR        TO        MOD-IDKUNDNR-UT               
055600       INSPECT MOD-IDKUNDNR-UT    REPLACING LEADING ZERO BY SPACE         
055700     END-IF                                                               
055800                                                                          
055900     IF WS-IDKUNDNR NUMERIC                                               
056000       MOVE    WS-IDKUNDNR        TO      W-WDE8-IDKUNDNR                 
056100     ELSE                                                                 
056200       MOVE    NEJ                TO        NYCKLAR-SW                    
056300     END-IF                                                               
056400     .                                                                    
056500     EJECT                                                                
056600 BC-KONTROLLERA-IDKUNDRF SECTION.                                         
056700                                                                          
056800     IF MID-IDORDNR7-IN = ALL '+'                                         
056900       MOVE    MID-IDORDNR7-UT  TO        WS-IDORDNR7                     
057000       INSPECT WS-IDORDNR7      REPLACING LEADING SPACE BY ZERO           
057100     ELSE                                                                 
057200       MOVE    MID-IDORDNR7-IN  TO        WS-IDORDNR7                     
057300       MOVE    '7'              TO        MFS-IDPFK                       
057400       MOVE    SPACE            TO        MFS-KDTRTYP                     
057500     END-IF                                                               
057600                                                                          
057700     MOVE      WS-IDORDNR7      TO        MOD-IDORDNR7-UT                 
057800     INSPECT   MOD-IDORDNR7-UT  REPLACING LEADING ZERO BY SPACE           
057900                                                                          
058000     IF WS-IDORDNR7 NUMERIC AND WS-IDORDNR7 > ZERO                        
058100       MOVE    SPACE            TO        W-WDE8-IDKUNDRF                 
058200       MOVE    WS-IDORDNR7      TO        W-WDE8-IDORDNR7                 
058300     ELSE                                                                 
058400       MOVE    NEJ              TO        NYCKLAR-SW                      
058500     END-IF                                                               
058600     .                                                                    
058700     EJECT                                                                
058800 BE-INITIERA-UPPDATERINGS-FALT SECTION.                                   
058900                                                                          
059000     MOVE NEJ    TO MOD-FLBORT                                            
059100     MOVE SPACE  TO WS-KDFAKTYP                                           
059200                    WS-BEKUNDRF                                           
059300                    WS-BEVARREF                                           
059400                    WS-SPRAK                                              
059500                    WS-BEGMT-RAD1                                         
059600                    WS-BEGMT-RAD2                                         
059700                    WS-ADGMT-GATA                                         
059800                    WS-ADGMT-PADR                                         
059900                    WS-ADGMT-LAND                                         
060000                    WS-BEBETRAD-1                                         
060100                    WS-BEBETRAD-2                                         
060200                    WS-ADBETRAD-1                                         
060300                    WS-ADBETRAD-2                                         
060400                    WS-ADBETRAD-3                                         
060500                    WS-IDKST                                              
060600                                                                          
060700     MOVE ZERO     TO                                                     
060800                      WS-IDFTG                                            
060900                      WS-IDKONTO                                          
061000                      WS-TIFORDAT                                         
061200     MOVE '1.000'  TO WS-REOMRTAL                                         
061300     .                                                                    
061400     EJECT                                                                
061500 BF-KOLLA-UPPDATERINGS-FALT SECTION.                                      
061600                                                                          
061700     IF MID-KDFAKTYP      = ALL '+'                                       
061800       MOVE SPACE             TO WS-KDFAKTYP                              
061900     ELSE                                                                 
062000       MOVE MID-KDFAKTYP      TO WS-KDFAKTYP                              
062100     END-IF                                                               
062200                                                                          
062300     IF MID-BEKUNDRF      = ALL '+'                                       
062400       MOVE SPACE             TO WS-BEKUNDRF                              
062500     ELSE                                                                 
062600       MOVE MID-BEKUNDRF      TO WS-BEKUNDRF                              
062700     END-IF                                                               
062800                                                                          
062900                                                                          
063000     IF MID-BEVARREF      = ALL '+'                                       
063100       MOVE SPACE             TO WS-BEVARREF                              
063200     ELSE                                                                 
063300       MOVE MID-BEVARREF      TO WS-BEVARREF                              
063400     END-IF                                                               
063500                                                                          
063600     IF MID-IDSKYLT       = ALL '+'                                       
063700       MOVE SPACE             TO WS-SPRAK                                 
063800     ELSE                                                                 
063900       MOVE MID-IDSKYLT       TO WS-SPRAK                                 
064000     END-IF                                                               
064100                                                                          
064200     IF MID-BEGMT-RAD1  = ALL '+'                                         
064300       MOVE SPACE             TO WS-BEGMT-RAD1                            
064400     ELSE                                                                 
064500       MOVE MID-BEGMT-RAD1  TO WS-BEGMT-RAD1                              
064600     END-IF                                                               
064700                                                                          
064800     IF MID-BEGMT-RAD2  = ALL '+'                                         
064900       MOVE SPACE             TO WS-BEGMT-RAD2                            
065000     ELSE                                                                 
065100       MOVE MID-BEGMT-RAD2  TO WS-BEGMT-RAD2                              
065200     END-IF                                                               
065300                                                                          
065400     IF MID-ADGMT-GATA    = ALL '+'                                       
065500       MOVE SPACE             TO WS-ADGMT-GATA                            
065600     ELSE                                                                 
065700       MOVE MID-ADGMT-GATA    TO WS-ADGMT-GATA                            
065800     END-IF                                                               
065900                                                                          
066000     IF MID-ADGMT-PADR    = ALL '+'                                       
066100       MOVE SPACE             TO WS-ADGMT-PADR                            
066200     ELSE                                                                 
066300       MOVE MID-ADGMT-PADR    TO WS-ADGMT-PADR                            
066400     END-IF                                                               
066500                                                                          
066600     IF MID-ADGMT-LAND    = ALL '+'                                       
066700       MOVE SPACE             TO WS-ADGMT-LAND                            
066800     ELSE                                                                 
066900       MOVE MID-ADGMT-LAND    TO WS-ADGMT-LAND                            
067000     END-IF                                                               
067100                                                                          
067200     IF MID-BEBETRAD-1 = ALL '+'                                          
067300       MOVE SPACE             TO WS-BEBETRAD-1                            
067400     ELSE                                                                 
067500       MOVE MID-BEBETRAD-1 TO WS-BEBETRAD-1                               
067600     END-IF                                                               
067700                                                                          
067800     IF MID-BEBETRAD-2 = ALL '+'                                          
067900       MOVE SPACE             TO WS-BEBETRAD-2                            
068000     ELSE                                                                 
068100       MOVE MID-BEBETRAD-2 TO WS-BEBETRAD-2                               
068200     END-IF                                                               
068300                                                                          
068400                                                                          
068500     IF MID-ADBETRAD-1 = ALL '+'                                          
068600       MOVE SPACE             TO WS-ADBETRAD-1                            
068700     ELSE                                                                 
068800       MOVE MID-ADBETRAD-1 TO WS-ADBETRAD-1                               
068900     END-IF                                                               
069000                                                                          
069100     IF MID-ADBETRAD-2 = ALL '+'                                          
069200       MOVE SPACE             TO WS-ADBETRAD-2                            
069300     ELSE                                                                 
069400       MOVE MID-ADBETRAD-2 TO WS-ADBETRAD-2                               
069500     END-IF                                                               
069600                                                                          
069700     IF MID-ADBETRAD-3 = ALL '+'                                          
069800       MOVE SPACE             TO WS-ADBETRAD-3                            
069900     ELSE                                                                 
070000       MOVE MID-ADBETRAD-3 TO WS-ADBETRAD-3                               
070100     END-IF                                                               
070200                                                                          
070300     IF MID-FLBORT = ALL '+'                                              
070400        MOVE NEJ             TO WS-FLBORT                                 
070500                                MOD-FLBORT                                
070600     ELSE                                                                 
070700        MOVE MID-FLBORT      TO WS-FLBORT                                 
070800        MOVE NEJ             TO MOD-FLBORT                                
070900     END-IF                                                               
071000     PERFORM BFB-KOLLA-MID-IDFTG                                          
071100     PERFORM BFC-KOLLA-MID-IDKONTO                                        
071200     PERFORM BFD-KOLLA-MID-IDKST                                          
071300     PERFORM BFE-KOLLA-MID-IDANALYS                                       
071400     PERFORM BFF-KOLLA-MID-REOMRTAL                                       
071500     PERFORM BFG-KOLLA-MID-FORFDAT                                        
071600     .                                                                    
071700     EJECT                                                                
071800 BFB-KOLLA-MID-IDFTG SECTION.                                             
071900                                                                          
072000     IF MID-IDFTG         = ALL '+'                                       
072100       MOVE ZERO                TO WS-IDFTG                               
072200     ELSE                                                                 
072300       MOVE MID-IDFTG           TO WS-IDFTG                               
072400       INSPECT WS-IDFTG REPLACING LEADING SPACES BY ZEROES                
072500       MOVE ZERO                TO WS-TALLY                               
072600       INSPECT WS-IDFTG TALLYING WS-TALLY FOR CHARACTERS                  
072700                                                  BEFORE SPACE            
072800       IF WS-TALLY > ZERO                                                 
072900         MOVE WS-IDFTG (1:WS-TALLY) TO HELP-IDFTG                         
073000         MOVE HELP-IDFTG-ALFA   TO WS-IDFTG                               
073100       END-IF                                                             
073200       IF WS-IDFTG NOT NUMERIC                                            
073300         MOVE NEJ               TO ALLT-SW                                
073400         MOVE MFS-ROER-EJ-FAELT TO MOD-IDFTG                              
073500         MOVE MFS-NUM-FAELT-FEL TO MOD-IDFTG-ATTR                         
073600         MOVE FELM-UPPLYSTA-FALT-FEL-001 TO MED-IDMFSFEL                  
073700       ELSE                                                               
073800         MOVE WS-IDFTG          TO MOD-IDFTG                              
073900       END-IF                                                             
074000     END-IF                                                               
074100     .                                                                    
074200     EJECT                                                                
074300 BFC-KOLLA-MID-IDKONTO SECTION.                                           
074400                                                                          
074500     IF MID-IDKONTO       = ALL '+'                                       
074600       MOVE ZERO                TO WS-IDKONTO                             
074700     ELSE                                                                 
074800       MOVE MID-IDKONTO         TO WS-IDKONTO                             
074900       INSPECT WS-IDKONTO    REPLACING LEADING SPACES BY ZEROES           
075000       MOVE ZERO                TO WS-TALLY                               
075100       INSPECT WS-IDKONTO    TALLYING WS-TALLY FOR CHARACTERS             
075200                                                  BEFORE SPACE            
075300       IF WS-TALLY > ZERO                                                 
075400         MOVE WS-IDKONTO (1:WS-TALLY) TO HELP-IDKONTO                     
075500         MOVE HELP-IDKONTO-ALFA TO WS-IDKONTO                             
075600       END-IF                                                             
075700       IF WS-IDKONTO    NOT NUMERIC                                       
075800         MOVE NEJ               TO ALLT-SW                                
075900         MOVE MFS-ROER-EJ-FAELT TO MOD-IDKONTO                            
076000         MOVE MFS-NUM-FAELT-FEL TO MOD-IDKONTO-ATTR                       
076100         MOVE FELM-UPPLYSTA-FALT-FEL-001 TO MED-IDMFSFEL                  
076200       ELSE                                                               
076300         MOVE WS-IDKONTO        TO MOD-IDKONTO                            
076400       END-IF                                                             
076500     END-IF                                                               
076600     .                                                                    
076700     EJECT                                                                
076800 BFE-KOLLA-MID-IDANALYS  SECTION.                                         
076900                                                                          
077000     IF MID-IDANALYS      = ALL '+'                                       
077100       MOVE SPACE               TO WS-IDANALYS                            
077200     ELSE                                                                 
077300       MOVE MID-IDANALYS        TO WS-IDANALYS                            
077400                                   MOD-IDANALYS                           
077500     END-IF                                                               
077600     .                                                                    
077700     EJECT                                                                
077800 BFD-KOLLA-MID-IDKST SECTION.                                             
077900                                                                          
078000     IF MID-IDKST         = ALL '+'                                       
078100       MOVE SPACE               TO WS-IDKST                               
078200     ELSE                                                                 
078300       MOVE MID-IDKST           TO WS-IDKST                               
078400                                   MOD-IDKST                              
080000     END-IF                                                               
080100     .                                                                    
080200     EJECT                                                                
080300 BFF-KOLLA-MID-REOMRTAL SECTION.                                          
080400                                                                          
080500     IF MID-REOMRTAL NOT = ALL '+'                                        
080600       MOVE WS-IDDISTR            TO DIST79-IDDISTR                       
080700       IF DIST79-DEALER-PRICE                                             
080800         MOVE ALL '+'             TO MID-REOMRTAL                         
080900       END-IF                                                             
081000     END-IF                                                               
081100                                                                          
081200     IF ALLT-OK                                                           
081300       IF MID-REOMRTAL      = ALL '+'                                     
081400         MOVE ZERO                TO WS-REOMRTAL                          
081500         MOVE +0                  TO WS-REOMRTAL-SPAR                     
081600       ELSE                                                               
081700         MOVE MID-REOMRTAL        TO DEC-IDFRIDATA                        
081800         MOVE +1                  TO DEC-KVHELTAL                         
081900         MOVE +3                  TO DEC-KVDECIMAL                        
082000         CALL WDECEDIT USING DEC-WDECAREA                                 
082100         MOVE DEC-IDEDITDATA      TO WS-REOMRTAL-NUM                      
082200                                     WS-REOMRTAL-SPAR                     
082300         IF DEC-KDSVAR-FEL                                                
082400           MOVE NEJ               TO ALLT-SW                              
082500           MOVE MFS-ROER-EJ-FAELT TO MOD-REOMRTAL                         
082600           MOVE MFS-NUM-FAELT-FEL TO MOD-REOMRTAL-ATTR                    
082700           MOVE FELM-UPPLYSTA-FALT-FEL-001 TO MED-IDMFSFEL                
082800         ELSE                                                             
082900           MOVE WS-REOMRTAL       TO MOD-REOMRTAL                         
083000         END-IF                                                           
083100       END-IF                                                             
083200     END-IF                                                               
083300     .                                                                    
083400     EJECT                                                                
083500 BFG-KOLLA-MID-FORFDAT SECTION.                                           
083600                                                                          
083700     IF MID-FORFDAT       = ALL '+'                                       
083800       MOVE ZERO                TO WS-TIFORDAT                            
083900     ELSE                                                                 
084000       MOVE MID-FORFDAT         TO WS-TIFORDAT                            
084100       INSPECT WS-TIFORDAT   REPLACING LEADING SPACES BY ZEROES           
084200       MOVE ZERO                TO WS-TALLY                               
084300       INSPECT WS-TIFORDAT   TALLYING WS-TALLY FOR CHARACTERS             
084400                                                  BEFORE SPACE            
084500       IF WS-TALLY > ZERO                                                 
084600         MOVE WS-TIFORDAT   (1:WS-TALLY) TO HELP-TIFORDAT                 
084700         MOVE HELP-TIFORDAT-ALFA TO WS-TIFORDAT                           
084800       END-IF                                                             
084900       IF WS-TIFORDAT   NOT NUMERIC                                       
085000         MOVE NEJ               TO ALLT-SW                                
085100         MOVE MFS-ROER-EJ-FAELT TO MOD-FORFDAT                            
085200         MOVE MFS-NUM-FAELT-FEL TO MOD-FORFDAT-ATTR                       
085300         MOVE FELM-UPPLYSTA-FALT-FEL-001 TO MED-IDMFSFEL                  
085400       ELSE                                                               
085500         MOVE WS-TIFORDAT       TO MOD-FORFDAT                            
085600       END-IF                                                             
085700     END-IF                                                               
085800     .                                                                    
085900     EJECT                                                                
086000 F-LAES-VISA-INFO SECTION.                                                
086100                                                                          
086200     PERFORM FA-LAES-PROFORMAHUVUD                                        
086300                                                                          
086400     IF NOT ALLT-OK  AND MED-IDMFSFEL NOT = SPACE                         
086500       CALL    WMEDKONV   USING MED-WMEDAREA                              
086600       MOVE    MED-MFSFEL TO    MOD-TEMFSFEL                              
086700       PERFORM MFS-RENSA-FAELT-UT                                         
086800     END-IF                                                               
086900     .                                                                    
087000     EJECT                                                                
087100 FA-LAES-PROFORMAHUVUD SECTION.                                           
087200                                                                          
087300     PERFORM IMS-GHU-WDE801                                               
087400                                                                          
087500     IF SEGMENT-SAKNAS OR                                                 
087600        PHUV-FLBORT = 'J'                                                 
087700       MOVE NEJ                   TO ALLT-SW                              
087800       MOVE SPACE                 TO MED-IDMFSFEL                         
087900       PERFORM MFS-RENSA-FAELT-UT                                         
088000       MOVE FELM-ORDER-SAKNAS-701 TO MED-IDMFSFEL                         
088100     ELSE                                                                 
088200         IF PHUV-KDPROTYP = 'L' AND                                       
088300            PHUV-IDUSER NOT = MSG-SIGNON-USERID                           
088400            MOVE FELM-OBEHORIG-ANV-405 TO MED-IDMFSFEL                    
088500            MOVE NEJ                   TO ALLT-SW                         
088600         ELSE                                                             
088700            PERFORM FAA-TA-HAND-OM-PHUV-INFO                              
088800         END-IF                                                           
088900     END-IF                                                               
089000     .                                                                    
089100     EJECT                                                                
089200 FAA-TA-HAND-OM-PHUV-INFO SECTION.                                        
089300                                                                          
089400     IF MFS-UPDATE                                                        
089500       PERFORM  FAAA-LAGG-UT-FASTA-VARDEN                                 
089600     ELSE                                                                 
089700       MOVE PHUV-KDFAKTYP     TO MOD-KDFAKTYP                             
089800       MOVE PHUV-BEKUNDRF     TO MOD-BEKUNDRF                             
089900       MOVE PHUV-IDFTG        TO HELP-IDFTG                               
090000       MOVE HELP-IDFTG        TO MOD-IDFTG                                
090100       MOVE PHUV-REOMRTAL     TO HELP-REOMRTAL                            
090200       MOVE HELP-REOMRTAL     TO MOD-REOMRTAL                             
090300       MOVE PHUV-BEVARREF     TO MOD-BEVARREF                             
090400       MOVE PHUV-IDKONTO      TO HELP-IDKONTO                             
090500       MOVE HELP-IDKONTO      TO MOD-IDKONTO                              
090600       MOVE PHUV-IDANALYS     TO HELP-IDANALYS                            
090700       MOVE HELP-IDANALYS     TO MOD-IDANALYS                             
090800       MOVE PHUV-IDSKYLT      TO MOD-IDSKYLT                              
090900       MOVE PHUV-TIFORDAT     TO HELP-TIFORDAT                            
091000       MOVE HELP-TIFORDAT     TO MOD-FORFDAT                              
091100       MOVE PHUV-IDKST        TO MOD-IDKST                                
091300       MOVE PHUV-BEGMT-RAD1   TO MOD-BEGMT-RAD1                           
091400       MOVE PHUV-BEGMT-RAD2   TO MOD-BEGMT-RAD2                           
091500       MOVE PHUV-ADGMT-GATA   TO MOD-ADGMT-GATA                           
091600       MOVE PHUV-ADGMT-PADR   TO MOD-ADGMT-PADR                           
091700       MOVE PHUV-ADGMT-LAND   TO MOD-ADGMT-LAND                           
091800       MOVE PHUV-BEBETRAD-1   TO MOD-BEBETRAD-1                           
091900       MOVE PHUV-BEBETRAD-2   TO MOD-BEBETRAD-2                           
092000       MOVE PHUV-ADBETRAD-1   TO MOD-ADBETRAD-1                           
092100       MOVE PHUV-ADBETRAD-2   TO MOD-ADBETRAD-2                           
092200       MOVE PHUV-ADBETRAD-3   TO MOD-ADBETRAD-3                           
092300       PERFORM  FAAA-LAGG-UT-FASTA-VARDEN                                 
092400     END-IF                                                               
092500     .                                                                    
092600     EJECT                                                                
092700                                                                          
092800 FAAA-LAGG-UT-FASTA-VARDEN SECTION.                                       
092900                                                                          
093000     MOVE PHUV-KDORDKL        TO HELP-KDORDKL                             
093100     MOVE HELP-KDORDKL        TO MOD-KDORDKL                              
093200     MOVE PHUV-KDFRAKT        TO HELP-KDFRAKT                             
093300     MOVE HELP-KDFRAKT        TO MOD-KDFRAKT                              
093400     MOVE PHUV-KDPROTYP       TO MOD-KDPROTYP                             
093500     IF PHUV-TIORDDAT > ZERO                                              
093600        MOVE PHUV-TIORDDAT    TO WS-TIORDDAT                              
093700        MOVE WS-TIORDDAT      TO MOD-TIORDDAT                             
093800     ELSE                                                                 
093900        MOVE SPACE            TO MOD-TIORDDAT                             
094000     END-IF                                                               
094100     .                                                                    
094200     EJECT                                                                
094300 G-KONTROLLERA-USERS-AVSIKT SECTION.                                      
094400                                                                          
094500     MOVE NEJ TO SW-DATA-FORANDRAT                                        
094600*    IF WS-FLBORT = JA                                                    
094700*       CONTINUE                                                          
094800*    ELSE                                                                 
094900        IF WS-KDFAKTYP    NOT = PHUV-KDFAKTYP                             
095000           MOVE JA TO SW-DATA-FORANDRAT                                   
095100                      SW-GOR-LOGISK-KONTROLL                              
095200        END-IF                                                            
095300                                                                          
095400                                                                          
095500        IF WS-BEKUNDRF NOT = PHUV-BEKUNDRF                                
095600         MOVE JA TO SW-DATA-FORANDRAT                                     
095700        END-IF                                                            
095800                                                                          
095900        IF WS-IDFTG-NUM NOT = PHUV-IDFTG                                  
096000           MOVE JA TO SW-DATA-FORANDRAT                                   
096100                      SW-GOR-LOGISK-KONTROLL                              
096200        END-IF                                                            
096300                                                                          
096400        IF WS-REOMRTAL-SPAR NOT = PHUV-REOMRTAL                           
096500           MOVE JA TO SW-DATA-FORANDRAT                                   
096600                      SW-GOR-LOGISK-KONTROLL                              
096700                      SW-REOMRTAL                                         
096800        END-IF                                                            
096900                                                                          
097000        IF WS-BEVARREF NOT = PHUV-BEVARREF                                
097100         MOVE JA TO SW-DATA-FORANDRAT                                     
097200        END-IF                                                            
097300                                                                          
097400        IF WS-IDKONTO-NUM NOT = PHUV-IDKONTO                              
097500           MOVE JA TO SW-DATA-FORANDRAT                                   
097600                      SW-GOR-LOGISK-KONTROLL                              
097700        END-IF                                                            
097800                                                                          
097900        IF WS-IDANALYS NOT = PHUV-IDANALYS                                
098000           MOVE JA TO SW-DATA-FORANDRAT                                   
098100                      SW-GOR-LOGISK-KONTROLL                              
098200        END-IF                                                            
098300                                                                          
098400        IF WS-SPRAK NOT = PHUV-IDSKYLT                                    
098500         MOVE JA TO SW-DATA-FORANDRAT                                     
098600        END-IF                                                            
098700                                                                          
098800        IF WS-TIFORDAT-NUM NOT = PHUV-TIFORDAT                            
098900           MOVE JA TO SW-DATA-FORANDRAT                                   
099000        END-IF                                                            
099100                                                                          
099203        IF WS-IDKST NOT = PHUV-IDKST                                      
099300           MOVE JA TO SW-DATA-FORANDRAT                                   
099400                      SW-GOR-LOGISK-KONTROLL                              
099500        END-IF                                                            
099600                                                                          
099700        IF WS-BEGMT-RAD1 NOT = PHUV-BEGMT-RAD1                            
099800         MOVE JA TO SW-DATA-FORANDRAT                                     
099900        END-IF                                                            
100000                                                                          
100100        IF WS-BEGMT-RAD2 NOT = PHUV-BEGMT-RAD2                            
100200         MOVE JA TO SW-DATA-FORANDRAT                                     
100300        END-IF                                                            
100400                                                                          
100500        IF WS-ADGMT-GATA   NOT = PHUV-ADGMT-GATA                          
100600         MOVE JA TO SW-DATA-FORANDRAT                                     
100700        END-IF                                                            
100800                                                                          
100900        IF WS-ADGMT-PADR   NOT = PHUV-ADGMT-PADR                          
101000         MOVE JA TO SW-DATA-FORANDRAT                                     
101100        END-IF                                                            
101200                                                                          
101300        IF WS-ADGMT-LAND   NOT = PHUV-ADGMT-LAND                          
101400         MOVE JA TO SW-DATA-FORANDRAT                                     
101500        END-IF                                                            
101600                                                                          
101700        IF WS-BEBETRAD-1 NOT = PHUV-BEBETRAD-1                            
101800         MOVE JA TO SW-DATA-FORANDRAT                                     
101900        END-IF                                                            
102000                                                                          
102100        IF WS-BEBETRAD-2 NOT = PHUV-BEBETRAD-2                            
102200         MOVE JA TO SW-DATA-FORANDRAT                                     
102300        END-IF                                                            
102400                                                                          
102500        IF WS-ADBETRAD-1 NOT = PHUV-ADBETRAD-1                            
102600         MOVE JA TO SW-DATA-FORANDRAT                                     
102700        END-IF                                                            
102800                                                                          
102900        IF WS-ADBETRAD-2 NOT = PHUV-ADBETRAD-2                            
103000         MOVE JA TO SW-DATA-FORANDRAT                                     
103100        END-IF                                                            
103200                                                                          
103300        IF WS-ADBETRAD-3 NOT = PHUV-ADBETRAD-3                            
103400         MOVE JA TO SW-DATA-FORANDRAT                                     
103500        END-IF                                                            
103600*    END-IF                                                               
103700                                                                          
103800     IF MFS-UPDATE          AND                                           
103900        SW-DATA-FORANDRAT = NEJ  AND                                      
104000        WS-FLBORT = NEJ                                                   
104100       MOVE    NEJ                   TO    ALLT-SW                        
104200       MOVE    FELM-INGET-ANDRAT-414 TO    MED-IDMFSFEL                   
104300       CALL    WMEDKONV              USING MED-WMEDAREA                   
104400       MOVE    MED-MFSFEL            TO    MOD-TEMFSFEL                   
104500       PERFORM MFS-ROR-EJ-FAELT-UT                                        
104600     ELSE                                                                 
104700       IF MFS-UPDATE          AND                                         
104800          SW-DATA-FORANDRAT = JA                                          
104900          IF WS-FLBORT = NEJ                                              
105000             CONTINUE                                                     
105100          ELSE                                                            
105200             MOVE JA                 TO    ALLT-SW                        
105300             MOVE FELM-TOO-MANY-FUNCTIONS                                 
105400                                     TO    MED-IDMFSFEL                   
105500             CALL WMEDKONV           USING MED-WMEDAREA                   
105600             MOVE MED-MFSFEL         TO    MOD-TEMFSFEL                   
105700*            PERFORM MFS-RENSA-FAELT-UT                                   
105800             MOVE SPACE              TO MFS-KDTRTYP                       
105900             MOVE ' '                TO MFS-IDPFK                         
106000             PERFORM F-LAES-VISA-INFO                                     
106100          END-IF                                                          
106200       ELSE                                                               
106300         IF MFS-ENTER          AND                                        
106400            SW-DATA-FORANDRAT = JA                                        
106500           MOVE    NEJ                    TO    ALLT-SW                   
106600           MOVE    FELM-PF11-FOR-UPPD-407 TO    MED-IDMFSFEL              
106700           CALL    WMEDKONV               USING MED-WMEDAREA              
106800           MOVE    MED-MFSFEL             TO    MOD-TEMFSFEL              
106900           PERFORM MFS-ROR-EJ-FAELT-UT                                    
107000         END-IF                                                           
107100       END-IF                                                             
107200     END-IF                                                               
107300                                                                          
107400     IF MFS-UPDATE AND WS-FLBORT = JA  AND                                
107500                SW-DATA-FORANDRAT = NEJ                                   
107600        IF PHUV-FLBORT = NEJ                                              
107700           IF MID-FLANNULL NOT = JA                                       
107800              PERFORM MFS-ROR-EJ-FAELT-UT                                 
107900              MOVE WS-VARNINGS-TEXT TO MOD-TEMFSINF                       
108000              MOVE JA TO MOD-FLANNULL                                     
108100              MOVE NEJ TO ALLT-SW                                         
108200           ELSE                                                           
108300              MOVE NEJ TO MOD-FLANNULL                                    
108400              IF MOD-TEMFSFEL = SPACE                                     
108500                 MOVE FELM-ORDERN-ANNULL-052 TO    MED-IDMFSINF           
108600                 CALL WMEDKONV               USING MED-WMEDAREA           
108700                 MOVE MED-MFSINF             TO    MOD-TEMFSINF           
108800              END-IF                                                      
108900           END-IF                                                         
109000        END-IF                                                            
109100     END-IF                                                               
109200                                                                          
109300     .                                                                    
109400     EJECT                                                                
109500 H-KOLLA-INDATA-UPPDATERA-PH SECTION.                                     
109600                                                                          
109700     PERFORM HA-GOR-FORMELL-KONTROLL                                      
109800     IF ALLT-OK                                                           
109900       PERFORM HB-LAS-KUNDREG                                             
110000       IF ALLT-OK                                                         
110100         IF SW-GOR-LOGISK-KONTROLL = JA                                   
110200           PERFORM HC-GOR-LOGISK-KONTROLL                                 
110300         END-IF                                                           
110400         IF ALLT-OK                                                       
110500           PERFORM HE-UPPDATERA-PROFORMAHUVUD                             
110600           PERFORM HH-FYLL-MOD                                            
110700           IF ALLT-OK                                                     
110800              IF WS-FLBORT = JA                                           
110900                MOVE FELM-ORDERN-ANNULL-052 TO MED-IDMFSINF               
111000              ELSE                                                        
111100                MOVE FELM-UPPDAT-UTFORD-101 TO   MED-IDMFSINF             
111200                CALL WMEDKONV              USING MED-WMEDAREA             
111300                MOVE MED-MFSINF            TO    MOD-TEMFSINF             
111400              END-IF                                                      
111500           END-IF                                                         
111600         END-IF                                                           
111700       END-IF                                                             
111800     END-IF                                                               
111900                                                                          
112000     IF NOT ALLT-OK                                                       
112100       CALL    WMEDKONV      USING MED-WMEDAREA                           
112200       MOVE    MED-MFSFEL    TO    MOD-TEMFSFEL                           
112300       PERFORM MFS-ROR-EJ-FAELT-UT                                        
112400     END-IF                                                               
112500     .                                                                    
112600     EJECT                                                                
112700 HA-GOR-FORMELL-KONTROLL SECTION.                                         
112800                                                                          
112900     PERFORM HAA-RED-LANKAREA-W411OHFK                                    
113000     CALL    W411OHFK USING OHFK-W411OHFK                                 
113100     PERFORM HAB-KOLLA-OM-FK-FEL                                          
113200     .                                                                    
113300     EJECT                                                                
113400 HAA-RED-LANKAREA-W411OHFK SECTION.                                       
113500                                                                          
113600     MOVE ALL '+'        TO OHFK-W411OHFK                                 
113700                                                                          
113800     IF WS-FLBORT NOT = JA AND NEJ                                        
113900        MOVE NEJ         TO ALLT-SW                                       
114000        MOVE MFS-ALFA-FAELT-FEL                                           
114100                         TO MOD-FLBORT-ATTR                               
114200     END-IF                                                               
114300                                                                          
114400     IF WS-FLBORT = JA                                                    
114500        IF SW-DATA-FORANDRAT = JA                                         
114600           MOVE NEJ      TO ALLT-SW                                       
114700           MOVE MFS-ALFA-FAELT-FEL                                        
114800                         TO MOD-FLBORT-ATTR                               
114900        END-IF                                                            
115000     END-IF                                                               
115100                                                                          
115200     IF WS-FLBORT = NEJ                                                   
115300        MOVE 'PROF'             TO OHFK-IDSYSTEM                          
115400        MOVE PHUV-KDORDKL       TO OHFK-KDORDKL                           
115500        MOVE PHUV-KDFRAKT       TO OHFK-KDFRAKT                           
115600        MOVE PHUV-KDPROTYP      TO OHFK-KDPROTYP                          
115700        MOVE WS-KDFAKTYP        TO OHFK-KDFAKTYP                          
115800        MOVE WS-IDFTG           TO OHFK-IDFTG                             
115900        MOVE WS-IDKONTO         TO OHFK-IDKONTO                           
116000        MOVE WS-IDANALYS        TO OHFK-IDANALYS                          
116100        MOVE WS-IDKST           TO OHFK-IDKST                             
116200        MOVE WS-SPRAK           TO OHFK-IDSKYLT                           
116300        MOVE WS-TIFORDAT        TO OHFK-TIFORDAT                          
116400        MOVE DAGENS-DATUM       TO OHFK-TIREGDAT                          
116500        MOVE WS-DAGENS-TID-HHMM TO OHFK-TIHHMM                            
116600     END-IF                                                               
116700     .                                                                    
116800     EJECT                                                                
116900 HAB-KOLLA-OM-FK-FEL SECTION.                                             
117000                                                                          
117100     MOVE JA                   TO ALLT-SW                                 
117200                                                                          
117300     IF OHFK-KDFRAKT-OK    = NEJ                                          
117400       MOVE NEJ                TO ALLT-SW                                 
117500     END-IF                                                               
117600                                                                          
117700     IF OHFK-KDFAKTYP-OK   = NEJ                                          
117800       MOVE NEJ                TO ALLT-SW                                 
117900       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDFAKTYP-ATTR                       
118000     END-IF                                                               
118100                                                                          
118200     IF OHFK-IDFTG-OK      = NEJ                                          
118300       MOVE NEJ                TO ALLT-SW                                 
118400       MOVE MFS-NUM-FAELT-FEL  TO MOD-IDFTG-ATTR                          
118500     END-IF                                                               
118600                                                                          
118700     IF OHFK-IDKONTO-OK    = NEJ                                          
118800       MOVE NEJ                TO ALLT-SW                                 
118900       MOVE MFS-NUM-FAELT-FEL  TO MOD-IDKONTO-ATTR                        
119000     END-IF                                                               
119100                                                                          
119200     IF OHFK-IDANALYS-OK    = NEJ                                         
119300       MOVE NEJ                TO ALLT-SW                                 
119400       MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDANALYS-ATTR                      
119500     END-IF                                                               
119600                                                                          
119700     IF OHFK-IDKST-OK      = NEJ                                          
119800       MOVE NEJ                TO ALLT-SW                                 
119900       MOVE MFS-NUM-FAELT-FEL  TO MOD-IDKST-ATTR                          
120000     END-IF                                                               
120100                                                                          
120200     IF OHFK-IDSKYLT-OK    = NEJ                                          
120300       MOVE NEJ                TO ALLT-SW                                 
120400       MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDSKYLT-ATTR                       
120500     END-IF                                                               
120600                                                                          
120700     IF OHFK-TIFORDAT-OK   = NEJ                                          
120800       MOVE NEJ                TO ALLT-SW                                 
120900       MOVE MFS-NUM-FAELT-FEL  TO MOD-FORFDAT-ATTR                        
121000     ELSE                                                                 
121100       PERFORM HABA-KOLLA-DATUM                                           
121200     END-IF                                                               
121300                                                                          
121400     IF NOT ALLT-OK                                                       
121500       MOVE FELM-UPPLYSTA-FALT-FEL-001 TO MED-IDMFSFEL                    
121600     END-IF                                                               
121700     .                                                                    
121800     EJECT                                                                
121900 HABA-KOLLA-DATUM SECTION.                                                
122000                                                                          
122100     MOVE DAGENS-DATUM         TO MAX-FORF-DATUM                          
122200     ADD 6                     TO MAX-MAN                                 
122300     IF MAX-MAN > 12                                                      
122400        SUBTRACT 12            FROM MAX-MAN                               
122500        ADD 1                  TO MAX-AR                                  
122600     END-IF                                                               
122700                                                                          
122800     MOVE MID-FORFDAT      TO TMP1-YYMMDD                                 
122900     MOVE MAX-FORF-DATUM   TO TMP2-YYMMDD                                 
123000     PERFORM WY2000P1                                                     
123100     IF TMP1-YYMMDD > TMP2-YYMMDD                                         
123200        MOVE MFS-NUM-FAELT-FEL TO MOD-FORFDAT-ATTR                        
123300        MOVE NEJ               TO ALLT-SW                                 
123400     END-IF                                                               
123500                                                                          
123600     .                                                                    
123700     EJECT                                                                
123800 HB-LAS-KUNDREG SECTION.                                                  
123900                                                                          
124000     IF WS-FLBORT = NEJ                                                   
124100        PERFORM HBA-RED-LANKAREA-W411KREG                                 
124200        CALL W411KREG USING KREG-W411KREG                                 
124300                               KREG-GMTA-PCB                              
124400                               KREG-GMTB-PCB                              
124500                               KREG-GMTC-PCB                              
124600                               KREG-BETC-PCB                              
124700        PERFORM HBB-KOLLA-OM-KREG-FEL                                     
124800     END-IF                                                               
124900     .                                                                    
125000     EJECT                                                                
125100 HBA-RED-LANKAREA-W411KREG SECTION.                                       
125200                                                                          
125300     MOVE SPACE            TO KREG-W411KREG                               
125400                                                                          
125500     MOVE WS-IDDISTR       TO KREG-IDDISTR                                
125600     MOVE WS-IDKUNDNR      TO KREG-IDKUNDNR                               
125700     MOVE 'PROF'           TO KREG-IDSYSTEM                               
125800     MOVE SPACE            TO KREG-IDDC-TVS                               
125900     MOVE PHUV-KDFRAKT     TO KREG-KDFRAKT-IN                             
126000     MOVE PHUV-KDORDKL     TO KREG-KDORDKL                                
126100                                                                          
126200     MOVE SPACE            TO KREG-KDFAKTYP-IN                            
126300     MOVE ZERO             TO KREG-KDFRAKT                                
126400                              KREG-KDTULLVE                               
126500                              KREG-KDFDKRAV                               
126600                              KREG-KDORDING                               
126700                              KREG-KDBEKALT                               
126800                              KREG-RESLATT                                
126900                              KREG-KVLEDTIM-0                             
127000                              KREG-KVLEDTIM-1                             
127100                              KREG-KVLEDTIM-2                             
127200                              KREG-KVLEDTIM-3                             
127300                              KREG-KVLEDTIM-4                             
127400     MOVE NEJ              TO KREG-FLVORKO                                
127500                              KREG-FLVORFK                                
127600     .                                                                    
127700     EJECT                                                                
127800 HBB-KOLLA-OM-KREG-FEL SECTION.                                           
127900                                                                          
128000     IF KREG-IDDISTR-OK  = NEJ OR                                         
128100        KREG-IDKUNDNR-OK = NEJ OR                                         
128200        KREG-KDFRAKT-OK  = NEJ                                            
128300       MOVE NEJ                      TO ALLT-SW                           
128400       MOVE FELM-KUNDUPPG-SAKNAS-063 TO MED-IDMFSFEL                      
128500     END-IF                                                               
128600     .                                                                    
128700     EJECT                                                                
128800 HC-GOR-LOGISK-KONTROLL SECTION.                                          
128900                                                                          
129000     IF WS-FLBORT = NEJ                                                   
129100        PERFORM HCA-RED-LANKAREA-W411OHLK                                 
129200        CALL W411OHLK USING OHLK-W411OHLK OHLK-WDM2-PCB                   
129300                                          OHLK-XXKP-PCB                   
129400                                          KREG-GMTA-PCB                   
129500                                          SAP-SAPC-PCB                    
129600                                          WDB6-PCB                        
129700        PERFORM HCB-KOLLA-OM-LK-FEL                                       
129800     END-IF                                                               
129900     .                                                                    
130000     EJECT                                                                
130100 HCA-RED-LANKAREA-W411OHLK SECTION.                                       
130200                                                                          
130300     MOVE 'PROF'         TO OHLK-IDSYSTEM                                 
130400     MOVE WS-IDDISTR     TO OHLK-IDDISTR                                  
130500     MOVE WS-IDKUNDNR    TO OHLK-IDKUNDNR                                 
130600     MOVE ZERO           TO OHLK-IDORDNR                                  
130700     MOVE PHUV-KDORDKL   TO OHLK-KDORDKL                                  
130800     MOVE SPACE          TO OHLK-SEC-KDSVAR                               
130900     MOVE JA             TO OHLK-FLAUTORD                                 
131000     MOVE NEJ            TO OHLK-FLVORKO                                  
131100     MOVE NEJ            TO OHLK-FLORDSPE                                 
131200     MOVE KREG-FLOKFAK-G TO OHLK-FLOKFAK-G                                
131300     MOVE KREG-FLOKFAK-N TO OHLK-FLOKFAK-N                                
131400     MOVE KREG-FLOKFAK-R TO OHLK-FLOKFAK-R                                
131500     MOVE KREG-FLOKFAK-K TO OHLK-FLOKFAK-K                                
131600     MOVE WS-IDFTG       TO OHLK-IDFTG                                    
131700     MOVE WS-IDKONTO     TO OHLK-IDKONTO                                  
131800     MOVE WS-IDANALYS    TO OHLK-IDANALYS                                 
131900     MOVE WS-IDKST       TO OHLK-IDKST                                    
132000     MOVE '0000000'      TO OHLK-IDKAMPRF                                 
132100     MOVE WC-CDC-SE      TO OHLK-IDDC                                     
132200     MOVE SPACE          TO OHLK-IDDC-TVS                                 
132300     MOVE WS-KDFAKTYP    TO OHLK-KDFAKTYP                                 
132400     MOVE ZERO           TO OHLK-KDTPOTYP                                 
132500     MOVE ZERO           TO OHLK-TITPO                                    
132600     .                                                                    
132700     EJECT                                                                
132800 HCB-KOLLA-OM-LK-FEL SECTION.                                             
132900                                                                          
133000     IF OHLK-KDFAKTYP-OK   = NEJ                                          
133100       MOVE NEJ                TO ALLT-SW                                 
133200       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDFAKTYP-ATTR                       
133300     END-IF                                                               
133400                                                                          
133500     IF OHLK-IDKONTO-OK    = NEJ                                          
133600       MOVE NEJ                TO ALLT-SW                                 
133700       MOVE MFS-NUM-FAELT-FEL  TO MOD-IDKONTO-ATTR                        
133800     END-IF                                                               
133900                                                                          
134000     IF OHLK-IDANALYS-OK    = NEJ                                         
134100       MOVE NEJ                TO ALLT-SW                                 
134200       MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDANALYS-ATTR                      
134300     END-IF                                                               
134400                                                                          
134500     IF OHLK-IDFTG-OK      = NEJ                                          
134600       MOVE NEJ                TO ALLT-SW                                 
134700       MOVE MFS-NUM-FAELT-FEL  TO MOD-IDFTG-ATTR                          
134800     END-IF                                                               
134900                                                                          
135000     IF OHLK-IDKST-OK      = NEJ                                          
135100       MOVE NEJ                TO ALLT-SW                                 
135200       MOVE MFS-NUM-FAELT-FEL  TO MOD-IDKST-ATTR                          
135300     END-IF                                                               
135400                                                                          
135500*    IF OHLK-IDKONTO-OK = JA                                              
135600*      IF OHLK-IDKONTO = ZERO AND PHUV-IDKONTO > ZERO                     
135700*        MOVE NEJ              TO OHLK-IDKONTO-OK                         
135800*        MOVE MFS-NUM-FAELT-FEL TO MOD-IDKONTO-ATTR                       
135900*      END-IF                                                             
136000*    END-IF                                                               
136100*                                                                         
136200*    IF OHLK-IDANALYS-OK = JA                                             
136300*      IF OHLK-IDANALYS = SPACE AND PHUV-IDANALYS NOT = SPACE             
136400*        MOVE NEJ              TO OHLK-IDANALYS-OK                        
136500*        MOVE MFS-NUM-FAELT-FEL TO MOD-IDANALYS-ATTR                      
136600*      END-IF                                                             
136700*    END-IF                                                               
136800*                                                                         
136900*    IF OHLK-IDKONTO-OK = JA                                              
137000*      IF OHLK-IDKONTO > ZERO AND PHUV-IDKONTO = ZERO                     
137100*        MOVE NEJ              TO OHLK-IDKONTO-OK                         
137200*        MOVE MFS-NUM-FAELT-FEL TO MOD-IDKONTO-ATTR                       
137300*      END-IF                                                             
137400*    END-IF                                                               
137500*                                                                         
137600*    IF OHLK-IDANALYS-OK = JA                                             
137700*      IF OHLK-IDANALYS NOT = SPACE AND PHUV-IDANALYS = SPACE             
137800*        MOVE NEJ              TO OHLK-IDANALYS-OK                        
137900*        MOVE MFS-NUM-FAELT-FEL TO MOD-IDANALYS-ATTR                      
138000*      END-IF                                                             
138100*    END-IF                                                               
138200                                                                          
138300     IF OHLK-IDFTG-OK = JA                                                
138400       IF OHLK-IDFTG = ZERO AND PHUV-IDFTG > ZERO                         
138500         MOVE NEJ              TO OHLK-IDFTG-OK                           
138600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDFTG-ATTR                         
138700       END-IF                                                             
138800     END-IF                                                               
138900                                                                          
139000     IF OHLK-IDFTG-OK = JA                                                
139100       IF OHLK-IDFTG > ZERO AND PHUV-IDFTG = ZERO                         
139200         MOVE NEJ              TO OHLK-IDFTG-OK                           
139300         MOVE MFS-NUM-FAELT-FEL TO MOD-IDFTG-ATTR                         
139400       END-IF                                                             
139500     END-IF                                                               
139600                                                                          
139700*    IF OHLK-IDKST-OK = JA                                                
139800*      IF OHLK-IDKST = ZERO AND PHUV-IDKST > ZERO                         
139900*        MOVE NEJ              TO OHLK-IDKST-OK                           
140000*        MOVE MFS-NUM-FAELT-FEL TO MOD-IDKST-ATTR                         
140100*      END-IF                                                             
140200*    END-IF                                                               
140300*                                                                         
140400*    IF OHLK-IDKST-OK = JA                                                
140500*      IF OHLK-IDKST > ZERO AND PHUV-IDKST = ZERO                         
140600*        MOVE NEJ              TO OHLK-IDKST-OK                           
140700*        MOVE MFS-NUM-FAELT-FEL TO MOD-IDKST-ATTR                         
140800*      END-IF                                                             
140900*    END-IF                                                               
141000                                                                          
141100     IF NOT ALLT-OK                                                       
141200       MOVE FELM-UPPLYSTA-FALT-FEL-001 TO MED-IDMFSFEL                    
141300     END-IF                                                               
141400     .                                                                    
141500     EJECT                                                                
141600 HE-UPPDATERA-PROFORMAHUVUD SECTION.                                      
141700                                                                          
141800     PERFORM HEA-RED-LANKAREA-WDE801                                      
141900     PERFORM IMS-REPL-WDE801                                              
142000     IF NY-OMRAKNING                                                      
142100        PERFORM HEB-STARTA-OMRAKNING                                      
142200     END-IF                                                               
142300     .                                                                    
142400     EJECT                                                                
142500 HEA-RED-LANKAREA-WDE801 SECTION.                                         
142600                                                                          
142700     IF WS-FLBORT = JA                                                    
142800        MOVE WS-FLBORT      TO PHUV-FLBORT                                
142900     ELSE                                                                 
143000        MOVE WS-KDFAKTYP     TO PHUV-KDFAKTYP                             
143100        MOVE WS-BEKUNDRF     TO PHUV-BEKUNDRF                             
143200        MOVE WS-IDFTG-NUM    TO PHUV-IDFTG                                
143300        MOVE WS-BEVARREF     TO PHUV-BEVARREF                             
143400        MOVE WS-IDKONTO      TO PHUV-IDKONTO                              
143500        MOVE WS-IDANALYS     TO PHUV-IDANALYS                             
143600        MOVE WS-SPRAK        TO PHUV-IDSKYLT                              
143700        MOVE WS-TIFORDAT-NUM TO PHUV-TIFORDAT                             
143800        MOVE WS-IDKST        TO PHUV-IDKST                                
143900*--                                                                       
144000        MOVE WS-BEGMT-RAD1   TO PHUV-BEGMT-RAD1                           
144100        MOVE WS-BEGMT-RAD2   TO PHUV-BEGMT-RAD2                           
144200        MOVE WS-ADGMT-GATA   TO PHUV-ADGMT-GATA                           
144300        MOVE WS-ADGMT-PADR   TO PHUV-ADGMT-PADR                           
144400        MOVE WS-ADGMT-LAND   TO PHUV-ADGMT-LAND                           
144500        MOVE WS-BEBETRAD-1   TO PHUV-BEBETRAD-1                           
144600        MOVE WS-BEBETRAD-2   TO PHUV-BEBETRAD-2                           
144700        MOVE WS-ADBETRAD-1   TO PHUV-ADBETRAD-1                           
144800        MOVE WS-ADBETRAD-2   TO PHUV-ADBETRAD-2                           
144900        MOVE WS-ADBETRAD-3   TO PHUV-ADBETRAD-3                           
145000                                                                          
145100        MOVE DAGENS-DAT-AAMMDD TO PHUV-TIUPPDAT                           
145200        MOVE DAGENS-TID        TO PHUV-TIUPPTID                           
145300     END-IF                                                               
145400     .                                                                    
145500     EJECT                                                                
145600 HEB-STARTA-OMRAKNING SECTION.                                            
145700                                                                          
145800     MOVE PHUV-IDDISTR     TO MOD-MID-IDDISTR                             
145900     MOVE PHUV-IDKUNDNR    TO MOD-MID-IDKUNDNR                            
146000     MOVE PHUV-IDKUNDRF    TO MOD-MID-IDKUNDRF                            
146100     MOVE WS-REOMRTAL-SPAR TO MOD-MID-REOMRTAL                            
146200     MOVE ZERO             TO MOD-MID-SUORDV                              
146300                              MOD-MID-SUORDV-LOC                          
146400                              MOD-MID-SUORDV-LOCPREL                      
146500     MOVE LOW-VALUE        TO MOD-MID-NYCKEL-GRP                          
146600                                                                          
146700     MOVE +1               TO ALT1-KDMFSFOR                               
146800     PERFORM IMS-ISRT-MSG-ALT1                                            
146900     .                                                                    
147000     EJECT                                                                
147100 HH-FYLL-MOD SECTION.                                                     
147200                                                                          
147300     IF WS-FLBORT = JA                                                    
147400        PERFORM MFS-RENSA-FAELT-UT                                        
147500     ELSE                                                                 
147600        MOVE WS-KDFAKTYP   TO MOD-KDFAKTYP                                
147700        MOVE WS-BEKUNDRF   TO MOD-BEKUNDRF                                
147800        MOVE WS-IDFTG      TO MOD-IDFTG                                   
147900                                                                          
148000        MOVE WS-REOMRTAL-NUM TO MOD-REOMRTAL                              
148100        MOVE WS-BEVARREF   TO MOD-BEVARREF                                
148200        MOVE WS-IDKONTO    TO MOD-IDKONTO                                 
148300        MOVE WS-IDANALYS   TO MOD-IDANALYS                                
148400                                                                          
148500        MOVE WS-SPRAK      TO MOD-IDSKYLT                                 
148600        MOVE WS-TIFORDAT   TO MOD-FORFDAT                                 
148700        MOVE WS-IDKST      TO MOD-IDKST                                   
148800                                                                          
148900        MOVE WS-BEGMT-RAD1   TO MOD-BEGMT-RAD1                            
149000        MOVE WS-BEGMT-RAD2   TO MOD-BEGMT-RAD2                            
149100                                                                          
149200        MOVE WS-ADGMT-GATA   TO MOD-ADGMT-GATA                            
149300        MOVE WS-ADGMT-PADR   TO MOD-ADGMT-PADR                            
149400        MOVE WS-ADGMT-LAND   TO MOD-ADGMT-LAND                            
149500                                                                          
149600        MOVE WS-BEBETRAD-1   TO MOD-BEBETRAD-1                            
149700        MOVE WS-BEBETRAD-2   TO MOD-BEBETRAD-2                            
149800                                                                          
149900        MOVE WS-ADBETRAD-1   TO MOD-ADBETRAD-1                            
150000        MOVE WS-ADBETRAD-2   TO MOD-ADBETRAD-2                            
150100        MOVE WS-ADBETRAD-3   TO MOD-ADBETRAD-3                            
150200     END-IF                                                               
150300     .                                                                    
150400     EJECT                                                                
150500 MFS-RENSA-FAELT-UT SECTION.                                              
150600                                                                          
150700     MOVE MFS-RENSA-FAELT TO                                              
150800                             MOD-KDFAKTYP                                 
150900                             MOD-BEKUNDRF                                 
151000                             MOD-IDFTG                                    
151100                             MOD-KDORDKL                                  
151200                             MOD-KDFRAKT                                  
151300                             MOD-KDPROTYP                                 
151400                             MOD-BEVARREF                                 
151500                             MOD-IDKONTO                                  
151600                             MOD-IDSKYLT                                  
151700                             MOD-FORFDAT                                  
151800                             MOD-IDANALYS                                 
151900                             MOD-IDKST                                    
152000                             MOD-BEGMT-RAD1                               
152100                             MOD-BEGMT-RAD2                               
152200                             MOD-ADGMT-GATA                               
152300                             MOD-ADGMT-PADR                               
152400                             MOD-ADGMT-LAND                               
152500                             MOD-BEBETRAD-1                               
152600                             MOD-BEBETRAD-2                               
152700                             MOD-ADBETRAD-1                               
152800                             MOD-ADBETRAD-2                               
152900                             MOD-ADBETRAD-3                               
153000                             MOD-TIORDDAT                                 
153100                                                                          
153200     MOVE 1.000           TO MOD-REOMRTAL                                 
153300     .                                                                    
153400     EJECT                                                                
153500 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
153600                                                                          
153700     MOVE MFS-ROER-EJ-FAELT TO                                            
153800                             MOD-KDFAKTYP                                 
153900                             MOD-BEKUNDRF                                 
154000                             MOD-IDFTG                                    
154100                             MOD-KDORDKL                                  
154200                             MOD-KDFRAKT                                  
154300                             MOD-KDPROTYP                                 
154400                             MOD-REOMRTAL                                 
154500                             MOD-BEVARREF                                 
154600                             MOD-IDKONTO                                  
154700                             MOD-IDSKYLT                                  
154800                             MOD-FORFDAT                                  
154900                             MOD-IDANALYS                                 
155000                             MOD-IDKST                                    
155100                             MOD-BEGMT-RAD1                               
155200                             MOD-BEGMT-RAD2                               
155300                             MOD-ADGMT-GATA                               
155400                             MOD-ADGMT-PADR                               
155500                             MOD-ADGMT-LAND                               
155600                             MOD-BEBETRAD-1                               
155700                             MOD-BEBETRAD-2                               
155800                             MOD-ADBETRAD-1                               
155900                             MOD-ADBETRAD-2                               
156000                             MOD-ADBETRAD-3                               
156100                             MOD-FLBORT                                   
156200                             MOD-TIORDDAT                                 
156300     .                                                                    
156400     EJECT                                                                
156500 IMS-GET-MSG SECTION.                                                     
156600                                                                          
156700     MOVE    '  QC'          TO    GODK-STATUSKODER                       
156800     CALL    CBLTDLI         USING GU   MSG-PCB MSG-IO-AREA               
156900     MOVE    MSG-STATUS-CODE TO    STATUS-WS                              
157000     PERFORM IMS-STATUSKONTROLL                                           
157100     .                                                                    
157200     SKIP3                                                                
157300 IMS-INSERT-MSG SECTION.                                                  
157400                                                                          
157500     IF ENGLISH-TEXT                                                      
157600       MOVE 'N' TO MFS-KDHUVOMR                                           
157700     END-IF                                                               
157800     MOVE    LOW-VALUE       TO    MSG-KDZ1 MSG-KDZ2                      
157900     MOVE    SPACE           TO    GODK-STATUSKODER                       
158000     CALL    CBLTDLI         USING ISRT MSG-PCB MSG-IO-AREA               
158100                                        MFS-IDMOD                         
158200     MOVE    MSG-STATUS-CODE TO    STATUS-WS                              
158300     PERFORM IMS-STATUSKONTROLL                                           
158400     .                                                                    
158500     EJECT                                                                
158600 IMS-ISRT-MSG-ALT1 SECTION.                                               
158700                                                                          
158800     MOVE    SPACE           TO    GODK-STATUSKODER                       
158900     CALL    CBLTDLI         USING ISRT ALT1-PCB ALT1-P-TO-P-SW           
159000     MOVE    ALT1-STATUS-CODE TO    STATUS-WS                             
159100     PERFORM IMS-STATUSKONTROLL                                           
159200     .                                                                    
159300     EJECT                                                                
159400 IMS-GHU-WDE801 SECTION.                                                  
159500                                                                          
159600     STRING  'WLPROC01(WDE801KY =' W-IDGMTREF-X ')'                       
159700             DELIMITED BY SIZE INTO    SSA1                               
159800     MOVE    '  GE'              TO    GODK-STATUSKODER                   
159900     CALL CBLTDLI USING GHU PROC-PCB   DLI-IO-AREA SSA1                   
160000     MOVE    PROC-STATUS-CODE    TO    STATUS-WS                          
160100     PERFORM IMS-STATUSKONTROLL                                           
160200     .                                                                    
160300                                                                          
160400 IMS-REPL-WDE801 SECTION.                                                 
160500                                                                          
160600     MOVE    '  '                TO    GODK-STATUSKODER                   
160700     CALL CBLTDLI  USING REPL PROC-PCB      DLI-IO-AREA                   
160800     MOVE    PROC-STATUS-CODE    TO    STATUS-WS                          
160900     PERFORM IMS-STATUSKONTROLL                                           
161000     .                                                                    
161100     EJECT                                                                
161200 IMS-STATUSKONTROLL SECTION.                                              
161300                                                                          
161400     SET STATUS-IX TO 1                                                   
161500     SEARCH GODK-STATUS                                                   
161600       AT END                                                             
161700         CALL FELLOG                                                      
161800       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                            
161900         CONTINUE                                                         
162000     END-SEARCH                                                           
162100     .                                                                    
162200     EJECT                                                                
162300*    -COPY WY2000P1                                                       
