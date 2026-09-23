000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL017200.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   04/10/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.LDC.UPDINVESTIGATIONBAL                         
000800*    WEB-LDC: WL017200 PROGRAM IS A REPLICA OF W5010800 PROGRAM           
000900*             AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                    
001000*                                                                         
001100*    FUNCTION:                                                            
001200*        *                                                                
001300*        UPPDATERAR UTREDNINGSSALDO WDK611 SAMT INVENTERINGS-             
001400*        INFORMATION WDH102 + WDGX.                                       
001500*                                                                         
001600*        PROGRAMMET UPPDATERAR WDR9 BASEN - SAPA                          
001700*                                                                         
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: WL0172T                                             
002100*        REQUEST:     WL0172I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    WL0172O1                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800*    -COPY WY2000W1                                                       
003900      SKIP3                                                               
004000 77  IDPGM                       PIC X(08)   VALUE 'WL017200'.            
004100                                                                          
004200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004300 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004400 77  KDRC-DISPLAY                PIC Z(5).                                
004500 77  CURR-DISPLAY                PIC X(16) VALUE 'MAIN'.                  
004600 77  CURR-SECTION                PIC X(16) VALUE 'MAIN'.                  
004700 77  CURR-IMS-SECTION            PIC X(16) VALUE SPACE.                   
004800 77  IDARTNR-WS                  PIC X(9)    VALUE SPACE.                 
004900 77  W-SPAR-KVUTRS               PIC S9(7)   VALUE +0   COMP-3.           
005000                                                                          
005100 77  YES                         PIC X       VALUE 'J'.                   
005200 77  NOO                         PIC X       VALUE 'N'.                   
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
       77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
005500 77  WS-CDC-11                   PIC X(2)    VALUE '11'.                  
005600 77  WS-WDK611-UPDATE            PIC X(1)    VALUE SPACE.                 
005700 77  WS-WDK629-UPDATE            PIC X(1)    VALUE SPACE.                 
005800 77  IX                          PIC S9(9)   VALUE +0   COMP SYNC.        
005900 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
006000 77  WS-KDSORT                   PIC X(2)    VALUE SPACE.                 
006100 77  SUM-KVUTRS                  PIC S9(7)   VALUE +0   COMP-3.           
006200 77  INV-BELOPP                  PIC S9(9)   VALUE +0   COMP-3.           
006300 77  AUT-INV                     PIC X       VALUE 'N'.                   
006400 77  WS-PASSWORD-OK              PIC X(1)    VALUE 'N'.                   
006500 77  SPAR-INVKAT                 PIC S9(3)   VALUE +0   COMP-3.           
006600 77  SPAR-KVLS                   PIC S9(7)   VALUE +0   COMP-3.           
006700 77  SPAR-ADLAGOMR               PIC S9(3)   VALUE ZERO COMP-3.           
006800 77  SPAR-ADGANG                 PIC S9(3)   VALUE ZERO COMP-3.           
006900 77  SPAR-ADPLATS                PIC S9(5)   VALUE ZERO COMP-3.           
007000 77  SPAR-KVUTRS                 PIC S9(7)   VALUE +0   COMP-3.           
007100 77  SPAR-PRAVCOST               PIC S9(7)V9(2) VALUE +0 COMP-3.          
007200 77  WS-KVANTAL                  PIC S9(7)   VALUE ZERO COMP-3.           
       77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
       77  W-IDARTNR-EDIT-X            PIC Z(9).                                
       77  WS-ADDRESS-WHSTOCKA         PIC X(50)                                
             VALUE 'CARPARTS.PULS.WHSTOCKADJ'.                                  
       77  WS-ADDRESS-MQASYNC          PIC X(50)                                
             VALUE 'CARPARTS.PULS.MQASYNC'.                                     
007300                                                                          
007400 77  WS-SEC-IDUSER               PIC X(7)    VALUE 'NOLLJAG'.             
007500 77  WS-SEC-IDTRANS              PIC X(4)    VALUE 'L172'.                
007600                                                                          
007700 77  WS-IDDISTR-NUM4             PIC 9(4).                                
007800 77  WS-IDKUNDNR-NUM6            PIC 9(6).                                
007900                                                                          
008000 77  KEYS-SW                     PIC X       VALUE 'J'.                   
008100     88  KEYS-OK                             VALUE 'J'.                   
008200     88  KEYS-WRONG                          VALUE 'N'.                   
008300                                                                          
008400 01  DIVERSE.                                                             
008500     03  INLEV-KOLL              PIC X       VALUE 'N'.                   
008600         88  INLEV-OK                        VALUE 'J'.                   
008700*    03  INVJUST-KOLL            PIC X       VALUE 'N'.                   
008800*        88  INVJUST-OK                      VALUE 'J'.                   
008900     03  INLEV-DATUM             PIC S9(7)   COMP-3  VALUE +0.            
009000     03  INV-ROT-FINNS           PIC X       VALUE 'J'.                   
009100     03  INV-FINNS               PIC X       VALUE 'N'.                   
009200     03  HOEGLAG-KOLL            PIC X       VALUE 'N'.                   
009300         88  HOEGLAG-OK                      VALUE 'J'.                   
009400     03  HOEGLAG-SALDO           PIC S9(9)   COMP-3.                      
009500     03  KOMMENTAR.                                                       
009600       05  FILLER                PIC X(17).                               
009700       05  KOMM-11               PIC X(6).                                
009800       05  FILLER                PIC X.                                   
009900       05  KOMM-SKR              PIC X.                                   
           03  W-DATUM-Y.                                                       
               05  W-DATUM-LOCAL       PIC 9(6).                                
           03  AKTUELL-TID-X.                                                   
               05  AKTUELL-TTMM-LOC    PIC 9(4).                                
               05  FILLER              PIC 9(4).                                
010000                                                                          
010100 01  FILLER                      PIC X(8)    VALUE 'WORKAREA'.            
010200*01   -COPY  WORKAREA                                                     
010300                                                                          
010400 01  BUFFERT-FEL-MEDDELANDE.                                              
010500     03 BUFFERT-FEL.                                                      
010600       05  FILLER                PIC X(16)                                
010700       VALUE 'FINNS I BUFFERT'.                                           
010800       05  ADBUFFOMR-1           PIC Z(2).                                
010900       05  FILLER                PIC X       VALUE  SPACE.                
011000       05  ADBUFFOMR-2           PIC Z(2).                                
011100       05  FILLER                PIC X       VALUE  SPACE.                
011200       05  ADBUFFOMR-3           PIC Z(2).                                
011300       05  FILLER                PIC X(16)   VALUE  SPACE.                
011400* - - - - - - - - - - - - -  BYTES-OBJEKTTEST                             
011500*                                                                         
011600 01  TEST-IDARTNR                PIC 9(9)   COMP-3.                       
011700*01  FILLER -COPY WWBYT09      -RED TEST-IDARTNR                          
011800                                                                          
011900*                                                                         
012000 01  KOLLA-WORKDAY               PIC X       VALUE 'N'.                   
012100     88 KOLLA-WORKDAY-OK                     VALUE 'J'.                   
012200                                                                          
012300 01  WS-INLEV-DATUM-AREA.                                                 
012400     03 WS-INLEV-DATUM           PIC 9(8)   VALUE ZERO.                   
012500     03 FILLER REDEFINES WS-INLEV-DATUM.                                  
012600       05 WS-INLEV-DATUM-SEKEL   PIC 9(2).                                
012700       05 WS-INLEV-DATUM-AA      PIC 9(2).                                
012800       05 WS-INLEV-DATUM-MMDD    PIC 9(4).                                
012900     03  FILLER REDEFINES WS-INLEV-DATUM.                                 
013000       05 WS-INLEV-DATUM-AAAA    PIC 9(4).                                
013100       05 WS-INLEV-DATUM-MMDD    PIC 9(4).                                
013200                                                                          
013300 01  WS-INV-DAREGDAT-AREA.                                                
013400     03  WS-INV-DAREGDAT     PIC 9(9) VALUE ZERO.                         
013500     03  FILLER REDEFINES WS-INV-DAREGDAT.                                
013600       05  WS-INV-NOLL         PIC 9(1).                                  
013700       05  WS-INV-AAAAMMDD     PIC 9(8).                                  
013800                                                                          
013900 01  WS-DAGENS-DATUM-1AA         PIC 9(8)    VALUE ZERO.                  
014000 01  WS-DAGENS-DATUM-AREA.                                                
014100     03 WS-DAGENS-DATUM           PIC 9(8)   VALUE ZERO.                  
014200     03 FILLER REDEFINES WS-DAGENS-DATUM.                                 
014300       05 WS-DAGENS-DATUM-SEKEL  PIC 9(2).                                
014400       05 WS-DAGENS-DATUM-AAMMDD PIC 9(6).                                
014500     03  FILLER REDEFINES WS-DAGENS-DATUM.                                
014600       05 WS-DAGENS-DATUM-AAAA   PIC 9(4).                                
014700       05 WS-DAGENS-DATUM-MMDD   PIC 9(4).                                
014800                                                                          
014900 01  DAGENS-TIAAMMDD             PIC S9(6).                               
015000 01  DAGENS-DATUM                PIC S9(8).                               
       01  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
015100 01  WS-TIAAAAMMDD               PIC S9(8).                               
015200 01  TRANS-TID                   PIC 9(9).                                
015300                                                                          
015400 01  WS-TISEGKEYAREA.                                                     
015500     03  WS-TIAAAAMMDDL      PIC 9(9) VALUE ZERO.                         
015600     03  FILLER REDEFINES WS-TIAAAAMMDDL.                                 
015700         05  WS-SEKEL        PIC 9(2).                                    
015800         05  WS-TIAAMMDD     PIC 9(6).                                    
015900         05  WS-LOPNR        PIC 9(1).                                    
016000     03  WS-TISEGKEY         PIC S9(9)  VALUE ZERO COMP-3.                
016100     03  WS-TISEGKEY2        PIC S9(9)  VALUE ZERO COMP-3.                
016200                                                                          
016300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
016400 01  GENERAL-SUBPROGRAMS.                                                 
016500     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
016600     03  WSECURIT            PIC X(8)    VALUE 'WSECURIT'.                
016700     03  FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
016800     03  ABEND               PIC X(8)    VALUE 'ABEND   '.                
016900     03  WZ01SUB             PIC X(8)    VALUE 'WZ01SUB '.                
           03  WZ01SEND            PIC X(8)    VALUE 'WZ01SEND'.                
017000     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
017100     03  WORKDAY             PIC X(8)    VALUE 'WORKDAY '.                
017200     03  W009CIA             PIC X(8)    VALUE 'W009CIA '.                
           03  WL01TIDZ            PIC X(8)    VALUE 'WL01TIDZ'.                
017300                                                                          
017400*    --- PARAMETERS TO WSECURIT                                           
017500*01   -COPY  WSECAREA                                                     
017600                                                                          
017700 01  FILLER                      PIC X(8)    VALUE 'WDATKONV'.            
017800*01   -COPY  WDATAREA                                                     
       01  FILLER              PIC X(16) VALUE 'WL01TIDZ-AREA'.                 
      *01  -COPY WL01TIDZ                                                       
017900                                                                          
018000*    --- PARAMETRAR TILL SUBPROGRAM W009CIA                               
018100*01 -COPY W009CIA                                                         
       01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
      *01  -COPY WZ01SEND                                                       
           EJECT                                                                
       01  FILLER                      PIC X(16)   VALUE 'MSG PROP'.            
      *01  -COPY WZ04PROP                                                       
           EJECT                                                                
      *    NOTAFISCAL                                                           
       01  NOTF-AREA.                                                           
      *    03  -COPY W611NOTF                                                   
           EJECT                                                                
                                                                                
018200                                                                          
018300*    --- PARAMETERS TO ABEND                                              
018400                                                                          
018500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
018600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
018700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
018800     SKIP3                                                                
018900 01  MESSAGE-CODES.                                                       
019000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
019100     EJECT                                                                
019200*                                                                         
019300 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
019400     SKIP3                                                                
019500*01  -COPY WZ01SUB                                                        
019600     EJECT                                                                
019700 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
019800     SKIP3                                                                
019900 01  REQU-AREA.                                                           
020000*    03  -COPY WZ01REQU                                                   
020100*    03  -COPY WL0172I1                                                   
020200     EJECT                                                                
020300 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
020400     SKIP3                                                                
020500 01  RESP-AREA.                                                           
020600*    03  -COPY WZ01RESP                                                   
020700*    03  -COPY WL0172O1                                                   
020800                                                                          
020900 01  FILLER                      PIC X(16)   VALUE 'DC-CODES '.           
021000                                                                          
021100*01  -COPY WWDC99                                                         
021200                                                                          
021300 01  FILLER                      PIC X(10)   VALUE 'DLINYCKLAR'.          
021400 01  NYCKLAR-TILL-DLI.                                                    
021500*                                                                         
021600     03  W-IDARTNR-X.                                                     
021700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.          
021800*                                                                         
021900     03  W-KDSEGKEY-X.                                                    
022000         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
022100*                                                                         
022200     03  W-WDE4CSEQ-X.                                                    
022300         05  W-IDARTNR2          PIC S9(9)   VALUE ZERO  COMP-3.          
022400*                                                                         
022500     03  W-DAINLEVNYCK-X.                                                 
022600         05  W-DAINLEV           PIC  9(16).                              
022700*                                                                         
022800     03  W-IDLEVNYCK-X.                                                   
022900         05  W-IDPTYP            PIC X(3)    VALUE 'R34'.                 
023000*                                                                         
023100     03  W-INVNYCK-X.                                                     
023200         05  W-IDHTYP            PIC X(4).                                
023300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
023400         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
023500*                                                                         
023600     03  W-IDDC-X.                                                        
023700         05  W-IDDC-WDK7         PIC X(2)    VALUE SPACE.                 
023800*                                                                         
023900     03  W-WDD811KY-X.                                                    
024000         05  W-IDDC-WDD8         PIC X(2)    VALUE SPACE.                 
024100         05  W-ADBUFFOMR         PIC S9(3)   VALUE +1    COMP-3.          
024200         05  W-DABUFPAF          PIC  9(8)   VALUE ZERO.                  
024300         05  W-ADBUFFGANG        PIC S9(3)   VALUE +0    COMP-3.          
024400         05  W-ADBUFFPL          PIC S9(5)   VALUE +0    COMP-3.          
024500*                                                                         
024600     03  W-ORDSTA-X.                                                      
024700         05  W-ORDSTA            PIC S9      COMP-3  VALUE +4.            
024800*                                                                         
024900     03  W-WDGXKEY-ROT-X.                                                 
025000         05  FILLER              PIC X(4)    VALUE '5115'.                
025100         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
025200*                                                                         
025300     03  W-WDH1KEY-X-MIN.                                                 
025400         05  W-IDDC-WDH1-MIN     PIC X(2)    VALUE SPACE.                 
025500         05  W-KDINVKAT-MIN      PIC S9(3)   VALUE +11   COMP-3.          
025600         05  W-TISEGKEY-MIN      PIC S9(9)   VALUE ZERO  COMP-3.          
025700         05  W-DAREGDAT-SORT-MIN     PIC 9(8)  VALUE ZERO.                
025800*                                                                         
025900     03  W-WDH1KEY-X-MAX.                                                 
026000         05  W-IDDC-WDH1-MAX     PIC X(2)    VALUE SPACE.                 
026100         05  W-KDINVKAT-MAX      PIC S9(3)   VALUE +11   COMP-3.          
026200         05  W-TISEGKEY-MAX      PIC S9(9)   VALUE ZERO  COMP-3.          
026300         05  W-DAREGDAT-SORT-MAX     PIC 9(8)  VALUE ZERO.                
026400*                                                                         
026500     03  W-IDPRODNR-X.                                                    
026600         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO  COMP-3.          
026700*                                                                         
026800     03  W-IDDC-B6-X.                                                     
026900         05 W-IDDC-B6                  PIC X(2).                          
027000                                                                          
027100     03  W-WDGXKEY-2231-X.                                                
027200         05  W-IDHTYP-2231       PIC X(4)    VALUE '2231'.                
027300         05  FILLER              PIC X(26)   VALUE SPACE.                 
027400     03  W-WDGXKEY-2232-X.                                                
027500         05  W-IDANSK-X.                                                  
027600             07  W-IDANSK-2232   PIC S9(3)   COMP-3.                      
027700         05  FILLER              PIC X(3)    VALUE LOW-VALUE.             
027800                                                                          
027900*                                                                         
028000*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
028100*****                                                                     
028200 01  IMS-WS.                                                              
028300   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
028400     SKIP3                                                                
028500*****                    **** STATUS-KOD FRÅN IMS                         
028600   03    STATUS-WS       PIC XX.                                          
028700         88  SEGMENT-FOUND       VALUE '  '.                              
028800         88  SEGMENT-MISSING     VALUE 'GE'.                              
028900         88  SEGMENT-EXISTS      VALUE 'II'.                              
029000         88  INDEX-EXISTS        VALUE 'NI'.                              
029100                                                                          
029200   03    GOOD-STATUSCODES.                                                
029300     05  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029400                                                                          
029500 01      SSA1            PIC X(128) VALUE SPACE.                          
029600 01      SSA2            PIC X(128) VALUE SPACE.                          
029700 01      SSA3            PIC X(128) VALUE SPACE.                          
029800                                                                          
029900*                            IMS FUNKTIONSKODER                           
030000*01      -COPY W0003                                                      
030100                                                                          
030200*-------- WDK6-ARTIKELREG                                                 
030300 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK601'.           
030400 01  DLI-IO-WDK601.                                                       
030500*  03  -COPY WDK601.                                                      
030600                                                                          
030700 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK611'.           
030800 01  DLI-IO-WDK611.                                                       
030900*  03  -COPY WDK611.                                                      
031000                                                                          
031100 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK629'.           
031200 01  DLI-IO-WDK629.                                                       
031300*    03  -COPY WDK629                                                     
031400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL201'.                      
031500 01  DLI-IO-WDL201.                                                       
031600*    03  -COPY WDL201 -PRE INL-                                           
031700                                                                          
031800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL211'.                      
031900 01  DLI-IO-WDL211.                                                       
032000*    03  -COPY WDL211 -PRE INL-                                           
032100                                                                          
032200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL221'.                      
032300 01  DLI-IO-WDL221.                                                       
032400*    03  -COPY WDL221 -PRE INL-                                           
032500                                                                          
032600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL231'.                      
032700 01  DLI-IO-WDL231.                                                       
032800*    03  -COPY WDL231 -PRE INL-                                           
032900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL222'.                      
033000 01  DLI-IO-WDL222.                                                       
033100*    03  -COPY WDL222 -PRE INL-                                           
033200                                                                          
033300 01  FILLER                      PIC X(16)   VALUE 'WDH101'.              
033400*01  WDH101   -COPY WDH101 -PRE INV-                                      
033500                                                                          
033600 01  FILLER                      PIC X(16)   VALUE 'WDH111  '.            
033700*01  WDH111   -COPY WDH111 -PRE INV-                                      
033800                                                                          
033900 01  FILLER                      PIC X(16)   VALUE 'WDH121  '.            
034000*01  WDH121   -COPY WDH121 -PRE INV-                                      
034100                                                                          
034200 01  FILLER                      PIC X(16)   VALUE 'WLINVC01'.            
034300*01  WLINVC01 -COPY WDH701                                                
034400                                                                          
034500 01  FILLER                      PIC X(16)   VALUE 'WLINVC11'.            
034600*01  WLINVC11 -COPY WDH711                                                
034700                                                                          
034800 01  FILLER                      PIC X(16)   VALUE 'WLINLC01'.            
034900*01  WLINLC01 -COPY WDL601 -PRE INLC-                                     
035000                                                                          
035100 01  FILLER                      PIC X(16)   VALUE 'WLINLC11'.            
035200*01  WLINLC11 -COPY WDL611 -PRE INLC-                                     
035300                                                                          
035400 01  FILLER                      PIC X(16) VALUE 'WLLOGA01'.              
035500*01  WLLOGA01    -COPY WDL901                                             
035600                                                                          
035700 01  FILLER                      PIC X(16)   VALUE 'WLXXEF11'.            
035800*01  WLXXEF11 -COPY WDGX5116                                              
035900                                                                          
036000 01  FILLER                   PIC X(16) VALUE 'DLI-IO-E411-01'.           
036100 01  DLI-IO-E411-01.                                                      
036200*    03  -COPY WDE411 -PRE RAD-                                           
036300*    03  -COPY WDE401 -PRE HUV-                                           
036400                                                                          
036500 01  FILLER                      PIC X(16)   VALUE 'WLARTD01'.            
036600*01  WLARTD01 -COPY WDD801 -PRE ARTD-                                     
036700                                                                          
036800 01  FILLER                      PIC X(16)   VALUE 'WLARTD11'.            
036900*01  WLARTD11 -COPY WDD811 -PRE ARTD-                                     
037000                                                                          
037100 01  FILLER                      PIC X(16)   VALUE 'WDK711  '.            
037200*01  WDK711   -COPY WDK711                                                
037300                                                                          
037400 01  FILLER                      PIC X(16)   VALUE 'WDK722  '.            
037500*01  WDK722   -COPY WDK722                                                
037600                                                                          
037700 01  FILLER                      PIC X(16) VALUE 'WLSAPA01'.              
037800*01  WLSAPA01    -COPY WDR901                                             
037900*    05 -COPY W510EKHA -RED FIL-WDR901-DATA                               
038000                                                                          
038100     EJECT                                                                
038200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
038300 01   DLI-IO-AREA-B601.                                                   
038400*     03  -COPY WDB601                                                    
038500                                                                          
038600 01  FILLER                      PIC X(16) VALUE 'WDR801'.                
038700*01  WDR801      -COPY WDR801 -PRE WDR8-                                  
038800*    05 -COPY W510EKHA -RED WDR8-FIL-WDR801-DATA -PRE WDR8-               
038900                                                                          
039000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E601'.         
039100 01  DLI-IO-E601.                                                         
039200*  03 -COPY WDE601                                                        
039300*                                                                         
039400 01  FILLER                   PIC X(16) VALUE 'ALT2191-IO-AREA'.          
039500 01  ALT2191-IO-AREA.                                                     
039600  03     ALT2-LL                 PIC S9(4) COMP SYNC.                     
039700  03     ALT2-Z1                 PIC X(1)  VALUE LOW-VALUE.               
039800  03     ALT2-Z2                 PIC X(1)  VALUE LOW-VALUE.               
039900  03     ALT2-TRANSKOD           PIC X(8)  VALUE 'W2T191X '.              
040000  03     ALT2-IDTRANS            PIC X(4)  VALUE '5108'.                  
040100  03     ALT2-SPRAK              PIC X(1)  VALUE '2'.                     
040200     SKIP2                                                                
040300* 03     MID -COPY W2I19101   -PRE ALT2-                                  
040400     EJECT                                                                
040500 LINKAGE SECTION.                                                         
040600*01  -COPY W0009                -PRE MSG-                                 
040700                                                                          
040800*01  -COPY W0009                -PRE ALT2191-                             
       01  MQASYNC-PCB     PIC X(1).                                            
040900                                                                          
041000*01  -COPY W0008                -PRE INVA-                                
041100     05  FILLER      PIC X(1).                                            
041200                                                                          
041300*01  -COPY W0008                -PRE WDK6-                                
041400     05  FILLER      PIC X(1).                                            
041500                                                                          
041600*01  -COPY W0008                -PRE WDE4-                                
041700     05  FILLER      PIC X(1).                                            
041800                                                                          
041900*01  -COPY W0008                -PRE ARTD-                                
042000     05  FILLER      PIC X(1).                                            
042100                                                                          
042200*01  -COPY W0008                -PRE XXEF-                                
042300     05  FILLER      PIC X(1).                                            
042400                                                                          
042500*01  -COPY W0008                -PRE WDK7-                                
042600     05  FILLER      PIC X(1).                                            
042700                                                                          
042800*01  -COPY W0008                -PRE INLC-                                
042900     05  FILLER      PIC X(1).                                            
043000                                                                          
043100*01  -COPY W0008                -PRE INVC-                                
043200     05  FILLER      PIC X(1).                                            
043300                                                                          
043400*01  -COPY W0008                -PRE LOGA-                                
043500     05  FILLER      PIC X(1).                                            
043600                                                                          
043700*01  -COPY W0008                -PRE SAPA-                                
043800     05  FILLER      PIC X(1).                                            
043900                                                                          
044000*01  -COPY W0008                -PRE WDB6-                                
044100     05  FILLER      PIC X(1).                                            
044200                                                                          
044300*01  -COPY W0008                -PRE WDR8-                                
044400     05  FILLER      PIC X(1).                                            
044500                                                                          
044600*01  -COPY W0008                -PRE WDE6-                                
044700     05  FILLER      PIC X(1).                                            
044800*01  -COPY W0008                -PRE WDL2-                                
044900     05  FILLER      PIC X(1).                                            
045000                                                                          
045100     EJECT                                                                
045200 PROCEDURE DIVISION USING MSG-PCB  ALT2191-PCB MQASYNC-PCB                
045300                          INVA-PCB WDK6-PCB WDE4-PCB                      
045400                          ARTD-PCB XXEF-PCB                               
045500                          WDK7-PCB INLC-PCB INVC-PCB                      
045600                          LOGA-PCB SAPA-PCB WDB6-PCB WDR8-PCB             
045700                          WDE6-PCB WDL2-PCB.                              
045800 MAIN SECTION.                                                            
045900     ENTRY 'DLITCBL' USING MSG-PCB ALT2191-PCB MQASYNC-PCB                
046000                          INVA-PCB WDK6-PCB WDE4-PCB                      
046100                          ARTD-PCB XXEF-PCB                               
046200                          WDK7-PCB INLC-PCB INVC-PCB                      
046300                          LOGA-PCB SAPA-PCB WDB6-PCB WDR8-PCB             
046400                          WDE6-PCB WDL2-PCB.                              
046500                                                                          
046600     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
046700     IF SUB-KDRC = 0                                                      
046800       PERFORM A-INIT                                                     
046900       PERFORM B-CHECK-KEYS                                               
047000       IF KEYS-OK                                                         
047100          PERFORM C-CHECK-PASSWORD                                        
047200                                                                          
047300          IF WS-PASSWORD-OK = JA                                          
047400             PERFORM D-UPDATE-DB                                          
047500          END-IF                                                          
047600       END-IF                                                             
047700       PERFORM S02-RETURN-RESPONSE                                        
047800     END-IF                                                               
047900                                                                          
048000                                                                          
048100     MOVE ZERO TO RETURN-CODE                                             
048200     GOBACK                                                               
048300     .                                                                    
048400                                                                          
048500 A-INIT SECTION.                                                          
048600     MOVE 'A-INIT'  TO CURR-SECTION                                       
048700                                                                          
048800     MOVE ALL '+'   TO RESP-AREA                                          
048900     MOVE SPACE     TO RESP-IDMSG-ERROR                                   
049000                       RESP-IDMSG-INFO                                    
049100                       RESP-IDELMT-ERROR                                  
049200     MOVE 001       TO RESP-IDMSGVER                                      
049300                                                                          
049400     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-TIAAAAMMDD                    
049500                                         WS-INV-AAAAMMDD                  
049600     ACCEPT DAGENS-TIAAMMDD FROM DATE                                     
049600     ACCEPT DAGENS-TID      FROM TIME                                     
           MOVE DAGENS-TID   TO AKTUELL-TID-X                                   
049700     .                                                                    
049800                                                                          
049900 B-CHECK-KEYS SECTION.                                                    
050000     MOVE 'B-CHECK-KEYS'   TO CURR-SECTION                                
050100                                                                          
050200     MOVE JA               TO KEYS-SW                                     
050300     MOVE REQU-IDDC-KEY TO W-IDDC-B6                                      
050400                           WS-IDDC                                        
050500     PERFORM IMS-GU-WDB601                                                
050600     IF SEGMENT-MISSING                                                   
050700        MOVE '026'              TO RESP-IDMSG-ERROR                       
050800*       INVALID      ***                                                  
050900        MOVE 'IDDC'             TO RESP-IDELMT-ERROR                      
051000        MOVE NOO                TO KEYS-SW                                
051100     END-IF                                                               
051200                                                                          
051300     IF REQU-IDARTNR-KEY NOT NUMERIC                                      
051400        MOVE '025'              TO RESP-IDMSG-ERROR                       
051500*       NOT NUMERIC  ***                                                  
051600        MOVE 'IDARTNR'          TO RESP-IDELMT-ERROR                      
051700        MOVE NOO                TO KEYS-SW                                
051800     END-IF                                                               
051900     IF REQU-IDPW-KEY = SPACE OR ALL '+'                                  
052000        MOVE '026'              TO RESP-IDMSG-ERROR                       
052100*       INVALID      ***                                                  
052200        MOVE 'IDPW'             TO RESP-IDELMT-ERROR                      
052300        MOVE NOO                TO KEYS-SW                                
052400     END-IF                                                               
052500                                                                          
052600     IF KEYS-OK                                                           
052700        MOVE REQU-IDDC-KEY    TO RESP-IDDC-KEY                            
052800        MOVE REQU-IDARTNR-KEY TO RESP-IDARTNR-KEY                         
052900        MOVE REQU-IDPW-KEY    TO RESP-IDPW-KEY                            
053000     END-IF                                                               
053100     .                                                                    
053200                                                                          
053300 C-CHECK-PASSWORD SECTION.                                                
053400     MOVE 'C-CHECK-PASSWORD' TO CURR-SECTION                              
053500                                                                          
053600     MOVE WS-SEC-IDUSER      TO SEC-IDUSER                                
053700     MOVE WS-SEC-IDTRANS     TO SEC-IDTRANS                               
053800     MOVE REQU-IDPW-KEY      TO SEC-IDKEY                                 
053900                                                                          
054000     CALL WSECURIT USING SEC-IDUSER                                       
054100                         SEC-IDTRANS                                      
054200                         SEC-IDKEY                                        
054300                         SEC-KDSVAR                                       
054400                                                                          
054500     IF SEC-KDSVAR = SPACE                                                
054600        MOVE JA  TO WS-PASSWORD-OK                                        
054700     ELSE                                                                 
054800        MOVE '023'              TO RESP-IDMSG-ERROR                       
054900*      INVALID-KEY  ***                                                   
055000        MOVE 'IDPW'             TO RESP-IDELMT-ERROR                      
055100        MOVE NOO                TO KEYS-SW                                
055200     END-IF                                                               
055300     .                                                                    
055400                                                                          
055500 D-UPDATE-DB        SECTION.                                              
055600     MOVE 'D-UPDATE-DB     ' TO CURR-SECTION                              
055700*                                                                         
055800*  BEHANDLING AV INRAPPORTERADE ARTIKLAR                                  
055900*  TVÅ FALL KAN FÖREKOMMA:                                                
056000*      1  EN JUSTERING GÖRS DIREKT                                        
056100*      2  EN INVENTERING KATEGORI  2 SKAPAS                               
056200                                                                          
056300     MOVE REQU-IDARTNR-KEY TO W-IDARTNR                                   
056400                              W-IDARTNR2                                  
056500                              TEST-IDARTNR                                
056600     MOVE W-IDARTNR        TO IDARTNR-WS                                  
056700     MOVE REQU-IDDC-KEY    TO W-IDDC                                      
056800                              W-IDDC-WDK7                                 
056900                              W-IDDC-WDD8                                 
057000     IF NOT BYT09-OBJEKT                                                  
057100        PERFORM IMS-01-GET-ARTC-ROT                                       
057200        IF SEGMENT-FOUND                                                  
057300           MOVE ART-KDSORT TO WS-KDSORT                                   
057400           PERFORM DA-GET-CDC-INFO                                        
057500           IF W-IDDC NOT = WS-CDC-11                                      
057600           PERFORM DB-GET-SDC-INFO                                        
057700           END-IF                                                         
057800           IF SEGMENT-FOUND                                               
057900              IF SPAR-KVUTRS = ZERO                                       
058000                 PERFORM DC-KOLLA-HOEGLAGER                               
058100                 PERFORM IMS-06-GET-ORDERRADER-SEG                        
058200                 PERFORM UNTIL SEGMENT-MISSING                            
058300                    IF HUV-KORD-IDDC = REQU-IDDC-KEY                      
058400                       IF RAD-ORAD-IDLEVNR = SPACE OR                     
058500                          RAD-ORAD-FLDIRLEV = NOO                         
058600                          ADD RAD-ORAD-KVAVBART   TO   SUM-KVUTRS         
058700                          SUBTRACT RAD-ORAD-KVLEVART                      
058800                                                  FROM SUM-KVUTRS         
058900                       END-IF                                             
059000                    END-IF                                                
059100                    PERFORM IMS-06-GET-ORDERRADER-SEG                     
059200                 END-PERFORM                                              
059300                 IF SUM-KVUTRS < +1                                       
059400                    IF SUM-KVUTRS = 0                                     
059500*       MEDDELANDE-4/5 INGEN UPPDATAERING UTREDNINGSSALDO = 0             
059600                       MOVE '265'     TO RESP-IDMSG-INFO                  
059700                    ELSE                                                  
059800*       MEDDELANDE-4/5 INGEN UPPDATAERING UTREDNINGSSALDO NEGATIVT        
059900                       MOVE '265'     TO RESP-IDMSG-INFO                  
060000                    END-IF                                                
060100                 ELSE                                                     
060200                    PERFORM DD-KOLLA-INVENTERING                          
060300                    IF INV-FINNS = JA                                     
060400                       PERFORM DE-INVENTERING-FINNS                       
060500                    ELSE                                                  
060600                       COMPUTE INV-BELOPP =                               
060700                           SUM-KVUTRS * CLAG-PRARTSTD                     
060800                                                                          
060900                       MOVE NOO       TO AUT-INV                          
061000                       IF INV-BELOPP > DCS-KVINVAUT                       
061100                         MOVE NOO    TO AUT-INV                           
061200                       ELSE                                               
061300                         MOVE JA     TO AUT-INV                           
061400                       END-IF                                             
061500                                                                          
061600                       IF AUT-INV = JA                                    
061700                         IF W-IDDC = WS-CDC-11                            
061800                          PERFORM DF-KOLLA-INLEV-CDC                      
061900                         ELSE                                             
062000                          PERFORM DF-KOLLA-INLEV-SDC                      
062100                         END-IF                                           
062200                         IF INLEV-OK                                      
062300                            PERFORM DG-SKAPA-JUSTERING                    
062400                         ELSE                                             
062500                            PERFORM DH-SKAPA-INVENTERING                  
062600                         END-IF                                           
062700                       ELSE                                               
062800                          PERFORM DH-SKAPA-INVENTERING                    
062900                    END-IF                                                
063000                 END-IF                                                   
063100                 IF WS-WDK611-UPDATE = JA                                 
063200                   PERFORM IMS-REPL-WDK611                                
063300                   PERFORM S06-FLYTTA-LOGG-WDK6                           
063400                   PERFORM DGB-UPPDATERA-LOGG                             
063500                 END-IF                                                   
063600                 IF WS-WDK629-UPDATE = JA                                 
063700                   PERFORM IMS-GHNP-WDK629                                
063800                   IF SEGMENT-FOUND                                       
063900                     IF CREF-FLREFNYO = JA                                
064000                       MOVE NEJ         TO CREF-FLREFNYO                  
064100                       PERFORM IMS-REPL-WDK629                            
064200                     END-IF                                               
064300                   END-IF                                                 
064400                 END-IF                                                   
064500              END-IF                                                      
064600           ELSE                                                           
064700*       FEL-3  UTREDNINGSSALDOT ÄR REDAN UPPDATERAT                       
064800              MOVE '268'          TO RESP-IDMSG-ERROR                     
064900              MOVE 'KVUTRS'       TO RESP-IDELMT-ERROR                    
065000           END-IF                                                         
065100         ELSE                                                             
065200            MOVE '025'            TO RESP-IDMSG-ERROR                     
065300*           PART MISSING      ***                                         
065400            MOVE 'IDARTNR'        TO RESP-IDELMT-ERROR                    
065500         END-IF                                                           
065600       ELSE                                                               
065700          MOVE '025'              TO RESP-IDMSG-ERROR                     
065800*         PART MISSING      ***                                           
065900          MOVE 'IDARTNR'          TO RESP-IDELMT-ERROR                    
066000       END-IF                                                             
066100     ELSE                                                                 
066200*       FEL-6  DÅLIGT OBJEKT FÅR EJ UPPDATERAS                            
066300        MOVE '218'                TO RESP-IDMSG-ERROR                     
066400     END-IF                                                               
066500     .                                                                    
066600 DA-GET-CDC-INFO SECTION.                                                 
066700     MOVE 'DA-GET-CDC-INFO ' TO CURR-SECTION                              
066800                                                                          
066900     PERFORM IMS-02-GET-ARTC11                                            
067000                                                                          
067100     IF SEGMENT-FOUND                                                     
067200       MOVE CLAG-ADLAGOMR   TO SPAR-ADLAGOMR                              
067300       MOVE CLAG-ADGANG     TO SPAR-ADGANG                                
067400       MOVE CLAG-ADPLATS    TO SPAR-ADPLATS                               
067500       MOVE CLAG-KVLS       TO SPAR-KVLS                                  
067600                                                                          
067700       IF CLAG-KVUTRS = 0                                                 
067800         MOVE CLAG-KVUTRS   TO SPAR-KVUTRS                                
067900         MOVE CLAG-KVLS     TO SUM-KVUTRS                                 
068000       ELSE                                                               
068100         MOVE CLAG-KVUTRS   TO SPAR-KVUTRS                                
068200       END-IF                                                             
068300     END-IF                                                               
068400     .                                                                    
068500 DB-GET-SDC-INFO SECTION.                                                 
068600     MOVE 'DB-GET-SDC-INFO ' TO CURR-SECTION                              
068700                                                                          
068800     PERFORM IMS-03-GET-WDK7ART                                           
068900                                                                          
069000     IF SEGMENT-FOUND                                                     
069100       MOVE SLAG-ADLAGOMR   TO SPAR-ADLAGOMR                              
069200       MOVE SLAG-ADGANG     TO SPAR-ADGANG                                
069300       MOVE SLAG-ADPLATS    TO SPAR-ADPLATS                               
069400       MOVE SLAG-KVLS       TO SPAR-KVLS                                  
069500       MOVE SLAG-PRAVCOST   TO SPAR-PRAVCOST                              
069600                                                                          
069700       IF SLAG-KVUTRS = 0                                                 
069800         MOVE SLAG-KVUTRS   TO SPAR-KVUTRS                                
069900         MOVE SLAG-KVLS     TO SUM-KVUTRS                                 
070000       ELSE                                                               
070100         MOVE SLAG-KVUTRS   TO SPAR-KVUTRS                                
070200       END-IF                                                             
070300     END-IF                                                               
070400     .                                                                    
070500 DC-KOLLA-HOEGLAGER  SECTION.                                             
070600     MOVE 'DC-KOLLA-HOEGLAGER' TO CURR-SECTION                            
070700*                                                                         
070800*  KONTROLLERA OM HÖGLAGERSALDO F + OF = 0                                
070900*                                                                         
071000     MOVE JA   TO HOEGLAG-KOLL                                            
071100     MOVE ZERO TO HOEGLAG-SALDO                                           
071200     MOVE ZERO TO ADBUFFOMR-1                                             
071300                  ADBUFFOMR-2                                             
071400                  ADBUFFOMR-3                                             
071500                                                                          
071600     PERFORM IMS-04-GU-ARTD-SALDO                                         
071700     IF SEGMENT-FOUND                                                     
071800        MOVE +1 TO IX                                                     
071900                   INDX                                                   
072000        PERFORM IMS-05-GNP-ADR-SALDO                                      
072100        PERFORM UNTIL SEGMENT-MISSING OR INDX = +4                        
072200           ADD ARTD-SALDO-KVBUFF-F  TO HOEGLAG-SALDO                      
072300           ADD ARTD-SALDO-KVBUFF-OF TO HOEGLAG-SALDO                      
072400           IF HOEGLAG-SALDO > +0                                          
072500              IF IX = +1                                                  
072600                 MOVE ARTD-SALDO-ADBUFFOMR TO                             
072700                      ADBUFFOMR-1                                         
072800              ELSE                                                        
072900                 IF IX = +2                                               
073000                    MOVE ARTD-SALDO-ADBUFFOMR TO                          
073100                         ADBUFFOMR-2                                      
073200                 ELSE                                                     
073300                    IF IX = +3                                            
073400                       MOVE ARTD-SALDO-ADBUFFOMR TO                       
073500                            ADBUFFOMR-3                                   
073600                    END-IF                                                
073700                 END-IF                                                   
073800              END-IF                                                      
073900              ADD +1 TO IX                                                
074000              MOVE NOO TO HOEGLAG-KOLL                                    
074100              MOVE ZERO TO HOEGLAG-SALDO                                  
074200           END-IF                                                         
074300           ADD +1 TO INDX                                                 
074400           PERFORM IMS-05-GNP-ADR-SALDO                                   
074500        END-PERFORM                                                       
074600     END-IF                                                               
074700     .                                                                    
074800 DD-KOLLA-INVENTERING  SECTION.                                           
074900     MOVE 'DD-KOLLA-INVENTERING      ' TO CURR-SECTION                    
075000*  KONTROLLERA OM INVENTERING MED KAT = 6 EJ FINNS PÅ WDH1                
075100                                                                          
075200     MOVE NOO TO INV-FINNS                                                
075300     PERFORM IMS-07-GET-INVENT-ROT                                        
075400                                                                          
075500     IF SEGMENT-MISSING                                                   
075600       MOVE NOO TO INV-ROT-FINNS                                          
075700     ELSE                                                                 
075800       PERFORM IMS-08-GET-INVSEG                                          
075900       PERFORM UNTIL SEGMENT-MISSING OR INV-FINNS = JA                    
076000         IF INV-INV-KDINVKAT NOT = +6                                     
076100          IF INV-INV-IDDC = REQU-IDDC-KEY AND                             
076200                            INV-INV-FLINVBEH = NOO                        
076300            MOVE JA  TO INV-FINNS                                         
076400          ELSE                                                            
076500            PERFORM IMS-08-GET-INVSEG                                     
076600          END-IF                                                          
076700         ELSE                                                             
076800           PERFORM IMS-08-GET-INVSEG                                      
076900         END-IF                                                           
077000       END-PERFORM                                                        
077100     END-IF                                                               
077200     .                                                                    
077300 DE-INVENTERING-FINNS  SECTION.                                           
077400     MOVE 'DE-INVENTERING-FINNS      ' TO CURR-SECTION                    
077500*                                                                         
077600*  BEHANDLING OM INVENTERING KATEGORI EJ = 6 REDAN FINNS PÅ WDH1          
077700                                                                          
077800     MOVE INV-INV-KDINVKAT    TO SPAR-INVKAT                              
077900     MOVE INV-INV-TEINVANM    TO KOMMENTAR                                
078000     MOVE 'KAT 11'            TO KOMM-11                                  
078100     MOVE INV-INV-FLINVSKR    TO KOMM-SKR                                 
078200     MOVE KOMMENTAR           TO INV-INV-TEINVANM                         
078300                                                                          
078400     PERFORM IMS-09-REPL-INVSEG                                           
078500                                                                          
078600     MOVE REQU-IDDC-KEY       TO W-IDDC-WDH1-MIN                          
078700                                 W-IDDC-WDH1-MAX                          
078800                                                                          
078900     PERFORM IMS-10-GET-INVENT-KAT11                                      
079000                                                                          
079100     IF SEGMENT-MISSING                                                   
079200       IF SPAR-INVKAT   = +1 OR +2 OR +3 OR +4 OR +9                      
079300         MOVE SUM-KVUTRS      TO INV-INV-KVJUSTKV                         
079400         MOVE REQU-IDDC-KEY   TO INV-INV-IDDC                             
079500         MOVE +11             TO INV-INV-KDINVKAT                         
079600         MOVE SPAR-ADLAGOMR   TO INV-INV-ADLAGOMR                         
079700         MOVE SPAR-ADGANG     TO INV-INV-ADGANG                           
079800         MOVE SPAR-ADPLATS    TO INV-INV-ADPLATS                          
079900         MOVE NOO             TO INV-INV-FLINVSKR                         
080000         MOVE JA              TO INV-INV-FLINVBEH                         
080100                                                                          
080200         MOVE NOO             TO INV-INV-FLINV2B                          
080300         MOVE NOO             TO INV-INV-FLINV3E                          
080400         IF W-IDDC = WS-CDC-11                                            
080500         MOVE JA              TO INV-INV-FLINV3E                          
080600         END-IF                                                           
080700         MOVE NOO             TO INV-INV-FLINV4N                          
080800         MOVE JA              TO INV-INV-FLINV2B                          
080900                                                                          
081000         MOVE NOO             TO INV-INV-FLINV2C                          
081100         MOVE NOO             TO INV-INV-FLINV2D                          
081200         MOVE NOO             TO INV-INV-FLINV4P                          
081300         MOVE NOO             TO INV-INV-FLINV4R                          
081400         MOVE ZERO            TO INV-INV-KDINVKAT-OLD                     
081500         MOVE NOO             TO INV-INV-FLINV85                          
081600         MOVE SPACE           TO INV-INV-FILLER1                          
081700                                 INV-INV-FILLER2                          
081800         MOVE ART-IDFKNGRP    TO INV-INV-IDFKNGRP                         
081900         MOVE +1              TO INV-INV-KDINVPRIO                        
082000         MOVE ART-KDPRODSL    TO INV-INV-KDPRODSL                         
082100         MOVE CLAG-KDPSLLOC   TO INV-INV-KDPSLLOC                         
082200         MOVE CLAG-KDVVKL     TO INV-INV-KDVVKL                           
082300         MOVE SPACE           TO INV-INV-TEINVANM                         
082400         MOVE ZERO            TO INV-INV-IDPRTOMG                         
082500                                 INV-INV-IDLOPNR                          
082600                                 INV-INV-KVAKS-OLD                        
082700                                 INV-INV-KVEFRS-OLD                       
082800                                 INV-INV-KVLS-OLD                         
082900         MOVE DAGENS-TIAAMMDD TO WS-TIAAMMDD                              
083000         PERFORM S03-SKAPA-WDH1DAT                                        
083100         MOVE WS-TISEGKEY2    TO INV-INV-TISEGKEY                         
083200         MOVE WS-INV-DAREGDAT TO INV-INV-DAREGDAT-CRE                     
083300                                 INV-INV-DAREGDAT                         
083400***      COMPUTE INV-INV-DAREGDAT-SORT =                                  
083500***        99999999 - WS-INV-DAREGDAT                                     
083600         MOVE 99999999        TO INV-INV-DAREGDAT-SORT                    
083700         MOVE ZERO            TO INV-INV-DAREGDAT-PR1                     
083800                                 INV-INV-DAREGDAT-PR2                     
083900                                 INV-INV-DAREGDAT-PR3                     
084000                                                                          
084100         PERFORM IMS-11-INSERT-INVENTERING                                
084200         IF SEGMENT-EXISTS OR INDEX-EXISTS                                
084300           PERFORM UNTIL SEGMENT-FOUND                                    
084400             ADD +1  TO WS-TISEGKEY2                                      
084500             MOVE WS-TISEGKEY2    TO INV-INV-TISEGKEY                     
084600             PERFORM IMS-11-INSERT-INVENTERING                            
084700           END-PERFORM                                                    
084800         END-IF                                                           
084900                                                                          
085000*** INSERT PÅ WDH121 SEGMENTET ***                                        
085100         MOVE REQU-IDUSER TO INV-INVL-IDUSER                              
085200         MOVE '0'         TO INV-INVL-KDSEGKEY                            
085300         PERFORM IMS-INSERT-WDH121                                        
085400         MOVE SPACE       TO INV-INVL-IDUSER                              
085500         MOVE '1'         TO INV-INVL-KDSEGKEY                            
085600         PERFORM IMS-INSERT-WDH121                                        
085700         MOVE SPACE       TO INV-INVL-IDUSER                              
085800         MOVE '2'         TO INV-INVL-KDSEGKEY                            
085900         PERFORM IMS-INSERT-WDH121                                        
086000         MOVE SPACE       TO INV-INVL-IDUSER                              
086100         MOVE '3'         TO INV-INVL-KDSEGKEY                            
086200         PERFORM IMS-INSERT-WDH121                                        
086300                                                                          
086400**** SLUT PÅ INSERT PÅ WDH121 SEGMENT                                     
086500         PERFORM S04-UPPD-ART-MED-UTRSALDO                                
086600       END-IF                                                             
086700                                                                          
086800       IF  DCS-LAND-NON-VCC-OWNED                                         
086900       OR (DCS-NDC-NA AND DCS-USA)                                        
087000          IF SLAG-IDDC-REF = SPACE                                        
087100            PERFORM S05-SAEND-LARM-2191-MID                               
087200          END-IF                                                          
087300       END-IF                                                             
087400                                                                          
087500       IF SPAR-INVKAT = +1 OR +2 OR +3 OR +4 OR +9                        
087600         IF W-IDDC = WS-CDC-11                                            
087700          MOVE SUM-KVUTRS TO CLAG-KVUTRS                                  
087800          MOVE JA TO WS-WDK611-UPDATE                                     
087900          PERFORM S06-SAEND-LARM-2191-MID-CDC                             
088000         ELSE                                                             
088100          PERFORM IMS-03-GET-WDK7ART                                      
088200          MOVE SUM-KVUTRS TO SLAG-KVUTRS                                  
088300                                                                          
088400          PERFORM IMS-14-REPL-WDK7-SEGM                                   
088500         END-IF                                                           
088600       END-IF                                                             
088700                                                                          
088800       IF SPAR-INVKAT   = +8                                              
088900*         MEDDELANDE-1  BORTJUSTERING REDAN GJORD                         
089000          MOVE '267'               TO RESP-IDMSG-ERROR                    
089100       ELSE                                                               
089200*         MEDDELANDE    UTREDNINGSSALDO REDAN UPPDATERAT                  
089300          MOVE '268'               TO RESP-IDMSG-ERROR                    
089400          MOVE 'KVUTRS'            TO RESP-IDELMT-ERROR                   
089500       END-IF                                                             
089600     ELSE                                                                 
089700        PERFORM IMS-03-GET-WDK7ART                                        
089800        MOVE SUM-KVUTRS TO SLAG-KVUTRS                                    
089900                                                                          
090000        PERFORM IMS-14-REPL-WDK7-SEGM                                     
090100        PERFORM S04-UPPD-ART-MED-UTRSALDO                                 
090200     END-IF                                                               
090300     .                                                                    
090400 DF-KOLLA-INLEV-CDC SECTION.                                              
090500     MOVE NEJ TO INLEV-KOLL                                               
090600     MOVE +0 TO INLEV-DATUM                                               
090700     PERFORM IMS-GU-WDL201                                                
090800     IF SEGMENT-MISSING                                                   
090900      MOVE JA TO INLEV-KOLL                                               
091000     ELSE                                                                 
091100      PERFORM IMS-GNP-WDL211                                              
091200      PERFORM UNTIL SEGMENT-MISSING OR INLEV-DATUM NOT = +0               
091300       MOVE INL-INL-DAINLEV TO W-DAINLEV                                  
091400       PERFORM IMS-GNP-WDL221                                             
091500       PERFORM UNTIL SEGMENT-MISSING OR INLEV-DATUM NOT = +0              
091600        IF INL-MOT-IDDC = WS-CDC-11                                       
091700          IF INL-MOT-KDRT NOT = 77 AND 88 AND 99                          
091800             IF INL-MOT-KVAVIS > +0                                       
091900                IF INL-MOT-KVAVIS NOT = INL-MOT-KVFORDEL                  
092000                   IF INL-MOT-IDPTYP = 'R31' OR '310'                     
092100                      PERFORM IMS-GNP-WDL231                              
092200                      IF SEGMENT-FOUND                                    
092300                         MOVE INL-DEL-TIREGDAT TO                         
092400                              INLEV-DATUM                                 
092500                      END-IF                                              
092600                   ELSE                                                   
092700                      IF INL-MOT-IDPTYP = 'R32'                           
092800                         MOVE INL-MOT-TIUPPDAT TO                         
092900                              INLEV-DATUM                                 
093000                      END-IF                                              
093100                   END-IF                                                 
093200                END-IF                                                    
093300             END-IF                                                       
093400          END-IF                                                          
093500        END-IF                                                            
093600        IF INLEV-DATUM = +0                                               
093700           PERFORM IMS-GNP-WDL221                                         
093800        END-IF                                                            
093900       END-PERFORM                                                        
094000       PERFORM IMS-GNP-WDL222                                             
094100       PERFORM UNTIL SEGMENT-MISSING OR                                   
094200                     INL-DIR-IDDC = WS-CDC-11                             
094300          PERFORM IMS-GNP-WDL222                                          
094400       END-PERFORM                                                        
094500       IF SEGMENT-FOUND                                                   
094600          MOVE INL-DIR-TIAVSDAT   TO TMP1-YYMMDD                          
094700          MOVE INLEV-DATUM        TO TMP2-YYMMDD                          
094800          PERFORM WY2000P1                                                
094900          IF TMP1-YYMMDD > TMP2-YYMMDD                                    
095000             MOVE INL-DIR-TIAVSDAT TO INLEV-DATUM                         
095100          END-IF                                                          
095200       END-IF                                                             
095300       IF INLEV-DATUM = +0                                                
095400          PERFORM IMS-GNP-WDL211                                          
095500       END-IF                                                             
095600      END-PERFORM                                                         
095700     END-IF                                                               
095800     IF INLEV-DATUM NOT = +0                                              
095900***  OM INLEVERANS SKETT FÖR HÖGST 10 DAGAR SEDAN FÖR SDC OCH             
096000***                      FÖR HÖGST 10 DAGAR SEDAN FÖR CDC                 
096100***  SKER INGEN AUTOMATJUSTERING.                                         
096200***  ÄNDRAT 2000-10-27  , NY DAGSGRÄNSER ENL. BERIT JEBSEN                
096300       MOVE INLEV-DATUM        TO WS-INLEV-DATUM                          
096400       MOVE DAGENS-TIAAMMDD    TO WS-DAGENS-DATUM                         
096500                                                                          
096600       IF WS-INLEV-DATUM-AA > 50                                          
096700         MOVE 19               TO WS-INLEV-DATUM-SEKEL                    
096800       ELSE                                                               
096900         MOVE 20               TO WS-INLEV-DATUM-SEKEL                    
097000       END-IF                                                             
097100********************************************************                  
097200*** POSTER ÄLDRE ÄN ETT ÅR BEHÖVS INTE KOLLAS MED WORKDAY                 
097300********************************************************                  
097400       MOVE 20                 TO WS-DAGENS-DATUM-SEKEL                   
097500       COMPUTE WS-DAGENS-DATUM-1AA = WS-DAGENS-DATUM - 10000              
097600       IF WS-DAGENS-DATUM-1AA <= WS-INLEV-DATUM                           
097700         MOVE JA               TO KOLLA-WORKDAY                           
097800       ELSE                                                               
097900         MOVE NEJ              TO KOLLA-WORKDAY                           
098000         MOVE JA               TO INLEV-KOLL                              
098100       END-IF                                                             
098200       IF KOLLA-WORKDAY-OK                                                
098300         MOVE 001                TO WORK-KDCALL                           
098400         MOVE WS-CDC-11          TO WORK-IDDC                             
098500         MOVE INLEV-DATUM        TO WORK-TIAAMMDD-FOM                     
098600         MOVE DAGENS-TIAAMMDD    TO WORK-TIAAMMDD-TOM                     
098700         CALL WORKDAY USING         WORK-KDCALL                           
098800                                    WORK-DATE-AREA                        
098900                                    WORK-KDSVAR                           
099000         IF WORK-KDSVAR-OK                                                
099100           IF NOT DCS-CDC                                                 
099200             IF WORK-KVWORKD > 10                                         
099300               MOVE JA           TO INLEV-KOLL                            
099400             END-IF                                                       
099500           ELSE                                                           
099600             IF WORK-KVWORKD > 10                                         
099700               MOVE JA           TO INLEV-KOLL                            
099800             END-IF                                                       
099900           END-IF                                                         
100000         END-IF                                                           
100100       END-IF                                                             
100200     ELSE                                                                 
100300        MOVE JA TO INLEV-KOLL                                             
100400     END-IF                                                               
100500     .                                                                    
100600     EJECT                                                                
100700 DF-KOLLA-INLEV-SDC SECTION.                                              
100800     MOVE 'DF-KOLLA-INLEV-SDC        ' TO CURR-SECTION                    
100900*                                                                         
101000*  KONTROLLERA OM INLEV. HAR KOMMIT SENASTE MÅNADEN FÖR SDC.              
101100*                                                                         
101200     MOVE NOO TO INLEV-KOLL                                               
101300     MOVE +0 TO INLEV-DATUM                                               
101400                                                                          
101500     PERFORM IMS-15-GET-WDL601                                            
101600                                                                          
101700     IF SEGMENT-MISSING                                                   
101800       MOVE JA  TO INLEV-KOLL                                             
101900     ELSE                                                                 
102000       PERFORM IMS-16-GET-WDL611                                          
102100       PERFORM UNTIL SEGMENT-MISSING OR INLEV-DATUM NOT = +0              
102200          IF INLC-INL-IDDC = REQU-IDDC-KEY                                
102300            IF INLC-INL-IDPTYP = 'R32'                                    
102400              MOVE INLC-INL-TIINLINL TO INLEV-DATUM                       
102500            END-IF                                                        
102600          END-IF                                                          
102700                                                                          
102800          IF INLEV-DATUM = +0                                             
102900             PERFORM IMS-16-GET-WDL611                                    
103000          END-IF                                                          
103100       END-PERFORM                                                        
103200     END-IF                                                               
103300                                                                          
103400     IF INLEV-DATUM NOT = +0                                              
103500***  OM INLEVERANS SKETT FÖR HÖGST 10 DAGAR SEDAN                         
103600***  SKER INGEN AUTOMATJUSTERING.                                         
103700       MOVE 001                TO WORK-KDCALL                             
103800       MOVE REQU-IDDC-KEY      TO WORK-IDDC                               
103900       MOVE INLEV-DATUM        TO WORK-TIAAMMDD-FOM                       
104000       MOVE DAGENS-TIAAMMDD    TO WORK-TIAAMMDD-TOM                       
104100       CALL WORKDAY  USING        WORK-KDCALL                             
104200                                  WORK-DATE-AREA                          
104300                                  WORK-KDSVAR                             
104400       IF WORK-KDSVAR-OK                                                  
104500          IF WORK-KVWORKD > 10                                            
104600             MOVE JA           TO INLEV-KOLL                              
104700          END-IF                                                          
104800       END-IF                                                             
104900     ELSE                                                                 
105000        MOVE JA  TO INLEV-KOLL                                            
105100     END-IF                                                               
105200     .                                                                    
105300 DG-SKAPA-JUSTERING  SECTION.                                             
105400     MOVE 'DG-SKAPA-JUSTERING        ' TO CURR-SECTION                    
105500*                                                                         
105600*  SKAPA EN JUSTERING DIREKT                                              
105700*  SKAPA INVENTERING KAT 08 PÅ WDH1                                       
105800*  SKAPA EN ÅTERFÖRING TILL W111                                          
105900*                                                                         
106000     IF W-IDDC = WS-CDC-11                                                
106100       SUBTRACT SUM-KVUTRS FROM CLAG-KVLS                                 
106200       COMPUTE WS-KVANTAL =                                               
106300        CLAG-KVLS + CLAG-KVEFRS + CLAG-KVAKS-CDC                          
106400       MOVE '-'             TO LOGG-IDTECKEN-KVLS                         
106500       MOVE SUM-KVUTRS      TO W-SPAR-KVUTRS                              
106600       MOVE JA TO WS-WDK611-UPDATE                                        
106700       PERFORM S06-SAEND-LARM-2191-MID-CDC                                
106800                                                                          
106900       MOVE 'AAMMDD'        TO DAT-KDDATFORM                              
107000       MOVE DAGENS-TIAAMMDD TO DAT-I-TIDATUM                              
107100       CALL WDATKONV USING     DAT-KDDATFORM                              
107200                               DAT-I-TIDATUM                              
107300                               DAT-O-TIDATUM                              
107400                               DAT-KDSVAR                                 
107500                                                                          
107600       IF DAT-KDSVAR-FEL                                                  
107700         CALL FELLOG                                                      
107800       END-IF                                                             
107900                                                                          
108000       MOVE DAT-TIAAMMDD TO WS-TIAAMMDD                                   
108100                                                                          
108200       MULTIPLY -1 BY SUM-KVUTRS                                          
108300                                                                          
108400       MOVE DAT-TIAAVVD   TO CLAG-TIINVDAT                                
108500       MOVE SUM-KVUTRS    TO CLAG-KVINVS                                  
108600                                                                          
108700       MOVE JA TO WS-WDK611-UPDATE                                        
108800                                                                          
108900**** OM DET ÄR EN REFILLARTIKEL SÅ SKALL FLAGGA SÄTTAS TILL N             
109000       MOVE JA TO WS-WDK629-UPDATE                                        
109100**** REPLACE GÖRS SENARE I AB-SEKTIONEN EFTER K611 UPPDATERINGEN!!        
109200     ELSE                                                                 
109300     SUBTRACT SUM-KVUTRS FROM SLAG-KVLS                                   
109400     MOVE '-'             TO LOGG-IDTECKEN-KVLS                           
109500     MOVE SUM-KVUTRS      TO W-SPAR-KVUTRS                                
109600                                                                          
109700     MOVE 'AAMMDD'        TO DAT-KDDATFORM                                
109800     MOVE DAGENS-TIAAMMDD TO DAT-I-TIDATUM                                
109900     CALL WDATKONV USING     DAT-KDDATFORM                                
110000                             DAT-I-TIDATUM                                
110100                             DAT-O-TIDATUM                                
110200                             DAT-KDSVAR                                   
110300                                                                          
110400     IF DAT-KDSVAR-FEL                                                    
110500        CALL FELLOG                                                       
110600     END-IF                                                               
110700                                                                          
110800     MOVE DAT-TIAAMMDD TO WS-TIAAMMDD                                     
110900                                                                          
111000     MULTIPLY -1 BY SUM-KVUTRS                                            
111100                                                                          
111200     MOVE DAT-TIAAVVD   TO SLAG-TIINVDAT                                  
111300     MOVE SUM-KVUTRS    TO SLAG-KVINVS                                    
111400     PERFORM IMS-14-REPL-WDK7-SEGM                                        
111500                                                                          
111600     IF  DCS-LAND-NON-VCC-OWNED                                           
111700     OR (DCS-NDC-NA AND DCS-USA)                                          
111800        IF SLAG-IDDC-REF = SPACE                                          
111900          PERFORM S05-SAEND-LARM-2191-MID                                 
112000        END-IF                                                            
112100     END-IF                                                               
112200                                                                          
112300     PERFORM DGA-FLYTTA-LOGG-WDK7                                         
112400     PERFORM DGB-UPPDATERA-LOGG                                           
112500     PERFORM DGC-ISRT-INVENTERINGSHISTORIK                                
112600     END-IF                                                               
112700     IF INV-ROT-FINNS = NOO                                               
112800       MOVE W-IDARTNR-X   TO INV-ART-IDARTNR                              
112900       PERFORM IMS-21-INSERT-ROT-INVENTERING                              
113000     END-IF                                                               
113100                                                                          
113200     MOVE SUM-KVUTRS      TO INV-INV-KVJUSTKV                             
113300                                                                          
113400     MOVE REQU-IDDC-KEY   TO INV-INV-IDDC                                 
113500     MOVE +8              TO INV-INV-KDINVKAT                             
113600     MOVE ZERO            TO INV-INV-KDINVKAT-OLD                         
113700     MOVE SPAR-ADLAGOMR   TO INV-INV-ADLAGOMR                             
113800     MOVE SPAR-ADGANG     TO INV-INV-ADGANG                               
113900     MOVE SPAR-ADPLATS    TO INV-INV-ADPLATS                              
114000     MOVE NOO             TO INV-INV-FLINVSKR                             
114100     MOVE JA              TO INV-INV-FLINVBEH                             
114200                                                                          
114300     MOVE NOO             TO INV-INV-FLINV2B                              
114400     MOVE NOO             TO INV-INV-FLINV2D                              
114500     MOVE NOO             TO INV-INV-FLINV3E                              
114600     IF W-IDDC = WS-CDC-11                                                
114700       MOVE JA            TO INV-INV-FLINV2D                              
114800       MOVE JA            TO INV-INV-FLINV3E                              
114900     END-IF                                                               
115000     MOVE NOO             TO INV-INV-FLINV4N                              
115100     MOVE NOO             TO INV-INV-FLINV4R                              
115200     MOVE JA              TO INV-INV-FLINV2B                              
115300     MOVE JA              TO INV-INV-FLINV2D                              
115400                                                                          
115500     MOVE NOO             TO INV-INV-FLINV2C                              
115600     MOVE NOO             TO INV-INV-FLINV4P                              
115700     MOVE NOO             TO INV-INV-FLINV85                              
115800     MOVE SPACE           TO INV-INV-FILLER1                              
115900                             INV-INV-FILLER2                              
116000     MOVE ART-IDFKNGRP    TO INV-INV-IDFKNGRP                             
116100     MOVE +1              TO INV-INV-KDINVPRIO                            
116200     MOVE ART-KDPRODSL    TO INV-INV-KDPRODSL                             
116300     MOVE CLAG-KDPSLLOC   TO INV-INV-KDPSLLOC                             
116400     MOVE CLAG-KDVVKL     TO INV-INV-KDVVKL                               
116500     PERFORM S03-SKAPA-WDH1DAT                                            
116600     MOVE WS-TISEGKEY2    TO INV-INV-TISEGKEY                             
116700     MOVE SPACE           TO INV-INV-TEINVANM                             
116800     MOVE ZERO            TO INV-INV-IDPRTOMG                             
116900                             INV-INV-IDLOPNR                              
117000                             INV-INV-KVAKS-OLD                            
117100                             INV-INV-KVEFRS-OLD                           
117200                             INV-INV-KVLS-OLD                             
117300     MOVE WS-INV-DAREGDAT TO INV-INV-DAREGDAT-CRE                         
117400                             INV-INV-DAREGDAT                             
117500**   COMPUTE INV-INV-DAREGDAT-SORT =                                      
117600**     99999999 - WS-INV-DAREGDAT                                         
117700     MOVE 99999999        TO INV-INV-DAREGDAT-SORT                        
117800     MOVE ZERO            TO INV-INV-DAREGDAT-PR1                         
117900                             INV-INV-DAREGDAT-PR2                         
118000                             INV-INV-DAREGDAT-PR3                         
118100                                                                          
118200     PERFORM IMS-11-INSERT-INVENTERING                                    
118300     IF SEGMENT-EXISTS OR INDEX-EXISTS                                    
118400       PERFORM UNTIL SEGMENT-FOUND                                        
118500         ADD +1  TO WS-TISEGKEY2                                          
118600         MOVE WS-TISEGKEY2    TO INV-INV-TISEGKEY                         
118700         PERFORM IMS-11-INSERT-INVENTERING                                
118800       END-PERFORM                                                        
118900     END-IF                                                               
119000                                                                          
119100*** INSERT PÅ WDH21 SEGMENTET ***                                         
119200         MOVE REQU-IDUSER TO INV-INVL-IDUSER                              
119300         MOVE '0'         TO INV-INVL-KDSEGKEY                            
119400         PERFORM IMS-INSERT-WDH121                                        
119500         MOVE SPACE       TO INV-INVL-IDUSER                              
119600         MOVE '1'         TO INV-INVL-KDSEGKEY                            
119700         PERFORM IMS-INSERT-WDH121                                        
119800         MOVE SPACE       TO INV-INVL-IDUSER                              
119900         MOVE '2'         TO INV-INVL-KDSEGKEY                            
120000         PERFORM IMS-INSERT-WDH121                                        
120100         MOVE SPACE       TO INV-INVL-IDUSER                              
120200         MOVE '3'         TO INV-INVL-KDSEGKEY                            
120300         PERFORM IMS-INSERT-WDH121                                        
120400                                                                          
120500**** SLUT PÅ INSERT PÅ WDH21 SEGMENT                                      
120600     IF SUM-KVUTRS NOT = 0                                                
120700       IF DCS-LAND-NON-VCC-OWNED                                          
120800       OR LDC-CN                                                          
120900          PERFORM DGE-UPPDATERA-WDR8                                      
121000          PERFORM IMS-24-ISRT-WDR801                                      
121100          PERFORM UNTIL SEGMENT-FOUND                                     
121200            ADD +1  TO WDR8-FIL-IDSEKVNR                                  
121300            PERFORM IMS-24-ISRT-WDR801                                    
121400          END-PERFORM                                                     
                IF DCS-KDTRADP = 'BR12'                                         
                  PERFORM S07-FIX-LOCAL-TIME                                    
                  PERFORM S10-SEND-OPEN                                         
                  MOVE SEND-IDCOM         TO WZ04-SEND-IDCOM                    
                  PERFORM S11-SEND-PUT-PROP                                     
                  MOVE ZERO               TO NOTF-IDSEKVNR                      
                  MOVE WDR8-EKH-KDEKHHT   TO NOTF-KDEKHHT                       
                  MOVE WDR8-EKH-KDEKSHT   TO NOTF-KDEKSHT                       
                  MOVE WDR8-EKH-DAVERDAT  TO NOTF-DAVERDAT                      
                  MOVE AKTUELL-TID-X(1:6) TO NOTF-TIREGTID                      
                  MOVE WDR8-EKH-IDVERGL   TO NOTF-IDVERGL                       
                  MOVE WDR8-EKH-IDDC-REC  TO NOTF-IDDC                          
                  MOVE ZERO               TO NOTF-IDFAKT                        
                                             NOTF-IDKUNDNR                      
                                             NOTF-IDORDER                       
                                             NOTF-IDKOLLI                       
                  MOVE WDR8-EKH-IDARTNR   TO W-IDARTNR-EDIT-X                   
                  MOVE FUNCTION TRIM(W-IDARTNR-EDIT-X LEADING)                  
                                          TO NOTF-IDARTNR20                     
                  MOVE WDR8-EKH-KVANTAL   TO NOTF-KVANTAL                       
                  ADD +1 TO NOTF-IDSEKVNR                                       
                  PERFORM S12-SEND-PUT                                          
                  PERFORM S13-SEND-CLOSE                                        
                END-IF                                                          
121500       ELSE                                                               
121600          PERFORM DGD-UPPDATERA-WDR9                                      
121700          PERFORM IMS-23-ISRT-WDR901                                      
121800          PERFORM UNTIL SEGMENT-FOUND                                     
121900            ADD +1  TO FIL-IDSEKVNR                                       
122000            PERFORM IMS-23-ISRT-WDR901                                    
122100          END-PERFORM                                                     
122200       END-IF                                                             
122300     END-IF                                                               
122400     PERFORM S04-UPPD-ART-MED-UTRSALDO                                    
122500                                                                          
122600*    MEDDELANDE-2  SALDO JUSTERAT - KAT 08                                
122700     MOVE '269'               TO RESP-IDMSG-INFO                          
122800     .                                                                    
122900 DGA-FLYTTA-LOGG-WDK7 SECTION.                                            
123000     MOVE 'DGA-FLYTTA-LOGG-WDK7      ' TO CURR-SECTION                    
123100* LÄGGER UPP SALDOLOGG I WDL9                                             
123200     MOVE W-IDARTNR             TO LOGG-IDARTNR                           
123300     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
123400     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - DAGENS-DATUM               
123500     ACCEPT TRANS-TID FROM TIME                                           
123600     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TID                 
123700     MOVE 9                       TO LOGG-IDSEKVNR                        
123800     MOVE REQU-IDDC-KEY           TO LOGG-IDDC                            
123900     MOVE 'MISC'                  TO LOGG-IDHUVTYP                        
124000     MOVE 'ADJ'                   TO LOGG-IDSUBTYP                        
124100     MOVE IDPGM                   TO LOGG-IDPGM                           
124200     MOVE 'L172'                  TO LOGG-IDTRANS                         
124300     MOVE REQU-IDUSER             TO LOGG-IDUSER                          
124400     MOVE SPACE                   TO LOGG-REF                             
124500     MOVE REQU-IDUSER             TO LOGG-IDUSER-IDPRCREF                 
124600     MOVE W-IDPRODNR              TO LOGG-IDPRODNR                        
124700     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS-PAV              
124800     MOVE SPACE                   TO LOGG-IDTECKEN-KVEFRS                 
124900     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS                  
125000     MOVE W-SPAR-KVUTRS           TO LOGG-KVART-SALDO                     
125100     MOVE SLAG-KVAKS-SDC          TO LOGG-KVAKS                           
125200     MOVE SLAG-KVAKS-PAV          TO LOGG-KVAKS-PAV                       
125300     MOVE SLAG-KVEFRS             TO LOGG-KVEFRS                          
125400     MOVE SLAG-KVLS               TO LOGG-KVLS                            
125500     MOVE ZERO                    TO LOGG-DAREGDAT-LADD                   
125600     .                                                                    
125700 S06-FLYTTA-LOGG-WDK6 SECTION.                                            
125800     MOVE 'S06-FLYTTA-LOGG-WDK6      ' TO CURR-SECTION                    
125900* LÄGGER UPP SALDOLOGG I WDL9                                             
126000     MOVE W-IDARTNR             TO LOGG-IDARTNR                           
126100     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
126200     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - DAGENS-DATUM               
126300     ACCEPT TRANS-TID FROM TIME                                           
126400     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TID                 
126500     MOVE 9                       TO LOGG-IDSEKVNR                        
126600     MOVE REQU-IDDC-KEY           TO LOGG-IDDC                            
126700     MOVE 'MISC'                  TO LOGG-IDHUVTYP                        
126800     MOVE 'ADJ'                   TO LOGG-IDSUBTYP                        
126900     MOVE IDPGM                   TO LOGG-IDPGM                           
127000     MOVE 'L172'                  TO LOGG-IDTRANS                         
127100     MOVE REQU-IDUSER             TO LOGG-IDUSER                          
127200     MOVE SPACE                   TO LOGG-REF                             
127300     MOVE REQU-IDUSER             TO LOGG-IDUSER-IDPRCREF                 
127400     MOVE W-IDPRODNR              TO LOGG-IDPRODNR                        
127500     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS-PAV              
127600     MOVE SPACE                   TO LOGG-IDTECKEN-KVEFRS                 
127700     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS                  
127800     MOVE W-SPAR-KVUTRS           TO LOGG-KVART-SALDO                     
127900     MOVE CLAG-KVAKS-PAV          TO LOGG-KVAKS-PAV                       
128000     MOVE CLAG-KVEFRS             TO LOGG-KVEFRS                          
128100     MOVE CLAG-KVLS               TO LOGG-KVLS                            
128200     MOVE ZERO                    TO LOGG-DAREGDAT-LADD                   
128300     COMPUTE LOGG-KVAKS = CLAG-KVAKS-CDC +                                
128400                          CLAG-KVAKS-T                                    
128500     .                                                                    
128600                                                                          
128700 DGB-UPPDATERA-LOGG SECTION.                                              
128800     MOVE 'DGB-UPPDATERA-LOGG       ' TO CURR-SECTION                     
128900                                                                          
129000     PERFORM IMS-17-ISRT-WDL901                                           
129100     IF SEGMENT-EXISTS                                                    
129200        PERFORM UNTIL NOT SEGMENT-EXISTS                                  
129300          ADD -1 TO LOGG-IDSEKVNR                                         
129400          PERFORM IMS-17-ISRT-WDL901                                      
129500        END-PERFORM                                                       
129600     END-IF                                                               
129700     .                                                                    
129800 DGC-ISRT-INVENTERINGSHISTORIK SECTION.                                   
129900     MOVE 'DGC-ISRT-INVENTERINGSHISTORIK' TO CURR-SECTION                 
130000                                                                          
130100     PERFORM IMS-18-GET-INVHIST-ROT                                       
130200     IF SEGMENT-MISSING                                                   
130300       MOVE W-IDARTNR TO INVA-IDARTNR                                     
130400       PERFORM IMS-19-ISRT-INVHIST-ROT                                    
130500     END-IF                                                               
130600                                                                          
130700     MOVE WS-TIAAAAMMDD(1:2)  TO WS-SEKEL                                 
130800     MOVE 9                   TO WS-LOPNR                                 
130900                                                                          
131000     COMPUTE WS-TISEGKEY = 999999999 - WS-TIAAAAMMDDL                     
131100                                                                          
131200     MOVE WS-TISEGKEY         TO INVH-TISEGKEY                            
131300                                                                          
131400     MOVE REQU-IDDC-KEY       TO INVH-IDDC                                
131500     MOVE WS-TIAAAAMMDD       TO INVH-DAREGDAT-CRE                        
131600                                 INVH-DAREGDAT-CLO                        
131700     MOVE SUM-KVUTRS          TO INVH-KVJUSTKV                            
131800     MOVE 8                   TO INVH-KDJUSTYP                            
131900     MOVE REQU-IDUSER         TO INVH-IDUSER-CLO                          
132000                                 INVH-IDUSER-CRE                          
132100     MOVE NOO                 TO INVH-FLAUTLSJ                            
132200     MOVE SPACE               TO INVH-IDPW                                
132300     MOVE CLAG-PRARTSTD       TO INVH-PRARTSTD                            
132400     MOVE ZERO                TO INVH-DAREGDAT-PR1                        
132500                                 INVH-DAREGDAT-PR2                        
132600                                 INVH-DAREGDAT-PR3                        
132700     MOVE SPACE               TO INVH-IDUSER-PR1                          
132800                                 INVH-IDUSER-PR2                          
132900                                 INVH-IDUSER-PR3                          
133000                                                                          
           IF W-IDDC = WS-CDC-11                                                
             MOVE WS-KVANTAL        TO INVH-KVANTAL                             
           ELSE                                                                 
             MOVE +0                TO INVH-KVANTAL                             
           END-IF                                                               
133200                                                                          
133300     PERFORM IMS-20-ISRT-INVHIST-SEGM                                     
133400                                                                          
133500     PERFORM UNTIL NOT SEGMENT-EXISTS                                     
133600       IF SEGMENT-EXISTS                                                  
133700         SUBTRACT 1 FROM INVH-TISEGKEY                                    
133800         PERFORM IMS-20-ISRT-INVHIST-SEGM                                 
133900       END-IF                                                             
134000     END-PERFORM                                                          
134100     .                                                                    
134200 DGD-UPPDATERA-WDR9 SECTION.                                              
134300     MOVE 'DGD-UPPDATERA-WDR9  ' TO CURR-SECTION                          
134400                                                                          
134500     MOVE IDPGM            TO FIL-IDPGM                                   
134600     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
134700     MOVE DAGENS-DATUM     TO FIL-DAREGDAT                                
134800     ACCEPT FIL-TIKLOCK    FROM TIME                                      
134900     MOVE 1                TO FIL-IDSEKVNR                                
135000     MOVE 'W510EKHA'       TO FIL-IDCPYTXT                                
135100     MOVE REQU-IDUSER      TO FIL-IDUSER                                  
135200     MOVE W-IDARTNR        TO EKH-IDARTNR                                 
135300     MOVE '403'            TO EKH-KDEKHHT                                 
135400     MOVE '408'            TO EKH-KDEKSHT                                 
135500     MOVE 'DET'            TO EKH-KDEKNIVA                                
135600     MOVE W-IDDC           TO EKH-IDDC-SEND                               
135700                              EKH-IDDC-REC                                
135800     MOVE +0               TO EKH-IDDISTR                                 
135900     MOVE +0               TO EKH-IDKUNDNR                                
136000                                                                          
136100     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
136200     MOVE W-IDARTNR        TO CIA-IDARTBET-IN                             
136300     CALL W009CIA USING       CIA-W009CIA                                 
136400     MOVE CIA-IDARTBET-UT TO EKH-IDVERGL                                  
136500                                                                          
136600     MOVE DAGENS-DATUM     TO EKH-DAVERDAT                                
136700     MOVE ART-KDPRODSL     TO EKH-KDPRODSL                                
136800     MOVE ZERO             TO EKH-KDPSLLOC                                
136900     MOVE SPACE            TO EKH-FLLSBOK                                 
137000     MOVE 'SEK'            TO EKH-KDVALISO                                
137100     MOVE 1.00             TO EKH-PRKURS                                  
137200     MOVE ZERO             TO EKH-PRARTNTO                                
137300     MOVE ZERO             TO EKH-PRARTSJK                                
137400     MOVE ZERO             TO EKH-PRHEMTAG                                
137500     MOVE CLAG-PRARTSTD    TO EKH-PRARTSTD                                
137600     MOVE ZERO             TO EKH-PRLANDCO                                
137700     MOVE ZERO             TO EKH-PRINK                                   
137800     MOVE ZERO             TO EKH-PRDIRLON                                
137900     MOVE ZERO             TO EKH-PRDMTRL                                 
138000     MOVE ZERO             TO EKH-PROVRPAL                                
138100     MOVE ZERO             TO EKH-SUBEL                                   
138200     COMPUTE EKH-KVANTAL = W-SPAR-KVUTRS * -1                             
138300     MOVE 'L172'           TO EKH-IDTRANS                                 
138400     MOVE ZERO             TO EKH-BEVAT                                   
138500                              EKH-IDANALYS                                
138600                              EKH-IDKONTO                                 
138700                              EKH-KDANMORS                                
138800                              EKH-KDFRAKT                                 
138900                              EKH-SUVAT                                   
139000     MOVE ZERO             TO EKH-DAAVIDAT                                
139100                              EKH-IDAVINR                                 
139200                              EKH-KDAVVTYP                                
139300                              EKH-KDRT                                    
139400                              EKH-KVANTMOT                                
139500                              EKH-KVAVIS                                  
139600     MOVE WS-KDSORT        TO EKH-KDSORT                                  
139700     MOVE NEJ              TO EKH-FLDCET                                  
139800     MOVE 'SEPV'           TO EKH-KDTRADP                                 
139900     MOVE SPACE            TO EKH-IDLEVNR                                 
140000                              EKH-IDKST                                   
140100     MOVE SPACE            TO EKH-IDKUNDRF                                
140200     MOVE SPACE            TO EKH-IDFAKT-EXP                              
140300     .                                                                    
140400                                                                          
140500 DGE-UPPDATERA-WDR8 SECTION.                                              
140600     MOVE 'DGE-UPPDATERA-WDR8  ' TO CURR-SECTION                          
140700                                                                          
140800     MOVE IDPGM            TO WDR8-FIL-IDPGM                              
140900     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
141000     MOVE DAGENS-DATUM     TO WDR8-FIL-TIREGDAT                           
141100     ACCEPT WDR8-FIL-TIKLOCK    FROM TIME                                 
141200     MOVE 1                TO WDR8-FIL-IDSEKVNR                           
141300     MOVE W-IDARTNR        TO WDR8-EKH-IDARTNR                            
141400     MOVE '403'            TO WDR8-EKH-KDEKHHT                            
141500     MOVE '408'            TO WDR8-EKH-KDEKSHT                            
141600     MOVE 'DET'            TO WDR8-EKH-KDEKNIVA                           
141700     MOVE W-IDDC           TO WDR8-EKH-IDDC-SEND                          
141800                              WDR8-EKH-IDDC-REC                           
141900     MOVE +0               TO WDR8-EKH-IDDISTR                            
142000     MOVE +0               TO WDR8-EKH-IDKUNDNR                           
142100                                                                          
142200     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
142300     MOVE W-IDARTNR        TO CIA-IDARTBET-IN                             
142400     CALL W009CIA USING       CIA-W009CIA                                 
142500     MOVE CIA-IDARTBET-UT TO WDR8-EKH-IDVERGL                             
142600                                                                          
142700     MOVE DAGENS-DATUM     TO WDR8-EKH-DAVERDAT                           
142800     MOVE ART-KDPRODSL     TO WDR8-EKH-KDPRODSL                           
142900     MOVE ZERO             TO WDR8-EKH-KDPSLLOC                           
143000     MOVE SPACE            TO WDR8-EKH-FLLSBOK                            
143100     MOVE 1.00             TO WDR8-EKH-PRKURS                             
143200     MOVE ZERO             TO WDR8-EKH-PRARTNTO                           
143300     MOVE ZERO             TO WDR8-EKH-PRARTSJK                           
143400     MOVE ZERO             TO WDR8-EKH-PRHEMTAG                           
143500     MOVE SPAR-PRAVCOST    TO WDR8-EKH-PRARTSTD                           
143600     MOVE ZERO             TO WDR8-EKH-PRLANDCO                           
143700     MOVE ZERO             TO WDR8-EKH-PRINK                              
143800     MOVE ZERO             TO WDR8-EKH-PRDIRLON                           
143900     MOVE ZERO             TO WDR8-EKH-PRDMTRL                            
144000     MOVE ZERO             TO WDR8-EKH-PROVRPAL                           
144100     MOVE ZERO             TO WDR8-EKH-SUBEL                              
144200     COMPUTE WDR8-EKH-KVANTAL = W-SPAR-KVUTRS * -1                        
144300     MOVE 'L172'           TO WDR8-EKH-IDTRANS                            
144400     MOVE ZERO             TO WDR8-EKH-BEVAT                              
144500                              WDR8-EKH-IDANALYS                           
144600                              WDR8-EKH-IDKONTO                            
144700                              WDR8-EKH-KDANMORS                           
144800                              WDR8-EKH-KDFRAKT                            
144900                              WDR8-EKH-SUVAT                              
145000     MOVE ZERO             TO WDR8-EKH-DAAVIDAT                           
145100                              WDR8-EKH-IDAVINR                            
145200                              WDR8-EKH-KDAVVTYP                           
145300                              WDR8-EKH-KDRT                               
145400                              WDR8-EKH-KVANTMOT                           
145500                              WDR8-EKH-KVAVIS                             
145600     MOVE WS-KDSORT        TO WDR8-EKH-KDSORT                             
145700     MOVE NEJ              TO WDR8-EKH-FLDCET                             
145800     MOVE SPACE            TO WDR8-EKH-IDLEVNR                            
145900                              WDR8-EKH-IDKST                              
146000     MOVE SPACE            TO WDR8-EKH-IDKUNDRF                           
146100     MOVE SPACE            TO WDR8-EKH-IDFAKT-EXP                         
146200     MOVE DCS-KDTRADP      TO WDR8-EKH-KDTRADP                            
146300     MOVE DCS-KDVALISO     TO WDR8-EKH-KDVALISO                           
146400     IF NDC-CN                                                            
146500       MOVE 'W570'         TO WDR8-FIL-IDCPYTXT(1:4)                      
146600     ELSE                                                                 
146700       IF NDC-IN                                                          
146800         MOVE 'W515'       TO WDR8-FIL-IDCPYTXT(1:4)                      
146900       ELSE                                                               
147000         MOVE DCS-KDTRADP  TO WDR8-FIL-IDCPYTXT(1:4)                      
147100       END-IF                                                             
147200     END-IF                                                               
147300     MOVE 'EKHA'           TO WDR8-FIL-IDCPYTXT(5:4)                      
147400     .                                                                    
147500 DH-SKAPA-INVENTERING  SECTION.                                           
147600     MOVE 'DH-SKAPA-INVENTERING  ' TO CURR-SECTION                        
147700*                                                                         
147800*  SKAPA INVENTERING KAT 2  PÅ WDH1                                       
147900*  UPPDATERA UTREDNINGSSALDO PÅ WDK6                                      
148000*                                                                         
148100                                                                          
148200     IF INV-ROT-FINNS = NOO                                               
148300       MOVE W-IDARTNR-X TO INV-ART-IDARTNR                                
148400       PERFORM IMS-21-INSERT-ROT-INVENTERING                              
148500     END-IF                                                               
148600                                                                          
148700     MOVE SUM-KVUTRS      TO INV-INV-KVJUSTKV                             
148800     MOVE REQU-IDDC-KEY   TO INV-INV-IDDC                                 
148900     MOVE +2              TO INV-INV-KDINVKAT                             
149000     MOVE ZERO            TO INV-INV-KDINVKAT-OLD                         
149100     MOVE SPAR-ADLAGOMR   TO INV-INV-ADLAGOMR                             
149200     MOVE SPAR-ADGANG     TO INV-INV-ADGANG                               
149300     MOVE SPAR-ADPLATS    TO INV-INV-ADPLATS                              
149400     MOVE NOO             TO INV-INV-FLINVSKR                             
149500     MOVE NOO             TO INV-INV-FLINVBEH                             
149600                                                                          
149700     MOVE NOO             TO INV-INV-FLINV2B                              
149800     MOVE NOO             TO INV-INV-FLINV3E                              
149900     IF W-IDDC = WS-CDC-11                                                
150000       MOVE JA            TO INV-INV-FLINV2D                              
150100     END-IF                                                               
150200     MOVE NOO             TO INV-INV-FLINV4N                              
150300     MOVE JA              TO INV-INV-FLINV2B                              
150400                                                                          
150500     MOVE NOO             TO INV-INV-FLINV2C                              
150600     MOVE NOO             TO INV-INV-FLINV2D                              
150700     MOVE NOO             TO INV-INV-FLINV4P                              
150800     MOVE NOO             TO INV-INV-FLINV4R                              
150900     MOVE NOO             TO INV-INV-FLINV85                              
151000     MOVE SPACE           TO INV-INV-FILLER1                              
151100                             INV-INV-FILLER2                              
151200     MOVE ART-IDFKNGRP    TO INV-INV-IDFKNGRP                             
151300     MOVE +1              TO INV-INV-KDINVPRIO                            
151400     MOVE ART-KDPRODSL    TO INV-INV-KDPRODSL                             
151500     MOVE CLAG-KDPSLLOC   TO INV-INV-KDPSLLOC                             
151600     MOVE CLAG-KDVVKL     TO INV-INV-KDVVKL                               
151700     MOVE SPACE           TO INV-INV-TEINVANM                             
151800     MOVE ZERO            TO INV-INV-IDPRTOMG                             
151900                             INV-INV-IDLOPNR                              
152000                             INV-INV-KVAKS-OLD                            
152100                             INV-INV-KVEFRS-OLD                           
152200                             INV-INV-KVLS-OLD                             
152300     MOVE DAGENS-TIAAMMDD TO WS-TIAAMMDD                                  
152400     PERFORM S03-SKAPA-WDH1DAT                                            
152500     MOVE WS-TISEGKEY2    TO INV-INV-TISEGKEY                             
152600     MOVE WS-INV-DAREGDAT TO INV-INV-DAREGDAT-CRE                         
152700                             INV-INV-DAREGDAT                             
152800**   COMPUTE INV-INV-DAREGDAT-SORT =                                      
152900**     99999999 - WS-INV-DAREGDAT                                         
153000     MOVE 99999999        TO INV-INV-DAREGDAT-SORT                        
153100     MOVE ZERO            TO INV-INV-DAREGDAT-PR1                         
153200                             INV-INV-DAREGDAT-PR2                         
153300                             INV-INV-DAREGDAT-PR3                         
153400                                                                          
153500     PERFORM IMS-11-INSERT-INVENTERING                                    
153600     IF SEGMENT-EXISTS OR INDEX-EXISTS                                    
153700       PERFORM UNTIL SEGMENT-FOUND                                        
153800         ADD +1  TO WS-TISEGKEY2                                          
153900         MOVE WS-TISEGKEY2    TO INV-INV-TISEGKEY                         
154000         PERFORM IMS-11-INSERT-INVENTERING                                
154100       END-PERFORM                                                        
154200     END-IF                                                               
154300                                                                          
154400*** INSERT PÅ WDH21 SEGMENTET ***                                         
154500     MOVE REQU-IDUSER TO INV-INVL-IDUSER                                  
154600     MOVE '0'         TO INV-INVL-KDSEGKEY                                
154700     PERFORM IMS-INSERT-WDH121                                            
154800     MOVE SPACE       TO INV-INVL-IDUSER                                  
154900     MOVE '1'         TO INV-INVL-KDSEGKEY                                
155000     PERFORM IMS-INSERT-WDH121                                            
155100     MOVE SPACE       TO INV-INVL-IDUSER                                  
155200     MOVE '2'         TO INV-INVL-KDSEGKEY                                
155300     PERFORM IMS-INSERT-WDH121                                            
155400     MOVE SPACE       TO INV-INVL-IDUSER                                  
155500     MOVE '3'         TO INV-INVL-KDSEGKEY                                
155600     PERFORM IMS-INSERT-WDH121                                            
155700                                                                          
155800**** SLUT PÅ INSERT PÅ WDH21 SEGMENT                                      
155900     PERFORM S04-UPPD-ART-MED-UTRSALDO                                    
156000                                                                          
156100     IF  DCS-LAND-NON-VCC-OWNED                                           
156200     OR (DCS-NDC-NA AND DCS-USA)                                          
156300        IF SLAG-IDDC-REF = SPACE                                          
156400          PERFORM S05-SAEND-LARM-2191-MID                                 
156500        END-IF                                                            
156600     END-IF                                                               
156700     IF W-IDDC = WS-CDC-11                                                
156800       MOVE SUM-KVUTRS TO CLAG-KVUTRS                                     
156900       MOVE JA TO WS-WDK611-UPDATE                                        
157000       PERFORM S06-SAEND-LARM-2191-MID-CDC                                
157100     ELSE                                                                 
157200       PERFORM IMS-03-GET-WDK7ART                                         
157300       MOVE SUM-KVUTRS TO SLAG-KVUTRS                                     
157400       PERFORM IMS-14-REPL-WDK7-SEGM                                      
157500     END-IF                                                               
157600*    MEDDELANDE    UTREDINGSSALDO UPPDATERAT                              
157700     MOVE '270'               TO RESP-IDMSG-INFO                          
157800     .                                                                    
157900*    --- DISPATCHER SECTIONS                                              
158000 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
158100     MOVE 'S01-FETCH-REQUEST-ARGUMENT' TO CURR-SECTION                    
158200                                                                          
158300     MOVE 'GETARG'                            TO SUB-KDFUNC               
158400     MOVE 'CARPARTS.LDC.UPDINVESTIGATIONBAL'  TO SUB-ADDISPABS            
158500     MOVE LENGTH OF REQU-AREA                 TO SUB-KVDLEN               
158600                                                                          
158700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
158800                                                                          
158900     IF SUB-KDRC > 0                                                      
159000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
159100       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
159200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
159300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
159400     END-IF                                                               
159500     .                                                                    
159600     SKIP3                                                                
159700 S02-RETURN-RESPONSE SECTION.                                             
159800     MOVE 'S02-RETURN-RESPONSE' TO CURR-SECTION                           
159900                                                                          
160000     MOVE 'RETURN'                   TO SUB-KDFUNC                        
160100     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
160200                                                                          
160300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
160400                                                                          
160500     IF SUB-KDRC > 0                                                      
160600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
160700       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
160800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
160900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
161000     END-IF                                                               
161100     .                                                                    
161200 S03-SKAPA-WDH1DAT  SECTION.                                              
161300     MOVE 'S03-SKAPA-WDH1DAT  ' TO CURR-SECTION                           
161400                                                                          
161500     MOVE WS-TIAAAAMMDD       TO WS-INV-DAREGDAT                          
161600     MOVE WS-TIAAAAMMDD(1:2)  TO WS-SEKEL                                 
161700     MOVE 0                   TO WS-LOPNR                                 
161800                                                                          
161900     MOVE WS-TIAAAAMMDDL TO WS-TISEGKEY2                                  
162000     .                                                                    
162100                                                                          
162200 S04-UPPD-ART-MED-UTRSALDO SECTION.                                       
162300     MOVE 'S04-UPPD-ART-MED-UTRSALDO' TO CURR-SECTION                     
162400                                                                          
162500*    UPPDATERAR WDG2-5116. ARTIKEL MED UTREDNINGSSALDO KNYTS              
162600*    TILL NOLLJAGARE - IDPW                                               
162700*                                                                         
162800     MOVE REQU-IDDC-KEY    TO 5116-IDDC                                   
162900     MOVE IDARTNR-WS       TO 5116-IDARTNR                                
163000     MOVE REQU-IDPW-KEY    TO 5116-IDPW                                   
163100     MOVE DAGENS-TIAAMMDD  TO 5116-TIUPPDAT                               
163200     ACCEPT 5116-TIUPPTID  FROM TIME                                      
163300                                                                          
163400     MOVE ZERO             TO 5116-IDPRODNR                               
163500                              5116-KDORDKL                                
163600                                                                          
163700     PERFORM IMS-12-INSERT-ART-UTREDNSALDO                                
163800     .                                                                    
163900 S05-SAEND-LARM-2191-MID  SECTION.                                        
164000                                                                          
164100     COMPUTE ALT2-LL = LENGTH OF ALT2-MID-W2I19101 + 17                   
164200     PERFORM IMS-GU-WDK722                                                
164300     IF SEGMENT-FOUND                                                     
164400       MOVE XLAG-IDANSK      TO ALT2-MID-IDANSK                           
164500     ELSE                                                                 
164600       MOVE ZERO             TO ALT2-MID-IDANSK                           
164700     END-IF                                                               
164800                                                                          
164900     PERFORM IMS-GU-WDE601                                                
165000     IF SEGMENT-FOUND                                                     
165100       MOVE VORD-IDDISTR     TO WS-IDDISTR-NUM4                           
165200       MOVE WS-IDDISTR-NUM4  TO ALT2-MID-IDDISTR                          
165300       MOVE VORD-IDKUNDNR    TO WS-IDKUNDNR-NUM6                          
165400       MOVE WS-IDKUNDNR-NUM6 TO ALT2-MID-IDKUNDNR                         
165500     ELSE                                                                 
165600       MOVE ZERO             TO ALT2-MID-IDDISTR                          
165700                                ALT2-MID-IDKUNDNR                         
165800     END-IF                                                               
165900                                                                          
166000     MOVE ZERO            TO ALT2-MID-KDCLAGER                            
166100     MOVE IDARTNR-WS      TO ALT2-MID-IDARTNR                             
166200     MOVE ZERO            TO ALT2-MID-TISENBEK-DAG                        
166300                             ALT2-MID-TISENBEK-KL                         
166400                             ALT2-MID-IDKUNDRF                            
166500     MOVE SPACE           TO ALT2-MID-IDKR                                
166600     MOVE 200             TO ALT2-MID-KDLARM                              
166700     MOVE 'J'             TO ALT2-MID-FLNYLARM                            
166800     MOVE SLAG-IDDC       TO ALT2-MID-IDDC                                
166900     MOVE SLAG-IDLEVNR    TO ALT2-MID-IDLEVNR                             
167000                                                                          
167100     PERFORM IMS-ISRT-ALT2191-MSG                                         
167200     .                                                                    
167300     EJECT                                                                
167400 S06-SAEND-LARM-2191-MID-CDC SECTION.                                     
167500                                                                          
167600     COMPUTE ALT2-LL = LENGTH OF ALT2-MID-W2I19101 + 17                   
167700       MOVE CLAG-IDANSK      TO ALT2-MID-IDANSK                           
167800                                                                          
167900     PERFORM IMS-GU-WDE601                                                
168000     IF SEGMENT-FOUND                                                     
168100       MOVE VORD-IDDISTR     TO WS-IDDISTR-NUM4                           
168200       MOVE WS-IDDISTR-NUM4  TO ALT2-MID-IDDISTR                          
168300       MOVE VORD-IDKUNDNR    TO WS-IDKUNDNR-NUM6                          
168400       MOVE WS-IDKUNDNR-NUM6 TO ALT2-MID-IDKUNDNR                         
168500     ELSE                                                                 
168600       MOVE ZERO             TO ALT2-MID-IDDISTR                          
168700                                ALT2-MID-IDKUNDNR                         
168800     END-IF                                                               
168900                                                                          
169000     MOVE ZERO            TO ALT2-MID-KDCLAGER                            
169100     MOVE IDARTNR-WS      TO ALT2-MID-IDARTNR                             
169200     MOVE ZERO            TO ALT2-MID-TISENBEK-DAG                        
169300                             ALT2-MID-TISENBEK-KL                         
169400                             ALT2-MID-IDKUNDRF                            
169500     MOVE SPACE           TO ALT2-MID-IDKR                                
169600     MOVE 200             TO ALT2-MID-KDLARM                              
169700     MOVE 'J'             TO ALT2-MID-FLNYLARM                            
169800     MOVE WS-CDC-11       TO ALT2-MID-IDDC                                
169900     MOVE SPACE           TO ALT2-MID-IDLEVNR                             
170000                                                                          
170100     PERFORM IMS-ISRT-ALT2191-MSG                                         
170200     .                                                                    
170300     EJECT                                                                
       S07-FIX-LOCAL-TIME SECTION.                                              
                                                                                
      ******* ADAPT DATE AND TIME FOR TIMEZONES                                 
           PERFORM IMS-GU-WDB601                                                
                                                                                
           MOVE '011'                TO MSGI-KDCALL                             
           MOVE DCS-IDTIDZON         TO MSGI-IDTIDZON                           
           MOVE DCS-IDDC             TO MSGI-IDDC                               
           MOVE DAGENS-TIAAMMDD      TO MSGI-TILOKDAT                           
           MOVE DAGENS-TID           TO MSGI-TILOKTID                           
           CALL WL01TIDZ USING          MSGI-WL01TIDZ                           
             MOVE MSGI-TILOKDAT(1:6) TO W-DATUM-Y                               
             MOVE MSGI-TILOKTID(1:4) TO AKTUELL-TID-X(1:4)                      
           .                                                                    
           EJECT                                                                
       S10-SEND-OPEN SECTION.                                                   
           MOVE 'OPEN'                        TO SEND-KDFUNC                    
           MOVE WS-ADDRESS-MQASYNC            TO SEND-ADDISPABS                 
           CALL WZ01SEND USING SEND-CONTROL-AREA                                
                               SEND-OPEN-AREA                                   
           IF SEND-KDRC > 0                                                     
             MOVE SEND-KDRC TO KDRC-DISPLAY                                     
             STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
             DELIMITED BY SIZE INTO FELTEXT                                     
             CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
           END-IF                                                               
           .                                                                    
           EJECT                                                                
       S11-SEND-PUT-PROP SECTION.                                               
                                                                                
           SET PROP-IX                 TO +1                                    
      *    MANDATORY PROPERTY THAT SPECIFIES THE ACTUAL DESTINATION             
           MOVE 'ADDRESS'              TO PROP-IDPROPTYPE  (PROP-IX)            
           MOVE 'ADDISPABS'            TO PROP-IDPROPNAME  (PROP-IX)            
           MOVE WS-ADDRESS-WHSTOCKA    TO PROP-BEPROPVALUE (PROP-IX)            
                                                                                
           SET PROP-IX              UP BY +1                                    
      *    OPTIONAL MQ MESSAGE PROPERTIES. CAN BE CASE-SENSITIVE                
           MOVE 'MQMPROP'              TO PROP-IDPROPTYPE  (PROP-IX)            
           MOVE 'CountryCode'          TO PROP-IDPROPNAME  (PROP-IX)            
           MOVE 'BR'                   TO PROP-BEPROPVALUE (PROP-IX)            
                                                                                
      *    SET THE NUMBER OF PROPERTIES (KVANTAL) SO CORRECT LENGTH             
      *    IS CALCULATED.                                                       
           SET PROP-KVANTAL            TO PROP-IX                               
                                                                                
           MOVE 'PUT'                            TO SEND-KDFUNC                 
           MOVE LENGTH OF PROP-WZ04PROP          TO SEND-KVDLEN                 
           MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
           CALL WZ01SEND USING SEND-CONTROL-AREA                                
                               SEND-KVDLEN                                      
                               PROP-WZ04PROP                                    
           IF SEND-KDRC > 1                                                     
             MOVE SEND-KDRC TO KDRC-DISPLAY                                     
             STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
             DELIMITED BY SIZE INTO FELTEXT                                     
             CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
           END-IF                                                               
           .                                                                    
           EJECT                                                                
       S12-SEND-PUT SECTION.                                                    
           MOVE 'PUT'                            TO SEND-KDFUNC                 
           MOVE LENGTH OF NOTF-AREA              TO SEND-KVDLEN                 
           MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
           CALL WZ01SEND USING SEND-CONTROL-AREA                                
                               SEND-KVDLEN                                      
                               NOTF-AREA                                        
           IF SEND-KDRC > 1                                                     
             MOVE SEND-KDRC TO KDRC-DISPLAY                                     
             STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
             DELIMITED BY SIZE INTO FELTEXT                                     
             CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
           END-IF                                                               
           .                                                                    
           EJECT                                                                
       S13-SEND-CLOSE SECTION.                                                  
           MOVE 'CLOSE'                    TO SEND-KDFUNC                       
                                                                                
           MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
           CALL WZ01SEND USING SEND-CONTROL-AREA                                
                                                                                
           IF SEND-KDRC > 0                                                     
             MOVE SEND-KDRC TO KDRC-DISPLAY                                     
             STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
             DELIMITED BY SIZE INTO FELTEXT                                     
             CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
           END-IF                                                               
           .                                                                    
           EJECT                                                                
170400 IMS-ISRT-ALT2191-MSG SECTION.                                            
170500                                                                          
170600     MOVE SPACE TO GOOD-STATUSCODES                                       
170700     CALL CBLTDLI USING ISRT ALT2191-PCB ALT2191-IO-AREA                  
170800     MOVE ALT2191-STATUS-CODE TO STATUS-WS                                
170900     PERFORM IMS-STATUS-CHECK                                             
171000     .                                                                    
171100     EJECT                                                                
171200 IMS-01-GET-ARTC-ROT SECTION.                                             
171300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
171400            DELIMITED BY SIZE INTO SSA1                                   
171500     MOVE '  GE' TO GOOD-STATUSCODES                                      
171600     CALL  CBLTDLI  USING GU WDK6-PCB DLI-IO-WDK601 SSA1                  
171700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
171800     PERFORM IMS-STATUS-CHECK                                             
171900     .                                                                    
172000 IMS-02-GET-ARTC11 SECTION.                                               
172100     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
172200            DELIMITED BY SIZE INTO SSA1                                   
172300     MOVE '  GE' TO GOOD-STATUSCODES                                      
172400     CALL  CBLTDLI  USING GHN WDK6-PCB DLI-IO-WDK611 SSA1                 
172500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
172600     PERFORM IMS-STATUS-CHECK                                             
172700     .                                                                    
172800 IMS-REPL-WDK611   SECTION.                                               
172900     MOVE '  '             TO GOOD-STATUSCODES                            
173000     CALL  CBLTDLI  USING REPL WDK6-PCB DLI-IO-WDK611                     
173100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
173200     PERFORM IMS-STATUS-CHECK                                             
173300     .                                                                    
173400 IMS-GHNP-WDK629  SECTION.                                                
173500     MOVE   'WDK629 '        TO SSA1                                      
173600     MOVE   'WDK629  '        TO SSA3                                     
173700     MOVE '  GE' TO GOOD-STATUSCODES                                      
173800     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK629 SSA1 SSA2 SSA3        
173900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
174000     PERFORM IMS-STATUS-CHECK                                             
174100     .                                                                    
174200     SKIP3                                                                
174300 IMS-REPL-WDK629 SECTION.                                                 
174400     MOVE '  ' TO GOOD-STATUSCODES                                        
174500     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK629                       
174600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
174700     PERFORM IMS-STATUS-CHECK                                             
174800     .                                                                    
174900     EJECT                                                                
175000 IMS-03-GET-WDK7ART SECTION.                                              
175100     MOVE 'IMS-03' TO CURR-IMS-SECTION                                    
175200                                                                          
175300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ') '                        
175400            DELIMITED BY SIZE INTO SSA1                                   
175500     STRING 'WDK711  (IDDC     =' W-IDDC-X ') '                           
175600            DELIMITED BY SIZE INTO SSA2                                   
175700     MOVE '  GE'           TO GOOD-STATUSCODES                            
175800     CALL CBLTDLI USING GHU WDK7-PCB WDK711 SSA1 SSA2                     
175900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
176000     PERFORM IMS-STATUS-CHECK                                             
176100     .                                                                    
176200 IMS-GU-WDK722  SECTION.                                                  
176300     MOVE 'GU-722' TO CURR-IMS-SECTION                                    
176400                                                                          
176500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ') '                        
176600            DELIMITED BY SIZE INTO SSA1                                   
176700     STRING 'WDK711  (IDDC     =' W-IDDC-X ') '                           
176800            DELIMITED BY SIZE INTO SSA2                                   
176900     MOVE 'WDK722   '      TO SSA3                                        
177000     MOVE '  GE'           TO GOOD-STATUSCODES                            
177100     CALL CBLTDLI USING GU WDK7-PCB WDK722 SSA1 SSA2 SSA3                 
177200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
177300     PERFORM IMS-STATUS-CHECK                                             
177400     .                                                                    
177500 IMS-04-GU-ARTD-SALDO    SECTION.                                         
177600     MOVE 'IMS-04' TO CURR-IMS-SECTION                                    
177700                                                                          
177800     STRING 'WLARTD01(IDARTNR  =' W-IDARTNR-X  ') '                       
177900            DELIMITED BY SIZE INTO SSA1                                   
178000     MOVE '  GE'           TO GOOD-STATUSCODES                            
178100     CALL CBLTDLI USING GU ARTD-PCB ARTD-WLARTD01 SSA1                    
178200     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
178300     PERFORM IMS-STATUS-CHECK                                             
178400     .                                                                    
178500 IMS-05-GNP-ADR-SALDO    SECTION.                                         
178600     MOVE 'IMS-05' TO CURR-IMS-SECTION                                    
178700                                                                          
178800     STRING 'WLARTD11(WDD811KY =' W-WDD811KY-X ')'                        
178900            DELIMITED BY SIZE INTO SSA1                                   
179000     MOVE '  GE'           TO GOOD-STATUSCODES                            
179100     CALL CBLTDLI USING GNP ARTD-PCB ARTD-WLARTD11 SSA1                   
179200     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
179300     PERFORM IMS-STATUS-CHECK                                             
179400     .                                                                    
179500 IMS-06-GET-ORDERRADER-SEG SECTION.                                       
179600     MOVE 'IMS-06' TO CURR-IMS-SECTION                                    
179700                                                                          
179800     STRING 'WDE411  *D(WDE4CSEQ =' W-WDE4CSEQ-X                          
179900                      '*KDRADSTA <' W-ORDSTA-X ')'                        
180000            DELIMITED BY SIZE INTO SSA1                                   
180100     MOVE 'WDE401  *D' TO SSA2                                            
180200     MOVE '  GEGB'           TO GOOD-STATUSCODES                          
180300     CALL CBLTDLI USING GN WDE4-PCB DLI-IO-E411-01 SSA1 SSA2              
180400     MOVE WDE4-STATUS-CODE   TO STATUS-WS                                 
180500     PERFORM IMS-STATUS-CHECK                                             
180600     .                                                                    
180700 IMS-07-GET-INVENT-ROT  SECTION.                                          
180800     MOVE 'IMS-07' TO CURR-IMS-SECTION                                    
180900                                                                          
181000     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ') '                        
181100            DELIMITED BY SIZE INTO SSA1                                   
181200     MOVE '  GE'           TO GOOD-STATUSCODES                            
181300     CALL CBLTDLI USING GU INVA-PCB INV-WDH101   SSA1                     
181400     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
181500     PERFORM IMS-STATUS-CHECK                                             
181600     .                                                                    
181700 IMS-08-GET-INVSEG  SECTION.                                              
181800     MOVE 'IMS-08' TO CURR-IMS-SECTION                                    
181900                                                                          
182000     MOVE   'WDH111   '  TO SSA1                                          
182100     MOVE '  GE'           TO GOOD-STATUSCODES                            
182200     CALL CBLTDLI USING GHNP INVA-PCB INV-WDH111   SSA1                   
182300     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
182400     PERFORM IMS-STATUS-CHECK                                             
182500     .                                                                    
182600 IMS-09-REPL-INVSEG  SECTION.                                             
182700     MOVE 'IMS-09' TO CURR-IMS-SECTION                                    
182800                                                                          
182900     MOVE '    '           TO GOOD-STATUSCODES                            
183000     CALL CBLTDLI USING REPL INVA-PCB INV-WDH111                          
183100     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
183200     PERFORM IMS-STATUS-CHECK                                             
183300     .                                                                    
183400 IMS-10-GET-INVENT-KAT11  SECTION.                                        
183500     MOVE 'IMS-10' TO CURR-IMS-SECTION                                    
183600                                                                          
183700     STRING 'WDH111  (WDH111KY>=' W-WDH1KEY-X-MIN                         
183800                    '&WDH111KY<=' W-WDH1KEY-X-MAX  ')'                    
183900            DELIMITED BY SIZE INTO SSA1                                   
184000     MOVE '  GE'           TO GOOD-STATUSCODES                            
184100     CALL CBLTDLI USING GNP INVA-PCB INV-WDH111   SSA1                    
184200     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
184300     PERFORM IMS-STATUS-CHECK                                             
184400     .                                                                    
184500 IMS-11-INSERT-INVENTERING SECTION.                                       
184600     MOVE 'IMS-11' TO CURR-IMS-SECTION                                    
184700                                                                          
184800     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ') '                        
184900            DELIMITED BY SIZE INTO SSA1                                   
185000     MOVE 'WDH111   ' TO SSA2                                             
185100     MOVE '  IINI'         TO GOOD-STATUSCODES                            
185200     CALL CBLTDLI USING ISRT INVA-PCB INV-WDH111   SSA1 SSA2              
185300     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
185400     PERFORM IMS-STATUS-CHECK                                             
185500     .                                                                    
185600 IMS-INSERT-WDH121      SECTION.                                          
185700                                                                          
185800     MOVE 'WDH121 ' TO SSA1                                               
185900     MOVE '  II' TO GOOD-STATUSCODES                                      
186000     CALL CBLTDLI USING ISRT INVA-PCB INV-WDH121 SSA1                     
186100     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
186200     PERFORM IMS-STATUS-CHECK                                             
186300     .                                                                    
186400     EJECT                                                                
186500 IMS-12-INSERT-ART-UTREDNSALDO SECTION.                                   
186600     MOVE 'IMS-12' TO CURR-IMS-SECTION                                    
186700                                                                          
186800     STRING 'WLXXEF01(WDGXKEY  =' W-WDGXKEY-ROT-X ')'                     
186900             DELIMITED BY SIZE INTO SSA1                                  
187000     MOVE   'WLXXEF11 ' TO SSA2                                           
187100     MOVE '  II'           TO GOOD-STATUSCODES                            
187200     CALL CBLTDLI USING ISRT XXEF-PCB WLXXEF11 SSA1 SSA2                  
187300     MOVE XXEF-STATUS-CODE TO STATUS-WS                                   
187400     PERFORM IMS-STATUS-CHECK                                             
187500     .                                                                    
187600 IMS-14-REPL-WDK7-SEGM SECTION.                                           
187700     MOVE 'IMS-14' TO CURR-IMS-SECTION                                    
187800                                                                          
187900     MOVE '  '           TO GOOD-STATUSCODES                              
188000     CALL CBLTDLI USING REPL WDK7-PCB WDK711                              
188100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
188200     PERFORM IMS-STATUS-CHECK                                             
188300     .                                                                    
188400 IMS-15-GET-WDL601      SECTION.                                          
188500     MOVE 'IMS-15' TO CURR-IMS-SECTION                                    
188600                                                                          
188700     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X  ') '                       
188800            DELIMITED BY SIZE INTO SSA1                                   
188900     MOVE '  GE'           TO GOOD-STATUSCODES                            
189000     CALL CBLTDLI USING GU INLC-PCB INLC-WLINLC01 SSA1                    
189100     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
189200     PERFORM IMS-STATUS-CHECK                                             
189300     .                                                                    
189400 IMS-16-GET-WDL611      SECTION.                                          
189500     MOVE 'IMS-16' TO CURR-IMS-SECTION                                    
189600                                                                          
189700     MOVE 'WLINLC11 '   TO SSA1                                           
189800     MOVE '  GE'           TO GOOD-STATUSCODES                            
189900     CALL CBLTDLI USING GNP INLC-PCB INLC-WLINLC11 SSA1                   
190000     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
190100     PERFORM IMS-STATUS-CHECK                                             
190200     .                                                                    
190300 IMS-17-ISRT-WDL901 SECTION.                                              
190400     MOVE 'IMS-17' TO CURR-IMS-SECTION                                    
190500                                                                          
190600     MOVE 'WLLOGA01 ' TO SSA1                                             
190700     MOVE '  II'           TO GOOD-STATUSCODES                            
190800     CALL CBLTDLI USING ISRT LOGA-PCB WLLOGA01 SSA1                       
190900     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
191000     PERFORM IMS-STATUS-CHECK                                             
191100     .                                                                    
191200 IMS-18-GET-INVHIST-ROT SECTION.                                          
191300     MOVE 'IMS-18' TO CURR-IMS-SECTION                                    
191400                                                                          
191500     STRING 'WLINVC01(IDARTNR  =' W-IDARTNR-X ') '                        
191600            DELIMITED BY SIZE INTO SSA1                                   
191700     MOVE '  GE'           TO GOOD-STATUSCODES                            
191800     CALL CBLTDLI USING GU INVC-PCB WLINVC01 SSA1                         
191900     MOVE INVC-STATUS-CODE TO STATUS-WS                                   
192000     PERFORM IMS-STATUS-CHECK                                             
192100     .                                                                    
192200 IMS-19-ISRT-INVHIST-ROT SECTION.                                         
192300     MOVE 'IMS-19' TO CURR-IMS-SECTION                                    
192400                                                                          
192500     MOVE   'WLINVC01 '  TO SSA1                                          
192600     MOVE '  '           TO GOOD-STATUSCODES                              
192700     CALL CBLTDLI USING ISRT INVC-PCB WLINVC01 SSA1                       
192800     MOVE INVC-STATUS-CODE TO STATUS-WS                                   
192900     PERFORM IMS-STATUS-CHECK                                             
193000     .                                                                    
193100 IMS-20-ISRT-INVHIST-SEGM SECTION.                                        
193200     MOVE 'IMS-20' TO CURR-IMS-SECTION                                    
193300                                                                          
193400     STRING 'WLINVC01(IDARTNR  =' W-IDARTNR-X ') '                        
193500            DELIMITED BY SIZE INTO SSA1                                   
193600     MOVE   'WLINVC11'          TO SSA2                                   
193700     MOVE '  II'                TO GOOD-STATUSCODES                       
193800     CALL CBLTDLI USING ISRT INVC-PCB WLINVC11 SSA1 SSA2                  
193900     MOVE INVC-STATUS-CODE      TO STATUS-WS                              
194000     PERFORM IMS-STATUS-CHECK                                             
194100     .                                                                    
194200 IMS-21-INSERT-ROT-INVENTERING SECTION.                                   
194300     MOVE 'IMS-21' TO CURR-IMS-SECTION                                    
194400                                                                          
194500     MOVE 'WDH101   '      TO SSA1                                        
194600     MOVE '  '             TO GOOD-STATUSCODES                            
194700     CALL CBLTDLI USING ISRT INVA-PCB INV-WDH101   SSA1                   
194800     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
194900     PERFORM IMS-STATUS-CHECK                                             
195000     .                                                                    
195100 IMS-23-ISRT-WDR901 SECTION.                                              
195200     MOVE 'IMS-23' TO CURR-IMS-SECTION                                    
195300                                                                          
195400     MOVE 'WLSAPA01 ' TO SSA1                                             
195500     MOVE '  II'             TO GOOD-STATUSCODES                          
195600     CALL CBLTDLI USING ISRT SAPA-PCB WLSAPA01 SSA1                       
195700     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
195800     PERFORM IMS-STATUS-CHECK                                             
195900     .                                                                    
196000 IMS-24-ISRT-WDR801 SECTION.                                              
196100     MOVE 'IMS-24' TO CURR-IMS-SECTION                                    
196200                                                                          
196300     MOVE 'WDR801   ' TO SSA1                                             
196400     MOVE '  II'             TO GOOD-STATUSCODES                          
196500     CALL CBLTDLI USING ISRT WDR8-PCB WDR8-WDR801 SSA1                    
196600     MOVE WDR8-STATUS-CODE TO STATUS-WS                                   
196700     PERFORM IMS-STATUS-CHECK                                             
196800     .                                                                    
196900 IMS-GU-WDB601    SECTION.                                                
197000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
197100          DELIMITED BY SIZE INTO SSA1                                     
197200     MOVE '  GE' TO GOOD-STATUSCODES                                      
197300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
197400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
197500     PERFORM IMS-STATUS-CHECK                                             
197600     .                                                                    
197700     EJECT                                                                
197800 IMS-GU-WDE601      SECTION.                                              
197900                                                                          
198000     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
198100            DELIMITED BY SIZE INTO SSA1                                   
198200     MOVE '  GE' TO GOOD-STATUSCODES                                      
198300     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E601 SSA1                      
198400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
198500     PERFORM IMS-STATUS-CHECK                                             
198600     .                                                                    
198700     SKIP2                                                                
198800 IMS-GU-WDL201 SECTION.                                                   
198900     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
199000          DELIMITED BY SIZE INTO SSA1                                     
199100     MOVE '  GE'           TO GOOD-STATUSCODES                            
199200     CALL CBLTDLI USING GU WDL2-PCB DLI-IO-WDL201 SSA1                    
199300     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
199400     PERFORM IMS-STATUS-CHECK                                             
199500     .                                                                    
199600 IMS-GNP-WDL211 SECTION.                                                  
199700     MOVE 'WDL211'         TO SSA1                                        
199800     MOVE '  GE'           TO GOOD-STATUSCODES                            
199900     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL211 SSA1                   
200000     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
200100     PERFORM IMS-STATUS-CHECK                                             
200200     .                                                                    
200300 IMS-GNP-WDL221  SECTION.                                                 
200400                                                                          
200500     STRING 'WDL211  (DAINLEV  =' W-DAINLEVNYCK-X ')'                     
200600            DELIMITED BY SIZE INTO SSA1                                   
200700     MOVE 'WDL221   ' TO SSA2                                             
200800     MOVE '  GE' TO GOOD-STATUSCODES                                      
200900     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL221 SSA1 SSA2              
201000     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
201100     PERFORM IMS-STATUS-CHECK                                             
201200     .                                                                    
201300 IMS-GNP-WDL231        SECTION.                                           
201400                                                                          
201500     STRING 'WDL211  (DAINLEV  =' W-DAINLEVNYCK-X ')'                     
201600            DELIMITED BY SIZE INTO SSA1                                   
201700     MOVE 'WDL221   ' TO SSA2                                             
201800     MOVE 'WDL231   ' TO SSA3                                             
201900     MOVE '  GE' TO GOOD-STATUSCODES                                      
202000     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL231                        
202100                          SSA1 SSA2 SSA3                                  
202200     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
202300     PERFORM IMS-STATUS-CHECK                                             
202400     .                                                                    
202500     EJECT                                                                
202600 IMS-GNP-WDL222 SECTION.                                                  
202700                                                                          
202800     STRING 'WDL211  (DAINLEV  =' W-DAINLEVNYCK-X ')'                     
202900            DELIMITED BY SIZE INTO SSA1                                   
203000     STRING 'WDL222  (IDPTYP   =' W-IDLEVNYCK-X ')'                       
203100            DELIMITED BY SIZE INTO SSA2                                   
203200     MOVE '  GE' TO GOOD-STATUSCODES                                      
203300     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL222 SSA1 SSA2              
203400     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
203500     PERFORM IMS-STATUS-CHECK                                             
203600     .                                                                    
203700     SKIP2                                                                
203800 IMS-STATUS-CHECK   SECTION.                                              
203900                                                                          
204000     SET STATUS-IX TO 1                                                   
204100     SEARCH GOOD-STATUS                                                   
204200       AT END                                                             
204300         CALL FELLOG                                                      
204400     WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                             
204500       CONTINUE                                                           
204600     END-SEARCH                                                           
204700     .                                                                    
204800*    -COPY WY2000P1                                                       
