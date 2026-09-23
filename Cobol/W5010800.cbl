000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5010800.                                                
000300 AUTHOR.         MATS VINNERFORS.                                         
000400 DATE-WRITTEN.   JUNI  81.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
000900*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0172               
001000*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
001100*                                                                         
001200*        UPPDATERAR UTREDNINGSSALDO WDK611 SAMT INVENTERINGS-             
001300*        INFORMATION WDH102 + WDGX.                                       
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR WDR9 BASEN - SAPA                          
001600*                                                                         
001700*        TRANS ÄVEN FRÅN 4325-BILDEN OCH 4397-BMP (VIA L123).             
001800*        - BEKRÄFTA FYSISKA NOLLOR KLASS 4                                
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: W5T108         FRÅN EGEN BILD                       
002200*                     W5T108X        FRÅN ANNAT PROGRAM                   
002300*        MID:         W5I10801                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        MOD:         W5O10801                                            
002700     EJECT                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP3                                                                
003000 DATA DIVISION.                                                           
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300*    -COPY WY2000W1                                                       
003400     SKIP3                                                                
003500 77  IDPGM                       PIC X(8)    VALUE 'W5010800'.            
003600 77  FILLER                      PIC X(8)    VALUE 'FELTEXT:'.            
003700 77  WS-SECTION                  PIC X(30)   VALUE SPACE.                 
003800 77  WS-IMS                      PIC X(30)   VALUE SPACE.                 
003900 77  JA                          PIC X(1)    VALUE 'J'.                   
004000 77  NEJ                         PIC X(1)    VALUE 'N'.                   
004100 77  WS-CDC-11                   PIC X(2)    VALUE '11'.                  
004200 77  IDARTNR-WS                  PIC X(9)    VALUE SPACE.                 
004300 77  W-EKH-IDARTNR               PIC X(9)    VALUE SPACE.                 
004400 77  IDPW-WS                     PIC X(8)    VALUE SPACE.                 
004500 77  WS-IDUSER                   PIC X(7)    VALUE 'NOLLJAG'.             
004600 77  WS-EGEN-BILD                PIC X(4)    VALUE '5108'.                
004700 77  WS-PASSWORD-OK              PIC X(1)    VALUE 'N'.                   
004800 77  WS-FLLO91                   PIC X(1)    VALUE 'N'.                   
004900 77  WS-WDK611-UPDATE            PIC X(1)    VALUE SPACE.                 
005000 77  WS-WDK629-UPDATE            PIC X(1)    VALUE SPACE.                 
005100 77  WS-KDSORT                   PIC X(2)    VALUE SPACE.                 
005200 77  W-SPAR-KVUTRS               PIC S9(7)   VALUE +0   COMP-3.           
005300 77  SUM-KVUTRS                  PIC S9(7)   VALUE +0   COMP-3.           
005400 77  SUM-KVUTRS-HF               PIC S9(7)   VALUE +0   COMP-3.           
005500 77  CL-INDEX                    PIC S9(9)   VALUE +0   COMP SYNC.        
005600 77  IX                          PIC S9(9)   VALUE +0   COMP SYNC.        
005700 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
005800 77  INV-BELOPP                  PIC S9(9)V99   VALUE +0   COMP-3.        
005900 77  AUT-INV                     PIC X       VALUE 'N'.                   
006000 77  SPAR-KVUTRS                 PIC S9(7)   VALUE +0   COMP-3.           
006100 77  SPAR-INVKAT                 PIC S9(3)   VALUE +0   COMP-3.           
006200 77  SPAR-KVLS                   PIC S9(7)   VALUE +0   COMP-3.           
006300 77  SPAR-ADLAGOMR               PIC S9(3)   VALUE ZERO COMP-3.           
006400 77  SPAR-ADGANG                 PIC S9(3)   VALUE ZERO COMP-3.           
006500 77  SPAR-ADPLATS                PIC S9(5)   VALUE ZERO COMP-3.           
006600 77  WS-KVANTAL                  PIC S9(7)   VALUE ZERO COMP-3.           
006700 77  WS-IDDISTR-NUM4             PIC 9(4).                                
006800 77  WS-IDKUNDNR-NUM6            PIC 9(6).                                
006900*                                                                         
007000*01 -COPY WWDCKONS                                                        
007100*                                                                         
007200 01  KOLLA-WORKDAY               PIC X       VALUE 'N'.                   
007300     88 KOLLA-WORKDAY-OK                     VALUE 'J'.                   
007400                                                                          
007700 01  WS-INLEV-DATUM-AREA.                                                 
007800     03 WS-INLEV-DATUM           PIC 9(8)   VALUE ZERO.                   
007900     03 FILLER REDEFINES WS-INLEV-DATUM.                                  
008000       05 WS-INLEV-DATUM-SEKEL   PIC 9(2).                                
008100       05 WS-INLEV-DATUM-AA      PIC 9(2).                                
008200       05 WS-INLEV-DATUM-MMDD    PIC 9(4).                                
008300     03  FILLER REDEFINES WS-INLEV-DATUM.                                 
008400       05 WS-INLEV-DATUM-AAAA    PIC 9(4).                                
008500       05 WS-INLEV-DATUM-MMDD    PIC 9(4).                                
008600                                                                          
008700 01  WS-DAGENS-DATUM-1AA         PIC 9(8)    VALUE ZERO.                  
008800 01  WS-DAGENS-DATUM-AREA.                                                
008900     03 WS-DAGENS-DATUM           PIC 9(8)   VALUE ZERO.                  
009000     03 FILLER REDEFINES WS-DAGENS-DATUM.                                 
009100       05 WS-DAGENS-DATUM-SEKEL  PIC 9(2).                                
009200       05 WS-DAGENS-DATUM-AAMMDD PIC 9(6).                                
009300     03  FILLER REDEFINES WS-DAGENS-DATUM.                                
009400       05 WS-DAGENS-DATUM-AAAA   PIC 9(4).                                
009500       05 WS-DAGENS-DATUM-MMDD   PIC 9(4).                                
009600                                                                          
009700                                                                          
009800 01  WS-INV-DAREGDAT-AREA.                                                
009900     03  WS-INV-DAREGDAT     PIC 9(9) VALUE ZERO.                         
010000     03  FILLER REDEFINES WS-INV-DAREGDAT.                                
010100       05  WS-INV-NOLL         PIC 9(1).                                  
010200       05  WS-INV-AAAAMMDD     PIC 9(8).                                  
010300                                                                          
010400 01  WS-TISEGKEYAREA.                                                     
010500     03  WS-TIAAAAMMDDL      PIC 9(9) VALUE ZERO.                         
010600     03  FILLER REDEFINES WS-TIAAAAMMDDL.                                 
010700         05  WS-SEKEL        PIC 9(2).                                    
010800         05  WS-TIAAMMDD     PIC 9(6).                                    
010900         05  WS-LOPNR        PIC 9(1).                                    
011000     03  WS-TISEGKEY         PIC S9(9)  VALUE ZERO COMP-3.                
011100     03  WS-TISEGKEY2        PIC S9(9)  VALUE ZERO COMP-3.                
011200                                                                          
011300                                                                          
011400 01  DAGENS-TIAAMMDD             PIC S9(6).                               
011500 01  DAGENS-DATUM                PIC S9(8).                               
011600 01  WS-TIAAAAMMDD               PIC S9(8).                               
011700 01  TRANS-TID                   PIC 9(9).                                
011800     EJECT                                                                
011900 01  WS-GENERELLA-SUBPROGRAM.                                             
012000     03    WSECURIT              PIC X(8)    VALUE 'WSECURIT'.            
012100     03    WDATKONV              PIC X(8)    VALUE 'WDATKONV'.            
012200     03    WORKDAY               PIC X(8)    VALUE 'WORKDAY '.            
012300     03    CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.            
012400     03    FELLOG                PIC X(8)    VALUE 'FELLOG  '.            
012500     03    W005INIT              PIC X(8)    VALUE 'W005INIT'.            
012600     03    W009CIA               PIC X(8)    VALUE 'W009CIA '.            
012700     EJECT                                                                
012800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
012900*01 -COPY WMSGINIT                                                        
013000     EJECT                                                                
013100*    --- PARAMETRAR TILL SUBPROGRAM W009CIA                               
013200*01 -COPY W009CIA                                                         
013300                                                                          
013400     EJECT                                                                
013500 01  FILLER                      PIC X(10)   VALUE 'DLINYCKLAR'.          
013600 01  NYCKLAR-TILL-DLI.                                                    
013700*                                                                         
013800     03  W-IDARTNR-X.                                                     
013900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.          
014000*                                                                         
014100     03  W-WDH1KEY-X-MIN.                                                 
014200         05  W-IDDC-WDH1-MIN     PIC X(2)    VALUE SPACE.                 
014300         05  W-KDINVKAT-MIN      PIC S9(3)   VALUE +11   COMP-3.          
014400         05  W-TISEGKEY-MIN      PIC S9(9)   VALUE ZERO  COMP-3.          
014500         05  W-DAREGDAT-SORT-MIN     PIC 9(8)  VALUE ZERO.                
014600*                                                                         
014700     03  W-WDH1KEY-X-MAX.                                                 
014800         05  W-IDDC-WDH1-MAX     PIC X(2)    VALUE SPACE.                 
014900         05  W-KDINVKAT-MAX      PIC S9(3)   VALUE +11   COMP-3.          
015000         05  W-TISEGKEY-MAX      PIC S9(9)   VALUE ZERO  COMP-3.          
015100         05  W-DAREGDAT-SORT-MAX     PIC 9(8)  VALUE ZERO.                
015200*                                                                         
015300     03  W-INVNYCK-X.                                                     
015400         05  W-IDHTYP            PIC X(4).                                
015500         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
015600         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
015700*                                                                         
015800     03  W-KDSEGKEY-X.                                                    
015900         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
016000*                                                                         
016100     03  W-IDDC-X.                                                        
016200         05  W-IDDC-WDK7         PIC X(2)    VALUE SPACE.                 
016300*                                                                         
016400     03  W-WDD811KY-X.                                                    
016500         05  W-IDDC-WDD8         PIC X(2)    VALUE SPACE.                 
016600         05  W-ADBUFFOMR         PIC S9(3)   VALUE +1    COMP-3.          
016700         05  W-DABUFPAF          PIC  9(8)   VALUE ZERO.                  
016800         05  W-ADBUFFGANG        PIC S9(3)   VALUE +0    COMP-3.          
016900         05  W-ADBUFFPL          PIC S9(5)   VALUE +0    COMP-3.          
017000*                                                                         
017100     03  W-WDE4CSEQ-X.                                                    
017200         05  W-IDARTNR2          PIC S9(9)   VALUE ZERO  COMP-3.          
017300*                                                                         
017400     03  W-IDPRODNR-X.                                                    
017500         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO  COMP-3.          
017600*                                                                         
017700     03  W-IDLEVNYCK-X.                                                   
017800         05  W-IDPTYP            PIC X(3)    VALUE 'R34'.                 
017900*                                                                         
018000     03  W-DAINLEVNYCK-X.                                                 
018100         05  W-DAINLEV           PIC  9(16).                              
018200*                                                                         
018300     03  W-ORDSTA-X.                                                      
018400         05  W-ORDSTA            PIC S9      COMP-3  VALUE +4.            
018500*                                                                         
018600     03  W-WDGXKEY-ROT-X.                                                 
018700         05  FILLER              PIC X(4)    VALUE '5115'.                
018800         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
018900*                                                                         
019000     03  W-IDDC-B6-X.                                                     
019100         05 W-IDDC-B6                  PIC X(2).                          
019200*                                                                         
019300     EJECT                                                                
019400 01  FILLER                      PIC X(9)    VALUE 'FELTEXTER'.           
019500 01  W-FEL-1.                                                             
019600     03  FEL-1-SVE               PIC X(10)   VALUE 'FEL NYCKEL'.          
019700     03  FEL-1-ENG               PIC X(10)   VALUE 'WRONG KEY '.          
019800 01  FILLER REDEFINES W-FEL-1.                                            
019900     03  FEL-1 OCCURS 2          PIC X(10).                               
020000     SKIP2                                                                
020100 01  W-FEL-2.                                                             
020200     03  FEL-2-SVE               PIC X(38)   VALUE 'ARTIKEL-NR SAK        
020300-        'NAS PÅ ARTIKEL-REGISTRET'.                                      
020400     03  FEL-2-ENG               PIC X(38)   VALUE 'THIS ARTICLE I        
020500-        'S NOT IN THE DATABASE   '.                                      
020600 01  FILLER REDEFINES W-FEL-2.                                            
020700     03  FEL-2 OCCURS 2          PIC X(38).                               
020800     SKIP2                                                                
020900 01  W-FEL-3.                                                             
021000     03  FEL-3-SVE               PIC X(40)   VALUE 'UTREDNINGSSALD        
021100-        'OT ÄR REDAN UPPDATERAT    '.                                    
021200     03  FEL-3-ENG               PIC X(40)   VALUE 'THE INVESTIGAT        
021300-        'ION BAL IS ALREADY UPDATED'.                                    
021400 01  FILLER REDEFINES W-FEL-3.                                            
021500     03  FEL-3 OCCURS 2          PIC X(40).                               
021600     SKIP2                                                                
021700 01  W-FEL-4.                                                             
021800     03  FEL-4-SVE               PIC X(40)   VALUE 'UPPDATERING EJ        
021900-        ' TILLÅTEN FRÅN ANNAN BILD '.                                    
022000     03  FEL-4-ENG               PIC X(40)   VALUE 'UPDATING NOT A        
022100-        'LLOWED FROM OTHER PICTURE '.                                    
022200 01  FILLER REDEFINES W-FEL-4.                                            
022300     03  FEL-4 OCCURS 2          PIC X(40).                               
022400     EJECT                                                                
022500 01  W-FEL-6.                                                             
022600     03  FEL-6-SVE               PIC X(40)   VALUE 'DÅLIGT OBJEKT         
022700-        'FÅR EJ UPPDATERAS'.                                             
022800     03  FEL-6-ENG               PIC X(40)   VALUE 'BAD CORE WILL         
022900-        'NOT BE UPDATED'.                                                
023000 01  FILLER REDEFINES W-FEL-6.                                            
023100     03  FEL-6 OCCURS 2          PIC X(40).                               
023200     SKIP2                                                                
023300 01  W-FEL-7.                                                             
023400     03  FEL-7-SVE               PIC X(40)   VALUE 'EJ AUKTORISERA        
023500-        'D ANVÄNDARE'.                                                   
023600     03  FEL-7-ENG               PIC X(40)   VALUE 'USER NOT AUTHO        
023700-        'RIZED'.                                                         
023800 01  FILLER REDEFINES W-FEL-7.                                            
023900     03  FEL-7 OCCURS 2          PIC X(40).                               
024000     EJECT                                                                
024100 01  W-FEL-8.                                                             
024200     03  FEL-8-SVE               PIC X(40)   VALUE 'EJ AUKTORISERA        
024300-        'D ANVÄNDARE,LAGEROMRÅDE 91'.                                    
024400     03  FEL-8-ENG               PIC X(40)   VALUE 'USER NOT AUTHO        
024500-        'RIZED, AREA 91'.                                                
024600 01  FILLER REDEFINES W-FEL-8.                                            
024700     03  FEL-8 OCCURS 2          PIC X(40).                               
024800     EJECT                                                                
024900 01  BUFFERT-FEL-MEDDELANDE.                                              
025000     03 BUFFERT-FEL-SV.                                                   
025100       05  FILLER                PIC X(16)                                
025200       VALUE 'FINNS I BUFFERT'.                                           
025300       05  ADBUFFOMR-1-SV        PIC Z(2).                                
025400       05  FILLER                PIC X       VALUE  SPACE.                
025500       05  ADBUFFOMR-2-SV        PIC Z(2).                                
025600       05  FILLER                PIC X       VALUE  SPACE.                
025700       05  ADBUFFOMR-3-SV        PIC Z(2).                                
025800       05  FILLER                PIC X(16)   VALUE  SPACE.                
025900     03 BUFFERT-FEL-BG.                                                   
026000       05  FILLER                PIC X(16)                                
026100       VALUE 'FOUND IN BUFFER'.                                           
026200       05  ADBUFFOMR-1-BG        PIC Z(2).                                
026300       05  FILLER                PIC X       VALUE  SPACE.                
026400       05  ADBUFFOMR-2-BG        PIC Z(2).                                
026500       05  FILLER                PIC X       VALUE  SPACE.                
026600       05  ADBUFFOMR-3-BG        PIC Z(2).                                
026700       05  FILLER                PIC X(16)   VALUE  SPACE.                
026800 01  FEL-MEDDELANDE-BUFFERT REDEFINES BUFFERT-FEL-MEDDELANDE.             
026900     03  FEL-MEDD-BUFFERT OCCURS 2.                                       
027000       05  FILLER                PIC X(16).                               
027100       05  ADBUFFOMR-1           PIC Z(2).                                
027200       05  FILLER                PIC X.                                   
027300       05  ADBUFFOMR-2           PIC Z(2).                                
027400       05  FILLER                PIC X.                                   
027500       05  ADBUFFOMR-3           PIC Z(2).                                
027600       05  FILLER                PIC X(16).                               
027700       EJECT                                                              
027800 01  W-MEDDELANDE.                                                        
027900     03  MEDDELANDE-SVE          PIC X(36)   VALUE 'UTREDNINGSSALD        
028000-        'O UPPDATERAT          '.                                        
028100     03  MEDDELANDE-ENG          PIC X(36)   VALUE 'THE INVESTIGAT        
028200-        'ION BALANCE IS UPDATED'.                                        
028300 01  FILLER REDEFINES W-MEDDELANDE.                                       
028400     03  MEDDELANDE OCCURS 2     PIC X(36).                               
028500                                                                          
028600 01  W-MEDDELANDE-1.                                                      
028700     03  MEDDELANDE-SVE-1        PIC X(36)   VALUE 'BORTJUSTERING         
028800-        'REDAN GJORD'.                                                   
028900     03  MEDDELANDE-ENG-1        PIC X(36)   VALUE 'ADJUSTMENT AL         
029000-        'READY DONE'.                                                    
029100 01  FILLER REDEFINES W-MEDDELANDE-1.                                     
029200     03  MEDDELANDE-1 OCCURS 2   PIC X(36).                               
029300                                                                          
029400 01  W-MEDDELANDE-2.                                                      
029500     03  MEDDELANDE-SVE-2        PIC X(36)   VALUE 'SALDO JUSTERAT        
029600-        ' - KAT 08'.                                                     
029700     03  MEDDELANDE-ENG-2        PIC X(36)   VALUE 'BALANCE IS ADJ        
029800-        'USTED - CAT 08'.                                                
029900 01  FILLER REDEFINES W-MEDDELANDE-2.                                     
030000     03  MEDDELANDE-2 OCCURS 2   PIC X(36).                               
030100                                                                          
030200     EJECT                                                                
030300 01  W-MEDDELANDE-4.                                                      
030400     03  MEDDELANDE-SVE-4        PIC X(61)                                
030500     VALUE 'INGEN UPPDATERING UTFÖRD - UTREDNINGSSALDO = 0'.              
030600     03  MEDDELANDE-ENG-4        PIC X(61)                                
030700     VALUE 'NO UPDATING OCCURRED - INVESTIG. BALANCES = 0'.               
030800 01  FILLER REDEFINES W-MEDDELANDE-4.                                     
030900     03  MEDDELANDE-4 OCCURS 2   PIC X(61).                               
031000 01  W-MEDDELANDE-5.                                                      
031100     03  MEDDELANDE-SVE-5        PIC X(61)                                
031200     VALUE 'INGEN UPPDATERING UTFÖRD - UTREDNINGSSALDO NEGATIVT'.         
031300     03  MEDDELANDE-ENG-5        PIC X(61)                                
031400     VALUE 'NO UPDATING OCCURRED - INVESTIG. BALANCES NEGATIVE'.          
031500 01  FILLER REDEFINES W-MEDDELANDE-5.                                     
031600     03  MEDDELANDE-5 OCCURS 2   PIC X(61).                               
031700     EJECT                                                                
031800 01  FILLER                      PIC X(8)    VALUE 'WDATKONV'.            
031900*01   -COPY  WDATAREA                                                     
032000     EJECT                                                                
032100 01  FILLER                      PIC X(8)    VALUE 'WORKAREA'.            
032200*01   -COPY  WORKAREA                                                     
032300     EJECT                                                                
032400*01   -COPY  WSECAREA                                                     
032500     EJECT                                                                
032600 01  FILLER                      PIC X(7)    VALUE 'DIVERSE'.             
032700 01  DIVERSE.                                                             
032800     03  INLEV-KOLL              PIC X       VALUE 'N'.                   
032900         88  INLEV-OK                        VALUE 'J'.                   
033000     03  INVJUST-KOLL            PIC X       VALUE 'N'.                   
033100         88  INVJUST-OK                      VALUE 'J'.                   
033200     03  HOEGLAG-KOLL            PIC X       VALUE 'N'.                   
033300         88  HOEGLAG-OK                      VALUE 'J'.                   
033400     03  INVROT-FINNS            PIC X       VALUE 'J'.                   
033500     03  INVJUST-DATUM           PIC S9(5)   COMP-3.                      
033600     03  INLEV-DATUM             PIC S9(7)   COMP-3  VALUE +0.            
033700     03  HOEGLAG-SALDO           PIC S9(9)   COMP-3.                      
033800     03  INV-ROT-FINNS           PIC X       VALUE 'J'.                   
033900     03  INV-FINNS               PIC X       VALUE 'N'.                   
034000     03  KOMMENTAR.                                                       
034100       05  FILLER                PIC X(17).                               
034200       05  KOMM-11               PIC X(6).                                
034300       05  FILLER                PIC X.                                   
034400       05  KOMM-SKR              PIC X.                                   
034500     EJECT                                                                
034600* - - - - - - - - - - - - -  BYTES-OBJEKTTEST                             
034700*                                                                         
034800 01  TEST-IDARTNR                PIC 9(9)   COMP-3.                       
034900*01  FILLER -COPY WWBYT09      -RED TEST-IDARTNR                          
035000     EJECT                                                                
035100*                                                               *         
035200*                AREOR FÖR MFS OCH SKÄRMHANTERING               *         
035300*                                                               *         
035400                                                                          
035500 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
035600                                                                          
035700*01  MID -COPY W5I10801 -PRE MID-                                         
035800     EJECT                                                                
035900*01  -COPY WMSGAREA                                                       
036000     EJECT                                                                
036100*    03  MOD -COPY W5O10801 -PRE MOD- -RED MSG-AREA                       
036200     EJECT                                                                
036300*01  -COPY WMFSAREA                                                       
036400     EJECT                                                                
036500*****************************************************************         
036600*                                                                         
036700*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
036800*                                                                         
036900 01  IMS-WS.                                                              
037000     03  FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
037100     SKIP3                                                                
037200*                        **** STATUS-KOD FRÅN IMS                         
037300     03  STATUS-WS               PIC X(2).                                
037400         88  SEGMENT-FINNS                   VALUE '  '.                  
037500         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
037600         88  SEGMENT-FINNS-REDAN             VALUE 'II'.                  
037700         88  INDEX-FINNS-REDAN               VALUE 'NI'.                  
037800                                                                          
037900     03  GODK-STATUSKODER.                                                
038000         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
038100     SKIP3                                                                
038200 01    SSA1                      PIC X(96).                               
038300 01    SSA2                      PIC X(96).                               
038400 01    SSA3                      PIC X(96).                               
038500     EJECT                                                                
038600*                            IMS FUNKTIONSKODER                           
038700*01  -COPY W0003                                                          
038800     EJECT                                                                
038900                                                                          
039000 01  FILLER                      PIC X(16)   VALUE 'WDH101'.              
039100*                                                                         
039200*01  WDH101 -COPY WDH101 -PRE INV-                                        
039300     EJECT                                                                
039400                                                                          
039500 01  FILLER                      PIC X(16)   VALUE 'WDH111'.              
039600*                                                                         
039700*01  WDH111 -COPY WDH111 -PRE INV-                                        
039800     EJECT                                                                
039900                                                                          
040000 01  FILLER                      PIC X(16)   VALUE 'WDH121'.              
040100*                                                                         
040200*01  WDH121 -COPY WDH121 -PRE INV-                                        
040300     EJECT                                                                
040400                                                                          
040500 01  FILLER                   PIC X(16) VALUE 'DLI-IO-E411-01'.           
040600*                                                                         
040700 01  DLI-IO-E411-01.                                                      
040800*    03  -COPY WDE411 -PRE RAD-                                           
040900     EJECT                                                                
041000*    03  -COPY WDE401 -PRE HUV-                                           
041100     EJECT                                                                
041200                                                                          
041300 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK601'.           
041400 01  DLI-IO-WDK601.                                                       
041500*  03  -COPY WDK601.                                                      
041600                                                                          
041700 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK611'.           
041800 01  DLI-IO-WDK611.                                                       
041900*  03  -COPY WDK611.                                                      
042000                                                                          
042100 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK629'.           
042200 01  DLI-IO-WDK629.                                                       
042300*    03  -COPY WDK629                                                     
042400                                                                          
042500 01  FILLER                      PIC X(16)   VALUE 'WLINLE01'.            
042600*                                                                         
042700*01  WLINLE01 -COPY WDL201 -PRE INL-                                      
042800     EJECT                                                                
042900                                                                          
043000 01  FILLER                      PIC X(16)   VALUE 'WLINLE11'.            
043100*                                                                         
043200*01  WLINLE11 -COPY WDL211 -PRE INL-                                      
043300     EJECT                                                                
043400                                                                          
043500 01  FILLER                      PIC X(16)   VALUE 'WLINLE21'.            
043600*                                                                         
043700*01  WLINLE21 -COPY WDL221 -PRE INL-                                      
043800     EJECT                                                                
043900                                                                          
044000 01  FILLER                      PIC X(16)   VALUE 'WLINLE31'.            
044100*                                                                         
044200*01  WLINLE31 -COPY WDL231 -PRE INL-                                      
044300     EJECT                                                                
044400                                                                          
044500 01  FILLER                      PIC X(16)   VALUE 'WLINLE22'.            
044600*                                                                         
044700*01  WLINLE22 -COPY WDL222 -PRE INL-                                      
044800     EJECT                                                                
044900                                                                          
045000 01  FILLER                      PIC X(16)   VALUE 'WLARTD01'.            
045100*                                                                         
045200*01  WLARTD01 -COPY WDD801 -PRE ARTD-                                     
045300     EJECT                                                                
045400                                                                          
045500 01  FILLER                      PIC X(16)   VALUE 'WLARTD11'.            
045600*                                                                         
045700*01  WLARTD11 -COPY WDD811 -PRE ARTD-                                     
045800     EJECT                                                                
045900                                                                          
046000 01  FILLER                      PIC X(16)   VALUE 'WLINLC01'.            
046100*                                                                         
046200*01  WLINLC01 -COPY WDL601 -PRE INLC-                                     
046300     EJECT                                                                
046400                                                                          
046500 01  FILLER                      PIC X(16)   VALUE 'WLINLC11'.            
046600*                                                                         
046700*01  WLINLC11 -COPY WDL611 -PRE INLC-                                     
046800     EJECT                                                                
046900                                                                          
047000 01  FILLER                      PIC X(16)   VALUE 'WLXXEF11'.            
047100*                                                                         
047200*01  WLXXEF11 -COPY WDGX5116                                              
047300     EJECT                                                                
047400                                                                          
047500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E601'.         
047600 01  DLI-IO-E601.                                                         
047700*  03 -COPY WDE601                                                        
047800     EJECT                                                                
047900                                                                          
048000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK711'.         
048100*                                                                         
048200 01  DLI-IO-WDK711.                                                       
048300*    03  -COPY WDK711                                                     
048400     EJECT                                                                
048500                                                                          
048600 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK722'.         
048700 01  DLI-IO-WDK722.                                                       
048800*    03  -COPY WDK722                                                     
048900     EJECT                                                                
049000                                                                          
049100 01  FILLER                      PIC X(16)   VALUE 'WLINVC01'.            
049200*                                                                         
049300*01  WLINVC01 -COPY WDH701                                                
049400     EJECT                                                                
049500                                                                          
049600 01  FILLER                      PIC X(16)   VALUE 'WLINVC01'.            
049700*                                                                         
049800*01  WLINVC11 -COPY WDH711                                                
049900     EJECT                                                                
050000*                                                                         
050100 01  FILLER                   PIC X(16) VALUE 'ALT2191-IO-AREA'.          
050200 01  ALT2191-IO-AREA.                                                     
050300  03     ALT2-LL                 PIC S9(4) COMP SYNC.                     
050400  03     ALT2-Z1                 PIC X(1)  VALUE LOW-VALUE.               
050500  03     ALT2-Z2                 PIC X(1)  VALUE LOW-VALUE.               
050600  03     ALT2-TRANSKOD           PIC X(8)  VALUE 'W2T191X '.              
050700  03     ALT2-IDTRANS            PIC X(4)  VALUE '5108'.                  
050800  03     ALT2-SPRAK              PIC X(1).                                
050900     SKIP2                                                                
051000* 03     MID -COPY W2I19101   -PRE ALT2-                                  
051100     EJECT                                                                
051200 01  FILLER                      PIC X(16) VALUE 'WLLOGA01'.              
051300*01  WLLOGA01    -COPY WDL901                                             
051400     EJECT                                                                
051500 01  FILLER                      PIC X(16) VALUE 'WLSAPA01'.              
051600*01  WLSAPA01    -COPY WDR901                                             
051700*    05 -COPY W510EKHA -RED FIL-WDR901-DATA                               
051800                                                                          
051900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
052000 01   DLI-IO-AREA-B601.                                                   
052100*     03  -COPY WDB601                                                    
052200                                                                          
052300     EJECT                                                                
052400                                                                          
052500 LINKAGE SECTION.                                                         
052600*01  -COPY W0009    -PRE MSG-                                             
052700                                                                          
052800*01  -COPY W0009    -PRE ALT2191-                                         
052900                                                                          
053000     EJECT                                                                
053100*01  -COPY W0008    -PRE USEA-                                            
053200         05  FILLER              PIC X(1).                                
053300                                                                          
053400*01  -COPY W0008    -PRE INVA-                                            
053500         05  FILLER              PIC X(1).                                
053600     EJECT                                                                
053700*01  -COPY W0008    -PRE WDK6-                                            
053800         05  FILLER              PIC X(1).                                
053900                                                                          
054000*01  -COPY W0008    -PRE WDE4-                                            
054100         05  FILLER              PIC X(1).                                
054200     EJECT                                                                
054300*01  -COPY W0008    -PRE INLE-                                            
054400         05  FILLER              PIC X(1).                                
054500                                                                          
054600*01  -COPY W0008    -PRE ARTD-                                            
054700         05  FILLER              PIC X(1).                                
054800     EJECT                                                                
054900*01  -COPY W0008    -PRE XXEF-                                            
055000         05  FILLER              PIC X(1).                                
055100                                                                          
055200*01  -COPY W0008    -PRE WDE6-                                            
055300         05  FILLER              PIC X(1).                                
055400     EJECT                                                                
055500*01  -COPY W0008    -PRE WDK7-                                            
055600         05  FILLER              PIC X(1).                                
055700                                                                          
055800*01  -COPY W0008    -PRE INLC-                                            
055900         05  FILLER              PIC X(1).                                
056000     EJECT                                                                
056100*01  -COPY W0008    -PRE INVC-                                            
056200         05  FILLER              PIC X(1).                                
056300                                                                          
056400     EJECT                                                                
056500*01  -COPY W0008    -PRE LOGA-                                            
056600         05  FILLER              PIC X(1).                                
056700                                                                          
056800     EJECT                                                                
056900*01  -COPY W0008    -PRE SAPA-                                            
057000         05  FILLER              PIC X(1).                                
057100                                                                          
057200     EJECT                                                                
057300*01  -COPY W0008    -PRE WDB6-                                            
057400         05  FILLER              PIC X(1).                                
057500                                                                          
057600     EJECT                                                                
057700 PROCEDURE DIVISION USING MSG-PCB ALT2191-PCB USEA-PCB                    
057800                          INVA-PCB WDK6-PCB WDE4-PCB INLE-PCB             
057900                          ARTD-PCB XXEF-PCB                               
058000                          WDE6-PCB WDK7-PCB INLC-PCB INVC-PCB             
058100                          LOGA-PCB SAPA-PCB WDB6-PCB.                     
058200 MAIN SECTION.                                                            
058300     ENTRY 'DLITCBL' USING MSG-PCB ALT2191-PCB USEA-PCB                   
058400                          INVA-PCB WDK6-PCB WDE4-PCB INLE-PCB             
058500                          ARTD-PCB XXEF-PCB                               
058600                          WDE6-PCB WDK7-PCB INLC-PCB INVC-PCB             
058700                          LOGA-PCB SAPA-PCB WDB6-PCB.                     
058800     PERFORM IMS-GET-MSG                                                  
058900     IF SEGMENT-FINNS                                                     
059000       PERFORM AA-INIT-SPARA-INPUT                                        
059100       IF IDARTNR-WS NOT NUMERIC                                          
059200             OR IDPW-WS = SPACE                                           
059300             OR DCS-KDDC = SPACE                                          
059400             OR DCS-CDC-TR                                                
059500         MOVE FEL-1 (CL-INDEX) TO MOD-TEMFSFEL                            
059600       ELSE                                                               
059700         IF MFS-UPD-X  OR MFS-IDTRANS = '5108'                            
059800           PERFORM S01-KOLLA-TILLAATEN-PASSWORD                           
059900                                                                          
060000           IF WS-PASSWORD-OK = JA AND WS-FLLO91 = NEJ                     
060100             PERFORM AB-UPPDATERA-BASER                                   
060200           ELSE                                                           
060300             IF WS-PASSWORD-OK = NEJ                                      
060400               MOVE FEL-7 (CL-INDEX) TO MOD-TEMFSFEL                      
060500             ELSE                                                         
060600               MOVE FEL-8 (CL-INDEX) TO MOD-TEMFSFEL                      
060700             END-IF                                                       
060800           END-IF                                                         
060900         ELSE                                                             
061000           MOVE FEL-4 (CL-INDEX) TO MOD-TEMFSFEL                          
061100         END-IF                                                           
061200       END-IF                                                             
061300                                                                          
061400       IF NOT MFS-UPD-X                                                   
061500         COMPUTE MSG-KVLL = LENGTH OF MOD-W5O10801 + 4                    
061600         PERFORM IMS-INSERT-MSG                                           
061700       END-IF                                                             
061800     END-IF                                                               
061900     MOVE ZERO TO RETURN-CODE                                             
062000     GOBACK                                                               
062100     .                                                                    
062200     EJECT                                                                
062300 AA-INIT-SPARA-INPUT SECTION.                                             
062400     MOVE 'AA-INIT            ' TO WS-SECTION                             
062500     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-TIAAAAMMDD                    
062600                                         WS-INV-AAAAMMDD                  
062700     ACCEPT DAGENS-TIAAMMDD FROM DATE                                     
062800     IF MSG-DUBBLA-TRANSKODER                                             
062900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I10801                 
063000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
063100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
063200     ELSE                                                                 
063300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I10801                  
063400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
063500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
063600     END-IF                                                               
063700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
063800     MOVE MSG-IDPFK TO MFS-IDPFK                                          
063900                                                                          
064000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
064100     MOVE '013'             TO MSGI-KDCALL                                
064200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
064300     MOVE '5108'            TO MSGI-IDTRANS                               
064400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
064500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
064600                                                                          
064700     IF MID-IDARTNR-IN = ALL '+'                                          
064800       MOVE MID-IDARTNR-UT TO IDARTNR-WS                                  
064900       INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO                 
065000     ELSE                                                                 
065100       MOVE MID-IDARTNR-IN TO IDARTNR-WS                                  
065200     END-IF                                                               
065300                                                                          
065400     IF MID-IDPW-IN = ALL '+'                                             
065500       IF MFS-IDTRANS = '5108'                                            
065600         MOVE MID-IDPW-UT TO IDPW-WS                                      
065700       ELSE                                                               
065800         MOVE SPACE TO IDPW-WS                                            
065900       END-IF                                                             
066000     ELSE                                                                 
066100       MOVE MID-IDPW-IN TO IDPW-WS                                        
066200     END-IF                                                               
066300                                                                          
066310     MOVE MSGI-IDDC              TO W-IDDC-B6                             
066400     IF MFS-UPD-X                                                         
066401       IF MID-IDDC-IN = ALL '+' OR SPACES                                 
066402         IF MID-IDDC-UT = ALL '+' OR SPACES                               
066403           CONTINUE                                                       
066404         ELSE                                                             
066405           MOVE MID-IDDC-UT      TO MSGI-IDDC                             
066406                                    W-IDDC-B6                             
066407         END-IF                                                           
066408       ELSE                                                               
066409         MOVE MID-IDDC-IN        TO MSGI-IDDC                             
066410                                    W-IDDC-B6                             
066411       END-IF                                                             
066420     END-IF                                                               
066600     PERFORM IMS-GU-WDB601                                                
066700                                                                          
066800                                                                          
066900     MOVE LOW-VALUE TO MSG-AREA                                           
067000     MOVE 'W5O108N1' TO MFS-IDMOD                                         
067100     MOVE '5108' TO MOD-IDTRANS                                           
067200     MOVE  '1'    TO ALT2-SPRAK                                           
067300                                                                          
067400     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
067500     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
067600     MOVE IDPW-WS TO MOD-IDPW-UT                                          
067700     INSPECT MID-IDPW-UT REPLACING ALL '+' BY SPACE                       
067800     MOVE MSGI-IDDC TO MOD-IDDC-UT                                        
067900     INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                  
068000                                                                          
068100     IF MSGI-IDLAND-SPR = 'GB'                                            
068200       MOVE 2 TO CL-INDEX                                                 
068300     ELSE                                                                 
068400       MOVE 1 TO CL-INDEX                                                 
068500     END-IF                                                               
068600     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN MOD-TEMFSFEL                  
068700                             MOD-IDPW-IN    MOD-TEMFSINF                  
068800                             MOD-IDDC-IN                                  
068900     .                                                                    
069000     EJECT                                                                
069100 AB-UPPDATERA-BASER SECTION.                                              
069200*                                                                         
069300*  BEHANDLING AV INRAPPORTERADE ARTIKLAR                                  
069400*  TVÅ FALL KAN FÖREKOMMA:                                                
069500*      1  EN JUSTERING GÖRS DIREKT                                        
069600*      2  EN INVENTERING KATEGORI  2 SKAPAS                               
069700     MOVE 'AB-UPPDAT          ' TO WS-SECTION                             
069800     MOVE IDARTNR-WS TO W-IDARTNR                                         
069900                        W-IDARTNR2                                        
070000                        TEST-IDARTNR                                      
070100     MOVE MSGI-IDDC  TO W-IDDC                                            
070200                        W-IDDC-WDK7                                       
070300                        W-IDDC-WDD8                                       
070400     IF NOT BYT09-OBJEKT                                                  
070500       PERFORM IMS-GU-WDK601                                              
070600       IF SEGMENT-FINNS                                                   
070700       MOVE ART-KDSORT TO WS-KDSORT                                       
070800         PERFORM QB-HAEMTA-CDC-INFO                                       
070900         IF NOT DCS-CDC                                                   
071000           PERFORM QA-HAEMTA-SDC-INFO                                     
071100         END-IF                                                           
071200         IF SEGMENT-FINNS                                                 
071300           IF SPAR-KVUTRS = ZERO                                          
071400             PERFORM AE-KOLLA-HOEGLAGER                                   
071500             IF HOEGLAG-OK OR (NOT DCS-CDC)                               
071600               PERFORM IMS-GET-ORDERRADER-SEG                             
071700               PERFORM UNTIL SEGMENT-SAKNAS                               
071800                 IF HUV-KORD-IDDC = MSGI-IDDC                             
071900                   IF RAD-ORAD-IDLEVNR = SPACE OR                         
072000                      RAD-ORAD-FLDIRLEV = NEJ                             
072100                     ADD RAD-ORAD-KVAVBART      TO   SUM-KVUTRS           
072200                     SUBTRACT RAD-ORAD-KVLEVART FROM SUM-KVUTRS           
072300                   END-IF                                                 
072400                 END-IF                                                   
072500                 PERFORM IMS-GET-ORDERRADER-SEG                           
072600               END-PERFORM                                                
072700               IF SUM-KVUTRS < +1                                         
072800                 IF SUM-KVUTRS = 0                                        
072900                   MOVE MEDDELANDE-4(CL-INDEX) TO                         
073000                        MOD-TEMFSINF                                      
073100                 ELSE                                                     
073200                   MOVE MEDDELANDE-5(CL-INDEX) TO                         
073300                        MOD-TEMFSINF                                      
073400                 END-IF                                                   
073500               ELSE                                                       
073600                 PERFORM AI-KOLLA-INVENTERING                             
073700                 IF INV-FINNS = JA                                        
073800                   PERFORM AJ-INVENTERING-FINNS                           
073900                 ELSE                                                     
074000                   COMPUTE INV-BELOPP =                                   
074100                           SUM-KVUTRS * CLAG-PRARTSTD                     
074200                                                                          
074300                   IF INV-BELOPP > DCS-KVINVAUT                           
074400                      MOVE NEJ    TO AUT-INV                              
074500                   ELSE                                                   
074600                      MOVE JA     TO AUT-INV                              
074700                   END-IF                                                 
074800                                                                          
074900                   IF AUT-INV = JA                                        
075000                     IF DCS-CDC                                           
075100                       PERFORM AC-KOLLA-INLEV                             
075200                     ELSE                                                 
075300                       PERFORM AQ-KOLLA-INLEV-SDC                         
075400                     END-IF                                               
075500                     IF INLEV-OK                                          
075600                       PERFORM AF-SKAPA-JUSTERING                         
075700                     ELSE                                                 
075800                       PERFORM AH-SKAPA-INVENTERING                       
075900                     END-IF                                               
076000                   ELSE                                                   
076100                     PERFORM AH-SKAPA-INVENTERING                         
076200                   END-IF                                                 
076300                 END-IF                                                   
076400               END-IF                                                     
076500               IF WS-WDK611-UPDATE = JA                                   
076600                 PERFORM IMS-REPL-WDK611                                  
076700                 PERFORM S06-FLYTTA-LOGG-WDK6                             
076800                 PERFORM S08-UPPDATERA-LOGG                               
076900               END-IF                                                     
077000               IF WS-WDK629-UPDATE = JA                                   
077100                 PERFORM IMS-GHNP-WDK629                                  
077200                 IF SEGMENT-FINNS                                         
077300                   IF CREF-FLREFNYO = JA                                  
077400                     MOVE NEJ         TO CREF-FLREFNYO                    
077500                     PERFORM IMS-REPL-WDK629                              
077600                   END-IF                                                 
077700                 END-IF                                                   
077800               END-IF                                                     
077900             ELSE                                                         
078000               MOVE FEL-MEDD-BUFFERT(CL-INDEX) TO MOD-TEMFSFEL            
078100             END-IF                                                       
078200           ELSE                                                           
078300             MOVE FEL-3 (CL-INDEX) TO MOD-TEMFSFEL                        
078400           END-IF                                                         
078500         ELSE                                                             
078600           MOVE FEL-2 (CL-INDEX) TO MOD-TEMFSFEL                          
078700         END-IF                                                           
078800       ELSE                                                               
078900         MOVE FEL-2 (CL-INDEX) TO MOD-TEMFSFEL                            
079000       END-IF                                                             
079100     ELSE                                                                 
079200       MOVE FEL-6 (CL-INDEX) TO MOD-TEMFSFEL                              
079300     END-IF                                                               
079400     .                                                                    
079500     EJECT                                                                
079600 AC-KOLLA-INLEV   SECTION.                                                
079700                                                                          
079800*  KONTROLLERA OM INLEV. HAR KOMMIT SENASTE MÅNADEN.                      
079900     MOVE 'AC-KOLLA           ' TO WS-SECTION                             
080000     MOVE NEJ TO INLEV-KOLL                                               
080100     MOVE +0 TO INLEV-DATUM                                               
080200                                                                          
080300     PERFORM IMS-GET-INLEVROT                                             
080400                                                                          
080500     IF SEGMENT-SAKNAS                                                    
080600       MOVE JA TO INLEV-KOLL                                              
080700     ELSE                                                                 
080800       PERFORM IMS-GET-INLEVNR                                            
080900       PERFORM UNTIL SEGMENT-SAKNAS OR INLEV-DATUM NOT = +0               
081000          MOVE INL-INL-DAINLEV TO W-DAINLEV                               
081100          PERFORM IMS-GET-INLEVTRANS3X                                    
081200          PERFORM UNTIL SEGMENT-SAKNAS OR INLEV-DATUM NOT = +0            
081300             IF INL-MOT-IDDC = WS-CDC-11                                  
081400                IF INL-MOT-KDRT NOT = 77 AND 88 AND 99                    
081500                   IF INL-MOT-KVAVIS > +0                                 
081600                      IF INL-MOT-KVAVIS NOT = INL-MOT-KVFORDEL            
081700                         IF INL-MOT-IDPTYP = 'R31' OR '310'               
081800                            PERFORM IMS-GET-P32TRANS                      
081900                            IF SEGMENT-FINNS                              
082000                               MOVE INL-DEL-TIREGDAT TO                   
082100                                    INLEV-DATUM                           
082200                            END-IF                                        
082300                         ELSE                                             
082400                            IF INL-MOT-IDPTYP = 'R32'                     
082500                               MOVE INL-MOT-TIUPPDAT TO                   
082600                                    INLEV-DATUM                           
082700                            END-IF                                        
082800                         END-IF                                           
082900                      END-IF                                              
083000                   END-IF                                                 
083100                END-IF                                                    
083200             END-IF                                                       
083300             IF INLEV-DATUM = +0                                          
083400                PERFORM IMS-GET-INLEVTRANS3X                              
083500             END-IF                                                       
083600          END-PERFORM                                                     
083700          PERFORM IMS-GET-INLEVTRANSR34                                   
083800          PERFORM UNTIL SEGMENT-SAKNAS OR                                 
083900                        INL-DIR-IDDC = WS-CDC-11                          
084000             PERFORM IMS-GET-INLEVTRANSR34                                
084100          END-PERFORM                                                     
084200          IF SEGMENT-FINNS                                                
084300             MOVE INL-DIR-TIAVSDAT   TO TMP1-YYMMDD                       
084400             MOVE INLEV-DATUM        TO TMP2-YYMMDD                       
084500             PERFORM WY2000P1                                             
084600             IF TMP1-YYMMDD > TMP2-YYMMDD                                 
084700                MOVE INL-DIR-TIAVSDAT TO INLEV-DATUM                      
084800             END-IF                                                       
084900          END-IF                                                          
085000          IF INLEV-DATUM = +0                                             
085100             PERFORM IMS-GET-INLEVNR                                      
085200          END-IF                                                          
085300       END-PERFORM                                                        
085400     END-IF                                                               
085500                                                                          
085600     IF INLEV-DATUM NOT = +0                                              
085700***  OM INLEVERANS SKETT FÖR HÖGST 10 DAGAR SEDAN FÖR SDC OCH             
085800***                      FÖR HÖGST 10 DAGAR SEDAN FÖR CDC                 
085900***  SKER INGEN AUTOMATJUSTERING.                                         
086000***  ÄNDRAT 2000-10-27  , NY DAGSGRÄNSER ENL. BERIT JEBSEN                
086100       MOVE INLEV-DATUM        TO WS-INLEV-DATUM                          
086200       MOVE DAGENS-TIAAMMDD    TO WS-DAGENS-DATUM                         
086300                                                                          
086400       IF WS-INLEV-DATUM-AA > 50                                          
086500         MOVE 19               TO WS-INLEV-DATUM-SEKEL                    
086600       ELSE                                                               
086700         MOVE 20               TO WS-INLEV-DATUM-SEKEL                    
086800       END-IF                                                             
086900********************************************************                  
087000*** POSTER ÄLDRE ÄN ETT ÅR BEHÖVS INTE KOLLAS MED WORKDAY                 
087100********************************************************                  
087200       MOVE 20                 TO WS-DAGENS-DATUM-SEKEL                   
087300       COMPUTE WS-DAGENS-DATUM-1AA = WS-DAGENS-DATUM - 10000              
087400       IF WS-DAGENS-DATUM-1AA <= WS-INLEV-DATUM                           
087500         MOVE JA               TO KOLLA-WORKDAY                           
087600       ELSE                                                               
087700         MOVE NEJ              TO KOLLA-WORKDAY                           
087800         MOVE JA               TO INLEV-KOLL                              
087900       END-IF                                                             
088000       IF KOLLA-WORKDAY-OK                                                
088100         MOVE 001                TO WORK-KDCALL                           
088200         MOVE WS-CDC-11          TO WORK-IDDC                             
088300         MOVE INLEV-DATUM        TO WORK-TIAAMMDD-FOM                     
088400         MOVE DAGENS-TIAAMMDD    TO WORK-TIAAMMDD-TOM                     
088500         CALL WORKDAY USING         WORK-KDCALL                           
088600                                    WORK-DATE-AREA                        
088700                                    WORK-KDSVAR                           
088800         IF WORK-KDSVAR-OK                                                
088900           IF NOT DCS-CDC                                                 
089000             IF WORK-KVWORKD > 10                                         
089100               MOVE JA           TO INLEV-KOLL                            
089200             END-IF                                                       
089300           ELSE                                                           
089400             IF WORK-KVWORKD > 10                                         
089500               MOVE JA           TO INLEV-KOLL                            
089600             END-IF                                                       
089700           END-IF                                                         
089800         END-IF                                                           
089900       END-IF                                                             
090000     ELSE                                                                 
090100        MOVE JA TO INLEV-KOLL                                             
090200     END-IF                                                               
090300     .                                                                    
090400     EJECT                                                                
090500 AE-KOLLA-HOEGLAGER  SECTION.                                             
090600*                                                                         
090700*  KONTROLLERA OM HÖGLAGERSALDO F + OF = 0                                
090800*                                                                         
090900     MOVE 'AE-KOLLA           ' TO WS-SECTION                             
091000     MOVE JA   TO HOEGLAG-KOLL                                            
091100     MOVE ZERO TO HOEGLAG-SALDO                                           
091200     MOVE ZERO TO ADBUFFOMR-1(CL-INDEX)                                   
091300                  ADBUFFOMR-2(CL-INDEX)                                   
091400                  ADBUFFOMR-3(CL-INDEX)                                   
091500                                                                          
091600     PERFORM IMS-GU-ARTD-SALDO                                            
091700     IF SEGMENT-FINNS                                                     
091800        MOVE +1 TO IX                                                     
091900                   INDX                                                   
092000        PERFORM IMS-GNP-ADR-SALDO                                         
092100        PERFORM UNTIL SEGMENT-SAKNAS OR INDX = +4                         
092200           ADD ARTD-SALDO-KVBUFF-F  TO HOEGLAG-SALDO                      
092300           ADD ARTD-SALDO-KVBUFF-OF TO HOEGLAG-SALDO                      
092400           IF HOEGLAG-SALDO > +0                                          
092500              IF IX = +1                                                  
092600                 MOVE ARTD-SALDO-ADBUFFOMR TO                             
092700                      ADBUFFOMR-1 (CL-INDEX)                              
092800              ELSE                                                        
092900                 IF IX = +2                                               
093000                    MOVE ARTD-SALDO-ADBUFFOMR TO                          
093100                         ADBUFFOMR-2 (CL-INDEX)                           
093200                 ELSE                                                     
093300                    IF IX = +3                                            
093400                       MOVE ARTD-SALDO-ADBUFFOMR TO                       
093500                            ADBUFFOMR-3 (CL-INDEX)                        
093600                    END-IF                                                
093700                 END-IF                                                   
093800              END-IF                                                      
093900              ADD +1 TO IX                                                
094000              MOVE NEJ TO HOEGLAG-KOLL                                    
094100              MOVE ZERO TO HOEGLAG-SALDO                                  
094200           END-IF                                                         
094300           ADD +1 TO INDX                                                 
094400           PERFORM IMS-GNP-ADR-SALDO                                      
094500        END-PERFORM                                                       
094600     END-IF                                                               
094700     .                                                                    
094800     EJECT                                                                
094900 AF-SKAPA-JUSTERING  SECTION.                                             
095000*                                                                         
095100*  SKAPA EN JUSTERING DIREKT                                              
095200*  SKAPA INVENTERING KAT 08 PÅ WDH1                                       
095300*  SKAPA EN ÅTERFÖRING TILL W111                                          
095400*                                                                         
095500     MOVE 'AF-SKAPA           ' TO WS-SECTION                             
095600     IF DCS-CDC                                                           
095700       SUBTRACT SUM-KVUTRS FROM CLAG-KVLS                                 
095800       COMPUTE WS-KVANTAL =                                               
095900        CLAG-KVLS + CLAG-KVEFRS + CLAG-KVAKS-CDC                          
096000       MOVE '-'             TO LOGG-IDTECKEN-KVLS                         
096100       MOVE SUM-KVUTRS      TO W-SPAR-KVUTRS                              
096200       MOVE JA TO WS-WDK611-UPDATE                                        
096300       PERFORM S03A-SAEND-LARM-2191-MID-CDC                               
096400                                                                          
096500       MOVE 'AAMMDD'        TO DAT-KDDATFORM                              
096600       MOVE DAGENS-TIAAMMDD TO DAT-I-TIDATUM                              
096700       CALL WDATKONV USING     DAT-KDDATFORM                              
096800                               DAT-I-TIDATUM                              
096900                               DAT-O-TIDATUM                              
097000                               DAT-KDSVAR                                 
097100                                                                          
097200       IF DAT-KDSVAR-FEL                                                  
097300         CALL FELLOG                                                      
097400       END-IF                                                             
097500                                                                          
097600       MOVE DAT-TIAAMMDD TO WS-TIAAMMDD                                   
097700                                                                          
097800       MULTIPLY -1 BY SUM-KVUTRS                                          
097900                                                                          
098000       MOVE DAT-TIAAVVD   TO CLAG-TIINVDAT                                
098100       MOVE SUM-KVUTRS    TO CLAG-KVINVS                                  
098200                                                                          
098300       MOVE JA TO WS-WDK611-UPDATE                                        
098400                                                                          
098500**** OM DET ÄR EN REFILLARTIKEL SÅ SKALL FLAGGA SÄTTAS TILL N             
098600       MOVE JA TO WS-WDK629-UPDATE                                        
098700**** REPLACE GÖRS SENARE I AB-SEKTIONEN EFTER K611 UPPDATERINGEN!!        
098800     ELSE                                                                 
098900       SUBTRACT SUM-KVUTRS FROM SLAG-KVLS                                 
099000       MOVE '-'             TO LOGG-IDTECKEN-KVLS                         
099100       MOVE SUM-KVUTRS      TO W-SPAR-KVUTRS                              
099200                                                                          
099300       MOVE 'AAMMDD'        TO DAT-KDDATFORM                              
099400       MOVE DAGENS-TIAAMMDD TO DAT-I-TIDATUM                              
099500       CALL WDATKONV USING     DAT-KDDATFORM                              
099600                               DAT-I-TIDATUM                              
099700                               DAT-O-TIDATUM                              
099800                               DAT-KDSVAR                                 
099900                                                                          
100000       IF DAT-KDSVAR-FEL                                                  
100100         CALL FELLOG                                                      
100200       END-IF                                                             
100300                                                                          
100400       MOVE DAT-TIAAMMDD TO WS-TIAAMMDD                                   
100500                                                                          
100600       MULTIPLY -1 BY SUM-KVUTRS                                          
100700                                                                          
100800       MOVE DAT-TIAAVVD   TO SLAG-TIINVDAT                                
100900       MOVE SUM-KVUTRS    TO SLAG-KVINVS                                  
101000       PERFORM IMS-REPL-WDK7-SEGM                                         
101100       PERFORM S07-FLYTTA-LOGG-WDK7                                       
101200       PERFORM S08-UPPDATERA-LOGG                                         
101300       PERFORM S03B-SAEND-LARM-2191-MID-NDC                               
101400                                                                          
101500                                                                          
101600     END-IF                                                               
101700                                                                          
101800     PERFORM AFA-ISRT-INVENTERINGSHISTORIK                                
101900                                                                          
102000     IF INV-ROT-FINNS = NEJ                                               
102100       MOVE W-IDARTNR-X TO INV-ART-IDARTNR                                
102200       PERFORM IMS-INSERT-ROT-INVENTERING                                 
102300     END-IF                                                               
102400                                                                          
102500     MOVE SUM-KVUTRS      TO INV-INV-KVJUSTKV                             
102600                                                                          
102700     MOVE MSGI-IDDC       TO INV-INV-IDDC                                 
102800     MOVE +8              TO INV-INV-KDINVKAT                             
102900     MOVE ZERO            TO INV-INV-KDINVKAT-OLD                         
103000     MOVE SPAR-ADLAGOMR   TO INV-INV-ADLAGOMR                             
103100     MOVE SPAR-ADGANG     TO INV-INV-ADGANG                               
103200     MOVE SPAR-ADPLATS    TO INV-INV-ADPLATS                              
103300     MOVE NEJ             TO INV-INV-FLINVSKR                             
103400     MOVE JA              TO INV-INV-FLINVBEH                             
103500                                                                          
103600     MOVE NEJ             TO INV-INV-FLINV2B                              
103700     MOVE NEJ             TO INV-INV-FLINV2D                              
103800     MOVE NEJ             TO INV-INV-FLINV3E                              
103900     MOVE NEJ             TO INV-INV-FLINV4N                              
104000     MOVE NEJ             TO INV-INV-FLINV4R                              
104100     IF DCS-CDC                                                           
104200       MOVE JA            TO INV-INV-FLINV2D                              
104300       MOVE JA            TO INV-INV-FLINV3E                              
104400     ELSE                                                                 
104500       IF DCS-NDC-NA                                                      
104600         MOVE JA          TO INV-INV-FLINV4N                              
104700         MOVE JA          TO INV-INV-FLINV4R                              
104800       ELSE                                                               
104900         MOVE JA          TO INV-INV-FLINV2B                              
105000         MOVE JA          TO INV-INV-FLINV2D                              
105100       END-IF                                                             
105200     END-IF                                                               
105300                                                                          
105400     MOVE NEJ             TO INV-INV-FLINV2C                              
105500     MOVE NEJ             TO INV-INV-FLINV4P                              
105600     MOVE NEJ             TO INV-INV-FLINV85                              
105700     MOVE SPACE           TO INV-INV-FILLER1                              
105800                             INV-INV-FILLER2                              
105900     MOVE ART-IDFKNGRP    TO INV-INV-IDFKNGRP                             
106000     MOVE +1              TO INV-INV-KDINVPRIO                            
106100     MOVE ART-KDPRODSL    TO INV-INV-KDPRODSL                             
106200     MOVE CLAG-KDPSLLOC   TO INV-INV-KDPSLLOC                             
106300     MOVE CLAG-KDVVKL     TO INV-INV-KDVVKL                               
106400     PERFORM S05-SKAPA-WDH1DAT                                            
106500     MOVE WS-TISEGKEY2    TO INV-INV-TISEGKEY                             
106600     MOVE SPACE           TO INV-INV-TEINVANM                             
106700     MOVE ZERO            TO INV-INV-IDPRTOMG                             
106800                             INV-INV-IDLOPNR                              
106900                             INV-INV-KVAKS-OLD                            
107000                             INV-INV-KVEFRS-OLD                           
107100                             INV-INV-KVLS-OLD                             
107200     MOVE WS-INV-DAREGDAT TO INV-INV-DAREGDAT                             
107300     MOVE WS-INV-DAREGDAT TO INV-INV-DAREGDAT-CRE                         
107400**   COMPUTE INV-INV-DAREGDAT-SORT =                                      
107500**     99999999 - WS-INV-DAREGDAT                                         
107600     MOVE 99999999        TO  INV-INV-DAREGDAT-SORT                       
107700     MOVE ZERO            TO INV-INV-DAREGDAT-PR1                         
107800                             INV-INV-DAREGDAT-PR2                         
107900                             INV-INV-DAREGDAT-PR3                         
108000*    MOVE SPACE           TO INV-INL-IDUSER-PR1                           
108100*                            INV-INL-IDUSER-PR2                           
108200*                            INV-INL-IDUSER-PR3                           
108300*                            INV-INL-IDUSER-CRE                           
108400                                                                          
108500     PERFORM IMS-INSERT-INVENTERING                                       
108600     IF SEGMENT-FINNS-REDAN OR INDEX-FINNS-REDAN                          
108700       PERFORM UNTIL SEGMENT-FINNS                                        
108800         ADD +1  TO WS-TISEGKEY2                                          
108900         MOVE WS-TISEGKEY2    TO INV-INV-TISEGKEY                         
109000         PERFORM IMS-INSERT-INVENTERING                                   
109100       END-PERFORM                                                        
109200     END-IF                                                               
109300                                                                          
109400*** INSERT PÅ WDH121 SEGMENTET ***                                        
109500     MOVE 'ISRT-WDH121 AF-SEC ' TO WS-SECTION                             
109600     MOVE MSGI-IDUSER TO INV-INVL-IDUSER                                  
109700     MOVE '0'         TO INV-INVL-KDSEGKEY                                
109800     PERFORM IMS-INSERT-WDH121                                            
109900     MOVE SPACE       TO INV-INVL-IDUSER                                  
110000     MOVE '1'         TO INV-INVL-KDSEGKEY                                
110100     PERFORM IMS-INSERT-WDH121                                            
110200     MOVE SPACE       TO INV-INVL-IDUSER                                  
110300     MOVE '2'         TO INV-INVL-KDSEGKEY                                
110400     PERFORM IMS-INSERT-WDH121                                            
110500     MOVE SPACE       TO INV-INVL-IDUSER                                  
110600     MOVE '3'         TO INV-INVL-KDSEGKEY                                
110700     PERFORM IMS-INSERT-WDH121                                            
110800                                                                          
110900**** SLUT PÅ INSERT PÅ WDH121 SEGMENT                                     
111000                                                                          
111100                                                                          
111200     IF SUM-KVUTRS NOT = 0                                                
111300       PERFORM S09-UPPDATERA-WDR9                                         
111400       PERFORM IMS-ISRT-WDR901                                            
111500       PERFORM UNTIL SEGMENT-FINNS                                        
111600         ADD +1  TO FIL-IDSEKVNR                                          
111700         PERFORM IMS-ISRT-WDR901                                          
111800       END-PERFORM                                                        
111900     END-IF                                                               
112000     PERFORM S02-UPPD-ART-MED-UTRSALDO                                    
112100                                                                          
112200     MOVE MEDDELANDE-2 (CL-INDEX) TO MOD-TEMFSINF                         
112300     .                                                                    
112400     EJECT                                                                
112500 AFA-ISRT-INVENTERINGSHISTORIK SECTION.                                   
112600     MOVE 'AFA-ISRT           ' TO WS-SECTION                             
112700     PERFORM IMS-GET-INVHIST-ROT                                          
112800     IF SEGMENT-SAKNAS                                                    
112900       MOVE W-IDARTNR TO INVA-IDARTNR                                     
113000       PERFORM IMS-ISRT-INVHIST-ROT                                       
113100     END-IF                                                               
113200                                                                          
113300     PERFORM S04-SKAPA-TISEGKEY                                           
113400                                                                          
113500     MOVE MSGI-IDDC           TO INVH-IDDC                                
113600     MOVE WS-TIAAAAMMDD       TO INVH-DAREGDAT-CRE                        
113700                                 INVH-DAREGDAT-CLO                        
113800     MOVE SUM-KVUTRS          TO INVH-KVJUSTKV                            
113900     MOVE 8                   TO INVH-KDJUSTYP                            
114000     IF IDPW-WS = SPACE                                                   
114100       MOVE MSG-SIGNON-USERID TO INVH-IDUSER-CLO                          
114200       MOVE MSG-SIGNON-USERID TO INVH-IDUSER-CRE                          
114300     ELSE                                                                 
114400       MOVE IDPW-WS           TO INVH-IDUSER-CLO                          
114500       MOVE IDPW-WS           TO INVH-IDUSER-CRE                          
114600     END-IF                                                               
114700     MOVE NEJ                 TO INVH-FLAUTLSJ                            
114800     MOVE SPACE               TO INVH-IDPW                                
114900     MOVE CLAG-PRARTSTD       TO INVH-PRARTSTD                            
115000     MOVE ZERO                TO INVH-DAREGDAT-PR1                        
115100                                 INVH-DAREGDAT-PR2                        
115200                                 INVH-DAREGDAT-PR3                        
115300     MOVE SPACE               TO INVH-IDUSER-PR1                          
115400                                 INVH-IDUSER-PR2                          
115500                                 INVH-IDUSER-PR3                          
115600     IF DCS-CDC                                                           
115700       MOVE WS-KVANTAL        TO INVH-KVANTAL                             
115800     ELSE                                                                 
115900       MOVE +0                TO INVH-KVANTAL                             
116000     END-IF                                                               
116100                                                                          
116200     PERFORM IMS-ISRT-INVHIST-SEGM                                        
116300                                                                          
116400     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
116500       IF SEGMENT-FINNS-REDAN                                             
116600         SUBTRACT 1 FROM INVH-TISEGKEY                                    
116700         PERFORM IMS-ISRT-INVHIST-SEGM                                    
116800       END-IF                                                             
116900     END-PERFORM                                                          
117000     .                                                                    
117100     EJECT                                                                
117200                                                                          
117300 AH-SKAPA-INVENTERING  SECTION.                                           
117400*                                                                         
117500*  SKAPA INVENTERING KAT 2  PÅ WDH1                                       
117600*  UPPDATERA UTREDNINGSSALDO PÅ WDK6                                      
117700*                                                                         
117800     MOVE 'AH-SKAPA           ' TO WS-SECTION                             
117900     PERFORM IMS-GET-INVENT-ROT                                           
118000     IF INV-ROT-FINNS = NEJ                                               
118100       MOVE W-IDARTNR-X TO INV-ART-IDARTNR                                
118200       PERFORM IMS-INSERT-ROT-INVENTERING                                 
118300     END-IF                                                               
118400                                                                          
118500     MOVE SUM-KVUTRS      TO INV-INV-KVJUSTKV                             
118600     MOVE MSGI-IDDC       TO INV-INV-IDDC                                 
118700     MOVE +2              TO INV-INV-KDINVKAT                             
118800     MOVE ZERO            TO INV-INV-KDINVKAT-OLD                         
118900     MOVE SPAR-ADLAGOMR   TO INV-INV-ADLAGOMR                             
119000     MOVE SPAR-ADGANG     TO INV-INV-ADGANG                               
119100     MOVE SPAR-ADPLATS    TO INV-INV-ADPLATS                              
119200     MOVE NEJ             TO INV-INV-FLINVSKR                             
119300     MOVE NEJ             TO INV-INV-FLINVBEH                             
119400                                                                          
119500     MOVE NEJ             TO INV-INV-FLINV2B                              
119600     MOVE NEJ             TO INV-INV-FLINV3E                              
119700     MOVE NEJ             TO INV-INV-FLINV4N                              
119800     IF DCS-CDC                                                           
119900       MOVE JA            TO INV-INV-FLINV3E                              
120000     ELSE                                                                 
120100       IF DCS-NDC-NA                                                      
120200         MOVE JA          TO INV-INV-FLINV4N                              
120300       ELSE                                                               
120400         MOVE JA          TO INV-INV-FLINV2B                              
120500       END-IF                                                             
120600     END-IF                                                               
120700                                                                          
120800     MOVE NEJ             TO INV-INV-FLINV2C                              
120900     MOVE NEJ             TO INV-INV-FLINV2D                              
121000     MOVE NEJ             TO INV-INV-FLINV4P                              
121100     MOVE NEJ             TO INV-INV-FLINV4R                              
121200     MOVE NEJ             TO INV-INV-FLINV85                              
121300     MOVE SPACE           TO INV-INV-FILLER1                              
121400                             INV-INV-FILLER2                              
121500     MOVE ART-IDFKNGRP    TO INV-INV-IDFKNGRP                             
121600     MOVE +1              TO INV-INV-KDINVPRIO                            
121700     MOVE ART-KDPRODSL    TO INV-INV-KDPRODSL                             
121800     MOVE CLAG-KDPSLLOC   TO INV-INV-KDPSLLOC                             
121900     MOVE CLAG-KDVVKL     TO INV-INV-KDVVKL                               
122000     MOVE SPACE           TO INV-INV-TEINVANM                             
122100     MOVE ZERO            TO INV-INV-IDPRTOMG                             
122200                             INV-INV-IDLOPNR                              
122300                             INV-INV-KVAKS-OLD                            
122400                             INV-INV-KVEFRS-OLD                           
122500                             INV-INV-KVLS-OLD                             
122600     MOVE DAGENS-TIAAMMDD TO WS-TIAAMMDD                                  
122700     PERFORM S05-SKAPA-WDH1DAT                                            
122800     MOVE WS-TISEGKEY2    TO INV-INV-TISEGKEY                             
122900     MOVE WS-INV-DAREGDAT TO INV-INV-DAREGDAT                             
123000***  COMPUTE INV-INV-DAREGDAT-SORT =                                      
123100***    99999999 - WS-INV-DAREGDAT                                         
123200     MOVE 99999999        TO  INV-INV-DAREGDAT-SORT                       
123300     MOVE WS-INV-DAREGDAT TO INV-INV-DAREGDAT-CRE                         
123400     MOVE ZERO            TO INV-INV-DAREGDAT-PR1                         
123500                             INV-INV-DAREGDAT-PR2                         
123600                             INV-INV-DAREGDAT-PR3                         
123700*    MOVE SPACE           TO INV-INV-IDUSER-PR1                           
123800*                            INV-INV-IDUSER-PR2                           
123900*                            INV-INV-IDUSER-PR3                           
124000                                                                          
124100     PERFORM IMS-INSERT-INVENTERING                                       
124200     IF SEGMENT-FINNS-REDAN OR INDEX-FINNS-REDAN                          
124300       PERFORM UNTIL SEGMENT-FINNS                                        
124400         ADD +1  TO WS-TISEGKEY2                                          
124500         MOVE WS-TISEGKEY2    TO INV-INV-TISEGKEY                         
124600         PERFORM IMS-INSERT-INVENTERING                                   
124700       END-PERFORM                                                        
124800     END-IF                                                               
124900*** INSERT PÅ WDH121 SEGMENTET ***                                        
125000     MOVE 'ISRT WDH121 AH-SEC ' TO WS-SECTION                             
125100     MOVE MSGI-IDUSER TO INV-INVL-IDUSER                                  
125200     MOVE '0'         TO INV-INVL-KDSEGKEY                                
125300     PERFORM IMS-INSERT-WDH121                                            
125400     MOVE SPACE       TO INV-INVL-IDUSER                                  
125500     MOVE '1'         TO INV-INVL-KDSEGKEY                                
125600     PERFORM IMS-INSERT-WDH121                                            
125700     MOVE SPACE       TO INV-INVL-IDUSER                                  
125800     MOVE '2'         TO INV-INVL-KDSEGKEY                                
125900     PERFORM IMS-INSERT-WDH121                                            
126000     MOVE SPACE       TO INV-INVL-IDUSER                                  
126100     MOVE '3'         TO INV-INVL-KDSEGKEY                                
126200     PERFORM IMS-INSERT-WDH121                                            
126300                                                                          
126400**** SLUT PÅ INSERT PÅ WDH121 SEGMENT                                     
126500                                                                          
126600     PERFORM S02-UPPD-ART-MED-UTRSALDO                                    
126700                                                                          
126800     IF DCS-CDC                                                           
126900       MOVE SUM-KVUTRS TO CLAG-KVUTRS                                     
127000       MOVE JA TO WS-WDK611-UPDATE                                        
127100       PERFORM S03A-SAEND-LARM-2191-MID-CDC                               
127200     ELSE                                                                 
127300       PERFORM IMS-GET-WDK7ART                                            
127400       MOVE SUM-KVUTRS TO SLAG-KVUTRS                                     
127500       PERFORM IMS-REPL-WDK7-SEGM                                         
127600       PERFORM S03B-SAEND-LARM-2191-MID-NDC                               
127700     END-IF                                                               
127800                                                                          
127900     MOVE MEDDELANDE (CL-INDEX) TO MOD-TEMFSINF                           
128000     .                                                                    
128100     EJECT                                                                
128200 AI-KOLLA-INVENTERING  SECTION.                                           
128300*  KONTROLLERA OM INVENTERING MED KAT = 6 EJ FINNS PÅ WDH1                
128400     MOVE 'AI-KOLLA           ' TO WS-SECTION                             
128500     MOVE NEJ TO INV-FINNS                                                
128600     PERFORM IMS-GET-INVENT-ROT                                           
128700                                                                          
128800     IF SEGMENT-SAKNAS                                                    
128900       MOVE NEJ TO INV-ROT-FINNS                                          
129000     ELSE                                                                 
129100       PERFORM IMS-GET-INVSEG                                             
129200       PERFORM UNTIL SEGMENT-SAKNAS OR INV-FINNS = JA                     
129300         IF INV-INV-KDINVKAT NOT = +6                                     
129400          IF INV-INV-IDDC = MSGI-IDDC AND INV-INV-FLINVBEH = NEJ          
129500            MOVE JA TO INV-FINNS                                          
129600          ELSE                                                            
129700            PERFORM IMS-GET-INVSEG                                        
129800          END-IF                                                          
129900         ELSE                                                             
130000           PERFORM IMS-GET-INVSEG                                         
130100         END-IF                                                           
130200       END-PERFORM                                                        
130300     END-IF                                                               
130400     .                                                                    
130500     EJECT                                                                
130600 AJ-INVENTERING-FINNS  SECTION.                                           
130700*                                                                         
130800*  BEHANDLING OM INVENTERING KATEGORI EJ = 6 REDAN FINNS PÅ WDH1          
130900     MOVE 'AJ-INVENT          ' TO WS-SECTION                             
131000     MOVE INV-INV-KDINVKAT    TO SPAR-INVKAT                              
131100     MOVE INV-INV-TEINVANM    TO KOMMENTAR                                
131200     MOVE 'KAT 11'            TO KOMM-11                                  
131300     MOVE INV-INV-FLINVSKR    TO KOMM-SKR                                 
131400     MOVE KOMMENTAR           TO INV-INV-TEINVANM                         
131500                                                                          
131600     PERFORM IMS-REPL-INVSEG                                              
131700                                                                          
131800     MOVE MSGI-IDDC           TO W-IDDC-WDH1-MIN                          
131900                                 W-IDDC-WDH1-MAX                          
132000                                                                          
132100     PERFORM IMS-GET-INVENT-KAT11                                         
132200                                                                          
132300     IF SEGMENT-SAKNAS                                                    
132400       IF SPAR-INVKAT   = +1 OR +2 OR +3 OR +4 OR +9                      
132500         MOVE SUM-KVUTRS      TO INV-INV-KVJUSTKV                         
132600         MOVE MSGI-IDDC       TO INV-INV-IDDC                             
132700         MOVE +11             TO INV-INV-KDINVKAT                         
132800         MOVE SPAR-ADLAGOMR   TO INV-INV-ADLAGOMR                         
132900         MOVE SPAR-ADGANG     TO INV-INV-ADGANG                           
133000         MOVE SPAR-ADPLATS    TO INV-INV-ADPLATS                          
133100         MOVE NEJ             TO INV-INV-FLINVSKR                         
133200         MOVE JA              TO INV-INV-FLINVBEH                         
133300                                                                          
133400         MOVE NEJ             TO INV-INV-FLINV2B                          
133500         MOVE NEJ             TO INV-INV-FLINV3E                          
133600         MOVE NEJ             TO INV-INV-FLINV4N                          
133700                                                                          
133800         IF DCS-CDC                                                       
133900           MOVE JA            TO INV-INV-FLINV3E                          
134000         ELSE                                                             
134100           IF DCS-NDC-NA                                                  
134200             MOVE JA          TO INV-INV-FLINV4N                          
134300           ELSE                                                           
134400             MOVE JA          TO INV-INV-FLINV2B                          
134500           END-IF                                                         
134600         END-IF                                                           
134700                                                                          
134800         MOVE NEJ             TO INV-INV-FLINV2C                          
134900         MOVE NEJ             TO INV-INV-FLINV2D                          
135000         MOVE NEJ             TO INV-INV-FLINV4P                          
135100         MOVE NEJ             TO INV-INV-FLINV4R                          
135200         MOVE ZERO            TO INV-INV-KDINVKAT-OLD                     
135300         MOVE NEJ             TO INV-INV-FLINV85                          
135400         MOVE SPACE           TO INV-INV-FILLER1                          
135500                                 INV-INV-FILLER2                          
135600         MOVE ART-IDFKNGRP    TO INV-INV-IDFKNGRP                         
135700         MOVE +1              TO INV-INV-KDINVPRIO                        
135800         MOVE ART-KDPRODSL    TO INV-INV-KDPRODSL                         
135900         MOVE CLAG-KDPSLLOC   TO INV-INV-KDPSLLOC                         
136000         MOVE CLAG-KDVVKL     TO INV-INV-KDVVKL                           
136100         MOVE SPACE           TO INV-INV-TEINVANM                         
136200         MOVE ZERO            TO INV-INV-IDPRTOMG                         
136300                                 INV-INV-IDLOPNR                          
136400                                 INV-INV-KVAKS-OLD                        
136500                                 INV-INV-KVEFRS-OLD                       
136600                                 INV-INV-KVLS-OLD                         
136700         MOVE DAGENS-TIAAMMDD TO WS-TIAAMMDD                              
136800         PERFORM S05-SKAPA-WDH1DAT                                        
136900         MOVE WS-TISEGKEY2    TO INV-INV-TISEGKEY                         
137000***      COMPUTE INV-INV-DAREGDAT-SORT =                                  
137100***        99999999 - WS-INV-DAREGDAT                                     
137200         MOVE 99999999        TO  INV-INV-DAREGDAT-SORT                   
137300         MOVE WS-INV-DAREGDAT TO INV-INV-DAREGDAT-CRE                     
137400         MOVE WS-INV-DAREGDAT TO INV-INV-DAREGDAT                         
137500         MOVE ZERO            TO INV-INV-DAREGDAT-PR1                     
137600                                 INV-INV-DAREGDAT-PR2                     
137700                                 INV-INV-DAREGDAT-PR3                     
137800*        MOVE SPACE           TO INV-INV-IDUSER-PR1                       
137900*                                INV-INV-IDUSER-PR2                       
138000*                                INV-INV-IDUSER-PR3                       
138100                                                                          
138200         PERFORM IMS-INSERT-INVENTERING                                   
138300         IF SEGMENT-FINNS-REDAN OR INDEX-FINNS-REDAN                      
138400           PERFORM UNTIL SEGMENT-FINNS                                    
138500             ADD +1  TO WS-TISEGKEY2                                      
138600             MOVE WS-TISEGKEY2    TO INV-INV-TISEGKEY                     
138700             PERFORM IMS-INSERT-INVENTERING                               
138800           END-PERFORM                                                    
138900         END-IF                                                           
139000*** INSERT PÅ WDH121 SEGMENTET ***                                        
139100         MOVE 'ISRT WDH121 AJ-SEC ' TO WS-SECTION                         
139200         MOVE MSGI-IDUSER TO INV-INVL-IDUSER                              
139300         MOVE '0'         TO INV-INVL-KDSEGKEY                            
139400         PERFORM IMS-INSERT-WDH121                                        
139500         MOVE SPACE       TO INV-INVL-IDUSER                              
139600         MOVE '1'         TO INV-INVL-KDSEGKEY                            
139700         PERFORM IMS-INSERT-WDH121                                        
139800         MOVE SPACE       TO INV-INVL-IDUSER                              
139900         MOVE '2'         TO INV-INVL-KDSEGKEY                            
140000         PERFORM IMS-INSERT-WDH121                                        
140100         MOVE SPACE       TO INV-INVL-IDUSER                              
140200         MOVE '3'         TO INV-INVL-KDSEGKEY                            
140300         PERFORM IMS-INSERT-WDH121                                        
140400                                                                          
140500**** SLUT PÅ INSERT PÅ WDH121 SEGMENT                                     
140600                                                                          
140700         PERFORM S02-UPPD-ART-MED-UTRSALDO                                
140800       END-IF                                                             
140900                                                                          
141000       IF SPAR-INVKAT = +1 OR +2 OR +3 OR +4 OR +9                        
141100         IF DCS-CDC                                                       
141200           MOVE SUM-KVUTRS TO CLAG-KVUTRS                                 
141300           MOVE JA TO WS-WDK611-UPDATE                                    
141400           PERFORM S03A-SAEND-LARM-2191-MID-CDC                           
141500         ELSE                                                             
141600           PERFORM IMS-GET-WDK7ART                                        
141700           MOVE SUM-KVUTRS TO SLAG-KVUTRS                                 
141800                                                                          
141900           PERFORM IMS-REPL-WDK7-SEGM                                     
142000           PERFORM S03B-SAEND-LARM-2191-MID-NDC                           
142100         END-IF                                                           
142200       END-IF                                                             
142300                                                                          
142400       IF SPAR-INVKAT   = +8                                              
142500         MOVE MEDDELANDE-1 (CL-INDEX) TO MOD-TEMFSINF                     
142600       ELSE                                                               
142700         MOVE MEDDELANDE (CL-INDEX) TO MOD-TEMFSINF                       
142800       END-IF                                                             
142900     ELSE                                                                 
143000       IF DCS-CDC                                                         
143100         MOVE SUM-KVUTRS TO CLAG-KVUTRS                                   
143200         MOVE JA         TO WS-WDK611-UPDATE                              
143300         PERFORM S03A-SAEND-LARM-2191-MID-CDC                             
143400       ELSE                                                               
143500         PERFORM IMS-GET-WDK7ART                                          
143600         MOVE SUM-KVUTRS TO SLAG-KVUTRS                                   
143700                                                                          
143800         PERFORM IMS-REPL-WDK7-SEGM                                       
143900         PERFORM S03B-SAEND-LARM-2191-MID-NDC                             
144000       END-IF                                                             
144100       PERFORM S02-UPPD-ART-MED-UTRSALDO                                  
144200       MOVE MEDDELANDE (CL-INDEX) TO MOD-TEMFSINF                         
144300     END-IF                                                               
144400     .                                                                    
144500     EJECT                                                                
144600 AQ-KOLLA-INLEV-SDC SECTION.                                              
144700*                                                                         
144800*  KONTROLLERA OM INLEV. HAR KOMMIT SENASTE MÅNADEN FÖR SDC.              
144900*                                                                         
145000     MOVE 'AQ-KOLLQ           ' TO WS-SECTION                             
145100     MOVE NEJ TO INLEV-KOLL                                               
145200     MOVE +0 TO INLEV-DATUM                                               
145300                                                                          
145400     PERFORM IMS-GET-WDL601                                               
145500                                                                          
145600     IF SEGMENT-SAKNAS                                                    
145700       MOVE JA TO INLEV-KOLL                                              
145800     ELSE                                                                 
145900       PERFORM IMS-GET-WDL611                                             
146000       PERFORM UNTIL SEGMENT-SAKNAS OR INLEV-DATUM NOT = +0               
146100          IF INLC-INL-IDDC = MSGI-IDDC                                    
146200            IF INLC-INL-IDPTYP = 'R32'                                    
146300              MOVE INLC-INL-TIINLINL TO INLEV-DATUM                       
146400            END-IF                                                        
146500          END-IF                                                          
146600                                                                          
146700          IF INLEV-DATUM = +0                                             
146800             PERFORM IMS-GET-WDL611                                       
146900          END-IF                                                          
147000       END-PERFORM                                                        
147100     END-IF                                                               
147200                                                                          
147300     IF INLEV-DATUM NOT = +0                                              
147400***  OM INLEVERANS SKETT FÖR HÖGST 10 DAGAR SEDAN FÖR SDC OCH             
147500***                      FÖR HÖGST 22 DAGAR SEDAN FÖR CDC                 
147600***  SKER INGEN AUTOMATJUSTERING.                                         
147700       MOVE 001                TO WORK-KDCALL                             
147800       MOVE MSGI-IDDC          TO WORK-IDDC                               
147900       MOVE INLEV-DATUM        TO WORK-TIAAMMDD-FOM                       
148000       MOVE DAGENS-TIAAMMDD    TO WORK-TIAAMMDD-TOM                       
148100       CALL WORKDAY  USING        WORK-KDCALL                             
148200                                  WORK-DATE-AREA                          
148300                                  WORK-KDSVAR                             
148400       IF WORK-KDSVAR-OK                                                  
148500         IF NOT DCS-CDC                                                   
148600           IF WORK-KVWORKD > 10                                           
148700             MOVE JA           TO INLEV-KOLL                              
148800           END-IF                                                         
148900         ELSE                                                             
149000           IF WORK-KVWORKD > 22                                           
149100             MOVE JA           TO INLEV-KOLL                              
149200           END-IF                                                         
149300         END-IF                                                           
149400       END-IF                                                             
149500     ELSE                                                                 
149600        MOVE JA TO INLEV-KOLL                                             
149700     END-IF                                                               
149800     .                                                                    
149900     EJECT                                                                
150000 QA-HAEMTA-SDC-INFO SECTION.                                              
150100     SKIP2                                                                
150200     MOVE 'QA-HAMTA           ' TO WS-SECTION                             
150300     PERFORM IMS-GET-WDK7ART                                              
150400                                                                          
150500     IF SEGMENT-FINNS                                                     
150600       MOVE SLAG-ADLAGOMR   TO SPAR-ADLAGOMR                              
150700       MOVE SLAG-ADGANG     TO SPAR-ADGANG                                
150800       MOVE SLAG-ADPLATS    TO SPAR-ADPLATS                               
150900       MOVE SLAG-KVLS       TO SPAR-KVLS                                  
151000                                                                          
151100       IF SLAG-KVUTRS = 0                                                 
151200         MOVE SLAG-KVUTRS   TO SPAR-KVUTRS                                
151300         MOVE SLAG-KVLS     TO SUM-KVUTRS                                 
151400       ELSE                                                               
151500         MOVE SLAG-KVUTRS   TO SPAR-KVUTRS                                
151600       END-IF                                                             
151700     END-IF                                                               
151800     .                                                                    
151900     EJECT                                                                
152000 QB-HAEMTA-CDC-INFO SECTION.                                              
152100     SKIP2                                                                
152200     MOVE 'QB-HAMTA           ' TO WS-SECTION                             
152300     PERFORM IMS-GHNP-WDK611                                              
152400                                                                          
152500     IF SEGMENT-FINNS                                                     
152600       MOVE CLAG-ADLAGOMR   TO SPAR-ADLAGOMR                              
152700       MOVE CLAG-ADGANG     TO SPAR-ADGANG                                
152800       MOVE CLAG-ADPLATS    TO SPAR-ADPLATS                               
152900       MOVE CLAG-KVLS       TO SPAR-KVLS                                  
153000                                                                          
153100       IF CLAG-KVUTRS = 0                                                 
153200         MOVE CLAG-KVUTRS   TO SPAR-KVUTRS                                
153300         MOVE CLAG-KVLS     TO SUM-KVUTRS                                 
153400       ELSE                                                               
153500         MOVE CLAG-KVUTRS   TO SPAR-KVUTRS                                
153600       END-IF                                                             
153700     END-IF                                                               
153800     .                                                                    
153900     EJECT                                                                
154000 S01-KOLLA-TILLAATEN-PASSWORD SECTION.                                    
154100     MOVE 'S01-KOLLA          ' TO WS-SECTION                             
154200     MOVE JA  TO WS-PASSWORD-OK                                           
154300     MOVE NEJ TO WS-FLLO91                                                
154400                                                                          
154500     IF NOT MFS-UPD-X                                                     
154600       MOVE WS-IDUSER    TO SEC-IDUSER                                    
154700       MOVE WS-EGEN-BILD TO SEC-IDTRANS                                   
154800       MOVE IDPW-WS      TO SEC-IDKEY                                     
154900       CALL WSECURIT USING SEC-IDUSER                                     
155000                           SEC-IDTRANS                                    
155100                           SEC-IDKEY                                      
155200                           SEC-KDSVAR                                     
155300       IF SEC-KDSVAR NOT = SPACE                                          
155400         MOVE NEJ TO WS-PASSWORD-OK                                       
155500       END-IF                                                             
155600** LAGEROMRÅDE 91 FÅR ENDAST UPPDATERAS MED PF23(ÄT 96008)                
155700       IF DCS-CDC                                                         
155800         IF NOT MFS-UPD-V                                                 
155900           MOVE IDARTNR-WS TO W-IDARTNR                                   
156000           PERFORM IMS-GU-WDK601                                          
156100           IF SEGMENT-FINNS                                               
156200             PERFORM IMS-GHNP-WDK611                                      
156300             IF CLAG-ADLAGOMR = 91                                        
156400               MOVE JA TO WS-FLLO91                                       
156500             END-IF                                                       
156600           END-IF                                                         
156700         END-IF                                                           
156800       END-IF                                                             
156900     END-IF                                                               
157000     IF DCS-CDC-TR                                                        
157100       MOVE NEJ TO WS-PASSWORD-OK                                         
157200     END-IF                                                               
157300     .                                                                    
157400     EJECT                                                                
157500 S02-UPPD-ART-MED-UTRSALDO SECTION.                                       
157600                                                                          
157700*    UPPDATERAR WDG2-5116. ARTIKEL MED UTREDNINGSSALDO KNYTS              
157800*    TILL NOLLJAGARE - IDPW                                               
157900*                                                                         
158000     MOVE 'S02-UPPD-ART       ' TO WS-SECTION                             
158100     MOVE IDARTNR-WS       TO 5116-IDARTNR                                
158200     MOVE IDPW-WS          TO 5116-IDPW                                   
158300     MOVE DAGENS-TIAAMMDD  TO 5116-TIUPPDAT                               
158400     ACCEPT 5116-TIUPPTID  FROM TIME                                      
158500                                                                          
158600     IF MFS-IDTRANS = '4325' OR '4397'                                    
158700         MOVE MID-IDDC-IN  TO 5116-IDDC                                   
158800         MOVE MID-IDPRODNR TO 5116-IDPRODNR                               
158900         MOVE MID-KDORDKL  TO 5116-KDORDKL                                
159000     ELSE                                                                 
159100         MOVE MSGI-IDDC    TO 5116-IDDC                                   
159200         MOVE ZERO         TO 5116-IDPRODNR                               
159300                              5116-KDORDKL                                
159400     END-IF                                                               
159500                                                                          
159600     PERFORM IMS-INSERT-ART-UTREDNSALDO                                   
159700     .                                                                    
159800     EJECT                                                                
159900 S03A-SAEND-LARM-2191-MID-CDC SECTION.                                    
160000                                                                          
160100     MOVE 'S03A-SAEND-LARM-2191-MID-CDC'  TO WS-SECTION                   
160200     COMPUTE ALT2-LL = LENGTH OF ALT2-MID-W2I19101 + 17                   
160300     MOVE CLAG-IDANSK     TO ALT2-MID-IDANSK                              
160400                                                                          
160500     PERFORM IMS-GU-WDE601                                                
160600     IF SEGMENT-FINNS                                                     
160700       MOVE VORD-IDDISTR     TO WS-IDDISTR-NUM4                           
160800       MOVE WS-IDDISTR-NUM4  TO ALT2-MID-IDDISTR                          
160900       MOVE VORD-IDKUNDNR    TO WS-IDKUNDNR-NUM6                          
161000       MOVE WS-IDKUNDNR-NUM6 TO ALT2-MID-IDKUNDNR                         
161100     ELSE                                                                 
161200       MOVE ZERO          TO ALT2-MID-IDDISTR                             
161300                             ALT2-MID-IDKUNDNR                            
161400     END-IF                                                               
161500                                                                          
161600     MOVE MFS-KDMFSFOR    TO ALT2-MID-KDCLAGER                            
161700     MOVE IDARTNR-WS      TO ALT2-MID-IDARTNR                             
161800     MOVE ZERO            TO ALT2-MID-TISENBEK-DAG                        
161900                             ALT2-MID-TISENBEK-KL                         
162000                             ALT2-MID-IDKUNDRF                            
162100     MOVE SPACE           TO ALT2-MID-IDKR                                
162200     MOVE 200             TO ALT2-MID-KDLARM                              
162300     MOVE 'J'             TO ALT2-MID-FLNYLARM                            
162400     MOVE WC-CDC-SE       TO ALT2-MID-IDDC                                
162500     MOVE SPACE           TO ALT2-MID-IDLEVNR                             
162600                                                                          
162700     PERFORM IMS-ISRT-ALT2191-MSG                                         
162800     .                                                                    
162900     EJECT                                                                
163000 S03B-SAEND-LARM-2191-MID-NDC  SECTION.                                   
163100                                                                          
163200     MOVE 'S03B-SAEND-LARM-2191-MID-NDC'   TO WS-SECTION                  
163300                                                                          
163400     COMPUTE ALT2-LL = LENGTH OF ALT2-MID-W2I19101 + 17                   
163500                                                                          
163600     MOVE W-IDARTNR              TO ALT2-MID-IDARTNR                      
163700     PERFORM IMS-GU-WDK722                                                
163800     IF SEGMENT-FINNS                                                     
163900       MOVE XLAG-IDANSK          TO ALT2-MID-IDANSK                       
164000     ELSE                                                                 
164100       MOVE ZERO                 TO ALT2-MID-IDANSK                       
164200     END-IF                                                               
164300                                                                          
164400     PERFORM IMS-GU-WDE601                                                
164500     IF SEGMENT-FINNS                                                     
164600       MOVE VORD-IDDISTR     TO WS-IDDISTR-NUM4                           
164700       MOVE WS-IDDISTR-NUM4  TO ALT2-MID-IDDISTR                          
164800       MOVE VORD-IDKUNDNR    TO WS-IDKUNDNR-NUM6                          
164900       MOVE WS-IDKUNDNR-NUM6 TO ALT2-MID-IDKUNDNR                         
165000     ELSE                                                                 
165100       MOVE ZERO             TO ALT2-MID-IDDISTR                          
165200                                ALT2-MID-IDKUNDNR                         
165300     END-IF                                                               
165400                                                                          
165500     MOVE MFS-KDMFSFOR       TO ALT2-MID-KDCLAGER                         
165600     MOVE IDARTNR-WS         TO ALT2-MID-IDARTNR                          
165700     MOVE ZERO               TO ALT2-MID-TISENBEK-DAG                     
165800                                ALT2-MID-TISENBEK-KL                      
165900                                ALT2-MID-IDKUNDRF                         
166000     MOVE SPACE              TO ALT2-MID-IDKR                             
166100     MOVE 200                TO ALT2-MID-KDLARM                           
166200     MOVE 'J'                TO ALT2-MID-FLNYLARM                         
166300     MOVE SLAG-IDDC          TO ALT2-MID-IDDC                             
166400     MOVE SLAG-IDLEVNR       TO ALT2-MID-IDLEVNR                          
166500                                                                          
166600     PERFORM IMS-ISRT-ALT2191-MSG                                         
166700     .                                                                    
166800     EJECT                                                                
166900 S04-SKAPA-TISEGKEY SECTION.                                              
167000     MOVE 'S04-SKAPA-TISEG    ' TO WS-SECTION                             
167100     MOVE WS-TIAAAAMMDD(1:2)  TO WS-SEKEL                                 
167200     MOVE 9                   TO WS-LOPNR                                 
167300                                                                          
167400     COMPUTE WS-TISEGKEY = 999999999 - WS-TIAAAAMMDDL                     
167500                                                                          
167600     MOVE WS-TISEGKEY TO INVH-TISEGKEY                                    
167700     .                                                                    
167800     EJECT                                                                
167900 S05-SKAPA-WDH1DAT  SECTION.                                              
168000     MOVE 'S05-SKAPA-WDH1DAT  ' TO WS-SECTION                             
168100     MOVE WS-TIAAAAMMDD       TO WS-INV-DAREGDAT                          
168200     MOVE WS-TIAAAAMMDD(1:2)  TO WS-SEKEL                                 
168300     MOVE 0                   TO WS-LOPNR                                 
168400                                                                          
168500     MOVE WS-TIAAAAMMDDL TO WS-TISEGKEY2                                  
168600                                                                          
168700     .                                                                    
168800     EJECT                                                                
168900 S06-FLYTTA-LOGG-WDK6 SECTION.                                            
169000* LÄGGER UPP SALDOLOGG I WDL9                                             
169100     MOVE 'S06-FLYTTA         ' TO WS-SECTION                             
169200     MOVE W-IDARTNR               TO LOGG-IDARTNR                         
169300     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
169400     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - DAGENS-DATUM               
169500     ACCEPT TRANS-TID FROM TIME                                           
169600     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TID                 
169700     MOVE 9                       TO LOGG-IDSEKVNR                        
169800     MOVE MSGI-IDDC               TO LOGG-IDDC                            
169900     MOVE 'MISC'                  TO LOGG-IDHUVTYP                        
170000     MOVE 'ADJ'                   TO LOGG-IDSUBTYP                        
170100     MOVE IDPGM                   TO LOGG-IDPGM                           
170200     MOVE MSGI-IDTRANS            TO LOGG-IDTRANS                         
170300     MOVE MSGI-IDUSER             TO LOGG-IDUSER                          
170400     MOVE SPACE                   TO LOGG-REF                             
170500     MOVE MSGI-IDUSER             TO LOGG-IDUSER-IDPRCREF                 
170600     MOVE W-IDPRODNR              TO LOGG-IDPRODNR                        
170700     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS-PAV              
170800     MOVE SPACE                   TO LOGG-IDTECKEN-KVEFRS                 
170900     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS                  
171000     MOVE W-SPAR-KVUTRS           TO LOGG-KVART-SALDO                     
171100     MOVE CLAG-KVAKS-PAV          TO LOGG-KVAKS-PAV                       
171200     MOVE CLAG-KVEFRS             TO LOGG-KVEFRS                          
171300     MOVE CLAG-KVLS               TO LOGG-KVLS                            
171400     COMPUTE LOGG-KVAKS = CLAG-KVAKS-CDC +                                
171500                          CLAG-KVAKS-T                                    
171600     MOVE ZERO                    TO LOGG-DAREGDAT-LADD                   
171700                                                                          
171800     .                                                                    
171900     EJECT                                                                
172000 S07-FLYTTA-LOGG-WDK7 SECTION.                                            
172100     MOVE 'S07-FLYTTA        ' TO WS-SECTION                              
172200* LÄGGER UPP SALDOLOGG I WDL9                                             
172300     MOVE W-IDARTNR             TO LOGG-IDARTNR                           
172400     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
172500     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - DAGENS-DATUM               
172600     ACCEPT TRANS-TID FROM TIME                                           
172700     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TID                 
172800     MOVE 9                       TO LOGG-IDSEKVNR                        
172900     MOVE MSGI-IDDC                 TO LOGG-IDDC                          
173000     MOVE 'MISC'                  TO LOGG-IDHUVTYP                        
173100     MOVE 'ADJ'                   TO LOGG-IDSUBTYP                        
173200     MOVE IDPGM                   TO LOGG-IDPGM                           
173300     MOVE MSGI-IDTRANS            TO LOGG-IDTRANS                         
173400     MOVE MSGI-IDUSER             TO LOGG-IDUSER                          
173500     MOVE SPACE                   TO LOGG-REF                             
173600     MOVE MSGI-IDUSER             TO LOGG-IDUSER-IDPRCREF                 
173700     MOVE W-IDPRODNR              TO LOGG-IDPRODNR                        
173800     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS-PAV              
173900     MOVE SPACE                   TO LOGG-IDTECKEN-KVEFRS                 
174000     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS                  
174100     MOVE W-SPAR-KVUTRS           TO LOGG-KVART-SALDO                     
174200     MOVE SLAG-KVAKS-SDC          TO LOGG-KVAKS                           
174300     MOVE SLAG-KVAKS-PAV          TO LOGG-KVAKS-PAV                       
174400     MOVE SLAG-KVEFRS             TO LOGG-KVEFRS                          
174500     MOVE SLAG-KVLS               TO LOGG-KVLS                            
174600     MOVE ZERO                    TO LOGG-DAREGDAT-LADD                   
174700                                                                          
174800     .                                                                    
174900     EJECT                                                                
175000 S08-UPPDATERA-LOGG SECTION.                                              
175100     MOVE 'S08-UPPDATERA      ' TO WS-SECTION                             
175200     PERFORM IMS-ISRT-WDL901                                              
175300     IF SEGMENT-FINNS-REDAN                                               
175400        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
175500          ADD -1 TO LOGG-IDSEKVNR                                         
175600          PERFORM IMS-ISRT-WDL901                                         
175700        END-PERFORM                                                       
175800     END-IF                                                               
175900     .                                                                    
176000     EJECT                                                                
176100 S09-UPPDATERA-WDR9 SECTION.                                              
176200     MOVE 'S09-UPPDAT         ' TO WS-SECTION                             
176300     MOVE 'W5010800'       TO FIL-IDPGM                                   
176400     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
176500     MOVE DAGENS-DATUM     TO FIL-DAREGDAT                                
176600     ACCEPT FIL-TIKLOCK    FROM TIME                                      
176700     MOVE 1                TO FIL-IDSEKVNR                                
176800     MOVE 'W510EKHA'       TO FIL-IDCPYTXT                                
176900     MOVE MSGI-IDUSER      TO FIL-IDUSER                                  
177000     MOVE W-IDARTNR        TO EKH-IDARTNR                                 
177100     MOVE '403'            TO EKH-KDEKHHT                                 
177200     MOVE '408'            TO EKH-KDEKSHT                                 
177300     MOVE 'DET'            TO EKH-KDEKNIVA                                
177400     MOVE W-IDDC           TO EKH-IDDC-SEND                               
177500                              EKH-IDDC-REC                                
177600     MOVE +0               TO EKH-IDDISTR                                 
177700     MOVE +0               TO EKH-IDKUNDNR                                
177800                                                                          
177900     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
178000     MOVE W-IDARTNR        TO CIA-IDARTBET-IN                             
178100     CALL W009CIA USING       CIA-W009CIA                                 
178200     MOVE CIA-IDARTBET-UT TO EKH-IDVERGL                                  
178300                                                                          
178400     MOVE DAGENS-DATUM     TO EKH-DAVERDAT                                
178500     MOVE ART-KDPRODSL     TO EKH-KDPRODSL                                
178600     MOVE ZERO             TO EKH-KDPSLLOC                                
178700     MOVE SPACE            TO EKH-FLLSBOK                                 
178800     MOVE 'SEK'            TO EKH-KDVALISO                                
178900     MOVE 1.00             TO EKH-PRKURS                                  
179000     MOVE ZERO             TO EKH-PRARTNTO                                
179100     MOVE ZERO             TO EKH-PRARTSJK                                
179200     MOVE ZERO             TO EKH-PRHEMTAG                                
179300     MOVE CLAG-PRARTSTD    TO EKH-PRARTSTD                                
179400     MOVE ZERO             TO EKH-PRLANDCO                                
179500     MOVE ZERO             TO EKH-PRINK                                   
179600     MOVE ZERO             TO EKH-PRDIRLON                                
179700     MOVE ZERO             TO EKH-PRDMTRL                                 
179800     MOVE ZERO             TO EKH-PROVRPAL                                
179900     MOVE ZERO             TO EKH-SUBEL                                   
180000     COMPUTE EKH-KVANTAL = W-SPAR-KVUTRS * -1                             
180100     MOVE '5108'           TO EKH-IDTRANS                                 
180200     MOVE ZERO             TO EKH-BEVAT                                   
180300                              EKH-IDANALYS                                
180400                              EKH-IDKONTO                                 
180500                              EKH-KDANMORS                                
180600                              EKH-KDFRAKT                                 
180700                              EKH-SUVAT                                   
180800     MOVE ZERO             TO EKH-DAAVIDAT                                
180900                              EKH-IDAVINR                                 
181000                              EKH-KDAVVTYP                                
181100                              EKH-KDRT                                    
181200                              EKH-KVANTMOT                                
181300                              EKH-KVAVIS                                  
181400     MOVE WS-KDSORT        TO EKH-KDSORT                                  
181500     IF W-IDDC = WC-SDC-NL-ET                                             
181600       MOVE JA             TO EKH-FLDCET                                  
181700     ELSE                                                                 
181800       MOVE NEJ            TO EKH-FLDCET                                  
181900     END-IF                                                               
182000     MOVE SPACE            TO EKH-KDTRADP                                 
182100                              EKH-IDKST                                   
182200                              EKH-IDLEVNR                                 
182300                              EKH-IDKUNDRF                                
182400                              EKH-IDFAKT-EXP                              
182500     .                                                                    
182600     EJECT                                                                
182700* IMS SEKTIONER                                                           
182800     SKIP3                                                                
182900 IMS-GET-MSG SECTION.                                                     
183000     MOVE '  QC' TO GODK-STATUSKODER                                      
183100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
183200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
183300     PERFORM IMS-STATUSKONTROLL                                           
183400     .                                                                    
183500     SKIP3                                                                
183600 IMS-INSERT-MSG SECTION.                                                  
183700     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
183800        MOVE '0' TO MFS-KDHUVOMR                                          
183900     END-IF                                                               
184000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
184100     MOVE SPACE TO GODK-STATUSKODER                                       
184200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
184300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
184400     PERFORM IMS-STATUSKONTROLL                                           
184500     .                                                                    
184600     SKIP2                                                                
184700 IMS-ISRT-ALT2191-MSG SECTION.                                            
184800     MOVE SPACE TO GODK-STATUSKODER                                       
184900     CALL CBLTDLI USING ISRT ALT2191-PCB ALT2191-IO-AREA                  
185000     MOVE ALT2191-STATUS-CODE TO STATUS-WS                                
185100     PERFORM IMS-STATUSKONTROLL                                           
185200     .                                                                    
185300     EJECT                                                                
185400 IMS-GU-WDK601 SECTION.                                                   
185500                                                                          
185600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ') '                        
185700            DELIMITED BY SIZE INTO SSA1                                   
185800     MOVE '  GE' TO GODK-STATUSKODER                                      
185900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
186000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
186100     PERFORM IMS-STATUSKONTROLL                                           
186200     .                                                                    
186300     SKIP2                                                                
186400 IMS-GHNP-WDK611 SECTION.                                                 
186500                                                                          
186600     MOVE 'WDK611 ' TO SSA1                                               
186700     MOVE '  GE' TO GODK-STATUSKODER                                      
186800     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                  
186900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
187000     PERFORM IMS-STATUSKONTROLL                                           
187100     .                                                                    
187200     SKIP2                                                                
187300 IMS-REPL-WDK611 SECTION.                                                 
187400                                                                          
187500     MOVE '  ' TO GODK-STATUSKODER                                        
187600     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
187700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
187800     PERFORM IMS-STATUSKONTROLL                                           
187900     .                                                                    
188000     EJECT                                                                
188100 IMS-GHNP-WDK629  SECTION.                                                
188200                                                                          
188300     MOVE   'WDK629 '        TO SSA1                                      
188400     MOVE '  GE' TO GODK-STATUSKODER                                      
188500     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK629 SSA1                  
188600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
188700     PERFORM IMS-STATUSKONTROLL                                           
188800     .                                                                    
188900     SKIP3                                                                
189000 IMS-REPL-WDK629 SECTION.                                                 
189100     MOVE '  ' TO GODK-STATUSKODER                                        
189200     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK629                       
189300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
189400     PERFORM IMS-STATUSKONTROLL                                           
189500     .                                                                    
189600     EJECT                                                                
189700 IMS-ISRT-WDL901 SECTION.                                                 
189800     SKIP2                                                                
189900     MOVE 'WLLOGA01 ' TO SSA1                                             
190000     MOVE '  II' TO GODK-STATUSKODER                                      
190100     CALL CBLTDLI USING ISRT LOGA-PCB WLLOGA01 SSA1                       
190200     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
190300     PERFORM IMS-STATUSKONTROLL                                           
190400     .                                                                    
190500     EJECT                                                                
190600 IMS-GET-INVENT-KAT11  SECTION.                                           
190700     MOVE 'IMS-GET-INVENT-KAT11' TO WS-IMS                                
190800     STRING 'WDH111  (WDH111KY>=' W-WDH1KEY-X-MIN                         
190900                    '&WDH111KY<=' W-WDH1KEY-X-MAX  ')'                    
191000            DELIMITED BY SIZE INTO SSA1                                   
191100     MOVE '  GE' TO GODK-STATUSKODER                                      
191200     CALL CBLTDLI USING GNP INVA-PCB INV-WDH111 SSA1                      
191300     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
191400     PERFORM IMS-STATUSKONTROLL                                           
191500     .                                                                    
191600     SKIP2                                                                
191700 IMS-INSERT-ROT-INVENTERING SECTION.                                      
191800     MOVE 'IMS-INSERT-ROT     ' TO WS-IMS                                 
191900     MOVE 'WDH101 ' TO SSA1                                               
192000     MOVE '  ' TO GODK-STATUSKODER                                        
192100     CALL CBLTDLI USING ISRT INVA-PCB INV-WDH101 SSA1                     
192200     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
192300     PERFORM IMS-STATUSKONTROLL                                           
192400     .                                                                    
192500     SKIP2                                                                
192600 IMS-INSERT-INVENTERING SECTION.                                          
192700     MOVE 'IMS-INSERT-INVENT  ' TO WS-IMS                                 
192800     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ') '                        
192900            DELIMITED BY SIZE INTO SSA1                                   
193000     MOVE 'WDH111 ' TO SSA2                                               
193100     MOVE '  IINI' TO GODK-STATUSKODER                                    
193200     CALL CBLTDLI USING ISRT INVA-PCB INV-WDH111 SSA1 SSA2                
193300     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
193400     PERFORM IMS-STATUSKONTROLL                                           
193500     .                                                                    
193600     EJECT                                                                
193700 IMS-INSERT-WDH121      SECTION.                                          
193800     MOVE 'IMS-INSERT-WDH121  ' TO WS-IMS                                 
193900     MOVE 'WDH121 ' TO SSA1                                               
194000     MOVE '  II' TO GODK-STATUSKODER                                      
194100     CALL CBLTDLI USING ISRT INVA-PCB INV-WDH121 SSA1                     
194200     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
194300     PERFORM IMS-STATUSKONTROLL                                           
194400     .                                                                    
194500     EJECT                                                                
194600 IMS-GET-INVENT-ROT  SECTION.                                             
194700     MOVE 'IMS-GET-INVENT-ROT ' TO WS-IMS                                 
194800     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ') '                        
194900            DELIMITED BY SIZE INTO SSA1                                   
195000     MOVE '  GE' TO GODK-STATUSKODER                                      
195100     CALL CBLTDLI USING GU INVA-PCB INV-WDH101 SSA1                       
195200     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
195300     PERFORM IMS-STATUSKONTROLL                                           
195400     .                                                                    
195500     SKIP2                                                                
195600 IMS-GET-INVSEG  SECTION.                                                 
195700     MOVE 'IMS-GET-INVSEG     ' TO WS-IMS                                 
195800     MOVE   'WDH111 '    TO SSA1                                          
195900     MOVE '  GE' TO GODK-STATUSKODER                                      
196000     CALL CBLTDLI USING GHNP INVA-PCB INV-WDH111 SSA1                     
196100     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
196200     PERFORM IMS-STATUSKONTROLL                                           
196300     .                                                                    
196400     SKIP2                                                                
196500 IMS-REPL-INVSEG  SECTION.                                                
196600     MOVE 'IMS-REPL-INVSEG    ' TO WS-IMS                                 
196700     MOVE '    ' TO GODK-STATUSKODER                                      
196800     CALL CBLTDLI USING REPL INVA-PCB INV-WDH111                          
196900     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
197000     PERFORM IMS-STATUSKONTROLL                                           
197100     .                                                                    
197200     EJECT                                                                
197300 IMS-GET-ORDERRADER-SEG SECTION.                                          
197400                                                                          
197500     STRING 'WDE411  *D(WDE4CSEQ =' W-WDE4CSEQ-X                          
197600                      '*KDRADSTA <' W-ORDSTA-X ')'                        
197700            DELIMITED BY SIZE INTO SSA1                                   
197800     MOVE 'WDE401  *D' TO SSA2                                            
197900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
198000     CALL CBLTDLI USING GN WDE4-PCB DLI-IO-E411-01 SSA1 SSA2              
198100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
198200     PERFORM IMS-STATUSKONTROLL                                           
198300     .                                                                    
198400     SKIP2                                                                
198500 IMS-GU-WDE601      SECTION.                                              
198600                                                                          
198700     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
198800            DELIMITED BY SIZE INTO SSA1                                   
198900     MOVE '  GE' TO GODK-STATUSKODER                                      
199000     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E601 SSA1                      
199100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
199200     PERFORM IMS-STATUSKONTROLL                                           
199300     .                                                                    
199400     SKIP2                                                                
199500 IMS-GET-INLEVROT    SECTION.                                             
199600                                                                          
199700     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X  ') '                       
199800            DELIMITED BY SIZE INTO SSA1                                   
199900     MOVE '  GE' TO GODK-STATUSKODER                                      
200000     CALL CBLTDLI USING GU INLE-PCB INL-WLINLE01 SSA1                     
200100     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
200200     PERFORM IMS-STATUSKONTROLL                                           
200300     .                                                                    
200400     SKIP2                                                                
200500 IMS-GET-INLEVNR     SECTION.                                             
200600                                                                          
200700     MOVE 'WLINLE11 '   TO SSA1                                           
200800     MOVE '  GE' TO GODK-STATUSKODER                                      
200900     CALL CBLTDLI USING GNP INLE-PCB INL-WLINLE11 SSA1                    
201000     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
201100     PERFORM IMS-STATUSKONTROLL                                           
201200     .                                                                    
201300     EJECT                                                                
201400 IMS-GET-INLEVTRANS3X  SECTION.                                           
201500                                                                          
201600     STRING 'WLINLE11(DAINLEV  =' W-DAINLEVNYCK-X ')'                     
201700            DELIMITED BY SIZE INTO SSA1                                   
201800     MOVE 'WLINLE21 ' TO SSA2                                             
201900     MOVE '  GE' TO GODK-STATUSKODER                                      
202000     CALL CBLTDLI USING GNP INLE-PCB INL-WLINLE21 SSA1 SSA2               
202100     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
202200     PERFORM IMS-STATUSKONTROLL                                           
202300     .                                                                    
202400     SKIP2                                                                
202500 IMS-GET-INLEVTRANSR34 SECTION.                                           
202600                                                                          
202700     STRING 'WLINLE11(DAINLEV  =' W-DAINLEVNYCK-X ')'                     
202800            DELIMITED BY SIZE INTO SSA1                                   
202900     STRING 'WLINLE22(IDPTYP   =' W-IDLEVNYCK-X ')'                       
203000            DELIMITED BY SIZE INTO SSA2                                   
203100     MOVE '  GE' TO GODK-STATUSKODER                                      
203200     CALL CBLTDLI USING GNP INLE-PCB INL-WLINLE22 SSA1 SSA2               
203300     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
203400     PERFORM IMS-STATUSKONTROLL                                           
203500     .                                                                    
203600     SKIP2                                                                
203700 IMS-GET-P32TRANS      SECTION.                                           
203800                                                                          
203900     STRING 'WLINLE11(DAINLEV  =' W-DAINLEVNYCK-X ')'                     
204000            DELIMITED BY SIZE INTO SSA1                                   
204100     MOVE 'WLINLE21 ' TO SSA2                                             
204200     MOVE 'WLINLE31 ' TO SSA3                                             
204300     MOVE '  GE' TO GODK-STATUSKODER                                      
204400     CALL CBLTDLI USING GNP INLE-PCB INL-WLINLE31                         
204500                          SSA1 SSA2 SSA3                                  
204600     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
204700     PERFORM IMS-STATUSKONTROLL                                           
204800     .                                                                    
204900     EJECT                                                                
205000 IMS-GU-ARTD-SALDO    SECTION.                                            
205100                                                                          
205200     STRING 'WLARTD01(IDARTNR  =' W-IDARTNR-X  ') '                       
205300            DELIMITED BY SIZE INTO SSA1                                   
205400     MOVE '  GE' TO GODK-STATUSKODER                                      
205500     CALL CBLTDLI USING GU ARTD-PCB ARTD-WLARTD01 SSA1                    
205600     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
205700     PERFORM IMS-STATUSKONTROLL                                           
205800     .                                                                    
205900     SKIP2                                                                
206000 IMS-GNP-ADR-SALDO    SECTION.                                            
206100                                                                          
206200     STRING 'WLARTD11(WDD811KY =' W-WDD811KY-X ')'                        
206300            DELIMITED BY SIZE INTO SSA1                                   
206400     MOVE '  GE' TO GODK-STATUSKODER                                      
206500     CALL CBLTDLI USING GNP ARTD-PCB ARTD-WLARTD11 SSA1                   
206600     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
206700     PERFORM IMS-STATUSKONTROLL                                           
206800     .                                                                    
206900     EJECT                                                                
207000 IMS-INSERT-ART-UTREDNSALDO SECTION.                                      
207100                                                                          
207200     STRING 'WLXXEF01(WDGXKEY  =' W-WDGXKEY-ROT-X ')'                     
207300             DELIMITED BY SIZE INTO SSA1                                  
207400     MOVE   'WLXXEF11 ' TO SSA2                                           
207500     MOVE '  II' TO GODK-STATUSKODER                                      
207600     CALL CBLTDLI USING ISRT XXEF-PCB WLXXEF11 SSA1 SSA2                  
207700     MOVE XXEF-STATUS-CODE TO STATUS-WS                                   
207800     PERFORM IMS-STATUSKONTROLL                                           
207900     .                                                                    
208000     EJECT                                                                
208100 IMS-GET-WDK7ART SECTION.                                                 
208200                                                                          
208300                                                                          
208400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
208500            DELIMITED BY SIZE INTO SSA1                                   
208600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
208700            DELIMITED BY SIZE INTO SSA2                                   
208800     MOVE '  GE' TO GODK-STATUSKODER                                      
208900     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
209000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
209100     PERFORM IMS-STATUSKONTROLL                                           
209200     .                                                                    
209300     SKIP2                                                                
209400 IMS-REPL-WDK7-SEGM SECTION.                                              
209500                                                                          
209600     MOVE '  ' TO GODK-STATUSKODER                                        
209700     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
209800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
209900     PERFORM IMS-STATUSKONTROLL                                           
210000     .                                                                    
210100     EJECT                                                                
210200                                                                          
210300 IMS-GU-WDK722 SECTION.                                                   
210400                                                                          
210500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
210600          DELIMITED BY SIZE INTO SSA1                                     
210700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
210800          DELIMITED BY SIZE INTO SSA2                                     
210900     MOVE 'WDK722 '           TO SSA3                                     
211000     MOVE '  GE'              TO GODK-STATUSKODER                         
211100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
211200     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
211300     PERFORM IMS-STATUSKONTROLL                                           
211400     .                                                                    
211500                                                                          
211600 IMS-GET-INVHIST-ROT SECTION.                                             
211700                                                                          
211800     STRING 'WLINVC01(IDARTNR  =' W-IDARTNR-X ') '                        
211900            DELIMITED BY SIZE INTO SSA1                                   
212000     MOVE '  GE' TO GODK-STATUSKODER                                      
212100     CALL CBLTDLI USING GU INVC-PCB WLINVC01 SSA1                         
212200     MOVE INVC-STATUS-CODE TO STATUS-WS                                   
212300     PERFORM IMS-STATUSKONTROLL                                           
212400     .                                                                    
212500     SKIP2                                                                
212600 IMS-ISRT-INVHIST-SEGM SECTION.                                           
212700                                                                          
212800     STRING 'WLINVC01(IDARTNR  =' W-IDARTNR-X ') '                        
212900            DELIMITED BY SIZE INTO SSA1                                   
213000     MOVE   'WLINVC11'  TO SSA2                                           
213100     MOVE '  II' TO GODK-STATUSKODER                                      
213200     CALL CBLTDLI USING ISRT INVC-PCB WLINVC11 SSA1 SSA2                  
213300     MOVE INVC-STATUS-CODE TO STATUS-WS                                   
213400     PERFORM IMS-STATUSKONTROLL                                           
213500     .                                                                    
213600     SKIP2                                                                
213700 IMS-ISRT-INVHIST-ROT SECTION.                                            
213800                                                                          
213900     MOVE   'WLINVC01 '  TO SSA1                                          
214000     MOVE '  ' TO GODK-STATUSKODER                                        
214100     CALL CBLTDLI USING ISRT INVC-PCB WLINVC01 SSA1                       
214200     MOVE INVC-STATUS-CODE TO STATUS-WS                                   
214300     PERFORM IMS-STATUSKONTROLL                                           
214400     .                                                                    
214500     EJECT                                                                
214600 IMS-GET-WDL601      SECTION.                                             
214700                                                                          
214800     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X  ') '                       
214900            DELIMITED BY SIZE INTO SSA1                                   
215000     MOVE '  GE' TO GODK-STATUSKODER                                      
215100     CALL CBLTDLI USING GU INLC-PCB INLC-WLINLC01 SSA1                    
215200     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
215300     PERFORM IMS-STATUSKONTROLL                                           
215400     .                                                                    
215500     SKIP2                                                                
215600 IMS-GET-WDL611      SECTION.                                             
215700                                                                          
215800     MOVE 'WLINLC11 '   TO SSA1                                           
215900     MOVE '  GE' TO GODK-STATUSKODER                                      
216000     CALL CBLTDLI USING GNP INLC-PCB INLC-WLINLC11 SSA1                   
216100     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
216200     PERFORM IMS-STATUSKONTROLL                                           
216300     .                                                                    
216400     EJECT                                                                
216500********** WDR9 PEDAL*********************************************        
216600 IMS-ISRT-WDR901 SECTION.                                                 
216700     SKIP2                                                                
216800     MOVE 'WLSAPA01 ' TO SSA1                                             
216900     MOVE '  II' TO GODK-STATUSKODER                                      
217000     CALL CBLTDLI USING ISRT SAPA-PCB WLSAPA01 SSA1                       
217100     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
217200     PERFORM IMS-STATUSKONTROLL                                           
217300     .                                                                    
217400     EJECT                                                                
217500 IMS-GU-WDB601    SECTION.                                                
217600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
217700          DELIMITED BY SIZE INTO SSA1                                     
217800     MOVE '  GE' TO GODK-STATUSKODER                                      
217900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
218000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
218100     PERFORM IMS-STATUSKONTROLL                                           
218200     IF SEGMENT-SAKNAS                                                    
218300         MOVE SPACE TO DCS-KDDC                                           
218400     END-IF                                                               
218500     .                                                                    
218600     EJECT                                                                
218700 IMS-STATUSKONTROLL SECTION.                                              
218800                                                                          
218900     SET STATUS-IX TO 1                                                   
219000     SEARCH GODK-STATUS                                                   
219100       AT END                                                             
219200         CALL FELLOG                                                      
219300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
219400         CONTINUE                                                         
219500     END-SEARCH                                                           
219600     .                                                                    
219700     EJECT                                                                
219800*    -COPY WY2000P1                                                       
