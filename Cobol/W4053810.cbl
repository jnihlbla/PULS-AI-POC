000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4053810.                                                
000400*AUTHOR.         LASSI OLGRENER.                                          
000500*DATE-WRITTEN.   93/02/10.                                                
000600                                                                          
000700**   REMARKS.                                                             
000800*                                                                         
000900* Uppdaterar EN fakt i taget man kan bara uppdatera FÖRE release          
001000* Man kan releasa faktura endast EN gång, tullnr ifylld = releasad        
001100*                                                                         
001200*                                                                         
001300*    FUNKTION:                                                            
001400*        VAL AV TULLFAKTUROR                                              
001500*                                                                         
001600*        PROGRAMMET UPPDATERAR WDM7                                       
001700*        PROGRAMMET LÄSER      WDM7A                                      
001800*        PROGRAMMET UPPDATERAR WDM8                                       
001900*        PROGRAMMET UPPDATERAR WDR1 (WDGX4588) Tullid                     
002000*        PROGRAMMET UPPDATERAR WDE6                                       
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSAKTION: W4T538                                              
002400*                     W4T538U                                             
002500*        MID:         W4I53801                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W4O53801                                            
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700 01  FILLER                      PIC X(16)                                
003800                                 VALUE 'CURRENT-SECTION'.                 
003900 01  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
004000                                                                          
004100 01  FILLER                      PIC X(16)                                
004200                                 VALUE 'CURRENT-SECTION2'.                
004300 01  CURRENT-SECTION2            PIC X(16)   VALUE SPACE.                 
004400                                                                          
004500 01  FILLER                      PIC X(16)                                
004600                                 VALUE 'CURR-IMS-SECTION'.                
004700 01  CURR-IMS-SECTION            PIC X(16)   VALUE SPACE.                 
004800                                                                          
004900*    -- CHECKED BY WY2000                                                 
005000 77  IDPGM                       PIC X(08)   VALUE 'W4053810'.            
005100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  YES                         PIC X       VALUE 'Y'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005600 77  MAX-INDX                    PIC S9(4)  VALUE +9    COMP SYNC.        
005700 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005800 77  KDTRPT-IX                   PIC S9(9)  VALUE +0    COMP SYNC.        
005900 77  ANT-SEGM                    PIC S9(3)  VALUE +0    COMP-3.           
006000 77  LAENGD-OMSTART              PIC S9(4)  VALUE +170  COMP SYNC.        
006100 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +682  COMP SYNC.        
006200 77  WS-IDDISTR-NUM              PIC 9(4)    VALUE ZERO.                  
006300 77  WS-IDKUNDNR-NUM             PIC 9(6)    VALUE ZERO.                  
006400 77  WS-KDFRAKT-NUM              PIC 9(2)    VALUE ZERO.                  
006500 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
006600 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
006700 77  WS-KDFRAKT                  PIC X(2)    VALUE SPACE.                 
006800 77  WS-IDSKEPPN                 PIC X(7)    VALUE SPACE.                 
006900 77  WS-IDFAKT                   PIC X(7)    VALUE SPACE.                 
007000 77  WS-FLCONTAIN                PIC X(1)    VALUE SPACE.                 
007100 77  WS-KDTRPTYP                 PIC 9       VALUE ZERO.                  
007200 77  WS-IDLBBET                  PIC X(12)   VALUE SPACE.                 
007300 77  WS-IDBOKN                   PIC X(15)   VALUE SPACE.                 
007400 77  WS-IDFORDREG                PIC X(30)   VALUE SPACE.                 
007500 77  WS-FLKLAR                   PIC X(1)    VALUE 'S'.                   
007600 77  WS-FLKLAR-YES               PIC X(1)    VALUE 'Y'.                   
007700 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007800 77  DAGENS-DATUM-PLUS-SJU-DAGAR PIC 9(6)    VALUE ZERO.                  
007900 77  FOREG-IDFAKT                PIC S9(7)   VALUE ZERO COMP-3.           
008000 77  AKT-FLCONTAIN               PIC X(1)    VALUE SPACE.                 
008100 77  AKT-KDTRPTYP                PIC 9       VALUE ZERO.                  
008200 01  FILLER.                                                              
008300     03  WS-DATUM-TID.                                                    
008400       05 WS-DATUM               PIC X(8)    VALUE SPACE.                 
008500       05 WS-KLOCKAN             PIC 9(10)   VALUE ZERO.                  
008600                                                                          
008700 01  DAGENS-TID                  PIC 9(8)   VALUE ZERO.                   
008800 01  FILLER1 REDEFINES DAGENS-TID.                                        
008900     03 WS-DAGENS-TID            PIC 9(6).                                
009000     03 FILLER                   PIC 9(2).                                
009100 01  FILLER2 REDEFINES DAGENS-TID.                                        
009200     03 WS-DAGENS-TID-HHMM       PIC 9(4).                                
009300     03 FILLER                   PIC 9(4).                                
009400     EJECT                                                                
009500 01  WS-TESTTID                  PIC 9(6)   VALUE ZERO.                   
009600 01  FILLER1 REDEFINES WS-TESTTID.                                        
009700     03 WS-DAGENS-TESTTID        PIC 9(6).                                
009800 01  FILLER2 REDEFINES WS-TESTTID.                                        
009900     03 WS-TESTTID-HHMM          PIC 9(4).                                
010000     03 FILLER                   PIC 9(2).                                
010100     EJECT                                                                
010200 01  WS-IDTULL.                                                           
010300   03  FILLER                    PIC X(2)    VALUE 'VP'.                  
010400   03  WS-IDTULLNR               PIC X(7)    VALUE ZERO.                  
010500   03  WS-RETULKS                PIC X(1)    VALUE ZERO.                  
010600     EJECT                                                                
010700                                                                          
010800 77  RAD-SW                      PIC X       VALUE 'N'.                   
010900     88  INGEN-RAD-VALD                      VALUE 'N'.                   
011000     88  RAD-VALD                            VALUE 'J'.                   
011100                                                                          
011200 77  SEND-SW                     PIC X       VALUE 'N'.                   
011300     88  NOT-SEND                            VALUE 'N'.                   
011400                                                                          
011500 77  UPPDATERA-SW                PIC X       VALUE 'N'.                   
011600     88  UPPDATERA                           VALUE 'J'.                   
011700                                                                          
011800 77  BORTTAG-SW                  PIC X       VALUE 'N'.                   
011900     88  BORTTAG                             VALUE 'J'.                   
012000                                                                          
012100 77  RELEASEA-SW                 PIC X       VALUE 'N'.                   
012200     88  RELEASEA                            VALUE 'J'.                   
012300                                                                          
012400 77  RESTART-SW                  PIC X       VALUE 'N'.                   
012500     88  RESTART                             VALUE 'J'.                   
012600                                                                          
012700 77  OMSTART-SW                  PIC X       VALUE 'N'.                   
012800     88  OMSTART                             VALUE 'J'.                   
012900                                                                          
013000 77  WDM7-BORTTAGEN-SW           PIC X       VALUE 'N'.                   
013100     88  WDM7-BORTTAGEN                      VALUE 'J'.                   
013200                                                                          
013300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
013400     88  INDATA-OK                           VALUE 'J'.                   
013500     88  INDATA-FEL                          VALUE 'N'.                   
013600                                                                          
013700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
013800     88  NYCKLAR-OK                          VALUE 'J'.                   
013900     88  NYCKLAR-FEL                         VALUE 'N'.                   
014000                                                                          
014100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
014200     88  EGEN-MID                            VALUE '4538'.                
014300     88  GODK-MID                            VALUE '4531' '4538'.         
014400     88  HELP-MID                            VALUE '0551'.                
014500                                                                          
014600 01  ALL-SPACE.                                                           
014700     03 FILLER                   PIC X(50)     VALUE SPACE.               
014800 01  ALL-PLUS.                                                            
014900     03 FILLER                   PIC X(50)     VALUE ALL '+'.             
015000                                                                          
015100     EJECT                                                                
015200 01  TEST-IDDISTR     PIC S9(5)        VALUE ZERO  COMP-3.                
015300*01  FILLER     -COPY WWDIST35   -RED  TEST-IDDISTR.                      
015400     EJECT                                                                
015500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
015600 01  GENERELLA-SUBPROGRAM.                                                
015700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
015800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016000     03  CHECK                   PIC X(8)    VALUE 'CHECK   '.            
016100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
016200     EJECT                                                                
016300*01 -COPY WWDCKONS                                                        
016400     EJECT                                                                
016500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
016600*01 -COPY WMSGINIT                                                        
016700     EJECT                                                                
016800*    --- PARAMETRAR TILL SUBPROGRAM CHECK                                 
016900     SKIP3                                                                
017000 01  CHECK-PARM.                                                          
017100*--- TULLID POS 1--4 ÄR FTGID ASCII-FORM V=86, P=80                       
017200*--- TULLID POS 5--11 FYLLS I MED IDTULLNR VID ANROP.                     
017300     03  TULLID                  PIC X(11)   VALUE '8680+TULLNR'.         
017400     03  LGD                     PIC S9(4) COMP SYNC VALUE +11.           
017500     03  WEIGHT                  PIC 9(11)   VALUE 21212121212.           
017600     03  FIGURES                 PIC S9(4) COMP SYNC VALUE +11.           
017700     03  KSIFFR                  PIC X(1).                                
017800     03  MODUL-10-11             PIC 9(2)    VALUE 10.                    
017900     03  ALT-A-B                 PIC X(1)    VALUE 'B'.                   
018000     EJECT                                                                
018100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
018200*01 -COPY WMEDAREA                                                        
018300     SKIP3                                                                
018400 01  MESSAGE-CODES.                                                       
019800     03  INF-TULL-ID-ENDED.                                               
019900       05  FILLER                PIC X(10)   VALUE                        
020000                                          'Tull-id VP'.                   
020100       05  INF-IDTULLNR          PIC X(7).                                
020200       05  INF-RETULKS           PIC X(1).                                
020300       05  FILLER                PIC X(6)    VALUE ' klart'.              
020400     03  INF-IDFAKT-DELETED.                                              
020500       05  FILLER                PIC X(7)    VALUE                        
020600                                          'Faktnr '.                      
020700       05  INF-IDFAKT            PIC X(7).                                
020800       05  FILLER                PIC X(27)   VALUE                        
020900           ' borttaget ur Tullregistret'.                                 
021000     EJECT                                                                
021100                                                                          
021200*    meddelanden för WL01MCNV                                             
021300 01  MESSAGE-CODES.                                                       
021400     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '020'.                 
021500     03  INF-PRESS-PF11          PIC X(3)    VALUE '013'.                 
021600     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '014'.                 
021700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
021800     03  INF-FIRST-PAGE          PIC X(3)    VALUE '010'.                 
021900     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '011'.                 
022000     03  INF-LAST-PAGE           PIC X(3)    VALUE '012'.                 
022100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
022200     03  ERR-INVOICE-MISSING     PIC X(3)    VALUE '025'.                 
022300     03  FORBIDDEN-UPDATE        PIC X(3)    VALUE '007'.                 
022400     03  ERR-TWO-FUNCTIONS       PIC X(3)    VALUE '046'.                 
022500     03  ERR-INFO-MISSING        PIC X(3)    VALUE '041'.                 
022600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
022700*                                                                         
022800 01  FILLER                      PIC X(16)   VALUE 'MSGSOP-AREA'.         
022900 01    MSG-SOP-AREA.                                                      
023000*  03    -COPY WMSGSOP                                                    
023100     SKIP3                                                                
023200*01  -COPY WMFSAREA                                                       
023300     EJECT                                                                
023400     EJECT                                                                
023500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023600     SKIP2                                                                
023700 01  NYCKLAR-TILL-DLI.                                                    
023800     03  W-WDM701KY-X.                                                    
023900         05  W-IDFAKT            PIC S9(7)   VALUE ZERO COMP-3.           
024000         05  W-IDORDNR7          PIC S9(7)   VALUE ZERO COMP-3.           
024100         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
024200         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
024300                                                                          
024400*---     Nyckel till första uppdaterade post av flera                     
024500*---     med samma faktnr                                                 
024600                                                                          
024700     03  W-WDM701KY-FIRST-X.                                              
024800         05  W-IDFAKT-FIRST      PIC S9(7)   VALUE ZERO COMP-3.           
024900         05  W-IDORDNR7-FIRST    PIC S9(7)   VALUE ZERO COMP-3.           
025000         05  W-IDKOLLI-FIRST     PIC S9(5)   VALUE ZERO COMP-3.           
025100         05  W-IDPRODNR-FIRST    PIC S9(7)   VALUE ZERO COMP-3.           
025200                                                                          
025300     03  W-WDM701KY-MIN-X.                                                
025400         05  W-IDFAKT-MIN        PIC S9(7)   VALUE ZERO COMP-3.           
025500         05  W-IDORDNR7-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
025600         05  W-IDKOLLI-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
025700         05  W-IDPRODNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
025800                                                                          
025900     03  W-WDM701KY-MAX-X.                                                
026000         05  W-IDFAKT-MAX        PIC S9(7)   VALUE ZERO COMP-3.           
026100         05  FILLER              PIC X(11)   VALUE HIGH-VALUE.            
026200                                                                          
026300     03  W-WDM7A1KY-MIN-X.                                                
026400         05  W-IDDISTR-A1-MIN    PIC S9(5)   VALUE ZERO COMP-3.           
026500         05  W-IDFAKT-A1-MIN     PIC S9(7)   VALUE ZERO COMP-3.           
026600         05  W-IDORDNR7-A1-MIN   PIC S9(7)   VALUE ZERO COMP-3.           
026700         05  W-IDKOLLI-A1-MIN    PIC S9(5)   VALUE ZERO COMP-3.           
026800         05  W-IDPRODNR-A1-MIN   PIC S9(7)   VALUE ZERO COMP-3.           
026900                                                                          
027000     03  W-WDM7A1KY-MAX-X.                                                
027100         05  W-IDDISTR-A1-MAX    PIC S9(5)   VALUE ZERO COMP-3.           
027200         05  FILLER              PIC X(15)   VALUE HIGH-VALUE.            
027300     EJECT                                                                
027400     03  W-WDM801KY-MIN-X.                                                
027500         05  W8-IDFAKT-MIN       PIC S9(7)   VALUE ZERO COMP-3.           
027600         05  FILLER              PIC X(7)    VALUE LOW-VALUE.             
027700         05  FILLER              PIC X(8)    VALUE LOW-VALUE.             
027800                                                                          
027900     03  W-WDM801KY-MAX-X.                                                
028000         05  W8-IDFAKT-MAX       PIC S9(7)   VALUE ZERO COMP-3.           
028100         05  FILLER              PIC X(7)    VALUE HIGH-VALUE.            
028200         05  FILLER              PIC X(8)    VALUE HIGH-VALUE.            
028300                                                                          
028400     03  W-WDE601KY-X.                                                    
028500         05  W-IDPRODNR-E6       PIC S9(7)   VALUE ZERO COMP-3.           
028600                                                                          
028700     03  W-WDE611KY-X.                                                    
028800         05  W-IDKOLLI-E6        PIC S9(5)   VALUE ZERO COMP-3.           
028900                                                                          
029000     03  W-WDGXKEY-4587-X.                                                
029100         05  FILLER              PIC X(4)    VALUE '4587'.                
029200         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
029300     03  W-KDSEGKEY-X.                                                    
029400         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
029500                                                                          
029600*    --- Sökfält vid läsning                                              
029700     03  W-IDTULLNR              PIC 9(7)    VALUE ZERO.                  
029800     03  W-IDDC                  PIC X(2)    VALUE SPACE.                 
029900                                                                          
030000     EJECT                                                                
030100*    --- STATUS-KOD FRÅN IMS                                              
030200 01  STATUS-WS-M7                PIC XX.                                  
030300     88  SEGMENT-FINNS-M7                    VALUE '  '.                  
030400     88  SEGMENT-FINNS-REDAN-M7              VALUE 'II'.                  
030500     88  SEGMENT-SAKNAS-M7                   VALUE 'GE'.                  
030600     88  BASEN-SLUT-M7                       VALUE 'GB'.                  
030700 01  STATUS-WS-E6                PIC XX.                                  
030800     88  SEGMENT-FINNS-E6                    VALUE '  '.                  
030900     88  SEGMENT-FINNS-REDAN-E6              VALUE 'II'.                  
031000     88  SEGMENT-SAKNAS-E6                   VALUE 'GE'.                  
031100     88  BASEN-SLUT-E6                       VALUE 'GB'.                  
031200                                                                          
031300 01  STATUS-WS                   PIC XX.                                  
031400     88  SEGMENT-FINNS                       VALUE '  '.                  
031500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
031600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
031700     88  BASEN-SLUT                          VALUE 'GB'.                  
031800     SKIP2                                                                
031900 01  GODK-STATUSKODER.                                                    
032000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
032100     SKIP3                                                                
032200 01  SSA1                        PIC X(200).                              
032300 01  SSA2                        PIC X(64).                               
032400     EJECT                                                                
032500*    --- IMS FUNKTIONSKODER                                               
032600*01  -COPY W0003                                                          
032700     EJECT                                                                
032800*    ---  DLI INPUT-OUTPUT AREA                                           
032900 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-AREA'.          
033000 01  DLI-IO-AREA.                                                         
033100*    03  -COPY WDM701                                                     
033200     SKIP3                                                                
033300 01  FILLER                     PIC X(16)  VALUE 'DLI-IO-WDM7A1'.         
033400 01  DLI-IO-WDM7A1.                                                       
033500*    03  -COPY WDM7A1                                                     
033600     EJECT                                                                
033700 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-AREA2'.         
033800 01  DLI-IO-AREA2.                                                        
033900*    03  -COPY WDGX4584                                                   
034000     EJECT                                                                
034100 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-AREA3'.         
034200 01  DLI-IO-AREA3.                                                        
034300*    03  -COPY WDGX4588                                                   
034400     SKIP3                                                                
034500 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDM801'.        
034600 01  DLI-M8-AREA.                                                         
034700*    03  -COPY WDM801                                                     
034800     SKIP3                                                                
034900 01  FILLER                     PIC X(16)  VALUE 'DLI-IO-WDE611'.         
035000 01  DLI-IO-WDE611.                                                       
035100*    03  -COPY WDE611                                                     
035200     EJECT                                                                
035300 LINKAGE SECTION.                                                         
035400                                                                          
035500 01  REQU-AREA.                                                           
035600*    03 -COPY WZ01REQU                                                    
035700*    03 -COPY W40538I1                                                    
035800     EJECT                                                                
035900 01  RESP-AREA.                                                           
036000*    03 -COPY WZ01RESP                                                    
036100*    03 -COPY W40538O1                                                    
036200     EJECT                                                                
036300 01  MAX-KVRADER                 PIC S9(4) COMP.                          
036400*01  -COPY W0009   -PRE ALT-                                              
036500     05  FILLER                  PIC X.                                   
036600*01  -COPY W0008  -PRE WDM7-                                              
036700     05  FILLER                  PIC X.                                   
036800*01  -COPY W0008  -PRE WDM7A-                                             
036900     05  FILLER                  PIC X.                                   
037000*01  -COPY W0008  -PRE WDM8-                                              
037100     05  FILLER                  PIC X.                                   
037200*01  -COPY W0008  -PRE 4587-                                              
037300     05  FILLER                  PIC X.                                   
037400*01  -COPY W0008  -PRE WDE6-                                              
037500     05  FILLER                  PIC X.                                   
037600     EJECT                                                                
037700 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA MAX-KVRADER                
037800                          ALT-PCB                                         
037900                          WDM7-PCB  WDM7A-PCB WDM8-PCB                    
038000                          4587-PCB  WDE6-PCB.                             
038100 MAIN SECTION.                                                            
038200     ENTRY 'DLITCBL' USING REQU-AREA RESP-AREA MAX-KVRADER                
038300                          ALT-PCB                                         
038400                          WDM7-PCB  WDM7A-PCB WDM8-PCB                    
038500                          4587-PCB  WDE6-PCB.                             
038600                                                                          
038700     MOVE FUNCTION CURRENT-DATE (1:8)  TO WS-DATUM                        
038800     MOVE FUNCTION CURRENT-DATE (9:8)  TO WS-KLOCKAN                      
038900                                                                          
039000     PERFORM A-INIT                                                       
039100     PERFORM B-KOLLA-NYCKLAR                                              
039200     IF NYCKLAR-OK                                                        
039300       IF REQU-UPDATE                                                     
039400         PERFORM G-KOLLA-INPUT                                            
039500         IF INDATA-OK                                                     
039600           PERFORM H-UPPDATERA                                            
039700         END-IF                                                           
039800       ELSE                                                               
039900         IF REQU-FIRST                                                    
040000           PERFORM C-FOERSTA-SIDA                                         
040100         ELSE                                                             
040200           IF REQU-NEXT                                                   
040300             PERFORM D-NAESTA-SIDA                                        
040400           ELSE                                                           
040500             PERFORM E-SAMMA-SIDA                                         
040600           END-IF                                                         
040700         END-IF                                                           
040800       END-IF                                                             
040900       IF INDATA-OK                                                       
041000         PERFORM F-LAES-VISA-INFO                                         
041100*        call fellog                                                      
041200       END-IF                                                             
041300     END-IF                                                               
041400                                                                          
041500     MOVE ZERO TO RETURN-CODE                                             
041600     GOBACK                                                               
041700     .                                                                    
041800     EJECT                                                                
041900 A-INIT SECTION.                                                          
042000                                                                          
042100     ACCEPT DAGENS-DATUM FROM DATE                                        
042200     ACCEPT DAGENS-TID   FROM TIME                                        
042300                                                                          
042400     MOVE ALL '+'                TO RESP-W40538O1                         
042500                                                                          
042600     MOVE 001                    TO RESP-IDMSGVER                         
042700     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
042800                                    RESP-IDMSG-INFO                       
042900                                    RESP-IDELMT-ERROR                     
043000                                                                          
043100     MOVE ZERO                   TO RESP-KVRADER                          
043200                                                                          
043300     MOVE REQU-IDFAKT-START      TO RESP-IDFAKT-START                     
043400     MOVE REQU-IDORDNR5-START    TO RESP-IDORDNR5-START                   
043500     MOVE REQU-IDKOLLI-START     TO RESP-IDKOLLI-START                    
043600     MOVE REQU-IDPRODNR-START    TO RESP-IDPRODNR-START                   
043700     MOVE REQU-IDDC-START        TO RESP-IDDC-START                       
043800     MOVE REQU-IDDISTR-START     TO RESP-IDDISTR-START                    
043900     MOVE ZERO                   TO RESP-IDFAKT-NEXT                      
044000                                    RESP-IDORDNR5-NEXT                    
044100                                    RESP-IDKOLLI-NEXT                     
044200                                    RESP-IDPRODNR-NEXT                    
044300                                    RESP-IDDISTR-NEXT                     
044400     MOVE SPACE                  TO RESP-IDDC-NEXT                        
044500                                                                          
044600     MOVE '4538' TO MSGSOP-IDTRANS                                        
044700     MOVE MFS-KDMFSFOR TO MSGSOP-KDMFSFOR                                 
044800     MOVE 'W476SA' TO MSGSOP-IDPROCESS                                    
044900     MOVE 'O' TO MSGSOP-KDSOPFUNK                                         
045000                                                                          
045100     PERFORM MFS-RENSA-FAELT-UT                                           
045200     PERFORM MFS-FORM-ATTR                                                
045300                                                                          
045400     .                                                                    
045500     EJECT                                                                
045600 B-KOLLA-NYCKLAR SECTION.                                                 
045700                                                                          
045800     MOVE 'B-KOLLA-NYCK'     TO CURRENT-SECTION                           
045900                                                                          
046000     MOVE LOW-VALUE          TO W-WDM701KY-MIN-X                          
046100                                W-WDM7A1KY-MIN-X                          
046200     MOVE HIGH-VALUE         TO W-WDM7A1KY-MAX-X                          
046300                                                                          
046400     IF REQU-IDFAKT-KEY = ALL '+'                                         
046500       MOVE ZERO             TO REQU-IDFAKT-KEY                           
046600     END-IF                                                               
046700                                                                          
046800     IF REQU-IDDISTR-KEY = ALL '+'                                        
046900       MOVE ZERO             TO REQU-IDDISTR-KEY                          
047000     END-IF                                                               
047100                                                                          
047200                                                                          
047300     INSPECT REQU-IDFAKT-KEY REPLACING LEADING SPACE BY ZERO              
047400                                                                          
047500     IF REQU-IDFAKT-KEY NUMERIC AND REQU-IDFAKT-KEY > ZERO                
047600       MOVE REQU-IDFAKT-KEY    TO W-IDFAKT-MIN                            
047700                                  W-IDFAKT-MAX                            
047800                                  W8-IDFAKT-MIN                           
047900                                  W8-IDFAKT-MAX                           
048000     ELSE                                                                 
048100*---   IDFAKT obligatorisk vid uppdatering                                
048200       IF REQU-UPDATE                                                     
048300         MOVE NEJ              TO NYCKLAR-SW                              
048400         MOVE 'IDFAKT'         TO RESP-IDELMT-ERROR                       
048500       ELSE                                                               
048600           PERFORM BA-KOLLA-OVRIGA-NYCKLAR                                
048700                                                                          
048800       END-IF                                                             
048900     END-IF                                                               
049000                                                                          
049100     MOVE REQU-IDDC-KEY        TO W-IDDC                                  
049200                                                                          
049300     IF NYCKLAR-FEL                                                       
049400*---FÖR ATT INTE FÅ NYCKLAR FEL NÄR MAN KOMMER FRÅN EN ICKE               
049500*---GODKÄND BILD                                                          
049600                                                                          
049700       MOVE ERR-INVOICE-MISSING  TO RESP-IDMSG-ERROR                      
049800       MOVE 'KEY'                TO RESP-IDELMT-ERROR                     
049900       MOVE ZERO                 TO RESP-KVRADER                          
050000       PERFORM MFS-RENSA-FAELT-IN                                         
050100       PERFORM MFS-RENSA-FAELT-UT                                         
050200     END-IF                                                               
050300                                                                          
050400     .                                                                    
050500     EJECT                                                                
050600 BA-KOLLA-OVRIGA-NYCKLAR SECTION.                                         
050700                                                                          
050800*--- om ej idfakt ifylld måste iddistr vara det                           
050900                                                                          
051000     MOVE 'BA-KOLLA-OVR '     TO CURRENT-SECTION                          
051100                                                                          
051200     IF REQU-IDDISTR-KEY NUMERIC AND REQU-IDDISTR-KEY > ZERO              
051300       MOVE REQU-IDDISTR-KEY   TO W-IDDISTR-A1-MIN                        
051400                                  W-IDDISTR-A1-MAX                        
051500     ELSE                                                                 
051600       MOVE NEJ                TO NYCKLAR-SW                              
051700       MOVE 'IDDISTR'          TO RESP-IDELMT-ERROR                       
051800     END-IF                                                               
051900                                                                          
052000     .                                                                    
052100     EJECT                                                                
052200 C-FOERSTA-SIDA SECTION.                                                  
052300                                                                          
052400     MOVE 'C-FOERSTA-SID'     TO CURRENT-SECTION                          
052500     MOVE INF-FIRST-PAGE         TO RESP-IDMSG-INFO                       
052600                                                                          
052700     PERFORM MFS-RENSA-FAELT-IN                                           
052800                                                                          
052900     .                                                                    
053000                                                                          
053100 D-NAESTA-SIDA SECTION.                                                   
053200                                                                          
053300     MOVE 'D-NAESTA-SID'     TO CURRENT-SECTION                           
053400                                                                          
053500*--- Övr nycklar ifyllda i B-                                             
053600     MOVE REQU-IDFAKT-START   TO W-IDFAKT-A1-MIN                          
053700     MOVE REQU-IDORDNR5-START TO W-IDORDNR7-MIN                           
053800                                 W-IDORDNR7-A1-MIN                        
053900     MOVE REQU-IDKOLLI-START  TO W-IDKOLLI-MIN                            
054000                                 W-IDKOLLI-A1-MIN                         
054100     MOVE REQU-IDPRODNR-START TO W-IDPRODNR-MIN                           
054200                                 W-IDPRODNR-A1-MIN                        
054300                                                                          
054400     PERFORM S03-OVR-REQU-TILL-RESP                                       
054500                                                                          
054600     PERFORM MFS-RENSA-FAELT-IN                                           
054700     .                                                                    
054800     EJECT                                                                
054900                                                                          
055000                                                                          
055100 E-SAMMA-SIDA SECTION.                                                    
055200                                                                          
055300     MOVE 'E-SAMMA-SIDA'    TO CURRENT-SECTION                            
055400                                                                          
055500     IF REQU-IDFAKT-START NOT = ALL '+' AND                               
055600        REQU-IDFAKT-START NUMERIC                                         
055700        MOVE REQU-IDFAKT-START TO W-IDFAKT-A1-MIN                         
055800     ELSE                                                                 
055900        MOVE ZERO              TO W-IDFAKT-A1-MIN                         
056000     END-IF                                                               
056100                                                                          
056200     IF REQU-IDORDNR5-START NOT = ALL '+' AND                             
056300        REQU-IDORDNR5-START NUMERIC                                       
056400        MOVE REQU-IDORDNR5-START TO W-IDORDNR7-MIN                        
056500                                    W-IDORDNR7-A1-MIN                     
056600     ELSE                                                                 
056700        MOVE ZERO              TO W-IDORDNR7-MIN                          
056800                                  W-IDORDNR7-A1-MIN                       
056900     END-IF                                                               
057000                                                                          
057100     IF REQU-IDKOLLI-START NOT = ALL '+' AND                              
057200        REQU-IDKOLLI-START NUMERIC                                        
057300        MOVE REQU-IDKOLLI-START TO W-IDKOLLI-MIN                          
057400                                   W-IDKOLLI-A1-MIN                       
057500     ELSE                                                                 
057600        MOVE ZERO              TO W-IDKOLLI-MIN                           
057700                                  W-IDKOLLI-A1-MIN                        
057800     END-IF                                                               
057900                                                                          
058000     IF REQU-IDPRODNR-START NOT = ALL '+' AND                             
058100        REQU-IDPRODNR-START NUMERIC                                       
058200        MOVE REQU-IDPRODNR-START TO W-IDPRODNR-MIN                        
058300                                    W-IDPRODNR-A1-MIN                     
058400     ELSE                                                                 
058500        MOVE ZERO              TO W-IDPRODNR-MIN                          
058600                                  W-IDPRODNR-A1-MIN                       
058700     END-IF                                                               
058800                                                                          
058900     IF REQU-IDDISTR-START NOT = ALL '+' AND                              
059000        REQU-IDDISTR-START NUMERIC                                        
059100        MOVE REQU-IDDISTR-START TO W-IDDISTR-A1-MIN                       
059200     ELSE                                                                 
059300        MOVE ZERO               TO W-IDDISTR-A1-MIN                       
059400     END-IF                                                               
059500                                                                          
059600     IF REQU-FLSLUT   = ALL '+' AND                                       
059700        REQU-FLBORT   = ALL '+' AND                                       
059800        REQU-KDTRPTYP = ALL '+' AND                                       
059900        REQU-FLCONTAIN = ALL '+' AND                                      
060000       (REQU-IDLBBET  = ALL '+' OR SPACE) AND                             
060100       (REQU-IDBOKN   = ALL '+' OR SPACE) AND                             
060200       (REQU-IDFORDREG = ALL '+' OR SPACE)                                
060300       PERFORM MFS-RENSA-FAELT-IN                                         
060400       MOVE REQU-FLCONTAIN-UT  TO RESP-FLCONTAIN-UT                       
060500       MOVE REQU-IDLBBET-UT    TO RESP-IDLBBET-UT                         
060600       MOVE REQU-KDTRPTYP-UT   TO RESP-KDTRPTYP-UT                        
060700       MOVE REQU-IDBOKN-UT     TO RESP-IDBOKN-UT                          
060800*      MOVE REQU-IDFORDREG-UT  TO RESP-IDFORDREG-UT                       
060900     ELSE                                                                 
061000       MOVE INF-PRESS-PF11 TO RESP-IDMSG-INFO                             
061100       PERFORM EA-REQU-INDATA-TILL-MOD                                    
061200     END-IF                                                               
061300     .                                                                    
061400     EJECT                                                                
061500 EA-REQU-INDATA-TILL-MOD SECTION.                                         
061600                                                                          
061700     MOVE 'EA-REQU-INDATA'     TO CURRENT-SECTION                         
061800                                                                          
061900     MOVE REQU-IDFAKT-START    TO RESP-IDFAKT-START                       
062000     MOVE REQU-IDORDNR5-START  TO RESP-IDORDNR5-START                     
062100     MOVE REQU-IDKOLLI-START   TO RESP-IDKOLLI-START                      
062200     MOVE REQU-IDPRODNR-START  TO RESP-IDPRODNR-START                     
062300     MOVE REQU-IDDC-START      TO RESP-IDDC-START                         
062400     MOVE REQU-IDDISTR-START   TO RESP-IDDISTR-START                      
062500                                                                          
062600     MOVE REQU-FLCONTAIN-UT    TO RESP-FLCONTAIN-UT                       
062700     MOVE REQU-IDLBBET-UT      TO RESP-IDLBBET-UT                         
062800     MOVE REQU-KDTRPTYP-UT     TO RESP-KDTRPTYP-UT                        
062900     MOVE REQU-IDBOKN-UT       TO RESP-IDBOKN-UT                          
063000*    MOVE REQU-IDFORDREG-UT    TO RESP-IDFORDREG-UT                       
063100     PERFORM S03-OVR-REQU-TILL-RESP                                       
063200                                                                          
063300     IF REQU-FLSLUT NOT = '+'                                             
063400       MOVE REQU-FLSLUT        TO RESP-FLSLUT                             
063500       MOVE MFS-ADD-LAES-IN-FAELT                                         
063600                               TO RESP-FLSLUT-ATTR                        
063700     ELSE                                                                 
063800       MOVE ALL-SPACE          TO RESP-FLSLUT                             
063900     END-IF                                                               
064000                                                                          
064100     IF REQU-FLBORT NOT = '+'                                             
064200       MOVE REQU-FLBORT        TO RESP-FLBORT                             
064300       MOVE MFS-ADD-LAES-IN-FAELT                                         
064400                               TO RESP-FLBORT-ATTR                        
064500     ELSE                                                                 
064600       MOVE ALL-SPACE          TO RESP-FLBORT                             
064700     END-IF                                                               
064800                                                                          
064900     PERFORM MFS-LAES-IN-IGEN                                             
065000                                                                          
065100     .                                                                    
065200     EJECT                                                                
065300 F-LAES-VISA-INFO SECTION.                                                
065400                                                                          
065500     MOVE 'F-LAES-VISA-INFO'   TO CURRENT-SECTION                         
065600                                                                          
065700*-- Vid sökning på Faktura visas raddata                                  
065800*-- Vid sökning på Distrikt visas en rad per faktura                      
065900                                                                          
066000     IF REQU-IDFAKT-KEY = SPACE OR ZERO                                   
066100       PERFORM FA-LAES-VID-SOEK-DISTR                                     
066200     ELSE                                                                 
066300       PERFORM FB-LAES-VID-SOEK-FAKT                                      
066400     END-IF                                                               
066500     .                                                                    
066600     eject                                                                
066700                                                                          
066800 FA-LAES-VID-SOEK-DISTR     SECTION.                                      
066900                                                                          
067000     MOVE 'FA-LAES-VID-S'        TO CURRENT-SECTION                       
067100                                                                          
067200     MOVE ZERO                   TO RESP-KVRADER                          
067300     PERFORM FAA-LAES-RADDATA                                             
067400                                                                          
067500     IF SEGMENT-SAKNAS OR BASEN-SLUT                                      
067600       MOVE ERR-INVOICE-MISSING  TO RESP-IDMSG-ERROR                      
067700       MOVE 'KEY'                TO RESP-IDELMT-ERROR                     
067800       PERFORM MFS-RENSA-FAELT-IN                                         
067900       PERFORM MFS-RENSA-FAELT-UT                                         
068000       IF REQU-UPDATE                                                     
068100         MOVE REQU-FLCONTAIN-UT     TO RESP-FLCONTAIN-UT                  
068200         MOVE REQU-IDLBBET-UT       TO RESP-IDLBBET-UT                    
068300         MOVE REQU-KDTRPTYP-UT      TO RESP-KDTRPTYP-UT                   
068400         MOVE REQU-IDBOKN-UT        TO RESP-IDBOKN-UT                     
068500*        MOVE REQU-IDFORDREG-UT     TO RESP-IDFORDREG-UT                  
068600         PERFORM S03-OVR-REQU-TILL-RESP                                   
068700       END-IF                                                             
068800     ELSE                                                                 
068900*---   Tullpost funnen                                                    
069000*---   om man sökt på dist, ska alla inmatn.fält stängas                  
069100       MOVE MFS-STAENG-FAELT        TO RESP-KDTRPTYP-ATTR                 
069200                                       RESP-FLCONTAIN-ATTR                
069300                                       RESP-IDLBBET-ATTR                  
069400                                       RESP-IDBOKN-ATTR                   
069500                                       RESP-IDFORDREG-ATTR                
069600                                       RESP-FLSLUT-ATTR                   
069700                                       RESP-FLBORT-ATTR                   
069800       MOVE ALL-SPACE               TO RESP-KDTRPTYP-UT                   
069900                                       RESP-FLCONTAIN-UT                  
070000                                       RESP-IDLBBET-UT                    
070100                                       RESP-IDBOKN-UT                     
070200*                                      RESP-IDFORDREG-UT                  
070300                                       RESP-IDFORDREG                     
070400                                                                          
070500       MOVE SEQA-IDFAKT             TO RESP-IDFAKT-START                  
070600       MOVE SEQA-IDORDNR7           TO RESP-IDORDNR5-START                
070700       MOVE SEQA-IDKOLLI            TO RESP-IDKOLLI-START                 
070800       MOVE SEQA-IDPRODNR           TO RESP-IDPRODNR-START                
070900       MOVE SEQA-IDDC               TO RESP-IDDC-START                    
071000       MOVE SEQA-IDDISTR            TO RESP-IDDISTR-START                 
071100                                                                          
071200       MOVE +1 TO INDX                                                    
071300       PERFORM UNTIL INDX > MAX-KVRADER                                   
071400         IF SEGMENT-FINNS                                                 
071500           MOVE SEQA-KDFAKTYP       TO RESP-IDFAKT  (INDX)                
071600           MOVE '-'                 TO RESP-IDFAKT  (INDX) (2:1)          
071700           MOVE SEQA-IDFAKT         TO RESP-IDFAKT  (INDX) (3:7)          
071800           MOVE SEQA-TIFAKT         TO RESP-TIFAKT  (INDX)                
071900           MOVE ALL-SPACE           TO RESP-IDDISTR (INDX)                
072000                                       RESP-IDKUNDNR(INDX)                
072100                                       RESP-IDORDNR5(INDX)                
072200                                       RESP-IDKOLLI (INDX)                
072300                                       RESP-KDORDKL (INDX)                
072400                                       RESP-FLORDSPE(INDX)                
072500                                       RESP-IDPRODNR(INDX)                
072600                                       RESP-TIREGDAT(INDX)                
072700                                       RESP-TIREGTID(INDX)                
072800                                       RESP-FLKLAR(INDX)                  
072900                                                                          
073000           PERFORM FAA-LAES-RADDATA                                       
073100                                                                          
073200*----      RESP-KVRADER är de antal rader som är ifyllda                  
073300*----      MAX-KVRADER  är fast 500 (web), 9 (mainfr)                     
073400                                                                          
073500           ADD +1                  TO RESP-KVRADER                        
073600                                                                          
073700         ELSE                                                             
073800           MOVE ALL-SPACE           TO RESP-IDFAKT (INDX)                 
073900                                       RESP-TIFAKT (INDX)                 
074000                                       RESP-IDDISTR (INDX)                
074100                                       RESP-IDKUNDNR(INDX)                
074200                                       RESP-IDORDNR5(INDX)                
074300                                       RESP-IDKOLLI (INDX)                
074400                                       RESP-KDORDKL (INDX)                
074500                                       RESP-FLORDSPE(INDX)                
074600                                       RESP-IDPRODNR(INDX)                
074700                                       RESP-TIREGDAT(INDX)                
074800                                       RESP-TIREGTID(INDX)                
074900                                       RESP-FLKLAR(INDX)                  
075000         END-IF                                                           
075100         ADD 1 TO INDX                                                    
075200                                                                          
075300       END-PERFORM                                                        
075400                                                                          
075500       IF SEGMENT-FINNS                                                   
075600         MOVE SEQA-IDFAKT           TO RESP-IDFAKT-NEXT                   
075700         MOVE SEQA-IDORDNR7         TO RESP-IDORDNR5-NEXT                 
075800         MOVE SEQA-IDKOLLI          TO RESP-IDKOLLI-NEXT                  
075900         MOVE SEQA-IDPRODNR         TO RESP-IDPRODNR-NEXT                 
076000         MOVE SEQA-IDDC             TO RESP-IDDC-NEXT                     
076100         MOVE SEQA-IDDISTR          TO RESP-IDDISTR-NEXT                  
076200                                                                          
076300         IF RESP-IDMSG-INFO = SPACE                                       
076400           MOVE INF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO                   
076500         END-IF                                                           
076600       ELSE                                                               
076700         MOVE RESP-IDFAKT-START     TO RESP-IDFAKT-NEXT                   
076800         MOVE RESP-IDORDNR5-START   TO RESP-IDORDNR5-NEXT                 
076900         MOVE RESP-IDKOLLI-START    TO RESP-IDKOLLI-NEXT                  
077000         MOVE RESP-IDPRODNR-START   TO RESP-IDPRODNR-NEXT                 
077100         MOVE RESP-IDDC-START       TO RESP-IDDC-NEXT                     
077200         MOVE RESP-IDDISTR-START    TO RESP-IDDISTR-NEXT                  
077300                                                                          
077400         IF RESP-IDMSG-INFO = SPACE                                       
077500           MOVE INF-LAST-PAGE TO RESP-IDMSG-INFO                          
077600         END-IF                                                           
077700       END-IF                                                             
077800     END-IF                                                               
077900     .                                                                    
078000                                                                          
078100 FAA-LAES-RADDATA SECTION.                                                
078200                                                                          
078300     MOVE 'FAA-LAES-RAD  '        TO CURRENT-SECTION                      
078400*---   Om IDDISTR ifyllt:                                                 
078500*---   Visa en rad per faktura i WDM7 för visst IDDISTR                   
078600*---   sålla bort de releasade och de med annat IDDC                      
078700                                                                          
078800                                                                          
078900     PERFORM S01B-LAES-GN-WDM7A                                           
079000                                                                          
079100     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
079200                   BASEN-SLUT OR                                          
079300                   FOREG-IDFAKT NOT = SEQA-IDFAKT                         
079400       PERFORM S01B-LAES-GN-WDM7A                                         
079500     END-PERFORM                                                          
079600                                                                          
079700     IF SEGMENT-FINNS AND                                                 
079800        FOREG-IDFAKT NOT = SEQA-IDFAKT                                    
079900       MOVE SEQA-IDFAKT         TO FOREG-IDFAKT                           
080000     END-IF                                                               
080100     .                                                                    
080200                                                                          
080300                                                                          
080400     eject                                                                
080500                                                                          
080600 FB-LAES-VID-SOEK-FAKT      SECTION.                                      
080700                                                                          
080800     MOVE 'FB-LAES-VID  '        TO CURRENT-SECTION                       
080900     MOVE ZERO                   TO RESP-KVRADER                          
081000                                                                          
081100*--  om vi uppdaterat/releasat igenom WDM7                                
081200*--  positionerar vi oss till första aktuella fakturaposten               
081300                                                                          
081400     IF REQU-UPDATE                                                       
081500       PERFORM IMS-GU-WDM701-FIRST                                        
081600     ELSE                                                                 
081700       PERFORM S01A-LAES-GHN-WDM701                                       
081800     END-IF                                                               
081900                                                                          
082000     IF SEGMENT-SAKNAS OR BASEN-SLUT                                      
082100       MOVE ERR-INVOICE-MISSING  TO RESP-IDMSG-ERROR                      
082200       MOVE 'KEY'                TO RESP-IDELMT-ERROR                     
082300       PERFORM MFS-RENSA-FAELT-IN                                         
082400       PERFORM MFS-RENSA-FAELT-UT                                         
082500       IF REQU-UPDATE                                                     
082600         MOVE REQU-FLCONTAIN-UT     TO RESP-FLCONTAIN-UT                  
082700         MOVE REQU-IDLBBET-UT       TO RESP-IDLBBET-UT                    
082800         MOVE REQU-KDTRPTYP-UT      TO RESP-KDTRPTYP-UT                   
082900         MOVE REQU-IDBOKN-UT        TO RESP-IDBOKN-UT                     
083000*        MOVE REQU-IDFORDREG-UT     TO RESP-IDFORDREG-UT                  
083100         PERFORM S03-OVR-REQU-TILL-RESP                                   
083200       END-IF                                                             
083300     ELSE                                                                 
083400*---   Tullpost funnen                                                    
083500       IF HUV-IDTULLNR > ZERO                                             
083600         PERFORM FBA-CLOSE-ALL-INPUT-FIELDS                               
083700       END-IF                                                             
083800                                                                          
083900       IF HUV-KDTRPTYP > ZERO                                             
084000         MOVE HUV-KDTRPTYP          TO RESP-KDTRPTYP-UT                   
084100       ELSE                                                               
084200         MOVE ALL-SPACE             TO RESP-KDTRPTYP-UT                   
084300       END-IF                                                             
084400       MOVE HUV-FLCONTAIN           TO RESP-FLCONTAIN-UT                  
084500       MOVE HUV-IDLBBET             TO RESP-IDLBBET-UT                    
084600       MOVE HUV-IDBOKN              TO RESP-IDBOKN-UT                     
084700       MOVE HUV-IDFORDREG           TO RESP-IDFORDREG                     
084800                                                                          
084900       IF REQU-UPDATE                                                     
085000         PERFORM MFS-RENSA-FAELT-IN                                       
085100       END-IF                                                             
085200                                                                          
085300       MOVE HUV-IDFAKT              TO RESP-IDFAKT-START                  
085400       MOVE HUV-IDORDNR7            TO RESP-IDORDNR5-START                
085500       MOVE HUV-IDKOLLI             TO RESP-IDKOLLI-START                 
085600       MOVE HUV-IDPRODNR            TO RESP-IDPRODNR-START                
085700       MOVE HUV-IDDC                TO RESP-IDDC-START                    
085800       MOVE HUV-IDDISTR             TO RESP-IDDISTR-START                 
085900                                                                          
086000       MOVE +1 TO INDX                                                    
086100       PERFORM UNTIL INDX > MAX-KVRADER                                   
086200         IF SEGMENT-FINNS                                                 
086300           MOVE HUV-KDFAKTYP        TO RESP-IDFAKT(INDX)                  
086400           MOVE '-'                 TO RESP-IDFAKT(INDX) (2:1)            
086500           MOVE HUV-IDFAKT          TO RESP-IDFAKT (INDX) (3:7)           
086600           MOVE HUV-TIFAKT          TO RESP-TIFAKT (INDX)                 
086700           IF REQU-IDFAKT-KEY = SPACE OR ZERO                             
086800             MOVE ALL-SPACE         TO RESP-IDDISTR (INDX)                
086900                                       RESP-IDKUNDNR(INDX)                
087000                                       RESP-IDORDNR5(INDX)                
087100                                       RESP-IDKOLLI (INDX)                
087200                                       RESP-KDORDKL (INDX)                
087300                                       RESP-FLORDSPE(INDX)                
087400                                       RESP-IDPRODNR(INDX)                
087500                                       RESP-TIREGDAT(INDX)                
087600                                       RESP-TIREGTID(INDX)                
087700                                       RESP-FLKLAR(INDX)                  
087800           ELSE                                                           
087900             MOVE HUV-IDDISTR       TO RESP-IDDISTR (INDX)                
088000             MOVE HUV-IDKUNDNR      TO RESP-IDKUNDNR(INDX)                
088100             MOVE HUV-IDORDNR7      TO RESP-IDORDNR5(INDX)                
088200             MOVE HUV-IDKOLLI       TO RESP-IDKOLLI (INDX)                
088300             MOVE HUV-KDORDKL       TO RESP-KDORDKL (INDX)                
088400             MOVE HUV-FLORDSPE      TO RESP-FLORDSPE(INDX)                
088500             MOVE HUV-IDPRODNR      TO RESP-IDPRODNR(INDX)                
088600             MOVE HUV-TIREGDAT      TO RESP-TIREGDAT(INDX)                
088700*            MOVE HUV-TIREGTID      TO RESP-TIREGTID(INDX)                
088800             MOVE HUV-TIREGTID      TO WS-TESTTID                         
088900             MOVE WS-TESTTID-HHMM   TO RESP-TIREGTID(INDX)                
089000             MOVE HUV-FLKLAR        TO RESP-FLKLAR  (INDX)                
089100           END-IF                                                         
089200                                                                          
089300           PERFORM S01A-LAES-GHN-WDM701                                   
089400                                                                          
089500*----      RESP-KVRADER är de antal rader som är ifyllda                  
089600*----      MAX-KVRADER  är fast 500 (web), 9 (mainfr)                     
089700                                                                          
089800           ADD +1                  TO RESP-KVRADER                        
089900                                                                          
090000         ELSE                                                             
090100           MOVE ALL-SPACE           TO RESP-IDFAKT (INDX)                 
090200                                       RESP-TIFAKT (INDX)                 
090300                                       RESP-IDDISTR (INDX)                
090400                                       RESP-IDKUNDNR(INDX)                
090500                                       RESP-IDORDNR5(INDX)                
090600                                       RESP-IDKOLLI (INDX)                
090700                                       RESP-KDORDKL (INDX)                
090800                                       RESP-FLORDSPE(INDX)                
090900                                       RESP-IDPRODNR(INDX)                
091000                                       RESP-TIREGDAT(INDX)                
091100                                       RESP-TIREGTID(INDX)                
091200                                       RESP-FLKLAR  (INDX)                
091300         END-IF                                                           
091400         ADD 1 TO INDX                                                    
091500                                                                          
091600       END-PERFORM                                                        
091700                                                                          
091800       IF SEGMENT-FINNS                                                   
091900         MOVE HUV-IDFAKT            TO RESP-IDFAKT-NEXT                   
092000         MOVE HUV-IDORDNR7          TO RESP-IDORDNR5-NEXT                 
092100         MOVE HUV-IDKOLLI           TO RESP-IDKOLLI-NEXT                  
092200         MOVE HUV-IDPRODNR          TO RESP-IDPRODNR-NEXT                 
092300         MOVE HUV-IDDC              TO RESP-IDDC-NEXT                     
092400         MOVE HUV-IDDISTR           TO RESP-IDDISTR-NEXT                  
092500                                                                          
092600         IF RESP-IDMSG-INFO = SPACE                                       
092700           MOVE INF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO                   
092800         END-IF                                                           
092900       ELSE                                                               
093000         MOVE RESP-IDFAKT-START     TO RESP-IDFAKT-NEXT                   
093100         MOVE RESP-IDORDNR5-START   TO RESP-IDORDNR5-NEXT                 
093200         MOVE RESP-IDKOLLI-START    TO RESP-IDKOLLI-NEXT                  
093300         MOVE RESP-IDPRODNR-START   TO RESP-IDPRODNR-NEXT                 
093400         MOVE RESP-IDDC-START       TO RESP-IDDC-NEXT                     
093500         MOVE RESP-IDDISTR-START    TO RESP-IDDISTR-NEXT                  
093600                                                                          
093700         IF RESP-IDMSG-INFO = SPACE                                       
093800           MOVE INF-LAST-PAGE TO RESP-IDMSG-INFO                          
093900         END-IF                                                           
094000       END-IF                                                             
094100     END-IF                                                               
094200     .                                                                    
094300     EJECT                                                                
094400 FBA-CLOSE-ALL-INPUT-FIELDS SECTION.                                      
094500                                                                          
094600     MOVE 'FBA-CLOSE  '        TO CURRENT-SECTION                         
094700*--- om man sökt på dist, ska alla inmatn.fält stängas                    
094800     MOVE MFS-STAENG-FAELT          TO RESP-KDTRPTYP-ATTR                 
094900                                       RESP-FLCONTAIN-ATTR                
095000                                       RESP-IDLBBET-ATTR                  
095100                                       RESP-IDBOKN-ATTR                   
095200                                       RESP-IDFORDREG-ATTR                
095300                                       RESP-FLSLUT-ATTR                   
095400                                       RESP-FLBORT-ATTR                   
095500     .                                                                    
095600                                                                          
095700 G-KOLLA-INPUT SECTION.                                                   
095800                                                                          
095900     MOVE 'G-KOLLA-INPUT'        TO CURRENT-SECTION                       
096000                                                                          
096100*--- Vid F11 får man antingen:                                            
096200*--- 1. Uppdatera inmatad info                                            
096300*--- 2. Releasa faktura                                                   
096400*--- 3. Ta bort faktura           (OK end om ej TULLNR-stämplad)          
096500                                                                          
096600     MOVE JA  TO INDATA-SW                                                
096700                                                                          
096800     IF REQU-INPUT = ALL '+' OR SPACE                                     
096900       MOVE ERR-PF11-AND-NO-DATA TO RESP-IDMSG-ERROR                      
097000       PERFORM MFS-ROER-EJ-ALLA-FAELT                                     
097100       MOVE NEJ                  TO INDATA-SW                             
097200     ELSE                                                                 
097300                                                                          
097400       IF REQU-FLSLUT = '+' OR JA OR YES OR NEJ OR SPACE                  
097500         IF REQU-FLSLUT = JA OR YES                                       
097600           MOVE JA               TO RELEASEA-SW                           
097700         END-IF                                                           
097800       ELSE                                                               
097900                                                                          
098000         PERFORM MFS-LAES-IN-IGEN                                         
098100         MOVE MFS-ALFA-FAELT-FEL TO RESP-FLSLUT-ATTR                      
098200         MOVE NEJ                TO INDATA-SW                             
098300       END-IF                                                             
098400                                                                          
098500       IF REQU-FLBORT = '+' OR 'B' OR 'D' OR SPACE                        
098600         IF REQU-FLBORT = 'B' OR 'D'                                      
098700           MOVE JA               TO BORTTAG-SW                            
098800         END-IF                                                           
098900       ELSE                                                               
099000                                                                          
099100         PERFORM MFS-LAES-IN-IGEN                                         
099200         MOVE MFS-ALFA-FAELT-FEL TO RESP-FLBORT-ATTR                      
099300         MOVE NEJ                TO INDATA-SW                             
099400       END-IF                                                             
099500                                                                          
099600*--- Är avsikten att uppdatera tulldata?                                  
099700                                                                          
099800       IF (REQU-KDTRPTYP  = ALL '+' OR = SPACE) AND                       
099900          (REQU-FLCONTAIN = ALL '+' OR = SPACE) AND                       
100000          (REQU-IDLBBET   = ALL '+' OR = SPACE) AND                       
100100          (REQU-IDBOKN    = ALL '+' OR = SPACE) AND                       
100200          (REQU-IDFORDREG = ALL '+' OR = SPACE)                           
100300         CONTINUE                                                         
100400       ELSE                                                               
100500         MOVE JA                 TO UPPDATERA-SW                          
100600       END-IF                                                             
100700                                                                          
100800       IF INDATA-OK                                                       
100900         IF BORTTAG                                                       
101000           IF RELEASEA OR UPPDATERA                                       
101100             MOVE ERR-TWO-FUNCTIONS    TO RESP-IDMSG-ERROR                
101200                                                                          
101300             PERFORM MFS-LAES-IN-IGEN                                     
101400             PERFORM MFS-ROER-EJ-ALLA-FAELT                               
101500             MOVE NEJ            TO INDATA-SW                             
101600           END-IF                                                         
101700         ELSE                                                             
101800           IF RELEASEA                                                    
101900             IF BORTTAG OR UPPDATERA                                      
102000                                                                          
102100               PERFORM MFS-LAES-IN-IGEN                                   
102200               MOVE ERR-TWO-FUNCTIONS  TO RESP-IDMSG-ERROR                
102300               PERFORM MFS-ROER-EJ-ALLA-FAELT                             
102400               MOVE NEJ          TO INDATA-SW                             
102500             ELSE                                                         
102600               PERFORM GB-KOLL-VID-RELEASE                                
102700             END-IF                                                       
102800           ELSE                                                           
102900             IF UPPDATERA                                                 
103000               IF BORTTAG OR RELEASEA                                     
103100                                                                          
103200                 PERFORM MFS-LAES-IN-IGEN                                 
103300                 MOVE ERR-TWO-FUNCTIONS TO RESP-IDMSG-ERROR               
103400                 PERFORM MFS-ROER-EJ-ALLA-FAELT                           
103500                 MOVE NEJ         TO INDATA-SW                            
103600               ELSE                                                       
103700                 PERFORM GA-KOLL-VID-UPPDAT                               
103800               END-IF                                                     
103900             ELSE                                                         
104000*---  Varken uppdatering, release eller borttag                           
104100               MOVE ERR-PF11-AND-NO-DATA TO RESP-IDMSG-ERROR              
104200               PERFORM MFS-ROER-EJ-ALLA-FAELT                             
104300               MOVE NEJ           TO INDATA-SW                            
104400             END-IF                                                       
104500           END-IF                                                         
104600         END-IF                                                           
104700       END-IF                                                             
104800     END-IF                                                               
104900                                                                          
105000     IF INDATA-FEL                                                        
105100       IF NOT RELEASEA                                                    
105200         IF RESP-IDMSG-ERROR NOT > ZERO                                   
105300           MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                  
105400         END-IF                                                           
105500       END-IF                                                             
105600                                                                          
105700       PERFORM MFS-ROER-EJ-ALLA-FAELT                                     
105800       IF RELEASEA AND RESP-IDMSG-ERROR = ERR-INFO-MISSING                
105900         MOVE MFS-ADD-SAETT-CURSOR TO RESP-KDTRPTYP-ATTR                  
106000         MOVE 'INF'   TO RESP-IDELMT-ERROR                                
106100       END-IF                                                             
106200                                                                          
106300       MOVE REQU-IDFAKT-START   TO RESP-IDFAKT-START                      
106400                                   RESP-IDFAKT-NEXT                       
106500       MOVE REQU-IDORDNR5-START TO RESP-IDORDNR5-START                    
106600                                   RESP-IDORDNR5-NEXT                     
106700       MOVE REQU-IDKOLLI-START   TO RESP-IDKOLLI-START                    
106800                                   RESP-IDKOLLI-NEXT                      
106900       MOVE REQU-IDPRODNR-START TO RESP-IDPRODNR-START                    
107000                                   RESP-IDPRODNR-NEXT                     
107100       MOVE REQU-IDDC-START     TO RESP-IDDC-START                        
107200                                   RESP-IDDC-NEXT                         
107300       MOVE REQU-IDDISTR-START  TO RESP-IDDISTR-START                     
107400                                   RESP-IDDISTR-NEXT                      
107500                                                                          
107600     END-IF                                                               
107700     .                                                                    
107800     EJECT                                                                
107900 GA-KOLL-VID-UPPDAT SECTION.                                              
108000                                                                          
108100     MOVE 'GA-KOLL-VID-UPP'      TO CURRENT-SECTION                       
108200     IF REQU-KDTRPTYP = 1 OR 2 OR 3 OR 4 OR                               
108300                       '+' OR SPACE OR ZERO                               
108400       IF REQU-KDTRPTYP = 1 OR 2 OR 3 OR 4                                
108500         MOVE REQU-KDTRPTYP      TO WS-KDTRPTYP                           
108600       ELSE                                                               
108700         MOVE ZERO               TO WS-KDTRPTYP                           
108800       END-IF                                                             
108900     ELSE                                                                 
109000       PERFORM MFS-LAES-IN-IGEN                                           
109100       MOVE MFS-NUM-FAELT-FEL    TO RESP-KDTRPTYP-ATTR                    
109200       MOVE NEJ                  TO INDATA-SW                             
109300     END-IF                                                               
109400                                                                          
109500     IF INDATA-OK                                                         
109600       IF REQU-FLCONTAIN = JA OR YES OR NEJ OR '+' OR SPACE               
109700         IF REQU-FLCONTAIN = JA OR YES                                    
109800           MOVE YES              TO WS-FLCONTAIN                          
109900         ELSE                                                             
110000           IF REQU-FLCONTAIN = '+'                                        
110100             MOVE SPACE          TO WS-FLCONTAIN                          
110200           ELSE                                                           
110300             MOVE REQU-FLCONTAIN TO WS-FLCONTAIN                          
110400           END-IF                                                         
110500         END-IF                                                           
110600       ELSE                                                               
110700         PERFORM MFS-LAES-IN-IGEN                                         
110800         MOVE MFS-ALFA-FAELT-FEL TO RESP-FLCONTAIN-ATTR                   
110900         MOVE NEJ                TO INDATA-SW                             
111000       END-IF                                                             
111100     END-IF                                                               
111200                                                                          
111300     IF REQU-IDLBBET = ALL '+' OR SPACE                                   
111400       MOVE SPACE                TO WS-IDLBBET                            
111500     ELSE                                                                 
111600       MOVE REQU-IDLBBET         TO WS-IDLBBET                            
111700     END-IF                                                               
111800                                                                          
111900     IF REQU-IDBOKN = ALL '+' OR SPACE                                    
112000       MOVE SPACE                TO WS-IDBOKN                             
112100     ELSE                                                                 
112200       MOVE REQU-IDBOKN          TO WS-IDBOKN                             
112300     END-IF                                                               
112400                                                                          
112500     IF REQU-IDFORDREG = ALL '+' OR SPACE                                 
112600       MOVE SPACE                TO WS-IDFORDREG                          
112700     ELSE                                                                 
112800       MOVE REQU-IDFORDREG       TO WS-IDFORDREG                          
112900     END-IF                                                               
113000                                                                          
113100*--- Kombinationskontroll vid uppdatering (ej release)                    
113200                                                                          
113300     IF INDATA-OK                                                         
113400       PERFORM S01A-LAES-GHN-WDM701                                       
113500       IF SEGMENT-FINNS                                                   
113600         IF REQU-FLCONTAIN = YES OR JA OR NEJ                             
113700           MOVE REQU-FLCONTAIN   TO AKT-FLCONTAIN                         
113800         ELSE                                                             
113900           MOVE HUV-FLCONTAIN    TO AKT-FLCONTAIN                         
114000         END-IF                                                           
114100                                                                          
114200         IF AKT-FLCONTAIN = YES OR JA                                     
114300           IF REQU-KDTRPTYP = 1 OR 2 OR 3 OR 4                            
114400             MOVE REQU-KDTRPTYP  TO AKT-KDTRPTYP                          
114500           ELSE                                                           
114600             MOVE HUV-KDTRPTYP   TO AKT-KDTRPTYP                          
114700           END-IF                                                         
114800           IF AKT-KDTRPTYP NOT = 1 AND 2                                  
114900             PERFORM MFS-LAES-IN-IGEN                                     
115000             MOVE MFS-ALFA-FAELT-FEL TO RESP-FLCONTAIN-ATTR               
115100             MOVE NEJ            TO INDATA-SW                             
115200           END-IF                                                         
115300         END-IF                                                           
115400       ELSE                                                               
115500         MOVE ERR-INVOICE-MISSING TO RESP-IDMSG-ERROR                     
115600         MOVE 'KEY'               TO RESP-IDELMT-ERROR                    
115700         PERFORM MFS-ROER-EJ-ALLA-FAELT                                   
115800         MOVE NEJ                        TO INDATA-SW                     
115900       END-IF                                                             
116000                                                                          
116100*---   Vi positionerar oss till början på WDM7 igen                       
116200*---   för att inte tappa första posten vid uppd/rel                      
116300*---   NEJ. Nya bud. Vi skippar första läsn M7 i HC-, den är              
116400*---   redan inläst här nu och vi använder denna i HC-                    
116500                                                                          
116600*---   PERFORM IMS-GU-WDM701-FIRST                                        
116700                                                                          
116800     END-IF                                                               
116900     .                                                                    
117000     EJECT                                                                
117100 GB-KOLL-VID-RELEASE   SECTION.                                           
117200                                                                          
117300     MOVE 'GB-KOLL-VID-REL'      TO CURRENT-SECTION                       
117400*--- Kombinationskontroller och test obligatoriska fält i WDM7            
117500*--- görs enbart vid release                                              
117600                                                                          
117700     PERFORM S01A-LAES-GHN-WDM701                                         
117800     IF SEGMENT-FINNS                                                     
117900       IF HUV-IDTULLNR = ZERO                                             
118000         IF HUV-KDTRPTYP = 1 OR 2 OR 3 OR 4                               
118100           MOVE HUV-KDTRPTYP             TO WS-KDTRPTYP                   
118200                                                                          
118300           IF HUV-KDTRPTYP = 1                                            
118400             IF HUV-FLCONTAIN = YES                                       
118500               IF HUV-IDLBBET = SPACE                                     
118600                                                                          
118700                 PERFORM MFS-LAES-IN-IGEN                                 
118800                 MOVE MFS-ALFA-FAELT-FEL TO RESP-IDLBBET-ATTR             
118900                 MOVE NEJ                TO INDATA-SW                     
119000               END-IF                                                     
119100               IF HUV-IDBOKN = SPACE                                      
119200                                                                          
119300                 PERFORM MFS-LAES-IN-IGEN                                 
119400                 MOVE MFS-ALFA-FAELT-FEL TO RESP-IDBOKN-ATTR              
119500                 MOVE NEJ                TO INDATA-SW                     
119600               END-IF                                                     
120300             ELSE                                                         
120400               IF HUV-FLCONTAIN NOT = NEJ                                 
120500                 PERFORM MFS-LAES-IN-IGEN                                 
120600                 MOVE MFS-ALFA-FAELT-FEL TO RESP-FLCONTAIN-ATTR           
120700                 MOVE NEJ                TO INDATA-SW                     
120800               END-IF                                                     
120900             END-IF                                                       
121000           ELSE                                                           
121100             IF HUV-KDTRPTYP = 2                                          
121200               IF HUV-FLCONTAIN = YES                                     
121300                 IF HUV-IDLBBET = SPACE                                   
121400                                                                          
121500                   PERFORM MFS-LAES-IN-IGEN                               
121600                   MOVE MFS-ALFA-FAELT-FEL TO RESP-IDLBBET-ATTR           
121700                   MOVE NEJ              TO INDATA-SW                     
121800                 END-IF                                                   
121900                 IF HUV-IDBOKN = SPACE                                    
122000                                                                          
122100                   PERFORM MFS-LAES-IN-IGEN                               
122200                   MOVE MFS-ALFA-FAELT-FEL TO RESP-IDBOKN-ATTR            
122300                   MOVE NEJ              TO INDATA-SW                     
122400                 END-IF                                                   
122500                 IF HUV-IDFORDREG = SPACE                                 
122600                                                                          
122700                   PERFORM MFS-LAES-IN-IGEN                               
122800                   MOVE MFS-ALFA-FAELT-FEL TO RESP-IDFORDREG-ATTR         
122900                   MOVE NEJ              TO INDATA-SW                     
123000                 END-IF                                                   
123100               ELSE                                                       
123200                 PERFORM MFS-LAES-IN-IGEN                                 
123300                 MOVE MFS-ALFA-FAELT-FEL TO RESP-FLCONTAIN-ATTR           
123400                 MOVE NEJ                TO INDATA-SW                     
123500               END-IF                                                     
123600             END-IF                                                       
123700           END-IF                                                         
123800                                                                          
123900           IF HUV-FLCONTAIN = YES AND                                     
124000             HUV-KDTRPTYP NOT = 1 AND 2                                   
124100             PERFORM MFS-LAES-IN-IGEN                                     
124200             MOVE MFS-ALFA-FAELT-FEL     TO RESP-FLCONTAIN-ATTR           
124300             MOVE NEJ                    TO INDATA-SW                     
124400           END-IF                                                         
124500         ELSE                                                             
124600                                                                          
124700           PERFORM MFS-LAES-IN-IGEN                                       
124800           MOVE MFS-ALFA-FAELT-FEL       TO RESP-KDTRPTYP-ATTR            
124900           MOVE NEJ                      TO INDATA-SW                     
125000         END-IF                                                           
125100         IF INDATA-FEL                                                    
125200           MOVE ERR-INFO-MISSING         TO RESP-IDMSG-ERROR              
125300           MOVE 'INF'   TO RESP-IDELMT-ERROR                              
125400           PERFORM MFS-ROER-EJ-ALLA-FAELT                                 
125500                                                                          
125600*---       vi rensar FLSLUT så man kan uppdatera info direkt              
125700           MOVE ALL-SPACE                TO RESP-FLSLUT                   
125800         END-IF                                                           
125900       ELSE                                                               
126000*---         FINNS INGET ATT RELEASEA (Urval saknas)                      
126100         MOVE ERR-INVOICE-MISSING  TO RESP-IDMSG-ERROR                    
126200         MOVE 'KEY'                TO RESP-IDELMT-ERROR                   
126300         PERFORM MFS-ROER-EJ-ALLA-FAELT                                   
126400         MOVE NEJ                        TO INDATA-SW                     
126500       END-IF                                                             
126600     ELSE                                                                 
126700*---   INGA POSTER PÅ DETTA FAKT NR                                       
126800       MOVE  ERR-INVOICE-MISSING         TO RESP-IDMSG-ERROR              
126900       MOVE 'IDFAKT'                     TO RESP-IDELMT-ERROR             
127000       PERFORM MFS-ROER-EJ-ALLA-FAELT                                     
127100       MOVE NEJ                          TO INDATA-SW                     
127200     END-IF                                                               
127300                                                                          
127400*--- Vi positionerar oss till början på WDM7 igen                         
127500*--- för att inte tappa första posten vid uppd/rel                        
127600*---   NEJ. Nya bud. Vi skippar första läsn M7 i HC-, den är              
127700*---   redan inläst här nu och vi använder denna i HC-                    
127800                                                                          
127900*--- PERFORM IMS-GU-WDM701-FIRST                                          
128000                                                                          
128100     .                                                                    
128200     EJECT                                                                
128300                                                                          
128400 H-UPPDATERA SECTION.                                                     
128500                                                                          
128600     MOVE 'H-UPPDATERA    '      TO CURRENT-SECTION                       
128700                                                                          
128800     IF BORTTAG                                                           
128900       PERFORM HA-RENSA-FAKTURA                                           
129000       PERFORM MFS-RENSA-FAELT-IN                                         
129100       PERFORM MFS-RENSA-FAELT-UT                                         
129200                                                                          
129300     ELSE                                                                 
129400*---   Antingen Release eller Uppdatera WDM7                              
129500       PERFORM HC-BEHANDLA-ALLT-PA-FAKTURA                                
129600     END-IF                                                               
129700     .                                                                    
129800     EJECT                                                                
129900 HA-RENSA-FAKTURA SECTION.                                                
130000                                                                          
130100     MOVE 'HA-RENSA-FAKT  '      TO CURRENT-SECTION                       
130200                                                                          
130300*--- Rensa alla poster på visst fakturanr i WDM7 utan Tullnr              
130400*--- Nycklar initierade i B-KOLLA-NYCKLAR                                 
130500*--- Här har vi inte läst WDM7 förut (jmfr kommentar i HC-)               
130600                                                                          
130700     PERFORM S01A-LAES-GHN-WDM701                                         
130800                                                                          
130900*--- Fanns inget att ta bort med Tullid = 0                               
131000     IF BASEN-SLUT OR SEGMENT-SAKNAS                                      
131100*******MOVE INF-DELETE-NOT-OK      TO RESP-IDMSG-ERROR                    
131200       MOVE  ERR-INVOICE-MISSING         TO RESP-IDMSG-ERROR              
131300       MOVE 'IDFAKT'                     TO RESP-IDELMT-ERROR             
131400     END-IF                                                               
131500                                                                          
131600     PERFORM UNTIL BASEN-SLUT OR SEGMENT-SAKNAS                           
131700       MOVE JA                     TO WDM7-BORTTAGEN-SW                   
131800       PERFORM IMS-DLET-WDM701                                            
131900       PERFORM S01A-LAES-GHN-WDM701                                       
132000     END-PERFORM                                                          
132100                                                                          
132200*--- Rensa motsvarande poster på detta fakturanr i WDM8                   
132300                                                                          
132400     IF WDM7-BORTTAGEN                                                    
132500       PERFORM IMS-GHU-WDM801                                             
132600       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                         
132700         PERFORM IMS-DLET-WDM801                                          
132800         PERFORM IMS-GHN-WDM801                                           
132900       END-PERFORM                                                        
133000                                                                          
133100       MOVE REQU-IDFAKT-KEY        TO INF-IDFAKT                          
133200       MOVE INF-UPDATE-DONE        TO RESP-IDMSG-INFO                     
133300     END-IF                                                               
133400     .                                                                    
133500 HC-BEHANDLA-ALLT-PA-FAKTURA SECTION.                                     
133600                                                                          
133700     MOVE 'HC-BEHANDLA-ALLT'     TO CURRENT-SECTION                       
133800                                                                          
133900*--- Vid release hämtar vi TULLID och uppdat M7 med detta                 
134000*--- samt startar SOP. Efter detta visas tullpost ej mer.                 
134100*--- Om end uppdat, så uppdat vi M7 med inmatade värden                   
134200                                                                          
134300*--- Första WDM7-post redan inläst i GA-/GB- vid uppdat/release           
134400*--- PERFORM S01A-LAES-GHN-WDM701                                         
134500                                                                          
134600*--- Spar första uppdaterade WDM7-posts nyckel, för att kunna läsa        
134700*--- härifrån igen i F-LAES-VISA efter uppdatering                        
134800                                                                          
134900     MOVE HUV-IDFAKT                TO W-IDFAKT-FIRST                     
135000     MOVE HUV-IDORDNR7              TO W-IDORDNR7-FIRST                   
135100     MOVE HUV-IDKOLLI               TO W-IDKOLLI-FIRST                    
135200     MOVE HUV-IDPRODNR              TO W-IDPRODNR-FIRST                   
135300                                                                          
135400                                                                          
135500     MOVE STATUS-WS TO STATUS-WS-M7                                       
135600                                                                          
135700     IF SEGMENT-FINNS                                                     
135800       IF RELEASEA                                                        
135900         PERFORM HCA-TA-UT-TULLID                                         
136000       END-IF                                                             
136100       MOVE HUV-IDPRODNR            TO W-IDPRODNR-E6                      
136200       MOVE HUV-IDKOLLI             TO W-IDKOLLI-E6                       
136300       PERFORM IMS-GHU-WDE611                                             
136400       MOVE STATUS-WS               TO STATUS-WS-E6                       
136500       MOVE STATUS-WS-M7            TO STATUS-WS                          
136600     END-IF                                                               
136700                                                                          
136800     PERFORM UNTIL BASEN-SLUT OR SEGMENT-SAKNAS                           
136900       IF RELEASEA                                                        
137000                                                                          
137100         MOVE 'VP'                  TO HUV-IDTULFTG                       
137200                                    KOLLI-IDTULFTG                        
137300         MOVE WS-IDTULLNR           TO HUV-IDTULLNR                       
137400                                    KOLLI-IDTULLNR                        
137500         MOVE WS-RETULKS            TO HUV-RETULKS                        
137600                                    KOLLI-RETULKS                         
137700         MOVE REQU-IDUSER           TO HUV-IDUSER                         
137800                                                                          
137900         MOVE DAGENS-DATUM          TO HUV-TIREGDAT                       
138000         MOVE WS-DAGENS-TID         TO HUV-TIREGTID                       
138300                                                                          
138400         IF HUV-IDDC = '21' OR '87'                                       
138500           MOVE WS-FLKLAR-YES       TO HUV-FLKLAR                         
138600         ELSE                                                             
138700           MOVE WS-FLKLAR           TO HUV-FLKLAR                         
138800         END-IF                                                           
138900       ELSE                                                               
139000*---     Endast uppdatatering , ej release                                
139100                                                                          
139200         IF WS-KDTRPTYP > ZERO                                            
139300           MOVE WS-KDTRPTYP         TO HUV-KDTRPTYP                       
139400         END-IF                                                           
139500         IF WS-FLCONTAIN NOT = SPACE                                      
139600           MOVE WS-FLCONTAIN        TO HUV-FLCONTAIN                      
139700         END-IF                                                           
139800         IF WS-IDLBBET NOT = SPACE                                        
139900           MOVE WS-IDLBBET          TO HUV-IDLBBET                        
140000         END-IF                                                           
140100         IF WS-IDBOKN NOT = SPACE                                         
140200           MOVE WS-IDBOKN           TO HUV-IDBOKN                         
140300         END-IF                                                           
140400         IF WS-IDFORDREG NOT = SPACE                                      
140500           MOVE WS-IDFORDREG        TO HUV-IDFORDREG                      
140600         END-IF                                                           
140700         MOVE REQU-IDUSER           TO HUV-IDUSER                         
140800       END-IF                                                             
140900                                                                          
141000       PERFORM IMS-REPL-WDM701                                            
141100                                                                          
141200       IF SEGMENT-FINNS-E6                                                
141300         PERFORM IMS-REPL-WDE611                                          
141400       END-IF                                                             
141500                                                                          
141600       PERFORM S01A-LAES-GHN-WDM701                                       
141700                                                                          
141800       MOVE STATUS-WS TO STATUS-WS-M7                                     
141900       IF SEGMENT-FINNS                                                   
142000         MOVE HUV-IDPRODNR          TO W-IDPRODNR-E6                      
142100         MOVE HUV-IDKOLLI           TO W-IDKOLLI-E6                       
142200         PERFORM IMS-GHU-WDE611                                           
142300         MOVE STATUS-WS             TO STATUS-WS-E6                       
142400         MOVE STATUS-WS-M7          TO STATUS-WS                          
142500       END-IF                                                             
142600     END-PERFORM                                                          
142700                                                                          
142800     IF RELEASEA                                                          
142900       PERFORM S02-STARTA-SOP-RUTIN                                       
143000     ELSE                                                                 
143100       MOVE INF-UPDATE-DONE         TO RESP-IDMSG-INFO                    
143200     END-IF                                                               
143300     .                                                                    
143400     EJECT                                                                
143500 HCA-TA-UT-TULLID SECTION.                                                
143600                                                                          
143700     MOVE 'HCA-TA-UT-TULLID '     TO CURRENT-SECTION                      
143800                                                                          
143900     PERFORM IMS-GET-WDGX4587-4588                                        
144000     MOVE 4588-IDTULLNR-AKT         TO WS-IDTULLNR                        
144100                                                                          
144200     ADD +1                         TO 4588-IDTULLNR-AKT                  
144300     IF 4588-IDTULLNR-AKT > 4588-IDTULLNR-MAX                             
144400       MOVE 4588-IDTULLNR-MIN       TO 4588-IDTULLNR-AKT                  
144500     END-IF                                                               
144600     PERFORM IMS-REPL-WDGX4588                                            
144700                                                                          
144800     MOVE WS-IDTULLNR               TO TULLID (5:7)                       
144900     CALL CHECK USING TULLID LGD WEIGHT FIGURES                           
145000                      KSIFFR MODUL-10-11 ALT-A-B                          
145100     MOVE KSIFFR                    TO WS-RETULKS                         
145200                                                                          
145300     .                                                                    
145400     EJECT                                                                
145500                                                                          
145600                                                                          
145700                                                                          
145800 S01A-LAES-GHN-WDM701 SECTION.                                            
145900                                                                          
146000     MOVE 'S01A-LAES-GHN-WDM701' TO CURRENT-SECTION2                      
146100                                                                          
146200     IF W-IDDC = WC-CDC-SE                                                
146300      PERFORM IMS-GHN-WDM701-SE                                           
146400     ELSE                                                                 
146500      PERFORM IMS-GHN-WDM701                                              
146600     END-IF                                                               
146700     .                                                                    
146800     EJECT                                                                
146900                                                                          
147000 S01B-LAES-GN-WDM7A SECTION.                                              
147100     MOVE 'S01B-LAES-GN-WDM7A' TO CURRENT-SECTION2                        
147200                                                                          
147300     IF W-IDDC = WC-CDC-SE                                                
147400      PERFORM IMS-GN-WDM7A-SE                                             
147500     ELSE                                                                 
147600      PERFORM IMS-GN-WDM7A                                                
147700     END-IF                                                               
147800     .                                                                    
147900     EJECT                                                                
148000                                                                          
148100                                                                          
148200 S02-STARTA-SOP-RUTIN SECTION.                                            
148300                                                                          
148400     MOVE 'S02-STARTA-SOP   '     TO CURRENT-SECTION                      
148500                                                                          
148600     STRING 'IDTULL(' WS-IDTULL ')'                                       
148700          DELIMITED BY SIZE INTO MSGSOP-TESYMBV                           
148800     PERFORM IMS-INSERT-ALTMSG                                            
148900                                                                          
149000     MOVE WS-IDTULLNR          TO INF-IDTULLNR                            
149100     MOVE WS-RETULKS           TO INF-RETULKS                             
149200*--- MOVE INF-TULL-ID-ENDED    TO RESP-TEMFSINF                           
149300     MOVE INF-UPDATE-DONE      TO RESP-IDMSG-INFO                         
149400                                                                          
149500     PERFORM MFS-RENSA-FAELT-IN                                           
149600     PERFORM MFS-RENSA-FAELT-UT                                           
149700     .                                                                    
149800 S03-OVR-REQU-TILL-RESP   SECTION.                                        
149900                                                                          
150000     IF REQU-KDTRPTYP NOT = '+'                                           
150100       MOVE REQU-KDTRPTYP      TO RESP-KDTRPTYP                           
150200     ELSE                                                                 
150300       MOVE ALL-SPACE          TO RESP-KDTRPTYP                           
150400     END-IF                                                               
150500     IF REQU-FLCONTAIN NOT = '+'                                          
150600       MOVE REQU-FLCONTAIN     TO RESP-FLCONTAIN                          
150700     ELSE                                                                 
150800       MOVE ALL-SPACE          TO RESP-FLCONTAIN                          
150900     END-IF                                                               
151000     IF REQU-IDLBBET = '+' OR SPACE                                       
151100       MOVE ALL-SPACE          TO RESP-IDLBBET                            
151200     ELSE                                                                 
151300       MOVE REQU-IDLBBET       TO RESP-IDLBBET                            
151400     END-IF                                                               
151500     IF REQU-IDBOKN = '+' OR SPACE                                        
151600       MOVE ALL-SPACE          TO RESP-IDBOKN                             
151700     ELSE                                                                 
151800       MOVE REQU-IDBOKN        TO RESP-IDBOKN                             
151900     END-IF                                                               
152000     IF REQU-IDFORDREG = '+' OR SPACE                                     
152100       MOVE ALL-SPACE          TO RESP-IDFORDREG                          
152200     ELSE                                                                 
152300       MOVE REQU-IDFORDREG     TO RESP-IDFORDREG                          
152400     END-IF                                                               
152500     .                                                                    
152600     EJECT                                                                
152700 MFS-RENSA-FAELT-UT SECTION.                                              
152800                                                                          
152900*    --- ALLA UTDATA-FÄLT                                                 
153000*    --- INKL. BLÄDDRINGSNYCKLAR                                          
153100     MOVE ALL-SPACE       TO                                              
153200                             RESP-KDTRPTYP-UT                             
153300                             RESP-FLCONTAIN-UT                            
153400                             RESP-IDLBBET-UT                              
153500                             RESP-IDBOKN-UT                               
153600                             RESP-IDFORDREG                               
153700*                            RESP-IDFORDREG-UT                            
153800     PERFORM MFS-RENSA-RAD-FAELT                                          
153900     .                                                                    
154000     SKIP2                                                                
154100 MFS-RENSA-RAD-FAELT SECTION.                                             
154200                                                                          
154300*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
154400     MOVE +1                TO INDX                                       
154500                                                                          
154600     PERFORM UNTIL INDX > MAX-KVRADER                                     
154700                                                                          
154800       MOVE ALL-SPACE       TO RESP-IDFAKT (INDX)                         
154900                               RESP-TIFAKT (INDX)                         
155000                               RESP-IDDISTR (INDX)                        
155100                               RESP-IDKUNDNR(INDX)                        
155200                               RESP-IDORDNR5(INDX)                        
155300                               RESP-IDKOLLI (INDX)                        
155400                               RESP-KDORDKL (INDX)                        
155500                               RESP-FLORDSPE(INDX)                        
155600                               RESP-IDPRODNR(INDX)                        
155700                               RESP-TIREGDAT(INDX)                        
155800                               RESP-TIREGTID(INDX)                        
155900                               RESP-FLKLAR  (INDX)                        
156000       ADD +1               TO INDX                                       
156100     END-PERFORM                                                          
156200     .                                                                    
156300     eject                                                                
156400 MFS-RENSA-FAELT-IN SECTION.                                              
156500                                                                          
156600*    --- ALLA INDATA-FÄLT                                                 
156700     MOVE ALL-SPACE            TO RESP-FLBORT                             
156800                                  RESP-FLSLUT                             
156900                                  RESP-KDTRPTYP                           
157000                                  RESP-FLCONTAIN                          
157100                                  RESP-IDLBBET                            
157200                                  RESP-IDBOKN                             
157300*                                 RESP-IDFORDREG                          
157400     .                                                                    
157500     EJECT                                                                
157600 MFS-ROER-EJ-ALLA-FAELT SECTION.                                          
157700                                                                          
157800*    --- ALLA UTDATA-FÄLT                                                 
157900*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
158000     MOVE ALL-PLUS          TO RESP-FLBORT                                
158100                               RESP-FLSLUT                                
158200                               RESP-KDTRPTYP                              
158300                               RESP-FLCONTAIN                             
158400                               RESP-IDLBBET                               
158500                               RESP-IDBOKN                                
158600                               RESP-IDFORDREG                             
158700                               RESP-KDTRPTYP-UT                           
158800                               RESP-FLCONTAIN-UT                          
158900                               RESP-IDLBBET-UT                            
159000                               RESP-IDBOKN-UT                             
159100*                              RESP-IDFORDREG-UT                          
159200                                                                          
159300     MOVE MAX-KVRADER       TO RESP-KVRADER                               
159400                                                                          
159500*--- webben vill ha REQU-KVRADER I RESP-KVRADER vid felmeddelande         
159600     IF MAX-KVRADER = 500                                                 
159700        MOVE REQU-KVRADER   TO RESP-KVRADER                               
159800     END-IF                                                               
159900                                                                          
160000     MOVE +1 TO INDX                                                      
160100                                                                          
160200     PERFORM UNTIL INDX > MAX-KVRADER                                     
160300       PERFORM MFS-ROER-EJ-RAD-FAELT                                      
160400       ADD +1 TO INDX                                                     
160500     END-PERFORM                                                          
160600                                                                          
160700     .                                                                    
160800     SKIP2                                                                
160900 MFS-ROER-EJ-RAD-FAELT  SECTION.                                          
161000                                                                          
161100*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
161200                                                                          
161300     MOVE ALL-PLUS          TO RESP-IDFAKT (INDX)                         
161400                               RESP-TIFAKT (INDX)                         
161500                               RESP-IDDISTR (INDX)                        
161600                               RESP-IDKUNDNR(INDX)                        
161700                               RESP-IDORDNR5(INDX)                        
161800                               RESP-IDKOLLI (INDX)                        
161900                               RESP-KDORDKL (INDX)                        
162000                               RESP-FLORDSPE(INDX)                        
162100                               RESP-IDPRODNR(INDX)                        
162200                               RESP-TIREGDAT(INDX)                        
162300                               RESP-TIREGTID(INDX)                        
162400                               RESP-FLKLAR  (INDX)                        
162500     .                                                                    
162600                                                                          
162700 MFS-FORM-ATTR SECTION.                                                   
162800                                                                          
162900*    --- ALLA INDATA-FÄLT                                                 
163000     MOVE MFS-FORMATETS-ATTR     TO RESP-FLBORT-ATTR                      
163100                                      RESP-FLSLUT-ATTR                    
163200                                      RESP-KDTRPTYP-ATTR                  
163300                                      RESP-FLCONTAIN-ATTR                 
163400                                      RESP-IDLBBET-ATTR                   
163500                                      RESP-IDBOKN-ATTR                    
163600                                      RESP-IDFORDREG-ATTR                 
163700     .                                                                    
163800     EJECT                                                                
163900 MFS-LAES-IN-IGEN SECTION.                                                
164000                                                                          
164100*    --- ALLA INDATA-FÄLT                                                 
164200                                                                          
164300     MOVE MFS-ADD-LAES-IN-FAELT TO    RESP-FLBORT-ATTR                    
164400                                      RESP-FLSLUT-ATTR                    
164500                                      RESP-KDTRPTYP-ATTR                  
164600                                      RESP-FLCONTAIN-ATTR                 
164700                                      RESP-IDLBBET-ATTR                   
164800                                      RESP-IDBOKN-ATTR                    
164900                                      RESP-IDFORDREG-ATTR                 
165000     .                                                                    
165100     EJECT                                                                
165200                                                                          
165300     EJECT                                                                
165400*----IMS SEKTIONER----                                                    
165500     SKIP3                                                                
165600     EJECT                                                                
165700 IMS-INSERT-ALTMSG SECTION.                                               
165800                                                                          
165900     MOVE 'IMS-INSERT-ALTMSG'     TO CURR-IMS-SECTION                     
166000                                                                          
166100     MOVE '  ' TO GODK-STATUSKODER                                        
166200     CALL CBLTDLI USING ISRT ALT-PCB MSG-SOP-AREA                         
166300     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
166400     PERFORM IMS-STATUSKONTROLL                                           
166500     .                                                                    
166600     EJECT                                                                
166700 IMS-GHN-WDM701 SECTION.                                                  
166800                                                                          
166900     MOVE 'IMS-GHN-WDM701   '     TO CURR-IMS-SECTION                     
167000                                                                          
167100*--- Läsning vid sökning på Fakturanr ej SE                               
167200                                                                          
167300     STRING 'WDM701  (WDM701KY=>' W-WDM701KY-MIN-X                        
167400                    '&WDM701KY=<' W-WDM701KY-MAX-X                        
167500                    '&IDDC     =' W-IDDC                                  
167600                    '&IDTULLNR>=' W-IDTULLNR ')'                          
167700          DELIMITED BY SIZE INTO SSA1                                     
167800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
167900     CALL CBLTDLI USING GHN WDM7-PCB DLI-IO-AREA SSA1                     
168000     MOVE WDM7-STATUS-CODE TO STATUS-WS                                   
168100     PERFORM IMS-STATUSKONTROLL                                           
168200     .                                                                    
168300     EJECT                                                                
168400 IMS-GHN-WDM701-SE   SECTION.                                             
168500                                                                          
168600     MOVE 'IMS-GHN-WDM701-SE'     TO CURR-IMS-SECTION                     
168700                                                                          
168800     STRING 'WDM701  (WDM701KY=>' W-WDM701KY-MIN-X                        
168900                    '&WDM701KY=<' W-WDM701KY-MAX-X                        
169000                    '&IDTULLNR>=' W-IDTULLNR                              
169100                    '&IDDC     =' WC-CDC-SE                               
169200                    '!'                                                   
169300                     'WDM701KY=>' W-WDM701KY-MIN-X                        
169400                    '&WDM701KY=<' W-WDM701KY-MAX-X                        
169500                    '&IDTULLNR>=' W-IDTULLNR                              
169600                    '&IDDC     =' WC-DDC-SE ')'                           
169700          DELIMITED BY SIZE INTO SSA1                                     
169800                                                                          
169900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
170000     CALL CBLTDLI USING GHN WDM7-PCB DLI-IO-AREA SSA1                     
170100     MOVE WDM7-STATUS-CODE TO STATUS-WS                                   
170200                                                                          
170300     PERFORM IMS-STATUSKONTROLL                                           
170400     .                                                                    
170500     EJECT                                                                
170600 IMS-GU-WDM701-FIRST SECTION.                                             
170700                                                                          
170800     MOVE 'IMS-GU-WDM701-FIRST'  TO CURR-IMS-SECTION                      
170900                                                                          
171000     STRING 'WDM701  (WDM701KY =' W-WDM701KY-FIRST-X                      
171100                    '&IDTULLNR>=' W-IDTULLNR ')'                          
171200          DELIMITED BY SIZE INTO SSA1                                     
171300     MOVE '  GE' TO GODK-STATUSKODER                                      
171400     CALL CBLTDLI USING GHU WDM7-PCB DLI-IO-AREA SSA1                     
171500     MOVE WDM7-STATUS-CODE TO STATUS-WS                                   
171600     PERFORM IMS-STATUSKONTROLL                                           
171700     .                                                                    
171800     SKIP3                                                                
171900 IMS-REPL-WDM701 SECTION.                                                 
172000                                                                          
172100     MOVE 'IMS-REPL-WDM701      '  TO CURR-IMS-SECTION                    
172200                                                                          
172300     MOVE '  ' TO GODK-STATUSKODER                                        
172400     CALL CBLTDLI USING REPL WDM7-PCB DLI-IO-AREA                         
172500     MOVE WDM7-STATUS-CODE TO STATUS-WS                                   
172600     PERFORM IMS-STATUSKONTROLL                                           
172700     .                                                                    
172800     SKIP3                                                                
172900 IMS-DLET-WDM701 SECTION.                                                 
173000                                                                          
173100     MOVE 'IMS-DLET-WDM701      '  TO CURR-IMS-SECTION                    
173200                                                                          
173300     MOVE '  ' TO GODK-STATUSKODER                                        
173400     CALL CBLTDLI USING DLET WDM7-PCB DLI-IO-AREA                         
173500     MOVE WDM7-STATUS-CODE TO STATUS-WS                                   
173600     PERFORM IMS-STATUSKONTROLL                                           
173700     .                                                                    
173800     EJECT                                                                
173900 IMS-GN-WDM7A-SE SECTION.                                                 
174000                                                                          
174100     MOVE 'IMS-GN-WDM7A-SE    '  TO CURR-IMS-SECTION                      
174200                                                                          
174300*--- Läsning vid sökning på Distrikt, DC=11/SE                            
174400                                                                          
174500     STRING 'WDM7A1  (WDM7A1KY=>' W-WDM7A1KY-MIN-X                        
174600                    '&WDM7A1KY=<' W-WDM7A1KY-MAX-X                        
174700                    '&IDDC     =' WC-CDC-SE                               
174800                    '!'                                                   
174900                     'WDM7A1KY=>' W-WDM7A1KY-MIN-X                        
175000                    '&WDM7A1KY=<' W-WDM7A1KY-MAX-X                        
175100                    '&IDDC     =' WC-DDC-SE ')'                           
175200                                                                          
175300          DELIMITED BY SIZE INTO SSA1                                     
175400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
175500     CALL CBLTDLI USING GN  WDM7A-PCB DLI-IO-WDM7A1 SSA1                  
175600     MOVE WDM7A-STATUS-CODE TO STATUS-WS                                  
175700     PERFORM IMS-STATUSKONTROLL                                           
175800     .                                                                    
175900     SKIP3                                                                
176000     EJECT                                                                
176100 IMS-GN-WDM7A    SECTION.                                                 
176200                                                                          
176300     MOVE 'IMS-GN-WDM7A       '  TO CURR-IMS-SECTION                      
176400                                                                          
176500*--- Läsning vid sökning på Distrikt, Unikt DC (Ej 11/SE)                 
176600                                                                          
176700     STRING 'WDM7A1  (WDM7A1KY=>' W-WDM7A1KY-MIN-X                        
176800                    '&WDM7A1KY=<' W-WDM7A1KY-MAX-X                        
176900                    '&IDDC     =' W-IDDC ')'                              
177000                                                                          
177100          DELIMITED BY SIZE INTO SSA1                                     
177200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
177300     CALL CBLTDLI USING GN  WDM7A-PCB DLI-IO-WDM7A1 SSA1                  
177400     MOVE WDM7A-STATUS-CODE TO STATUS-WS                                  
177500     PERFORM IMS-STATUSKONTROLL                                           
177600     .                                                                    
177700     SKIP3                                                                
177800     EJECT                                                                
177900 IMS-GHU-WDE611 SECTION.                                                  
178000                                                                          
178100     MOVE 'IMS-GHU-WDE611     '  TO CURR-IMS-SECTION                      
178200                                                                          
178300     STRING 'WDE601  (IDPRODNR =' W-WDE601KY-X ')'                        
178400          DELIMITED BY SIZE INTO SSA1                                     
178500     STRING 'WDE611  (IDKOLLI  =' W-WDE611KY-X ')'                        
178600          DELIMITED BY SIZE INTO SSA2                                     
178700     MOVE '  GE' TO GODK-STATUSKODER                                      
178800     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2              
178900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
179000     PERFORM IMS-STATUSKONTROLL                                           
179100     .                                                                    
179200     SKIP3                                                                
179300 IMS-REPL-WDE611 SECTION.                                                 
179400                                                                          
179500     MOVE 'IMS-REPL-WDE611     '  TO CURR-IMS-SECTION                     
179600                                                                          
179700     MOVE '  ' TO GODK-STATUSKODER                                        
179800     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE611                       
179900                                                                          
180000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
180100     PERFORM IMS-STATUSKONTROLL                                           
180200     .                                                                    
180300 IMS-GHU-WDM801 SECTION.                                                  
180400                                                                          
180500     MOVE 'IMS-GHU-WDM801    '  TO CURR-IMS-SECTION                       
180600                                                                          
180700     STRING 'WDM801  (WDM801KY=>' W-WDM801KY-MIN-X                        
180800                    '&WDM801KY=<' W-WDM801KY-MAX-X ')'                    
180900          DELIMITED BY SIZE INTO SSA1                                     
181000     MOVE '  GE' TO GODK-STATUSKODER                                      
181100     CALL CBLTDLI USING GHU WDM8-PCB DLI-M8-AREA SSA1                     
181200     MOVE WDM8-STATUS-CODE TO STATUS-WS                                   
181300     PERFORM IMS-STATUSKONTROLL                                           
181400     .                                                                    
181500     SKIP3                                                                
181600 IMS-GHN-WDM801 SECTION.                                                  
181700                                                                          
181800     MOVE 'IMS-GHN-WDM801    '  TO CURR-IMS-SECTION                       
181900                                                                          
182000     STRING 'WDM801  (WDM801KY=>' W-WDM801KY-MIN-X                        
182100                    '&WDM801KY=<' W-WDM801KY-MAX-X ')'                    
182200          DELIMITED BY SIZE INTO SSA1                                     
182300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
182400     CALL CBLTDLI USING GHN WDM8-PCB DLI-M8-AREA SSA1                     
182500     MOVE WDM8-STATUS-CODE TO STATUS-WS                                   
182600     PERFORM IMS-STATUSKONTROLL                                           
182700     .                                                                    
182800     SKIP3                                                                
182900 IMS-DLET-WDM801 SECTION.                                                 
183000                                                                          
183100     MOVE 'IMS-DLET-WDM801    '  TO CURR-IMS-SECTION                      
183200                                                                          
183300     MOVE '  ' TO GODK-STATUSKODER                                        
183400     CALL CBLTDLI USING DLET WDM8-PCB DLI-M8-AREA                         
183500     MOVE WDM8-STATUS-CODE TO STATUS-WS                                   
183600     PERFORM IMS-STATUSKONTROLL                                           
183700     .                                                                    
183800     EJECT                                                                
183900 IMS-GET-WDGX4587-4588 SECTION.                                           
184000                                                                          
184100     MOVE 'IMS-GET-WDGX4587   '  TO CURR-IMS-SECTION                      
184200                                                                          
184300*--- Hämta Tullid                                                         
184400     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4587-X ')'                    
184500          DELIMITED BY SIZE INTO SSA1                                     
184600     STRING 'WDR115  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
184700          DELIMITED BY SIZE INTO SSA2                                     
184800     MOVE '  GE' TO GODK-STATUSKODER                                      
184900     CALL CBLTDLI USING GHU 4587-PCB DLI-IO-AREA3 SSA1 SSA2               
185000     MOVE 4587-STATUS-CODE TO STATUS-WS                                   
185100     PERFORM IMS-STATUSKONTROLL                                           
185200     .                                                                    
185300     SKIP3                                                                
185400 IMS-REPL-WDGX4588 SECTION.                                               
185500                                                                          
185600     MOVE 'IMS-REPL-WDGX4587   '  TO CURR-IMS-SECTION                     
185700                                                                          
185800     MOVE '  ' TO GODK-STATUSKODER                                        
185900     CALL CBLTDLI USING REPL 4587-PCB DLI-IO-AREA3                        
186000     MOVE 4587-STATUS-CODE TO STATUS-WS                                   
186100     PERFORM IMS-STATUSKONTROLL                                           
186200     .                                                                    
186300     SKIP3                                                                
186400 IMS-STATUSKONTROLL SECTION.                                              
186500                                                                          
186600     SET STATUS-IX TO 1                                                   
186700     SEARCH GODK-STATUS                                                   
186800       AT END                                                             
186900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
187000         DELIMITED BY SIZE INTO FELTEXT                                   
187100         CALL FELLOG                                                      
187200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
187300         CONTINUE                                                         
187400     END-SEARCH                                                           
187500     .                                                                    
