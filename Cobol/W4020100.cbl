000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4020100.                                                
000400 AUTHOR.         STEFANO GIOBBI.                                          
000500 DATE-WRITTEN.   90/10/12.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*                                                                         
001100*        MHA 4201-BILDEN KAN MAN FRÅGA PÅ OCH ÄNDRA ORDERHUVUD.           
001200*        SUBPROGRAM ANROPAS FÖR BESTÄMNING AV NY TRANSPORT OCH            
001300*        FÖR ORDERAVSLUT.                                                 
001400*        DE SEGMENT I WDQ2 SOM PROGRAMMET LÄSER, LÄSES VIA SEK-           
001500*        UNDÄWINDEX C UTOM VID UPPDATERING AV WDQ201 OCH WDQ212.          
001600*        ALLMÄNNA UPPDATERINGSREGLER:                                     
001700*        VID UPPDATERING AV UPPGIFTER SOM GÄLLER FÖR VISST                
001800*        DC, MÅSTE ORDERRADERNA FÖR DETTA DC LIGGA I STATUS R.            
001900*        FÖR UPPGIFTER SOM FINNS I ORDERHUVUDET OCH ÄR GEMEN-             
002000*        SAMT FÖR ALLA DC, MÅSTE ORDERNS SAMTLIGA ORDERRADER              
002100*        LIGGA I STATUS R.                                                
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W4T201                                              
002500*                     W4T201U                                             
002600*        MID:         W4I20101                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W4O20101                                            
003000*                                                                         
003100*    SUBPROGRAM.                                                          
003200*        GEMENSAMMA:  W411TRAN                                            
003300*                     W413AVSO                                            
003400*                     W411EXCH                                            
003500*        GENERELLA:   WMEDKONV                                            
003600*                     CBLTDLI                                             
003700*                     FELLOG                                              
003800*                     ABEND                                               
003900*                                                                         
004000*  BASER:                                                                 
004100*        FYSISKT  LOGISKT    COPYTEXT    PREFIX (COPYTEXT)                
004200*  WDQ2  WDQ201   WLORQI01   WDQ201      OHUV-                            
004300*            12         12       12      ARB-                             
004400*  WDQ2C     C1   WLORQI01       C1      SEQC- (C-INDEX TILL WDQ2)        
004500*            12         12       12      ARB-                             
004600*            13         13       13      REF-                             
004700*  WDQ3  WDQ301   WLORQA01   WDQ301      ODEL-                            
004800*  WDB3  WDB301                                                           
004900*  WDB5  WDB501                                                           
005000*  WDB6  WDB601              WDB601      DCS-                             
005100*                                                                         
005200*  HÖSTEN 2004 GÖRAN KJELLSON                                             
005300*  ETRACKER 887753                                                        
005400*                                                                         
005500*  SEPT 2005 LINDA NILSSON                                                
005600*  ETRACKER 1334295                                                       
005700*                                                                         
005800     EJECT                                                                
005900     SKIP1                                                                
006000 ENVIRONMENT DIVISION.                                                    
006100     SKIP2                                                                
006200 DATA DIVISION.                                                           
006300 WORKING-STORAGE SECTION.                                                 
006400                                                                          
006500*    -- CHECKED BY WY2000                                                 
006600 77  IDPGM                       PIC X(08)   VALUE 'W4020100'.            
006700 77  FELTEXT-VID-CALL-ABEND      PIC X(64)   VALUE SPACE.                 
006800 77  JA                          PIC X       VALUE 'J'.                   
006900 77  NEJ                         PIC X       VALUE 'N'.                   
007000                                                                          
007100 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
007200                                                                          
007300*                                                                         
007400 01  TEST-IDKUNDNR               PIC S9(7)   VALUE ZERO COMP-3.           
007500*                                                                         
007600 01  TEST-IDDISTR                PIC S9(5)   VALUE ZERO COMP-3.           
007700*                                                                         
007800*      ----DISTR-DEALER-PRICE------                                       
007900*01  FILLER -COPY WWDIST79 -RED TEST-IDDISTR.                             
008000                                                                          
008100       EJECT                                                              
008200 01  WS-TALLY                    PIC S9(1)   VALUE +0.                    
008300 01  WS-TALLY-NUM      REDEFINES WS-TALLY                                 
008400                                 PIC 9(1).                                
008500 01  WS-BELAGINS-DEL1            PIC X(60)   VALUE SPACE.                 
008600 01  WS-BELAGINS-DEL2            PIC X(60)   VALUE SPACE.                 
008700 01  WS-BEGMT-RAD1               PIC X(35)   VALUE SPACE.                 
008800 01  WS-BEGMT-RAD2               PIC X(35)   VALUE SPACE.                 
008900 01  WS-ADGMT-GATA               PIC X(35)   VALUE SPACE.                 
009000 01  WS-ADGMT-PADR               PIC X(35)   VALUE SPACE.                 
009100 01  WS-ADGMT-LAND               PIC X(35)   VALUE SPACE.                 
009200 01  WS-KDFRAKT                  PIC X(2)    VALUE SPACE.                 
009300 01  WS-KDFRAKT-NUM    REDEFINES WS-KDFRAKT                               
009400                                 PIC 9(2).                                
009500 01  WS-BEKUNDRF                 PIC X(15)   VALUE SPACE.                 
009600 01  WS-BEGMRK-GRP.                                                       
009700     03  WS-BEGMRK-RAD1          PIC X(30)   VALUE SPACE.                 
009800     03  WS-BEGMRK-RAD2          PIC X(30)   VALUE SPACE.                 
009900                                                                          
010000 01  WS-TEDDI.                                                            
010100     03 FILLER                   PIC X(3) VALUE SPACE.                    
010200     03 WS-KDVALISO              PIC X(3).                                
010300     03 FILLER                   PIC X(5) VALUE SPACE.                    
010400                                                                          
010500 01  W-SPAR-IDKUNDRF.                                                     
010600     03  W-SPAR-IDORDNR7         PIC X(7)    VALUE '+++++++'.             
010700     03  FILLER                  PIC X(3)    VALUE '+++'.                 
010800                                                                          
010900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
011000     88  NYCKLAR-OK                          VALUE 'J'.                   
011100     88  NYCKLAR-FEL                         VALUE 'N'.                   
011200                                                                          
011300 77  ALLT-SW                     PIC X       VALUE 'J'.                   
011400     88  ALLT-OK                             VALUE 'J'.                   
011500                                                                          
011600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
011700     88  EGEN-MID                            VALUE '4201'.                
011800     88  GODK-MID                     VALUE '4201' '4204' '4207'          
011900                                            '4202' '4205' '4208'          
012000                                            '4203' '4206' '4209'.         
012100     EJECT                                                                
012200     SKIP2                                                                
012300 01  GENERELLA-SUBPROGRAM.                                                
012400*                                                                         
012500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013000*                                                                         
013100 01  GEMENSAMMA-SUBPROGRAM.                                               
013200*                                                                         
013300     03  W411TRAN                PIC X(8)    VALUE 'W411TRAN'.            
013400*            BESTÄM TRANSPORTAVGÅNGSTID                                   
013500     03  W413AVSO                PIC X(8)    VALUE 'W413AVSO'.            
013600*            AVSO (WOPS) - ORDERAVSLUT                                    
013700     03  W411EXCH                PIC X(8)    VALUE 'W411EXCH'.            
013800*            RÄKNA OM VALUTA DDI                                          
013900     EJECT                                                                
014000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
014100*01 -COPY WMSGINIT                                                        
014200     EJECT                                                                
014300*                                                                         
014400 01  FELMEDDELANDE-AREA.                                                  
014500     03  FILLER                  PIC X(16)   VALUE 'FELMEDD AREA'.        
014600     03  FELM-KORR-UPPLYSTA-FALT-001                                      
014700                                 PIC X(3)           VALUE '001'.          
014800     03  FELM-ORADER-SAKNAS-F-DC-028                                      
014900                                 PIC X(3)           VALUE '028'.          
015000     03  FELM-ORDERN-ANNULLERAD-052                                       
015100                                 PIC X(3)           VALUE '052'.          
015200     03  FELM-OH-KAN-EJ-ANDRAS-062                                        
015300                                 PIC X(3)           VALUE '062'.          
015400     03  FELM-KUNDUPPG-SAKNAS-063                                         
015500                                 PIC X(3)           VALUE '063'.          
015600     03  FELM-TRP-KAN-EJ-SATTAS-064                                       
015700                                 PIC X(3)           VALUE '064'.          
015800     03  FELM-FK-KAN-EJ-ANDRAS-068                                        
015900                                 PIC X(3)           VALUE '068'.          
016000     03  FELM-UPPDAT-UTFORD-101  PIC X(3)           VALUE '101'.          
016100     03  FELM-PF11-FOR-UPPD-407  PIC X(3)           VALUE '407'.          
016200     03  FELM-INGET-ANDRAT-414   PIC X(3)           VALUE '414'.          
016300     03  FELM-ORDER-SAKNAS-701   PIC X(3)           VALUE '701'.          
016400     EJECT                                                                
016500*                                                                         
016600 01  KONSTANT-AREA.                                                       
016700     03  FILLER                  PIC  X(16) VALUE 'KONSTANT AREA'.        
016800     03  K-KDORDKL-0             PIC S9(1)  COMP-3 VALUE +0.              
016900     03  K-KDORDKL-1             PIC S9(1)  COMP-3 VALUE +1.              
017000     03  K-KDORDKL-2             PIC S9(1)  COMP-3 VALUE +2.              
017100     03  K-KDORDKL-3             PIC S9(1)  COMP-3 VALUE +3.              
017200     03  K-KDORDKL-4             PIC S9(1)  COMP-3 VALUE +4.              
017300     03  K-KDTPOTYP-0            PIC S9(1)  COMP-3 VALUE +0.              
017400     03  K-KDODELSTA-R           PIC  X(1)         VALUE 'R'.             
017500     03  K-KDTRPKAT-A            PIC  X(1)         VALUE 'A'.             
017600     03  K-KDTRPKAT-B            PIC  X(1)         VALUE 'B'.             
017700     03  K-KDTRPKAT-C            PIC  X(1)         VALUE 'C'.             
017800     03  K-IDSYSTEM-IMS          PIC  X(3)         VALUE 'IMS'.           
017900     03  K-TRAN-KDSVAR-0-OK      PIC  X(1)         VALUE '0'.             
018000     03  K-KDORDBEH-ANDRING-OH-9 PIC  X(1)         VALUE '9'.             
018100     03  K-IDKUNDNR-UT-NOLL      PIC  X(6)         VALUE '     0'.        
018200     SKIP2                                                                
018300*                                                                         
018400 01  SWITCH-AREA.                                                         
018500     03  FILLER                  PIC X(16)    VALUE 'SWITCH AREA'.        
018600     03  SW-ORDERDELAR-FINNS     PIC X       VALUE 'J'.                   
018700     03  SW-DATA-FORANDRAT       PIC X       VALUE 'N'.                   
018800     03  SW-KDFRAKT-FORANDRAD    PIC X       VALUE 'N'.                   
018900     03  SW-KDFDKRAV-FORANDRAD   PIC X       VALUE 'N'.                   
019000     EJECT                                                                
019100*                                                                         
019200 01  SPAR-AREA.                                                           
019300     03  FILLER                  PIC X(16)      VALUE 'SPAR AREA'.        
019400     03  SPAR-KVRADER            PIC S9(5)   COMP-3 VALUE +0.             
019500     03  SPAR-SUORDV             PIC S9(9)V9(2)                           
019600                                             COMP-3 VALUE +0.             
019700     03  SPAR-SUORDV-LOC         PIC S9(9)V9(2)                           
019800                                             COMP-3 VALUE +0.             
019900     03  SPAR-SUORDV-LOCPREL     PIC S9(9)V9(2)                           
020000                                             COMP-3 VALUE +0.             
020100     03  SPAR-KDVALISO           PIC X(3)           VALUE SPACE.          
020200*                                                                         
020300     03  SPAR-ARB-BEGMRK-GRP.                                             
020400         05  SPAR-ARB-BEGMRK-RAD1                                         
020500                                 PIC X(30)          VALUE SPACE.          
020600         05  SPAR-ARB-BEGMRK-RAD2                                         
020700                                 PIC X(30)          VALUE SPACE.          
020800     03  SPAR-ARB-KDFRAKT        PIC  9(2)          VALUE ZERO.           
020900                                                                          
021000     03  SPAR-ARB-KDFDKRAV       PIC S9(3) COMP-3 VALUE +0.               
021100*                                                                         
021200     03  SPAR-WDB3-KVLEDTIM-0    PIC S9(3)V9(2)                           
021300                                             COMP-3 VALUE +0.             
021400     03  SPAR-WDB3-KVLEDTIM-1    PIC S9(3)V9(2)                           
021500                                             COMP-3 VALUE +0.             
021600     03  SPAR-WDB3-KVLEDTIM-2    PIC S9(3)V9(2)                           
021700                                             COMP-3 VALUE +0.             
021800     03  SPAR-WDB3-KVLEDTIM-3    PIC S9(3)V9(2)                           
021900                                             COMP-3 VALUE +0.             
022000     03  SPAR-WDB3-KVLEDTIM-4    PIC S9(3)V9(2)                           
022100                                             COMP-3 VALUE +0.             
022200     03  SPAR-WDB5-KDTRPKAT      PIC  X(1)          VALUE SPACE.          
022300     03  SPAR-WDB5-IDTRP.                                                 
022400         05  SPAR-WDB5-IDTRPLOS  PIC X(3)           VALUE SPACE.          
022500         05  SPAR-WDB5-IDTRPVAR  PIC X(2)           VALUE SPACE.          
022600                                                                          
022700     03  SPAR-WDB5-KDFDKRAV      PIC S9(3) COMP-3 VALUE +0.               
022800     EJECT                                                                
022900*                                                                         
023000 01  HELP-AREA.                                                           
023100     03  FILLER                  PIC X(16)      VALUE 'HELP AREA'.        
023200     03  HELP-KVRADER            PIC S9(5)   COMP-3 VALUE +0.             
023300     03  HELP-SUORDV             PIC S9(9)V9(2)                           
023400                                             COMP-3 VALUE +0.             
023500     03  HELP-KVRADER-OPACK      PIC  9(5)          VALUE ZERO.           
023600     03  HELP-SUORDV-OPACK       PIC  9(9)V9(2)     VALUE ZERO.           
023700     03  HELP-IDORDER            PIC  9(7)          VALUE ZERO.           
023800*                                                                         
023900     03  HELP-KDFRAKT            PIC  9(2)          VALUE ZERO.           
024000     03  HELP-KDFRAKT-ALFA    REDEFINES HELP-KDFRAKT                      
024100                                 PIC  X(2).                               
024200     03  HELP-BEGMRK-GRP.                                                 
024300         05  HELP-BEGMRK-RAD1  PIC  X(30)         VALUE SPACE.            
024400         05  HELP-BEGMRK-RAD2  PIC  X(30)         VALUE SPACE.            
024500     EJECT                                                                
024600 01  MESSAGE-CODES.                                                       
024700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
024800     SKIP3                                                                
024900*                                                                         
025000*                                                                         
025100*                                                                         
025200*                 WMEDKONV                                                
025300*                                                                         
025400*   -COPY WMEDAREA                                                        
025500     EJECT                                                                
025600*                                                                         
025700*                                                                         
025800*                                                                         
025900*                 SKÄRMHANTERING                                          
026000*                                                                         
026100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
026200     SKIP3                                                                
026300*01  MID -COPY W4I20101                                                   
026400     EJECT                                                                
026500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
026600     SKIP3                                                                
026700*01  -COPY WMSGAREA                                                       
026800     EJECT                                                                
026900     03  MOD REDEFINES MSG-AREA.                                          
027000*      05  -COPY W4O20101                                                 
027100     EJECT                                                                
027200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
027300     SKIP3                                                                
027400*01  -COPY WMFSAREA                                                       
027500     EJECT                                                                
027600***************************************************************           
027700*                 SUBPROGRAM                                  *           
027800***************************************************************           
027900*                                                                         
028000 01  FILLER                      PIC  X(16) VALUE                         
028100                                               'BEST TRPAVGTID  '.        
028200*                                                                         
028300*01 -COPY W411TRAN                                                        
028400     EJECT                                                                
028500*                                                                         
028600 01  FILLER                      PIC  X(16) VALUE                         
028700                                               'AVSO ORDERAVSL  '.        
028800*                                                                         
028900*01 -COPY W413AVSO                                                        
029000     EJECT                                                                
029100*                                                                         
029200 01  FILLER                      PIC  X(16) VALUE                         
029300                                               'OMRÄKNA VALUTA  '.        
029400*                                                                         
029500*01 -COPY W411EXCH                                                        
029600     EJECT                                                                
029700*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
029800*                                                                         
029900 01  FILLER                       PIC X(16)   VALUE 'IMS-WS'.             
030000                                                                          
030100 01  NYCKLAR-TILL-DLI.                                                    
030200*                                                                         
030300     03  W-WDQ2-IDDC-X.                                                   
030400         05  W-WDQ2-IDDC          PIC  X(2)   VALUE ZERO.                 
030500*                                                                         
030600     03  W-WDQ2CSEQ-X.                                                    
030700         05  W-WDQ2C-IDDISTR      PIC S9(5)   COMP-3 VALUE +0.            
030800         05  W-WDQ2C-IDKUNDNR     PIC S9(7)   COMP-3 VALUE +0.            
030900         05  W-WDQ2C-IDKUNDRF.                                            
031000             07  W-WDQ2C-IDORDNR7 PIC  9(7)          VALUE ZERO.          
031100             07  FILLER           PIC  X(3)          VALUE SPACE.         
031200*                                                                         
031300     03  W-WDQ301KY-MIN-X.                                                
031400         05  W-WDQ3-IDORDER-MIN   PIC S9(7)   COMP-3 VALUE +0.            
031500         05  W-WDQ3-IDDC-MIN      PIC  X(2)   VALUE ZERO.                 
031600         05  W-WDQ3-IDPRODNR-MIN  PIC S9(7)   COMP-3 VALUE +0.            
031700         05  W-WDQ3-IDPLKLIST-MIN PIC S9(3)   COMP-3 VALUE +0.            
031800*                                                                         
031900     03  W-WDQ301KY-MAX-X.                                                
032000         05  W-WDQ3-IDORDER-MAX   PIC S9(7)   COMP-3 VALUE +0.            
032100         05  W-WDQ3-IDDC-MAX      PIC  X(2)   VALUE ZERO.                 
032200         05  W-WDQ3-IDPRODNR-MAX  PIC S9(7)   COMP-3 VALUE +0.            
032300         05  W-WDQ3-IDPLKLIST-MAX PIC S9(3)   COMP-3 VALUE +0.            
032400*                                                                         
032500*                                                                         
032600     03  W-WDB301KY-X.                                                    
032700         05  W-IDDC-WDB3          PIC X(2)    VALUE SPACE.                
032800         05  W-IDDISTR-WDB3       PIC S9(5)   COMP-3 VALUE ZERO.          
032900         05  W-IDKUNDNR-WDB3      PIC S9(7)   COMP-3 VALUE ZERO.          
033000*                                                                         
033100     03  W-WDB301KY-DEF-X.                                                
033200         05  W-IDDC-WDB3-DEF      PIC X(2)    VALUE SPACE.                
033300         05  W-IDDISTR-WDB3-DEF   PIC S9(5)   COMP-3 VALUE ZERO.          
033400         05  W-IDKUNDNR-WDB3-DEF  PIC S9(7) VALUE +9999999 COMP-3.        
033500*                                                                         
033600     03  W-WDB501KY-X.                                                    
033700         05  W-IDDC-WDB5          PIC X(2)    VALUE SPACE.                
033800         05  W-KDFRAKT-WDB5       PIC S9(3)   VALUE ZERO COMP-3.          
033900         05  W-IDDISTR-WDB5       PIC S9(5)   VALUE ZERO COMP-3.          
034000         05  W-IDKUNDNR-WDB5      PIC S9(7)   VALUE ZERO COMP-3.          
034100*                                                                         
034200     03  W-WDB501KY-DEF-X.                                                
034300         05  W-IDDC-WDB5-DEF      PIC X(2)    VALUE SPACE.                
034400         05  W-KDFRAKT-WDB5-DEF   PIC S9(3)   VALUE ZERO COMP-3.          
034500         05  W-IDDISTR-WDB5-DEF   PIC S9(5)   VALUE ZERO COMP-3.          
034600         05  W-IDKUNDNR-WDB5-DEF  PIC S9(7) VALUE +9999999 COMP-3.        
034700*                                                                         
034800     03  W-IDDC-B6-X.                                                     
034900         05 W-IDDC-B6                  PIC X(2).                          
035000     EJECT                                                                
036000*    --- STATUS-KOD FRÅN IMS                                              
037000 01  STATUS-WS                    PIC X(2).                               
037100     88  SEGMENT-FINNS                       VALUE '  '.                  
037200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
037300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
037400     88  ANNAT-SEGMENT                       VALUE 'GK'.                  
037500     SKIP2                                                                
037600 01  GODK-STATUSKODER.                                                    
037700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
037800     SKIP3                                                                
037900 01  SSA1                        PIC X(128).                              
038000 01  SSA2                        PIC X(128).                              
038100 01  SSA3                        PIC X(128).                              
038200     EJECT                                                                
038300*    --- IMS FUNKTIONSKODER                                               
038400*01  -COPY W0003                                                          
038500     EJECT                                                                
038600*    ---  DLI INPUT-OUTPUT AREA                                           
038700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
038800     SKIP3                                                                
038900 01  FILLER                      PIC X(16)   VALUE                        
039000                                               'KUNDREG DCINFO  '.        
039100 01  DLI-IO-AREA-WDB3.                                                    
039200*    03  -COPY WDB301                                                     
039300     EJECT                                                                
039400 01  FILLER                      PIC X(16)   VALUE                        
039500                                               'KUNDREG FRAKTINF'.        
039600 01  DLI-IO-AREA-WDB5.                                                    
039700*    03  -COPY WDB501                                                     
039800     EJECT                                                                
039900 01  FILLER                      PIC X(16)   VALUE                        
040000                                               'ORDERDEL REG KÖ '.        
040100 01  DLI-IO-AREA-ODEL.                                                    
040200*    03  -COPY WDQ301                                                     
040300     EJECT                                                                
040400 01  FILLER                      PIC X(16)   VALUE                        
040503                                               'WDQ201 AREA     '.        
040603 01  DLI-IO-AREA-Q201.                                                    
040700*    03  -COPY WDQ201                                                     
040800     EJECT                                                                
040900 01  FILLER                      PIC X(16)   VALUE                        
041003                                               'WDQ211 AREA     '.        
041103 01  DLI-IO-AREA-Q211.                                                    
041200*    03  -COPY WDQ211                                                     
041300     EJECT                                                                
041400 01  FILLER                      PIC X(16)   VALUE                        
041503                                               'WDQ212 AREA     '.        
041603 01  DLI-IO-AREA-Q212.                                                    
041700*    03  -COPY WDQ212                                                     
041800     EJECT                                                                
041900 01  FILLER                      PIC X(16)   VALUE                        
041903                                               'WDQ221 AREA     '.        
042003 01  DLI-IO-AREA-Q221.                                                    
042103*    03  -COPY WDQ221                                                     
042203     EJECT                                                                
042303 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
042403 01   DLI-IO-AREA-B601.                                                   
043000*     03  -COPY WDB601                                                    
044000     EJECT                                                                
044100 LINKAGE SECTION.                                                         
044200*01  -COPY W0009      -PRE MSG-                                           
044300     EJECT                                                                
044400*01  -COPY W0008      -PRE WDQ2-                                          
044500     05  FILLER                  PIC X.                                   
044600     EJECT                                                                
044700*01  -COPY W0008      -PRE USEA-                                          
044800     05  FILLER                  PIC X.                                   
044900     EJECT                                                                
045000*01  -COPY W0008      -PRE ORQA-                                          
045100     05  FILLER                  PIC X.                                   
045200     EJECT                                                                
045300*01  -COPY W0008      -PRE GMTB-                                          
045400     05  FILLER                  PIC X.                                   
045500     EJECT                                                                
045600*01  -COPY W0008      -PRE GMTC-                                          
045700     05  FILLER                  PIC X.                                   
045800     EJECT                                                                
045900     05  FILLER                  PIC X.                                   
046000     EJECT                                                                
047000*01  -COPY W0008      -PRE WDB6-                                          
047100     05  FILLER                  PIC X.                                   
047200     EJECT                                                                
047300     SKIP3                                                                
047400 01  TRAN-XXKB-PCB               PIC X.                                   
047500 01  AVSO-WDE6-PCB               PIC X.                                   
047600 01  AVSO-ORQA-PCB               PIC X.                                   
047700 01  AVSO-WDQ2-PCB               PIC X.                                   
047800 01  AVSO-GMTB-PCB               PIC X.                                   
047900 01  AVSO-XXKA-PCB               PIC X.                                   
048000 01  AVSO-4437-PCB               PIC X.                                   
048100 01  AVSO-XXKE-PCB               PIC X.                                   
048200 01  AVSO-XXKF-PCB               PIC X.                                   
048300 01  AVSO-XXKG-PCB               PIC X.                                   
048400 01  AVSO-XXKH-PCB               PIC X.                                   
048500 01  AVSO-XXKI-PCB               PIC X.                                   
048600 01  AVSO-XXKP-PCB               PIC X.                                   
048700 01  AVSO-WDB2-PCB               PIC X.                                   
048800 01  AVSO-WDB6-PCB               PIC X.                                   
048900 01  ORDN-ORQL-PCB               PIC X.                                   
049000 01  ORDN-PROC-PCB               PIC X.                                   
049100 01  ORDN-ORQI-PCB               PIC X.                                   
049200 01  ORDN-WDQ3-PCB               PIC X.                                   
049300     EJECT                                                                
049400 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
049500                           WDQ2-PCB ORQA-PCB GMTB-PCB                     
049600                           GMTC-PCB                                       
049700                           WDB6-PCB                                       
049800                                                                          
049900                           TRAN-XXKB-PCB                                  
050000                                                                          
050100                           AVSO-WDE6-PCB AVSO-ORQA-PCB                    
050200                           AVSO-WDQ2-PCB                                  
050300                           AVSO-GMTB-PCB AVSO-XXKA-PCB                    
050400                           AVSO-4437-PCB AVSO-XXKE-PCB                    
050500                           AVSO-XXKF-PCB AVSO-XXKG-PCB                    
050600                           AVSO-XXKH-PCB AVSO-XXKI-PCB                    
050700                           AVSO-XXKP-PCB AVSO-WDB2-PCB                    
050800                           AVSO-WDB6-PCB                                  
050900                           ORDN-ORQL-PCB ORDN-PROC-PCB                    
051000                           ORDN-ORQI-PCB ORDN-WDQ3-PCB.                   
051100                                                                          
051200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
051300                           WDQ2-PCB ORQA-PCB GMTB-PCB                     
051400                           GMTC-PCB                                       
051500                           WDB6-PCB                                       
051600                                                                          
051700                           TRAN-XXKB-PCB                                  
051800                                                                          
051900                           AVSO-WDE6-PCB AVSO-ORQA-PCB                    
052000                           AVSO-WDQ2-PCB                                  
052100                           AVSO-GMTB-PCB AVSO-XXKA-PCB                    
052200                           AVSO-4437-PCB AVSO-XXKE-PCB                    
052300                           AVSO-XXKF-PCB AVSO-XXKG-PCB                    
052400                           AVSO-XXKH-PCB AVSO-XXKI-PCB                    
052500                           AVSO-XXKP-PCB AVSO-WDB2-PCB                    
052600                           AVSO-WDB6-PCB                                  
052700                           ORDN-ORQL-PCB ORDN-PROC-PCB                    
052800                           ORDN-ORQI-PCB ORDN-WDQ3-PCB.                   
052900                                                                          
053000     PERFORM IMS-GET-MSG                                                  
053100     IF SEGMENT-FINNS                                                     
053200                                                                          
053300       MOVE JA TO ALLT-SW                                                 
053400                                                                          
053500       PERFORM A-INIT                                                     
053600       PERFORM B-KOLLA-NYCKLAR                                            
053700       IF NYCKLAR-OK AND                                                  
053800          ALLT-OK                                                         
053900         PERFORM F-LAES-VISA-INFO                                         
054000         IF ALLT-OK                                                       
054100           PERFORM G-KONTROLLERA-USERS-AVSIKT                             
054200           IF MFS-UPDATE                                                  
054300             IF ALLT-OK                                                   
054400               PERFORM H-KOLLA-INDATA-UPPDATERA-OH                        
054500             END-IF                                                       
054600           END-IF                                                         
054700         END-IF                                                           
054800       END-IF                                                             
054900       PERFORM IMS-INSERT-MSG                                             
055000     END-IF                                                               
055100                                                                          
055200     MOVE   ZERO TO RETURN-CODE                                           
055300     GOBACK                                                               
055400     .                                                                    
055500     EJECT                                                                
055600 A-INIT SECTION.                                                          
055700                                                                          
055800     IF MSG-DUBBLA-TRANSKODER                                             
055900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I20101                 
056000       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
056100       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
056200     ELSE                                                                 
056300       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I20101                 
056400       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
056500       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
056600     END-IF                                                               
056700                                                                          
056800     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
056900     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
057000     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
057100                                                                          
057200     MOVE LOW-VALUE                       TO MSG-AREA                     
057300     MOVE 'W4O201N1'                      TO MFS-IDMOD                    
057400     MOVE '4201'                          TO MOD-IDTRANS                  
057500     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
057600                                             MOD-TEMFSINF                 
057700     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O20101 + 4                        
057800                                                                          
057900     IF NOT EGEN-MID                                                      
058000       MOVE SPACE                         TO MFS-KDTRTYP                  
058100       MOVE '7'                           TO MFS-IDPFK                    
058200     END-IF                                                               
058300     MOVE SPACE                           TO SPAR-KDVALISO                
058400                                                                          
058500     .                                                                    
058600     EJECT                                                                
058700 B-KOLLA-NYCKLAR SECTION.                                                 
058800                                                                          
058900     MOVE ALL '+'              TO MSGI-WMSGINIT                           
059000     MOVE '001'                TO MSGI-KDCALL                             
059100     MOVE MSG-SIGNON-USERID    TO MSGI-IDUSER                             
059200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
059300     MOVE '4201'            TO MSGI-IDTRANS                               
059400     IF EGEN-MID                                                          
059500        MOVE MID-IDDISTR-IN    TO MSGI-IDDISTR                            
059600        MOVE MID-IDKUNDNR-IN   TO MSGI-IDKUNDNR                           
059700        MOVE MID-IDORDNR7-IN   TO W-SPAR-IDORDNR7                         
059800        MOVE W-SPAR-IDKUNDRF   TO MSGI-IDKUNDRF                           
059900     END-IF                                                               
060000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
060100     MOVE MSGI-IDLAND-SPR      TO MED-IDSKYLT                             
060200     MOVE    JA               TO    NYCKLAR-SW                            
060300                                                                          
060400     MOVE    LOW-VALUE        TO    W-WDQ301KY-MIN-X                      
060500     MOVE    HIGH-VALUE       TO    W-WDQ301KY-MAX-X                      
060600                                                                          
060700     MOVE    MFS-RENSA-FAELT  TO    MOD-IDDISTR-IN                        
060800                                    MOD-IDKUNDNR-IN                       
060900                                    MOD-IDORDNR7-IN                       
061000                                    MOD-IDDC-IN                           
061100                                                                          
061200     PERFORM BA-KONTROLLERA-IDDISTR                                       
061300     PERFORM BB-KONTROLLERA-IDKUNDNR                                      
061400     PERFORM BC-KONTROLLERA-IDKUNDRF                                      
061500     PERFORM BD-KONTROLLERA-IDDC                                          
061600                                                                          
061700     IF NYCKLAR-FEL                                                       
061800       IF EGEN-MID                                                        
061900         MOVE  ERR-WRONG-KEY   TO    MED-IDMFSFEL                         
062000         CALL  WMEDKONV        USING MED-WMEDAREA                         
062100         MOVE  MED-MFSFEL      TO    MOD-TEMFSFEL                         
062200       END-IF                                                             
062300                                                                          
062400       PERFORM MFS-RENSA-FAELT-UT                                         
062500       MOVE    MFS-RENSA-FAELT TO    MOD-IDDISTR-UT                       
062600                                     MOD-IDKUNDNR-UT                      
062700                                     MOD-IDORDNR7-UT                      
062800     ELSE                                                                 
062900       IF NOT EGEN-MID                                                    
063000         PERFORM BE-INITIERA-UPPDATERINGS-FALT                            
063100       ELSE                                                               
063200         PERFORM BF-KOLLA-UPPDATERINGS-FALT                               
063300       END-IF                                                             
063400                                                                          
063500       IF NOT ALLT-OK                                                     
063600         CALL    WMEDKONV     USING MED-WMEDAREA                          
063700         MOVE    MED-MFSFEL   TO    MOD-TEMFSFEL                          
063800         PERFORM MFS-ROR-EJ-FAELT-UT                                      
063900       END-IF                                                             
064000     END-IF                                                               
064100     .                                                                    
064200     EJECT                                                                
064300 BA-KONTROLLERA-IDDISTR SECTION.                                          
064400                                                                          
064500     IF MID-IDDISTR-IN         NOT = ALL '+'                              
064600       MOVE    '7'             TO        MFS-IDPFK                        
064700       MOVE    SPACE           TO        MFS-KDTRTYP                      
064800     END-IF                                                               
064900                                                                          
065000     MOVE      MSGI-IDDISTR    TO        MOD-IDDISTR-UT                   
065100     INSPECT   MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE            
065200                                                                          
065300     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
065400       MOVE    MSGI-IDDISTR    TO        W-WDQ2C-IDDISTR                  
065500                                         W-IDDISTR-WDB3                   
065600                                         W-IDDISTR-WDB5                   
065700                                         W-IDDISTR-WDB3-DEF               
065800                                         W-IDDISTR-WDB5-DEF               
065900                                         TEST-IDDISTR                     
066000     ELSE                                                                 
066100       MOVE    NEJ             TO        NYCKLAR-SW                       
066200     END-IF                                                               
066300                                                                          
066400     .                                                                    
066500     EJECT                                                                
066600 BB-KONTROLLERA-IDKUNDNR SECTION.                                         
066700                                                                          
066800     IF MID-IDKUNDNR-IN           NOT = ALL '+'                           
066900       MOVE    '7'                TO        MFS-IDPFK                     
067000       MOVE    SPACE              TO        MFS-KDTRTYP                   
068000     END-IF                                                               
069000                                                                          
069100     MOVE MSGI-IDKUNDNR           TO MOD-IDKUNDNR-UT                      
069200     INSPECT MOD-IDKUNDNR-UT    REPLACING LEADING ZERO BY SPACE           
069300                                                                          
069400     IF MSGI-IDKUNDNR NUMERIC                                             
069500       MOVE    MSGI-IDKUNDNR     TO        W-WDQ2C-IDKUNDNR               
069600                                           W-IDKUNDNR-WDB3                
069700                                           W-IDKUNDNR-WDB5                
069800                                           TEST-IDKUNDNR                  
069900     ELSE                                                                 
070000       MOVE    NEJ                TO        NYCKLAR-SW                    
070100     END-IF                                                               
070200     .                                                                    
070300     EJECT                                                                
070400 BC-KONTROLLERA-IDKUNDRF SECTION.                                         
070500                                                                          
070600     IF MID-IDORDNR7-IN         NOT  = ALL '+'                            
070700       MOVE    '7'              TO        MFS-IDPFK                       
070800       MOVE    SPACE            TO        MFS-KDTRTYP                     
070900     END-IF                                                               
071000                                                                          
071100     MOVE      MSGI-IDKUNDRF(1:7) TO        MOD-IDORDNR7-UT               
071200     INSPECT   MOD-IDORDNR7-UT  REPLACING LEADING ZERO BY SPACE           
071300                                                                          
071400     IF MSGI-IDKUNDRF(1:7)          NUMERIC AND                           
071500        MSGI-IDKUNDRF(1:7)          > ZERO                                
071600       MOVE    SPACE                TO        W-WDQ2C-IDKUNDRF            
071700       MOVE    MSGI-IDKUNDRF(1:7)   TO        W-WDQ2C-IDORDNR7            
071800     ELSE                                                                 
071900       MOVE    NEJ                  TO        NYCKLAR-SW                  
072000     END-IF                                                               
072100     .                                                                    
072200     EJECT                                                                
072300 BD-KONTROLLERA-IDDC SECTION.                                             
072400                                                                          
072500     MOVE MSGI-IDDC              TO        W-IDDC-B6                      
072600                                           MOD-IDDC-UT                    
072700     PERFORM IMS-GU-WDB601                                                
072800                                                                          
072900     IF DCS-KDDC > SPACE AND NOT DCS-DDC AND NYCKLAR-OK                   
073000       MOVE      DCS-IDDC          TO      W-WDQ2-IDDC                    
073100                                           W-WDQ3-IDDC-MIN                
073200                                           W-WDQ3-IDDC-MAX                
073300                                           W-IDDC-WDB3                    
073400                                           W-IDDC-WDB3-DEF                
073500                                           W-IDDC-WDB5                    
073600                                           W-IDDC-WDB5-DEF                
073700     ELSE                                                                 
073800       MOVE      NEJ             TO        NYCKLAR-SW                     
073900     END-IF                                                               
074000     .                                                                    
074100     EJECT                                                                
074200 BE-INITIERA-UPPDATERINGS-FALT SECTION.                                   
074300                                                                          
074400     MOVE SPACE  TO WS-BELAGINS-DEL1                                      
074500                    WS-BELAGINS-DEL2                                      
074600                    WS-BEGMT-RAD1                                         
074700                    WS-BEGMT-RAD2                                         
074800                    WS-ADGMT-GATA                                         
074900                    WS-ADGMT-PADR                                         
075000                    WS-ADGMT-LAND                                         
075100                    WS-BEGMRK-RAD1                                        
075200                    WS-BEGMRK-RAD2                                        
075300                    WS-BEKUNDRF                                           
075400                                                                          
075500     MOVE ZERO   TO WS-KDFRAKT                                            
075600     .                                                                    
075700     EJECT                                                                
075800 BF-KOLLA-UPPDATERINGS-FALT SECTION.                                      
075900                                                                          
076000     IF MID-BELAGINS-DEL1 = ALL '+'                                       
076100       MOVE SPACE             TO WS-BELAGINS-DEL1                         
076200     ELSE                                                                 
076300       MOVE MID-BELAGINS-DEL1 TO WS-BELAGINS-DEL1                         
076400     END-IF                                                               
076500                                                                          
076600     IF MID-BELAGINS-DEL2 = ALL '+'                                       
076700       MOVE SPACE             TO WS-BELAGINS-DEL2                         
076800     ELSE                                                                 
076900       MOVE MID-BELAGINS-DEL2 TO WS-BELAGINS-DEL2                         
077000     END-IF                                                               
077100                                                                          
077200     IF MID-BEGMT-RAD1  = ALL '+'                                         
077300       MOVE SPACE             TO WS-BEGMT-RAD1                            
077400     ELSE                                                                 
077500       MOVE MID-BEGMT-RAD1    TO WS-BEGMT-RAD1                            
077600     END-IF                                                               
077700                                                                          
077800     IF MID-BEGMT-RAD2  = ALL '+'                                         
077900       MOVE SPACE             TO WS-BEGMT-RAD2                            
078000     ELSE                                                                 
078100       MOVE MID-BEGMT-RAD2    TO WS-BEGMT-RAD2                            
078200     END-IF                                                               
078300                                                                          
078400     IF MID-ADGMT-GATA  = ALL '+'                                         
078500       MOVE SPACE             TO WS-ADGMT-GATA                            
078600     ELSE                                                                 
078700       MOVE MID-ADGMT-GATA    TO WS-ADGMT-GATA                            
078800     END-IF                                                               
078900                                                                          
079000     IF MID-ADGMT-PADR  = ALL '+'                                         
079100       MOVE SPACE             TO WS-ADGMT-PADR                            
079200     ELSE                                                                 
079300       MOVE MID-ADGMT-PADR    TO WS-ADGMT-PADR                            
079400     END-IF                                                               
079500                                                                          
079600     IF MID-ADGMT-LAND  = ALL '+'                                         
079700       MOVE SPACE             TO WS-ADGMT-LAND                            
079800     ELSE                                                                 
079900       MOVE MID-ADGMT-LAND    TO WS-ADGMT-LAND                            
080000     END-IF                                                               
080100                                                                          
080200     IF MID-BEGMRK-RAD1 = ALL '+'                                         
080300       MOVE SPACE             TO WS-BEGMRK-RAD1                           
080400     ELSE                                                                 
080500       MOVE MID-BEGMRK-RAD1 TO WS-BEGMRK-RAD1                             
080600     END-IF                                                               
080700                                                                          
080800     IF MID-BEGMRK-RAD2 = ALL '+'                                         
080900       MOVE SPACE             TO WS-BEGMRK-RAD2                           
081000     ELSE                                                                 
081100       MOVE MID-BEGMRK-RAD2 TO WS-BEGMRK-RAD2                             
081200     END-IF                                                               
081300                                                                          
081400     PERFORM BFA-KOLLA-MID-KDFRAKT                                        
081500                                                                          
081600     IF MID-BEKUNDRF      = ALL '+'                                       
081700       MOVE SPACE             TO WS-BEKUNDRF                              
081800     ELSE                                                                 
081900       MOVE MID-BEKUNDRF      TO WS-BEKUNDRF                              
082000     END-IF                                                               
082100     .                                                                    
082200     EJECT                                                                
082300 BFA-KOLLA-MID-KDFRAKT SECTION.                                           
082400                                                                          
082500     IF MID-KDFRAKT = ALL '+'                                             
082600       MOVE ZERO TO WS-KDFRAKT                                            
082700     ELSE                                                                 
082800       MOVE    MID-KDFRAKT TO        WS-KDFRAKT                           
082900       INSPECT WS-KDFRAKT  REPLACING LEADING SPACES BY ZEROES             
083000       MOVE    ZERO        TO        WS-TALLY                             
083100       INSPECT WS-KDFRAKT  TALLYING  WS-TALLY FOR CHARACTERS              
083200                                                  BEFORE SPACE            
083300       IF WS-TALLY > ZERO                                                 
083400         MOVE WS-KDFRAKT (1:WS-TALLY) TO HELP-KDFRAKT                     
083500         MOVE HELP-KDFRAKT-ALFA       TO WS-KDFRAKT                       
083600       END-IF                                                             
083700       IF WS-KDFRAKT NOT NUMERIC                                          
083800         MOVE NEJ                         TO ALLT-SW                      
083900         MOVE MFS-ROER-EJ-FAELT           TO MOD-KDFRAKT                  
084000         MOVE MFS-NUM-FAELT-FEL           TO MOD-KDFRAKT-ATTR             
084100         MOVE FELM-KORR-UPPLYSTA-FALT-001 TO MED-IDMFSFEL                 
084200       ELSE                                                               
084300         IF WS-KDFRAKT = ZERO                                             
084400           MOVE NEJ                          TO ALLT-SW                   
084500           MOVE MFS-NUM-FAELT-FEL            TO MOD-KDFRAKT-ATTR          
084600           MOVE  FELM-KORR-UPPLYSTA-FALT-001 TO MED-IDMFSFEL              
084700         END-IF                                                           
084800       END-IF                                                             
084900       MOVE WS-KDFRAKT-NUM TO W-KDFRAKT-WDB5                              
085000                              W-KDFRAKT-WDB5-DEF                          
085100     END-IF                                                               
085200     .                                                                    
085300     EJECT                                                                
085400 F-LAES-VISA-INFO SECTION.                                                
085500                                                                          
085600     PERFORM FA-LAES-ORDERHUVUD-MHA-C-INDEX                               
085700                                                                          
085800     IF ALLT-OK                                                           
085900                                                                          
086000       PERFORM FC-LAES-ARBTAB                                             
086100                                                                          
086200       IF ALLT-OK                                                         
086300                                                                          
086400         IF SW-ORDERDELAR-FINNS = NEJ                                     
086500           PERFORM FD-HAMTA-RADER-ORDV-FRAN-Q211                          
086600           PERFORM FE-HAMTA-RADER-ORDV-FRAN-Q221                          
086700         END-IF                                                           
086800                                                                          
086900         PERFORM FF-FYLL-MOD-MED-RADER-ORDERV                             
087000                                                                          
087100       END-IF                                                             
087200     END-IF                                                               
087300                                                                          
087400     IF NOT ALLT-OK                                                       
087500       CALL    WMEDKONV   USING MED-WMEDAREA                              
087600       MOVE    MED-MFSFEL TO    MOD-TEMFSFEL                              
087700       PERFORM MFS-RENSA-FAELT-UT                                         
087800     END-IF                                                               
087900     .                                                                    
088000     EJECT                                                                
088100 FA-LAES-ORDERHUVUD-MHA-C-INDEX SECTION.                                  
088200                                                                          
088300     PERFORM IMS-GET-WDQ201-CSEQ                                          
088400                                                                          
088500     IF SEGMENT-SAKNAS                                                    
088600       MOVE NEJ                   TO ALLT-SW                              
088700       MOVE FELM-ORDER-SAKNAS-701 TO MED-IDMFSFEL                         
088800     ELSE                                                                 
088900       MOVE    OHUV-IDORDER TO W-WDQ3-IDORDER-MIN                         
089000                               W-WDQ3-IDORDER-MAX                         
089100                                                                          
089200       IF OHUV-FLBORT = JA                                                
089300         MOVE NEJ                        TO ALLT-SW                       
089400         MOVE FELM-ORDERN-ANNULLERAD-052 TO MED-IDMFSFEL                  
089500       ELSE                                                               
089600         IF OHUV-KDTPOTYP > K-KDTPOTYP-0  OR                              
089700            OHUV-FLKLAR   = NEJ                                           
089800           MOVE NEJ                       TO ALLT-SW                      
089900           MOVE FELM-OH-KAN-EJ-ANDRAS-062 TO MED-IDMFSFEL                 
090000         ELSE                                                             
090100           IF OHUV-IDDC-TVS     > ZERO    AND                             
090200              OHUV-IDDC-TVS NOT = MSGI-IDDC                               
090300             MOVE NEJ                         TO ALLT-SW                  
090400             MOVE FELM-ORADER-SAKNAS-F-DC-028 TO MED-IDMFSFEL             
090500           ELSE                                                           
090600             PERFORM FAA-TA-HAND-OM-OHUV-INFO                             
090700           END-IF                                                         
090800         END-IF                                                           
090900       END-IF                                                             
091000     END-IF                                                               
091100     .                                                                    
091200     EJECT                                                                
091300 FAA-TA-HAND-OM-OHUV-INFO SECTION.                                        
091400                                                                          
091500     IF MFS-UPDATE                                                        
091600       CONTINUE                                                           
091700     ELSE                                                                 
091800       MOVE OHUV-BELAGINS-DEL1 TO MOD-BELAGINS-DEL1                       
091900       MOVE OHUV-BELAGINS-DEL2 TO MOD-BELAGINS-DEL2                       
092000       MOVE OHUV-BEGMT-RAD1    TO MOD-BEGMT-RAD1                          
092100       MOVE OHUV-BEGMT-RAD2    TO MOD-BEGMT-RAD2                          
092200       MOVE OHUV-ADGMT-GATA    TO MOD-ADGMT-GATA                          
092300       MOVE OHUV-ADGMT-PADR    TO MOD-ADGMT-PADR                          
092400       MOVE OHUV-ADGMT-LAND    TO MOD-ADGMT-LAND                          
092500       MOVE OHUV-BEKUNDRF      TO MOD-BEKUNDRF                            
092600     END-IF                                                               
092700     .                                                                    
092800     EJECT                                                                
092900 FC-LAES-ARBTAB SECTION.                                                  
093000                                                                          
093100     PERFORM IMS-GNP-WDQ212-FIRST                                         
093200                                                                          
093300     IF SEGMENT-FINNS                                                     
093400       IF MFS-UPDATE                                                      
093500         MOVE ARB-BEGMRK         TO SPAR-ARB-BEGMRK-GRP                   
093600         MOVE ARB-KDFRAKT        TO SPAR-ARB-KDFRAKT                      
093700         MOVE ARB-KDFDKRAV       TO SPAR-ARB-KDFDKRAV                     
093800       ELSE                                                               
093900         MOVE ARB-BEGMRK         TO HELP-BEGMRK-GRP                       
094000         MOVE HELP-BEGMRK-RAD1   TO MOD-BEGMRK-RAD1                       
094100         MOVE HELP-BEGMRK-RAD2   TO MOD-BEGMRK-RAD2                       
094200         MOVE ARB-KDFRAKT        TO HELP-KDFRAKT                          
094300         MOVE HELP-KDFRAKT       TO MOD-KDFRAKT                           
094400       END-IF                                                             
094500       IF ARB-TIRFS > ZERO                                                
094600         PERFORM FCA-LAES-ORDERDELAR                                      
094700       ELSE                                                               
094800         MOVE NEJ TO SW-ORDERDELAR-FINNS                                  
094900       END-IF                                                             
095000     ELSE                                                                 
095100       IF MFS-UPDATE                                                      
095200         CONTINUE                                                         
095300       ELSE                                                               
095400         MOVE NEJ                         TO ALLT-SW                      
095500         MOVE FELM-ORADER-SAKNAS-F-DC-028 TO MED-IDMFSFEL                 
095600       END-IF                                                             
095700     END-IF                                                               
095800     .                                                                    
095900     EJECT                                                                
096000 FCA-LAES-ORDERDELAR SECTION.                                             
096100                                                                          
096200     PERFORM IMS-GN-ORQA01-INTERV                                         
096300                                                                          
096400     IF SEGMENT-FINNS                                                     
096500                                                                          
096600       PERFORM UNTIL SEGMENT-SAKNAS                  OR                   
096700                     ODEL-KDODELSTA NOT = K-KDODELSTA-R                   
096800                                                                          
096900         IF ODEL-KVRADER > ZERO                                           
097000           COMPUTE SPAR-KVRADER = SPAR-KVRADER + ODEL-KVRADER             
097100         END-IF                                                           
097200         IF ODEL-SUORDV > ZERO                                            
097300           COMPUTE SPAR-SUORDV  = SPAR-SUORDV  + ODEL-SUORDV              
097400         END-IF                                                           
097500                                                                          
097600         IF ODEL-SUORDV-LOC > ZERO                                        
097700           COMPUTE SPAR-SUORDV-LOC      =                                 
097800                   SPAR-SUORDV-LOC      + ODEL-SUORDV-LOC                 
097900         END-IF                                                           
098000         IF ODEL-SUORDV-LOCPREL > ZERO                                    
098100           COMPUTE SPAR-SUORDV-LOCPREL  =                                 
098200                   SPAR-SUORDV-LOCPREL  + ODEL-SUORDV-LOCPREL             
098300         END-IF                                                           
098400                                                                          
098500         IF SPAR-KDVALISO = SPACE                                         
098600            MOVE ODEL-KDVALISO      TO SPAR-KDVALISO                      
098700         END-IF                                                           
098800                                                                          
098900         PERFORM IMS-GN-ORQA01-INTERV                                     
099000       END-PERFORM                                                        
099100                                                                          
099200       IF SEGMENT-FINNS                                                   
099300         MOVE NEJ                       TO ALLT-SW                        
099400         MOVE FELM-OH-KAN-EJ-ANDRAS-062 TO MED-IDMFSFEL                   
099500       END-IF                                                             
099600     ELSE                                                                 
099700       MOVE NEJ  TO SW-ORDERDELAR-FINNS                                   
099800       MOVE ZERO TO SPAR-KVRADER                                          
099900                    SPAR-SUORDV                                           
100000                    SPAR-SUORDV-LOC                                       
100100                    SPAR-SUORDV-LOCPREL                                   
100200     END-IF                                                               
100300     .                                                                    
100400     EJECT                                                                
100500 FD-HAMTA-RADER-ORDV-FRAN-Q211 SECTION.                                   
100600                                                                          
100700     PERFORM IMS-GNP-WDQ211-OKVAL-FIRST                                   
100800                                                                          
100900     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
101000                   ANNAT-SEGMENT                                          
101100                                                                          
101200       IF DIRL-KVRADER > ZERO                                             
101300         COMPUTE SPAR-KVRADER = SPAR-KVRADER + DIRL-KVRADER               
101400       END-IF                                                             
101500                                                                          
101600       IF DIRL-SUORDV  > ZERO                                             
101700         COMPUTE SPAR-SUORDV  = SPAR-SUORDV  + DIRL-SUORDV                
101800       END-IF                                                             
101900                                                                          
102000       IF DIRL-SUORDV-LOC  > ZERO                                         
102100         COMPUTE SPAR-SUORDV-LOC      =                                   
102200                 SPAR-SUORDV-LOC      + DIRL-SUORDV-LOC                   
102300       END-IF                                                             
102400                                                                          
102500       IF DIRL-SUORDV-LOCPREL   > ZERO                                    
102600         COMPUTE SPAR-SUORDV-LOCPREL  =                                   
102700                 SPAR-SUORDV-LOCPREL  + DIRL-SUORDV-LOCPREL               
102800       END-IF                                                             
102900       IF SPAR-KDVALISO = SPACE                                           
103000          MOVE DIRL-KDVALISO   TO SPAR-KDVALISO                           
103100       END-IF                                                             
103200                                                                          
103300       PERFORM IMS-GNP-WDQ211-OKVAL                                       
103400     END-PERFORM                                                          
103500     .                                                                    
103600     EJECT                                                                
103700 FE-HAMTA-RADER-ORDV-FRAN-Q221 SECTION.                                   
103800                                                                          
103900     IF ARB-KDTRPKAT = K-KDTRPKAT-B OR                                    
104000                       K-KDTRPKAT-C                                       
104100                                                                          
104200       PERFORM IMS-GNP-WDQ221                                             
104300       PERFORM UNTIL SEGMENT-SAKNAS                                       
104400                                                                          
104600         COMPUTE SPAR-KVRADER        = SPAR-KVRADER                       
104700                                     + LOR-KVRADER                        
104900                                                                          
105100         COMPUTE SPAR-SUORDV         = SPAR-SUORDV                        
105200                                     + LOR-SUORDV                         
105500         COMPUTE SPAR-SUORDV-LOC     = SPAR-SUORDV-LOC                    
105600                                     + LOR-SUORDV-LOC                     
105900         COMPUTE SPAR-SUORDV-LOCPREL = SPAR-SUORDV-LOCPREL                
106000                                     + LOR-SUORDV-LOCPREL                 
106102         IF SPAR-KDVALISO = SPACE                                         
106302           MOVE LOR-KDVALISO        TO SPAR-KDVALISO                      
106402         END-IF                                                           
106500                                                                          
106600         PERFORM IMS-GNP-WDQ221                                           
106800       END-PERFORM                                                        
106900                                                                          
107000     END-IF                                                               
107100     .                                                                    
107200     EJECT                                                                
107300 FF-FYLL-MOD-MED-RADER-ORDERV SECTION.                                    
107400                                                                          
107500     MOVE SPAR-KVRADER       TO HELP-KVRADER-OPACK                        
107600     MOVE HELP-KVRADER-OPACK TO MOD-KVORDRAD                              
107700     MOVE SPAR-SUORDV        TO HELP-SUORDV-OPACK                         
107800*    FLYTTA UT RÄTT PRIS                                                  
107900     IF DIST79-DEALER-PRICE OR                                            
108100        DIST79-ECOM-PRICE                                                 
108200                                                                          
108300       COMPUTE SPAR-SUORDV-LOC = SPAR-SUORDV-LOC +                        
108400                                 SPAR-SUORDV-LOCPREL                      
108500       MOVE SPAR-SUORDV-LOC  TO HELP-SUORDV-OPACK                         
108600                                                                          
108700       IF SPAR-SUORDV-LOCPREL  > 0                                        
108800         MOVE '*' TO MOD-TEASTRIX                                         
108900       ELSE                                                               
109000         MOVE ' ' TO MOD-TEASTRIX                                         
109100       END-IF                                                             
110000     END-IF                                                               
111000     MOVE HELP-SUORDV-OPACK TO MOD-SUORDV                                 
112000     MOVE SPAR-KDVALISO     TO WS-KDVALISO                                
112100     MOVE WS-TEDDI          TO MOD-TEDDI                                  
112200                                                                          
112300     .                                                                    
112400     EJECT                                                                
112500 G-KONTROLLERA-USERS-AVSIKT SECTION.                                      
112600                                                                          
112700     MOVE NEJ TO SW-DATA-FORANDRAT                                        
112800                                                                          
112900     IF WS-BELAGINS-DEL1 NOT = OHUV-BELAGINS-DEL1                         
113000       MOVE JA TO SW-DATA-FORANDRAT                                       
113100     ELSE                                                                 
113200      IF WS-BELAGINS-DEL2 NOT = OHUV-BELAGINS-DEL2 AND                    
113300         SW-DATA-FORANDRAT    = NEJ                                       
113400        MOVE JA TO SW-DATA-FORANDRAT                                      
113500      ELSE                                                                
113600       IF WS-BEGMT-RAD1  NOT = OHUV-BEGMT-RAD1 AND                        
113700          SW-DATA-FORANDRAT    = NEJ                                      
113800         MOVE JA TO SW-DATA-FORANDRAT                                     
113900       ELSE                                                               
114000        IF WS-BEGMT-RAD2  NOT = OHUV-BEGMT-RAD2  AND                      
114100           SW-DATA-FORANDRAT    = NEJ                                     
114200          MOVE JA TO SW-DATA-FORANDRAT                                    
114300        ELSE                                                              
114400         IF WS-ADGMT-GATA  NOT = OHUV-ADGMT-GATA  AND                     
114500            SW-DATA-FORANDRAT    = NEJ                                    
114600           MOVE JA TO SW-DATA-FORANDRAT                                   
114700         ELSE                                                             
114800          IF WS-ADGMT-PADR  NOT = OHUV-ADGMT-PADR  AND                    
114900             SW-DATA-FORANDRAT    = NEJ                                   
115000            MOVE JA TO SW-DATA-FORANDRAT                                  
115100          ELSE                                                            
115200           IF WS-ADGMT-LAND  NOT = OHUV-ADGMT-LAND  AND                   
115300              SW-DATA-FORANDRAT    = NEJ                                  
115400             MOVE JA TO SW-DATA-FORANDRAT                                 
115500           ELSE                                                           
115600           MOVE ARB-BEGMRK TO HELP-BEGMRK-GRP                             
115700            IF WS-BEGMRK-RAD1 NOT = HELP-BEGMRK-RAD1 AND                  
115800               SW-DATA-FORANDRAT    = NEJ                                 
115900              MOVE JA TO SW-DATA-FORANDRAT                                
116000            ELSE                                                          
116100             IF WS-BEGMRK-RAD2 NOT = HELP-BEGMRK-RAD2 AND                 
116200                SW-DATA-FORANDRAT    = NEJ                                
116300               MOVE JA TO SW-DATA-FORANDRAT                               
116400             ELSE                                                         
116500              IF WS-KDFRAKT-NUM   NOT = ARB-KDFRAKT        AND            
116600                 SW-DATA-FORANDRAT    = NEJ                               
116700                MOVE JA         TO SW-DATA-FORANDRAT                      
116800              ELSE                                                        
116900               IF WS-BEKUNDRF      NOT = OHUV-BEKUNDRF      AND           
117000                SW-DATA-FORANDRAT      = NEJ                              
117100                MOVE JA TO SW-DATA-FORANDRAT                              
117200               END-IF                                                     
117300              END-IF                                                      
117400             END-IF                                                       
117500            END-IF                                                        
117600           END-IF                                                         
117700          END-IF                                                          
117800         END-IF                                                           
117900        END-IF                                                            
118000       END-IF                                                             
118100      END-IF                                                              
118200     END-IF                                                               
118300                                                                          
118400     IF MFS-UPDATE          AND                                           
118500        SW-DATA-FORANDRAT = NEJ                                           
118600       MOVE    NEJ                   TO    ALLT-SW                        
118700       MOVE    FELM-INGET-ANDRAT-414 TO    MED-IDMFSFEL                   
118800       CALL    WMEDKONV              USING MED-WMEDAREA                   
118900       MOVE    MED-MFSFEL            TO    MOD-TEMFSFEL                   
119000       MOVE    ARB-KDFRAKT           TO    MOD-KDFRAKT                    
119100       PERFORM MFS-ROR-EJ-FAELT-UT                                        
119200     ELSE                                                                 
119300       IF MFS-UPDATE          AND                                         
119400          SW-DATA-FORANDRAT = JA                                          
119500         CONTINUE                                                         
119600       ELSE                                                               
119700         IF MFS-ENTER          AND                                        
119800            SW-DATA-FORANDRAT = JA                                        
119900           MOVE    NEJ                    TO    ALLT-SW                   
120000           MOVE    FELM-PF11-FOR-UPPD-407 TO    MED-IDMFSFEL              
120100           CALL    WMEDKONV               USING MED-WMEDAREA              
120200           MOVE    MED-MFSFEL             TO    MOD-TEMFSFEL              
120300           MOVE    WS-KDFRAKT             TO    MOD-KDFRAKT               
120400           PERFORM MFS-ROR-EJ-FAELT-UT                                    
120500         END-IF                                                           
120600       END-IF                                                             
120700     END-IF                                                               
120800     .                                                                    
120900     EJECT                                                                
121000 H-KOLLA-INDATA-UPPDATERA-OH SECTION.                                     
121100                                                                          
121200     PERFORM HA-KOLL-OM-OHUVUPPG-FAR-ANDRAS                               
121300     IF ALLT-OK                                                           
121400       IF WS-KDFRAKT-NUM NOT = ARB-KDFRAKT                                
121500         PERFORM HB-KOLLA-OM-KDFRAKT-FAR-ANDRAS                           
121600         IF ALLT-OK                                                       
121700           PERFORM HC-LAS-KUNDREG                                         
121800           IF ALLT-OK                                                     
121900             PERFORM HD-BESTAM-NY-TRANSPORT                               
122000           END-IF                                                         
122100         END-IF                                                           
122200       END-IF                                                             
122300       IF ALLT-OK                                                         
122400         PERFORM HE-UPPDATERA-OHUV-ARBTAB                                 
122500         IF SW-KDFDKRAV-FORANDRAD = JA                                    
122600           PERFORM HG-UPPDATERA-KDFDKRAV                                  
122700         END-IF                                                           
122800         PERFORM HF-EV-GOR-ORDERAVSLUT                                    
122900         PERFORM HH-FYLL-MOD                                              
123000       END-IF                                                             
123100     END-IF                                                               
123200                                                                          
123300     IF ALLT-OK                                                           
123400       MOVE    FELM-UPPDAT-UTFORD-101 TO    MED-IDMFSINF                  
123500       CALL    WMEDKONV               USING MED-WMEDAREA                  
123600       MOVE    MED-MFSINF             TO    MOD-TEMFSINF                  
123700     ELSE                                                                 
123800       CALL    WMEDKONV               USING MED-WMEDAREA                  
123900       MOVE    MED-MFSFEL             TO    MOD-TEMFSFEL                  
124000       IF MED-IDMFSFEL = FELM-OH-KAN-EJ-ANDRAS-062                        
124100         PERFORM HI-SPECIALARN                                            
124200       ELSE                                                               
124300         MOVE    WS-KDFRAKT           TO    MOD-KDFRAKT                   
124400         PERFORM MFS-ROR-EJ-FAELT-UT                                      
124500       END-IF                                                             
124600     END-IF                                                               
124700     .                                                                    
124800     EJECT                                                                
124900 HA-KOLL-OM-OHUVUPPG-FAR-ANDRAS SECTION.                                  
125000                                                                          
125100     IF OHUV-IDDC-TVS = SPACE                                             
125200       IF WS-BELAGINS-DEL1 NOT = OHUV-BELAGINS-DEL1 OR                    
125300          WS-BELAGINS-DEL2 NOT = OHUV-BELAGINS-DEL2 OR                    
125400          WS-BEGMT-RAD1    NOT = OHUV-BEGMT-RAD1    OR                    
125500          WS-BEGMT-RAD2    NOT = OHUV-BEGMT-RAD2    OR                    
125600          WS-ADGMT-GATA    NOT = OHUV-ADGMT-GATA    OR                    
125700          WS-ADGMT-PADR    NOT = OHUV-ADGMT-PADR    OR                    
125800          WS-ADGMT-LAND    NOT = OHUV-ADGMT-LAND    OR                    
125900          WS-BEKUNDRF      NOT = OHUV-BEKUNDRF                            
126000         PERFORM HAA-KONTROLLERA-ODEL-STATUS                              
126100       END-IF                                                             
126200     END-IF                                                               
126300     .                                                                    
126400     EJECT                                                                
126500 HAA-KONTROLLERA-ODEL-STATUS SECTION.                                     
126600                                                                          
126700     MOVE LOW-VALUE    TO W-WDQ301KY-MIN-X                                
126800     MOVE HIGH-VALUE   TO W-WDQ301KY-MAX-X                                
126900     MOVE OHUV-IDORDER TO W-WDQ3-IDORDER-MIN                              
127000                          W-WDQ3-IDORDER-MAX                              
127100                                                                          
127200     PERFORM IMS-GN-ORQA01-STATUS-EJ-R                                    
127300                                                                          
127400     IF SEGMENT-FINNS                                                     
127500       MOVE NEJ                       TO ALLT-SW                          
127600       MOVE FELM-OH-KAN-EJ-ANDRAS-062 TO MED-IDMFSFEL                     
127700     END-IF                                                               
127800     .                                                                    
127900     EJECT                                                                
128000 HB-KOLLA-OM-KDFRAKT-FAR-ANDRAS SECTION.                                  
128100                                                                          
128200     IF OHUV-KDORDKL = ZERO                                               
128300                                                                          
128400       MOVE NEJ                       TO ALLT-SW                          
128500       MOVE FELM-FK-KAN-EJ-ANDRAS-068 TO MED-IDMFSFEL                     
128600       MOVE MFS-NUM-FAELT-FEL         TO MOD-KDFRAKT-ATTR                 
128700                                                                          
128800     END-IF                                                               
128900     .                                                                    
129000     EJECT                                                                
129100 HC-LAS-KUNDREG SECTION.                                                  
129200                                                                          
129300     PERFORM IMS-GU-GMTB-WDB301                                           
129400     IF SEGMENT-SAKNAS                                                    
129500       MOVE NEJ TO ALLT-SW                                                
129600     ELSE                                                                 
129700       MOVE DC-KVLEDTIM-0 TO SPAR-WDB3-KVLEDTIM-0                         
129800       MOVE DC-KVLEDTIM-1 TO SPAR-WDB3-KVLEDTIM-1                         
129900       MOVE DC-KVLEDTIM-2 TO SPAR-WDB3-KVLEDTIM-2                         
130000       MOVE DC-KVLEDTIM-3 TO SPAR-WDB3-KVLEDTIM-3                         
130100       MOVE DC-KVLEDTIM-4 TO SPAR-WDB3-KVLEDTIM-4                         
130200     END-IF                                                               
130300                                                                          
130400     PERFORM IMS-GU-GMTC-WDB501                                           
130500     IF SEGMENT-SAKNAS                                                    
130600       MOVE NEJ TO ALLT-SW                                                
130700     ELSE                                                                 
130800       PERFORM HCA-SPARA-WDB5-INFO                                        
130900     END-IF                                                               
131000                                                                          
131100     IF NOT ALLT-OK                                                       
131200       MOVE FELM-KUNDUPPG-SAKNAS-063 TO MED-IDMFSFEL                      
131300       MOVE MFS-NUM-FAELT-FEL        TO MOD-KDFRAKT-ATTR                  
131400     END-IF                                                               
131500     .                                                                    
131600     EJECT                                                                
131700                                                                          
131800 HCA-SPARA-WDB5-INFO SECTION.                                             
131900                                                                          
132000     IF SPAR-ARB-KDFDKRAV = FK-KDFDKRAV                                   
132100       MOVE NEJ         TO SW-KDFDKRAV-FORANDRAD                          
132200     ELSE                                                                 
132300       MOVE FK-KDFDKRAV TO SPAR-WDB5-KDFDKRAV                             
132400       MOVE JA          TO SW-KDFDKRAV-FORANDRAD                          
132500     END-IF                                                               
132600                                                                          
132700     MOVE FK-KDTRPKAT               TO SPAR-WDB5-KDTRPKAT                 
132800                                                                          
132900     IF FK-KDTRPKAT = K-KDTRPKAT-C                                        
133000       MOVE ZERO                    TO SPAR-WDB5-IDTRP                    
133100     ELSE                                                                 
133200       IF OHUV-KDORDKL = K-KDORDKL-0                                      
133300         MOVE FK-IDTRP-0            TO SPAR-WDB5-IDTRP                    
133400       ELSE                                                               
133500         IF OHUV-KDORDKL = K-KDORDKL-1                                    
133600           MOVE FK-IDTRP-1          TO SPAR-WDB5-IDTRP                    
133700         ELSE                                                             
133800           IF OHUV-KDORDKL = K-KDORDKL-2                                  
133900             MOVE FK-IDTRP-2        TO SPAR-WDB5-IDTRP                    
134000           ELSE                                                           
134100             IF OHUV-KDORDKL = K-KDORDKL-3                                
134200               MOVE FK-IDTRP-3      TO SPAR-WDB5-IDTRP                    
134300             ELSE                                                         
134400               IF OHUV-KDORDKL = K-KDORDKL-4                              
134500                 MOVE FK-IDTRP-4 TO SPAR-WDB5-IDTRP                       
134600               END-IF                                                     
134700             END-IF                                                       
134800           END-IF                                                         
134900         END-IF                                                           
135000       END-IF                                                             
135100     END-IF                                                               
135200     .                                                                    
135300     EJECT                                                                
135400 HD-BESTAM-NY-TRANSPORT SECTION.                                          
135500                                                                          
135600     IF SPAR-WDB5-KDTRPKAT = K-KDTRPKAT-A                                 
135700                                                                          
135800       PERFORM HDA-RED-LANKAREA-W411TRAN                                  
135900       CALL    W411TRAN USING TRAN-W411TRAN                               
136000                              TRAN-XXKB-PCB                               
136100                                                                          
136200       PERFORM HDB-KOLLA-OM-TRAN-FEL                                      
136300     END-IF                                                               
136400     .                                                                    
136500     EJECT                                                                
136600 HDA-RED-LANKAREA-W411TRAN SECTION.                                       
136700                                                                          
136800     MOVE   MSGI-TILOKDAT         TO   TRAN-TIREGDAT                      
136900     MOVE   MSGI-TILOKTID         TO   TRAN-TIHHMM-REG                    
137000     MOVE   K-IDSYSTEM-IMS        TO   TRAN-IDSYSTEM                      
137100     MOVE   SPAR-WDB5-IDTRP       TO   TRAN-IDTRP                         
137200     MOVE   MSGI-IDDC             TO   TRAN-IDDC                          
137300     MOVE   OHUV-KDORDKL          TO   TRAN-KDORDKL                       
137400     MOVE   SPAR-WDB5-KDTRPKAT    TO   TRAN-KDTRPKAT                      
137500     MOVE   SPAR-WDB3-KVLEDTIM-0  TO   TRAN-KVLEDTIM-0                    
137600     MOVE   SPAR-WDB3-KVLEDTIM-1  TO   TRAN-KVLEDTIM-1                    
137700     MOVE   SPAR-WDB3-KVLEDTIM-2  TO   TRAN-KVLEDTIM-2                    
137800     MOVE   SPAR-WDB3-KVLEDTIM-3  TO   TRAN-KVLEDTIM-3                    
137900     MOVE   SPAR-WDB3-KVLEDTIM-4  TO   TRAN-KVLEDTIM-4                    
138000     MOVE   ZERO                  TO   TRAN-TIRFS                         
138100     MOVE   OHUV-KDTPOTYP         TO   TRAN-KDTPOTYP                      
138200     .                                                                    
138300     EJECT                                                                
138400 HDB-KOLLA-OM-TRAN-FEL SECTION.                                           
138500                                                                          
138600     IF TRAN-KDSVAR NOT = K-TRAN-KDSVAR-0-OK                              
138700       MOVE NEJ                        TO ALLT-SW                         
138800       MOVE FELM-TRP-KAN-EJ-SATTAS-064 TO MED-IDMFSFEL                    
138900       MOVE MFS-ADD-LYS-UPP-FAELT      TO MOD-KDFRAKT-ATTR                
139000     END-IF                                                               
139100     .                                                                    
139200     EJECT                                                                
139300 HE-UPPDATERA-OHUV-ARBTAB SECTION.                                        
139400                                                                          
139500     PERFORM IMS-GHU-WDQ201                                               
139600                                                                          
139700     MOVE    WS-BELAGINS-DEL1 TO OHUV-BELAGINS-DEL1                       
139800     MOVE    WS-BELAGINS-DEL2 TO OHUV-BELAGINS-DEL2                       
139900     MOVE    WS-BEGMT-RAD1    TO OHUV-BEGMT-RAD1                          
140000     MOVE    WS-BEGMT-RAD2    TO OHUV-BEGMT-RAD2                          
140100     MOVE    WS-ADGMT-GATA    TO OHUV-ADGMT-GATA                          
140200     MOVE    WS-ADGMT-PADR    TO OHUV-ADGMT-PADR                          
140300     MOVE    WS-ADGMT-LAND    TO OHUV-ADGMT-LAND                          
140400     MOVE    WS-BEKUNDRF      TO OHUV-BEKUNDRF                            
140500     MOVE    ZERO             TO OHUV-IDDEPT                              
140600                                                                          
140700     PERFORM IMS-REPL-WDQ201                                              
140800                                                                          
140900     IF WS-KDFRAKT-NUM  NOT = ARB-KDFRAKT OR                              
141000        WS-BEGMRK-GRP NOT = ARB-BEGMRK                                    
141100                                                                          
141200       PERFORM IMS-GHNP-WDQ212                                            
141300                                                                          
141400       IF WS-KDFRAKT-NUM NOT = ARB-KDFRAKT                                
141500         MOVE JA                 TO SW-KDFRAKT-FORANDRAD                  
141600         MOVE WS-KDFRAKT-NUM     TO ARB-KDFRAKT                           
141700         MOVE SPAR-WDB5-KDTRPKAT TO ARB-KDTRPKAT                          
141800         MOVE SPAR-WDB5-IDTRP    TO ARB-IDTRP                             
141900         IF SPAR-WDB5-KDTRPKAT = K-KDTRPKAT-A                             
142000           MOVE TRAN-TIHHMM        TO ARB-TIHHMM                          
142100           MOVE TRAN-TIAAMMDD      TO ARB-DATRPAVD                        
142200           IF TRAN-TIAAMMDD NOT = ZERO                                    
142300             IF TRAN-TIAAMMDD < 500000                                    
142400               MOVE 20             TO ARB-DATRPAVD (1:2)                  
142500             ELSE                                                         
142600               IF TRAN-TIAAMMDD < 999999                                  
142700                 MOVE 19           TO ARB-DATRPAVD (1:2)                  
142800               ELSE                                                       
142900                 MOVE 99999999     TO ARB-DATRPAVD                        
143000               END-IF                                                     
143100             END-IF                                                       
143200           END-IF                                                         
143300           MOVE ZERO               TO ARB-TIRFS                           
143400         ELSE                                                             
143500           MOVE ZERO               TO ARB-DATRPAVD                        
143600                                      ARB-TIHHMM                          
143700         END-IF                                                           
143800         IF SW-KDFDKRAV-FORANDRAD = JA                                    
143900           MOVE SPAR-WDB5-KDFDKRAV TO ARB-KDFDKRAV                        
144000         END-IF                                                           
144100       END-IF                                                             
144200                                                                          
144300       MOVE    WS-BEGMRK-GRP   TO ARB-BEGMRK                              
144400       PERFORM IMS-REPL-WDQ212                                            
144500     END-IF                                                               
144600     .                                                                    
144700     EJECT                                                                
144800 HF-EV-GOR-ORDERAVSLUT SECTION.                                           
144900                                                                          
145000     IF SW-KDFRAKT-FORANDRAD = JA          AND                            
145100        SPAR-WDB5-KDTRPKAT   = K-KDTRPKAT-A                               
145200                                                                          
145300       PERFORM HFA-RED-LANKAREA-W413AVSO                                  
145400       CALL    W413AVSO USING AVSO-W413AVSO                               
145500                              AVSO-WDE6-PCB AVSO-ORQA-PCB                 
145600                              AVSO-WDQ2-PCB                               
145700                              AVSO-GMTB-PCB AVSO-XXKA-PCB                 
145800                              AVSO-4437-PCB AVSO-XXKE-PCB                 
145900                              AVSO-XXKF-PCB AVSO-XXKG-PCB                 
146000                              AVSO-XXKH-PCB AVSO-XXKI-PCB                 
146100                              AVSO-XXKP-PCB AVSO-WDB2-PCB                 
146200                              AVSO-WDB6-PCB                               
146300                              USEA-PCB      TRAN-XXKB-PCB                 
146400                              ORDN-ORQL-PCB ORDN-PROC-PCB                 
146500                              ORDN-ORQI-PCB ORDN-WDQ3-PCB                 
146600     END-IF                                                               
146700     .                                                                    
146800     EJECT                                                                
146900 HFA-RED-LANKAREA-W413AVSO SECTION.                                       
147000                                                                          
147100     MOVE OHUV-IDGMTREF TO AVSO-IDGMTREF                                  
147200     MOVE OHUV-IDORDER  TO AVSO-IDORDER                                   
147300     MOVE SPACE         TO AVSO-IDDC                                      
147400     MOVE ZERO          TO AVSO-TIRFS                                     
147500                           AVSO-TIAAMMDD                                  
147600                           AVSO-TIHHMM                                    
147700     MOVE W-IDTRANS     TO AVSO-IDTRANS                                   
147800     .                                                                    
147900     EJECT                                                                
148000 HH-FYLL-MOD SECTION.                                                     
148100                                                                          
148200     MOVE WS-BELAGINS-DEL1 TO MOD-BELAGINS-DEL1                           
148300     MOVE WS-BELAGINS-DEL2 TO MOD-BELAGINS-DEL2                           
148400                                                                          
148500     MOVE WS-BEGMT-RAD1    TO MOD-BEGMT-RAD1                              
148600     MOVE WS-BEGMT-RAD2    TO MOD-BEGMT-RAD2                              
148700                                                                          
148800     MOVE WS-ADGMT-GATA    TO MOD-ADGMT-GATA                              
148900     MOVE WS-ADGMT-PADR    TO MOD-ADGMT-PADR                              
149000     MOVE WS-ADGMT-LAND    TO MOD-ADGMT-LAND                              
149100                                                                          
149200     MOVE WS-BEGMRK-RAD1   TO MOD-BEGMRK-RAD1                             
149300     MOVE WS-BEGMRK-RAD2   TO MOD-BEGMRK-RAD2                             
149400                                                                          
149500     MOVE WS-KDFRAKT       TO MOD-KDFRAKT                                 
149600     MOVE WS-BEKUNDRF      TO MOD-BEKUNDRF                                
149700     .                                                                    
149800     EJECT                                                                
149900 HG-UPPDATERA-KDFDKRAV SECTION.                                           
150000                                                                          
150100     PERFORM IMS-GHU-WDQ301-STATUS-EJ-R                                   
150200     IF SEGMENT-FINNS                                                     
150300       PERFORM UNTIL SEGMENT-SAKNAS                                       
150400         MOVE SPAR-WDB5-KDFDKRAV TO ODEL-KDFDKRAV                         
150500         PERFORM IMS-REPL-ORQA01                                          
150600         PERFORM IMS-GHN-WDQ301-STATUS-EJ-R                               
150700       END-PERFORM                                                        
150800     END-IF                                                               
150900     .                                                                    
151000     EJECT                                                                
151100 HI-SPECIALARN SECTION.                                                   
151200                                                                          
151300     MOVE OHUV-BELAGINS-DEL1     TO MOD-BELAGINS-DEL1                     
151400     MOVE OHUV-BELAGINS-DEL2     TO MOD-BELAGINS-DEL2                     
151500                                                                          
151600     MOVE OHUV-BEGMT-RAD1        TO MOD-BEGMT-RAD1                        
151700     MOVE OHUV-BEGMT-RAD2        TO MOD-BEGMT-RAD2                        
151800                                                                          
151900     MOVE OHUV-ADGMT-GATA        TO MOD-ADGMT-GATA                        
152000     MOVE OHUV-ADGMT-PADR        TO MOD-ADGMT-PADR                        
152100     MOVE OHUV-ADGMT-LAND        TO MOD-ADGMT-LAND                        
152200                                                                          
152300     MOVE SPAR-ARB-BEGMRK-RAD1   TO MOD-BEGMRK-RAD1                       
152400     MOVE SPAR-ARB-BEGMRK-RAD2   TO MOD-BEGMRK-RAD2                       
152500                                                                          
152600     MOVE SPAR-ARB-KDFRAKT       TO MOD-KDFRAKT                           
152700     MOVE OHUV-BEKUNDRF          TO MOD-BEKUNDRF                          
152800     .                                                                    
152900     EJECT                                                                
153000 MFS-RENSA-FAELT-UT SECTION.                                              
153100                                                                          
153200     MOVE MFS-RENSA-FAELT TO                                              
153300                             MOD-SUORDV                                   
153400                             MOD-KVORDRAD                                 
153500                             MOD-BELAGINS-DEL1                            
153600                             MOD-BELAGINS-DEL2                            
153700                             MOD-BEGMT-RAD1                               
153800                             MOD-BEGMT-RAD2                               
153900                             MOD-ADGMT-GATA                               
154000                             MOD-ADGMT-PADR                               
154100                             MOD-ADGMT-LAND                               
154200                             MOD-BEGMRK-RAD1                              
154300                             MOD-BEGMRK-RAD2                              
154400                             MOD-KDFRAKT                                  
154500                             MOD-BEKUNDRF                                 
154600     .                                                                    
154700     EJECT                                                                
154800 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
154900                                                                          
155000     MOVE MFS-ROER-EJ-FAELT TO                                            
155100                             MOD-SUORDV                                   
155200                             MOD-KVORDRAD                                 
155300                             MOD-BELAGINS-DEL1                            
155400                             MOD-BELAGINS-DEL2                            
155500                             MOD-BEGMT-RAD1                               
155600                             MOD-BEGMT-RAD2                               
155700                             MOD-ADGMT-GATA                               
155800                             MOD-ADGMT-PADR                               
155900                             MOD-ADGMT-LAND                               
156000                             MOD-BEGMRK-RAD1                              
156100                             MOD-BEGMRK-RAD2                              
156200                                                                          
156300                             MOD-BEKUNDRF                                 
156400     .                                                                    
156500     EJECT                                                                
156600                                                                          
156700                                                                          
156800*                                                                         
156900*                                                                         
157000*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
157100*                 III     III MM MMMMM MM SSSS   SSSS                     
157200*                 IIIII IIIII MM  MMM  MM SSS SSS SSS                     
157300*                 IIIII IIIII MM M M M MM SSS  SSSSSS                     
157400*                 IIIII IIIII MM MM MM MM SSSSSS  SSS                     
157500*                 IIIII IIIII MM MMMMM MM SSS SSS SSS                     
157600*                 III     III MM MMMMM MM SSSS   SSSS                     
157700*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
157800*                                                                         
157900*                                                                         
158000 IMS-GET-MSG SECTION.                                                     
158100                                                                          
158200     MOVE    '  QC'          TO    GODK-STATUSKODER                       
158300     CALL    CBLTDLI         USING GU   MSG-PCB MSG-IO-AREA               
158400     MOVE    MSG-STATUS-CODE TO    STATUS-WS                              
158500     PERFORM IMS-STATUSKONTROLL                                           
158600     .                                                                    
158700                                                                          
158800 IMS-INSERT-MSG SECTION.                                                  
158900                                                                          
159000     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
159100       MOVE '0' TO MFS-KDHUVOMR                                           
159200     END-IF                                                               
159300     MOVE    LOW-VALUE       TO    MSG-KDZ1 MSG-KDZ2                      
159400     MOVE    SPACE           TO    GODK-STATUSKODER                       
159500     CALL    CBLTDLI         USING ISRT MSG-PCB MSG-IO-AREA               
159600                                        MFS-IDMOD                         
159700     MOVE    MSG-STATUS-CODE TO    STATUS-WS                              
159800     PERFORM IMS-STATUSKONTROLL                                           
159900     .                                                                    
160000     EJECT                                                                
160100 IMS-GET-WDQ201-CSEQ SECTION.                                             
160200                                                                          
160300     STRING  'WDQ201  (WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                       
160400             DELIMITED BY SIZE INTO    SSA1                               
160500     MOVE    '  GE'              TO    GODK-STATUSKODER                   
160600     CALL    CBLTDLI             USING GU   WDQ2-PCB                      
160703                                            DLI-IO-AREA-Q201 SSA1         
160800     MOVE    WDQ2-STATUS-CODE    TO    STATUS-WS                          
160900     PERFORM IMS-STATUSKONTROLL                                           
161000     .                                                                    
161100                                                                          
161200 IMS-GHU-WDQ201 SECTION.                                                  
161300                                                                          
161400     STRING  'WDQ201  (WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                       
161500             DELIMITED BY SIZE INTO    SSA1                               
161600     MOVE    '  '                TO    GODK-STATUSKODER                   
161700     CALL    CBLTDLI             USING GHU  WDQ2-PCB                      
161803                                            DLI-IO-AREA-Q201 SSA1         
161900     MOVE    WDQ2-STATUS-CODE    TO    STATUS-WS                          
162000     PERFORM IMS-STATUSKONTROLL                                           
162100     .                                                                    
162200                                                                          
162300 IMS-REPL-WDQ201 SECTION.                                                 
162400                                                                          
162500     MOVE    '  '                TO    GODK-STATUSKODER                   
162600     CALL    CBLTDLI             USING REPL WDQ2-PCB                      
162703                                            DLI-IO-AREA-Q201              
162800     MOVE    WDQ2-STATUS-CODE    TO    STATUS-WS                          
162900     PERFORM IMS-STATUSKONTROLL                                           
163000     .                                                                    
163100     EJECT                                                                
163200 IMS-GNP-WDQ211-OKVAL-FIRST SECTION.                                      
163300                                                                          
163400     MOVE    'WDQ211  *F'     TO    SSA1                                  
163500     MOVE    '  GE'           TO    GODK-STATUSKODER                      
163600     CALL    CBLTDLI          USING GNP  WDQ2-PCB                         
163703                                         DLI-IO-AREA-Q211 SSA1            
163800     MOVE    WDQ2-STATUS-CODE TO    STATUS-WS                             
163900     PERFORM IMS-STATUSKONTROLL                                           
164000     .                                                                    
164100                                                                          
164200 IMS-GNP-WDQ211-OKVAL SECTION.                                            
164300                                                                          
164400     MOVE    'WDQ211  '       TO    SSA1                                  
164500     MOVE    '  GE'           TO    GODK-STATUSKODER                      
164600     CALL    CBLTDLI          USING GNP  WDQ2-PCB                         
164703                                         DLI-IO-AREA-Q211 SSA1            
164800     MOVE    WDQ2-STATUS-CODE TO    STATUS-WS                             
164900     PERFORM IMS-STATUSKONTROLL                                           
165000     .                                                                    
165100     EJECT                                                                
165200 IMS-GNP-WDQ212-FIRST SECTION.                                            
165300                                                                          
165400     STRING  'WDQ212  *F(IDDC     =' W-WDQ2-IDDC-X ')'                    
165500             DELIMITED BY SIZE INTO    SSA1                               
165600     MOVE    '  GE'              TO    GODK-STATUSKODER                   
165700     CALL    CBLTDLI             USING GNP  WDQ2-PCB                      
165803                                            DLI-IO-AREA-Q212 SSA1         
165900     MOVE    WDQ2-STATUS-CODE    TO    STATUS-WS                          
166000     PERFORM IMS-STATUSKONTROLL                                           
166100     .                                                                    
166200                                                                          
166301 IMS-GNP-WDQ221 SECTION.                                                  
166401                                                                          
166501     STRING  'WDQ212  (IDDC     =' W-WDQ2-IDDC-X ')'                      
166601             DELIMITED BY SIZE INTO    SSA1                               
166701     MOVE    'WDQ221 '           TO    SSA2                               
166801     MOVE    '  GE'              TO    GODK-STATUSKODER                   
166901     CALL    CBLTDLI             USING GNP  WDQ2-PCB                      
167003                                       DLI-IO-AREA-Q221 SSA1 SSA2         
167101     MOVE    WDQ2-STATUS-CODE    TO    STATUS-WS                          
167201     PERFORM IMS-STATUSKONTROLL                                           
167301     .                                                                    
167401                                                                          
167501 IMS-GHNP-WDQ212 SECTION.                                                 
167601                                                                          
167701     STRING  'WDQ212  *F(IDDC     =' W-WDQ2-IDDC-X ')'                    
167801             DELIMITED BY SIZE INTO    SSA1                               
167901     MOVE    '  '                TO    GODK-STATUSKODER                   
168001     CALL    CBLTDLI             USING GHNP WDQ2-PCB                      
168103                                            DLI-IO-AREA-Q212 SSA1         
168201     MOVE    WDQ2-STATUS-CODE    TO    STATUS-WS                          
168301     PERFORM IMS-STATUSKONTROLL                                           
168401     .                                                                    
168501                                                                          
168601 IMS-REPL-WDQ212 SECTION.                                                 
168701                                                                          
168801     MOVE    '  '                TO    GODK-STATUSKODER                   
168901     CALL    CBLTDLI             USING REPL WDQ2-PCB                      
169003                                            DLI-IO-AREA-Q212              
169101     MOVE    WDQ2-STATUS-CODE    TO    STATUS-WS                          
169201     PERFORM IMS-STATUSKONTROLL                                           
169301     .                                                                    
169401     EJECT                                                                
169501 IMS-GN-ORQA01-INTERV SECTION.                                            
169601                                                                          
169701     STRING  'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                       
169801                     '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                   
169901             DELIMITED BY SIZE INTO    SSA1                               
170001     MOVE    '  GE'              TO    GODK-STATUSKODER                   
170101     CALL    CBLTDLI             USING GN   ORQA-PCB                      
170201                                DLI-IO-AREA-ODEL SSA1                     
170301     MOVE    ORQA-STATUS-CODE    TO    STATUS-WS                          
170401     PERFORM IMS-STATUSKONTROLL                                           
170501     .                                                                    
170601                                                                          
170701 IMS-GN-ORQA01-STATUS-EJ-R SECTION.                                       
170801                                                                          
170901     STRING  'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                       
171001                     '&WDQ301KY<=' W-WDQ301KY-MAX-X                       
171101                     '&KDODELSTNE' K-KDODELSTA-R    ')'                   
171201*                              NE STÅR FÖR NOT EQUAL                      
171301             DELIMITED BY SIZE INTO    SSA1                               
171401     MOVE    '  GE'              TO    GODK-STATUSKODER                   
171501     CALL    CBLTDLI             USING GU   ORQA-PCB                      
171601                                DLI-IO-AREA-ODEL SSA1                     
171701     MOVE    ORQA-STATUS-CODE    TO    STATUS-WS                          
171801     PERFORM IMS-STATUSKONTROLL                                           
171901     .                                                                    
172001     EJECT                                                                
172101 IMS-GHU-WDQ301-STATUS-EJ-R SECTION.                                      
172201                                                                          
172301     STRING  'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                       
172401                     '&WDQ301KY<=' W-WDQ301KY-MAX-X                       
172501                     '&KDODELSTNE' K-KDODELSTA-R    ')'                   
172601*                              NE STÅR FÖR NOT EQUAL                      
172701             DELIMITED BY SIZE INTO    SSA1                               
172801     MOVE    '  GE'              TO    GODK-STATUSKODER                   
172901     CALL    CBLTDLI             USING GHU  ORQA-PCB                      
173001                                 DLI-IO-AREA-ODEL SSA1                    
173101     MOVE    ORQA-STATUS-CODE    TO    STATUS-WS                          
173201     PERFORM IMS-STATUSKONTROLL                                           
173301     .                                                                    
173401     EJECT                                                                
173501 IMS-GHN-WDQ301-STATUS-EJ-R SECTION.                                      
173601                                                                          
173701     STRING  'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                       
173801                     '&WDQ301KY<=' W-WDQ301KY-MAX-X                       
173901                     '&KDODELSTNE' K-KDODELSTA-R    ')'                   
174001*                              NE STÅR FÖR NOT EQUAL                      
174101             DELIMITED BY SIZE INTO    SSA1                               
174201     MOVE    '  GE'              TO    GODK-STATUSKODER                   
174301     CALL    CBLTDLI             USING GHN  ORQA-PCB                      
174401                                 DLI-IO-AREA-ODEL SSA1                    
174501     MOVE    ORQA-STATUS-CODE    TO    STATUS-WS                          
174601     PERFORM IMS-STATUSKONTROLL                                           
174701     .                                                                    
174801     EJECT                                                                
174901 IMS-REPL-ORQA01 SECTION.                                                 
175001                                                                          
175101     MOVE    '  '                TO    GODK-STATUSKODER                   
175201     CALL    CBLTDLI             USING REPL ORQA-PCB                      
175301                                            DLI-IO-AREA-ODEL              
175401     MOVE    ORQA-STATUS-CODE    TO    STATUS-WS                          
175501     PERFORM IMS-STATUSKONTROLL                                           
175601     .                                                                    
175701     EJECT                                                                
175801 IMS-GU-GMTB-WDB301 SECTION.                                              
175901                                                                          
176001     STRING  'WDB301  (WDB301KY =' W-WDB301KY-X                           
176101                     '!WDB301KY =' W-WDB301KY-DEF-X  ')'                  
176201             DELIMITED BY SIZE INTO    SSA1                               
176301     MOVE    '  GE'              TO    GODK-STATUSKODER                   
176401     CALL    CBLTDLI             USING GU   GMTB-PCB                      
176501                                            DLI-IO-AREA-WDB3              
176601                                            SSA1                          
176701     MOVE    GMTB-STATUS-CODE    TO    STATUS-WS                          
176801     PERFORM IMS-STATUSKONTROLL                                           
176901     .                                                                    
177001     SKIP2                                                                
177101                                                                          
177201 IMS-GU-GMTC-WDB501 SECTION.                                              
177301                                                                          
177401     STRING  'WDB501  (WDB501KY =' W-WDB501KY-X                           
177501                     '!WDB501KY =' W-WDB501KY-DEF-X  ')'                  
177601             DELIMITED BY SIZE INTO    SSA1                               
177701     MOVE    '  GE'              TO    GODK-STATUSKODER                   
177801     CALL    CBLTDLI             USING GU   GMTC-PCB                      
177901                                            DLI-IO-AREA-WDB5              
178001                                            SSA1                          
178101     MOVE    GMTC-STATUS-CODE    TO    STATUS-WS                          
178201     PERFORM IMS-STATUSKONTROLL                                           
178301     .                                                                    
178401                                                                          
178501     EJECT                                                                
178601 IMS-GU-WDB601    SECTION.                                                
178701     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
178801          DELIMITED BY SIZE INTO SSA1                                     
178901     MOVE '  GE' TO GODK-STATUSKODER                                      
179001     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
179101     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
179201     PERFORM IMS-STATUSKONTROLL                                           
179301     IF SEGMENT-SAKNAS                                                    
180000        MOVE SPACE TO DCS-KDDC                                            
181000     END-IF                                                               
182000     .                                                                    
183000     EJECT                                                                
183100 IMS-STATUSKONTROLL SECTION.                                              
183200                                                                          
183300     SET STATUS-IX TO 1                                                   
183400     SEARCH GODK-STATUS                                                   
183500       AT END                                                             
183600         CALL FELLOG                                                      
183700       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                            
183800         CONTINUE                                                         
183900     END-SEARCH                                                           
184000     .                                                                    
