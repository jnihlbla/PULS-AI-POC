000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W476MANF.                                                
000400 AUTHOR.         STINA MOGREN.                                            
000500 DATE-WRITTEN.   06/06/30.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        PGM:ET SKRIVER FöR SAUDI ARABIEN                                 
001100*        - MANUFACTORING LIST                                             
001200*        -                                                                
001300*                                                                         
001400*        DISTRICT: 4848,  CUSTOMER 21, 36, 41 (SAUDI)                     
001500*        DISTRICT: 5410,  KUWAIT                                          
001600*        DISTRICT: 6246,  BAREIN                                          
001700*                                                                         
001800*        PGM:ET ANVÄNDER:                                                 
001900*            ALT-PCB     ANVÄNDS AV W006PRS1 (PCB FÖR PRINTER)            
002000*            WDE1                                                         
002100*            WDB2                                                         
002200*            WDB1                                                         
002300*            WDG7                                                         
002400*                                                                         
002500*                                                                         
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003300                                                                          
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800                                                                          
003900 WORKING-STORAGE SECTION.                                                 
004000     SKIP2                                                                
004100                                                                          
004200*    -- CHECKED BY WY2000                                                 
004300 77  IDPGM                       PIC X(8)    VALUE 'W476MANF'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  YES                         PIC X       VALUE 'Y'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700 77  TAB-IX                      PIC S9(5)   VALUE +0   COMP SYNC.        
004800 77  INDX                        PIC S9(5)   VALUE +0   COMP SYNC.        
004900 77  MAX-IX                      PIC S9(5)   VALUE +100 COMP SYNC.        
005000 77  SHIP-INDX                   PIC S9(5)   VALUE +0   COMP SYNC.        
005100 77  WS-SIDNR                    PIC S9(3)   VALUE +0   COMP-3.           
005200 77  WS-RADNR                    PIC S9(3)   VALUE +0   COMP-3.           
005300 77  WS-POST-RAEKNARE            PIC S9(5)   VALUE +0   COMP-3.           
005400 77  WS-ANTAL-RADER              PIC S9(3)   VALUE +0   COMP-3.           
005500 77  MAX-RADER                   PIC S9(3)   VALUE +40  COMP-3.           
005600 01  W-IND                       PIC S9(5)   VALUE ZERO COMP-3.           
005700 01  W-IDFAKT                    PIC S9(7)   VALUE ZERO COMP-3.           
005800 01  W-BEART                     PIC X(25)   VALUE SPACE.                 
005900     SKIP2                                                                
006000 01  FELTEXT.                                                             
006100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006300*                                                                         
006400 01  WS-SEKTION                  PIC X(50)   VALUE SPACES.                
006500*    - - - VALID IDDC CODES                                               
006600*                                                                         
006700 77  WS-DATUM                    PIC 9(6)    VALUE ZERO.                  
006800                                                                          
006900 01  WS-DATUM-AAMMDD.                                                     
007000     03  WS-DATUM-AA             PIC 9(2)          VALUE ZERO.            
007100     03  WS-DATUM-MM             PIC 9(2)          VALUE ZERO.            
007200     03  WS-DATUM-DD             PIC 9(2)          VALUE ZERO.            
007300                                                                          
007400 01 WS-DATUM-DDMMAA.                                                      
007500     03  WS-DD                   PIC 9(2)          VALUE ZERO.            
007600     03  WS-MM                   PIC 9(2)          VALUE ZERO.            
007700     03  WS-AA                   PIC 9(2)          VALUE ZERO.            
007800     EJECT                                                                
007900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008000 01  FILLER REDEFINES DAGENS-DATUM.                                       
008100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008400                                                                          
008500 01  WS-CENTURY-DATUM.                                                    
008600     03 WS-CENTURY               PIC 9(2).                                
008700     03 WS-AAMMDD.                                                        
008800       05 WS-AA2                 PIC 9(2).                                
008900       05 WS-MM2                 PIC 9(2).                                
009000       05 WS-DD2                 PIC 9(2).                                
009100     EJECT                                                                
009200 01  TEST-IDDISTR                PIC 9(5)   COMP-3.                       
009300     SKIP3                                                                
009400*    --- PARAMETRAR TILL COPYTEXT WWOMVAND                                
009500*01  -COPY WWOMVAND                                                       
009600     EJECT                                                                
009700 01  DYNAMISKA-SUBPROGRAM.                                                
009800*                                                                         
009900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010200     03  INTSOR                  PIC X(8)    VALUE 'INTSOR  '.            
010300     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
010400     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
010500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
010600     EJECT                                                                
010700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
011000     SKIP2                                                                
011100 77  KDRC-DISPLAY                PIC Z(5).                                
011200     EJECT                                                                
011300*    --- PARAMETRAR TILL POSTSUM                                          
011400*                                                                         
011500*01  -COPY W0005   -PRE  POSTSUM-                                         
011600     EJECT                                                                
011700*- - - - - - - - - - - - - -  PARAMETRAR TILL  INTSOR                     
011800 01  INTSOR-HJAELP-AREA.                                                  
011900     03  INTSOR-POST-ANTAL         PIC S9(3)  VALUE ZERO COMP-3.          
012000     03  INTSOR-POST-LAENGD        PIC S9(3)  VALUE ZERO COMP-3.          
012100     03  INTSOR-SORT-FAELT-LAENGD  PIC S9(3)  VALUE ZERO COMP-3.          
012200     SKIP3                                                                
012300 01  FILLER                      PIC X(16)   VALUE  'SEND-AREA'.          
012400 01  SEND-AREA.                                                           
012500*    03  -COPY WZ01SEND                                                   
012600                                                                          
012700 01  WS-PAGESKIP                   PIC X      VALUE '1'.                  
012800 01  WS-SKIP1                      PIC X      VALUE ' '.                  
012900 01  WS-SKIP2                      PIC X      VALUE '0'.                  
013000 01  WS-SKIP3                      PIC X      VALUE '-'.                  
013100                                                                          
013200 01  SEND-RAD-STYRTECKEN.                                                 
013300     03  STYRTECKEN-RAD          PIC X.                                   
013400     03  SEND-RAD                PIC X(120)  VALUE SPACE.                 
013500*                                                                         
013600*    --- PRINTPARAMETRAR (PRINTNING MED ÅTERSTART)                        
013700*                                                                         
013800*01  FILLER   -COPY W006PRAR                                              
013900     EJECT                                                                
014000                                                                          
014100 01  W-IDPRTLST                  PIC X(8).                                
014200 01  WS-PRT.                                                              
014300     03 WS-PRT-IDPRTLST          PIC X(8)  VALUE SPACE.                   
014400     03 WS-PRT-IDLIST.                                                    
014500        05 WS-IDLIST             PIC X(4)  VALUE 'MANF'.                  
014600        05 WS-PRT-IDDISTR        PIC 9(4).                                
014700        05 FILLER                PIC X(2).                                
014800     03 WS-PRT-LISTRAD.                                                   
014900        05 WS-RAD                PIC X(115).                              
015000     03 WS-PRT-DUMMY             PIC X(1).                                
015100                                                                          
015200 01  MANF-RUBRIK                 PIC X(50)   VALUE                        
015300       'MANUFACTORING LIST                                '.              
015400                                                                          
015500 01  FILLER                      PIC X(20)   VALUE SPACE.                 
015600 01  FILLER                      PIC X(16)   VALUE 'TABELL'.              
015700*    --- MANF-TABELL                                                      
015800 01  W476MANFTAB.                                                         
015900*                                                                         
016000*           MANF     BILAGA  TABELL                                       
016100*                            TABELL GRÄNSER                               
016200*                                                                         
016300*    *** FÖR TOTAL AV TABELLEN                                            
016400*                                                                         
016500*                                                                         
016600   03  FILLER.                                                            
016700*                                                                         
016800*    *** FÖR BEHANDLING AV TABELLEN                                       
016900*                                                                         
017000     05  MANFTAB-MAX-ANTAL-POST  PIC S9(4)  COMP VALUE +1500.             
017100     05  MANFTAB-ANTAL-POST      PIC S9(4)  COMP VALUE ZERO.              
017200*                                                                         
017300*    *** FÖR SORTERING  AV TABELLEN                                       
017400*                                                                         
017500     05  MANFTAB-POST-LAENGD     PIC S9(3) COMP-3 VALUE +5.               
017600     05  MANFTAB-IDLEVNR-LAENGD  PIC S9(3) COMP-3 VALUE +5.               
017700*                                                                         
017800*                                                                         
017900     05  MANF-TABELL  OCCURS  1500 TIMES.                                 
018000*                                                                         
018100         07 MANF-IDLEVNR       PIC X(5).                                  
018200*                      *** LEVERANTÖR ***                                 
018300     EJECT                                                                
018400                                                                          
018500 01  FILLER                    PIC X(16)  VALUE 'LIST-RADER'.             
018600 01  LIST-RADER.                                                          
018700                                                                          
018800     03 RUB-DATUM.                                                        
018900       05 FILLER               PIC X(90)  VALUE SPACE.                    
019000       05 DATUM.                                                          
019100         07 RUB-MM             PIC 99.                                    
019200         07 FILLER             PIC X     VALUE '/'.                       
019300         07 RUB-DD             PIC 99.                                    
019400         07 FILLER             PIC X     VALUE '/'.                       
019500         07 RUB-CC             PIC 99.                                    
019600         07 RUB-AA             PIC 99.                                    
019700                                                                          
019800     03 RAD-SIDNR.                                                        
019900       05 FILLER               PIC X(97)  VALUE SPACE.                    
020000       05 SIDNR                PIC Z(2)9.                                 
020100                                                                          
020200                                                                          
020300 01  RAD1.                                                                
020400     03  FILLER                    PIC X(30).                             
020500     03  RAD4H-IMPORTER            PIC X(35).                             
020600     03  FILLER                    PIC X(02).                             
020700     03  RAD1-TIAAMMDD             PIC 9(06).                             
020800     03  RAD1-IDDISTR              PIC Z(4)9.                             
020900     03  FILLER                    PIC X(01).                             
021000     03  RAD1-IDSHIPM              PIC Z(06)9.                            
021100     03  FILLER                    PIC X(04).                             
021200     03  RAD1-IDTRPTNR             PIC Z(02)9.                            
021300     03  FILLER                    PIC X(01).                             
021400     03  RAD1-IDLBBET              PIC X(12).                             
021500     03  FILLER                    PIC X(02).                             
021600     03  RAD1-PAGE-NO              PIC Z(03).                             
021700                                                                          
021800 01  RAD-HEAD.                                                            
021900     03  FILLER                    PIC X(15).                             
022000     03  RAD1H-IMPORTER-TEXT       PIC X(13).                             
022100     03  FILLER                    PIC X(2).                              
022200     03  RAD1H-IMPORTER            PIC X(35).                             
022300     03  FILLER                    PIC X(2).                              
022400     03  RAD-TYP-IDSHIP            PIC X(50).                             
022500                                                                          
022600 01  RAD2-HEAD.                                                           
022700     03  FILLER                    PIC X(30).                             
022800     03  RAD2H-IMPORTER            PIC X(35).                             
022900                                                                          
023000 01  RAD3-HEAD.                                                           
023100     03  FILLER                    PIC X(30).                             
023200     03  RAD3H-IMPORTER            PIC X(35).                             
023300                                                                          
023400 01  RAD5-HEAD.                                                           
023500     03  FILLER                    PIC X(30).                             
023600     03  RAD5H-IMPORTER            PIC X(35).                             
023700     03  FILLER                    PIC X(02).                             
023800     03  RAD5-FILL                 PIC X(09).                             
023900     03  RAD5-IDFAKT               PIC 9(07).                             
024000                                                                          
024100 01  MANF-RUB1.                                                           
024200     03  FILLER                    PIC X(10)    VALUE SPACE.              
024300     03  MANF-RUB1-1               PIC X(35)    VALUE 'NAME'.             
024400     03  FILLER                    PIC X(3)     VALUE SPACE.              
024500     03  FILLER                    PIC X(35)    VALUE SPACE.              
024600                                                                          
024700 01  MANF-RUB2.                                                           
024800     03  FILLER                    PIC X(10)    VALUE SPACE.              
024900     03  MANF-RUB2-1               PIC X(07)    VALUE 'POSTAL '.          
025000     03  MANF-RUB2-2               PIC X(07)    VALUE 'ADDRESS'.          
025100     03  FILLER                    PIC X(24)    VALUE SPACE.              
025200     03  MANF-RUB2-3               PIC X(35)    VALUE 'COUNTRY'.          
025300                                                                          
025400 01  MANF-RUB3.                                                           
025500     03  FILLER   PIC X(1)  VALUE SPACE.                                  
025600     03  FILLER   PIC X(28) VALUE 'WE CERTIFY THAT MERCHANDISE '.         
025700     03  FILLER   PIC X(26) VALUE 'DELIVERED UNDER INVOICE R-'.           
025800     03  MANF-RUB3-IDFAKT          PIC 9(7)     VALUE ZERO.               
025900     03  FILLER   PIC X(28) VALUE ' ARE MANUFACTURED BY :      '.         
026000*                                                                         
026100*    FÖR UTSKRIFT AV MANF BILAGA                                          
026200*                                                                         
026300 01  ARBETS-RAD.                                                          
026400     03  FILLER                    PIC X(02).                             
026500     03  RAD-IDLEVNR               PIC X(05).                             
026600     03  FILLER                    PIC X(03).                             
026700     03  RAD-ADR1                  PIC X(35).                             
026800     03  FILLER                    PIC X(03).                             
026900     03  RAD-ADR2                  PIC X(35).                             
027000     03  FILLER                    PIC X(02).                             
027100     EJECT                                                                
027200                                                                          
027300*                                                                         
027400     EJECT                                                                
027500 01  FILLER                    PIC X(16)   VALUE 'IMS-WS'.                
027600     SKIP2                                                                
027700 01  KEYS-TO-DLI.                                                         
027800     03  W-WDB101KY-X.                                                    
027900        05  W-WDB101-IDPARTNR   PIC X(09)   VALUE SPACE.                  
028000        05  W-WDB101-IDFTG      PIC 9(02)   VALUE ZERO.                   
028100                                                                          
028200     03  W-WDB201KY-X.                                                    
028300        05  W-WDB201-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.            
028400        05  W-WDB201-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.            
028500                                                                          
028600     03  W-IDSHIPM-X.                                                     
028700        05  W-IDSHIPM           PIC 9(7)    VALUE ZERO.                   
028800                                                                          
028900     03  W-WDE111KY-X.                                                    
029000        05  W-WDE111-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.            
029100        05  W-WDE111-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.            
029200                                                                          
029300     03  W-WDE111KY-MIN.                                                  
029400        05  W-WDE111-IDDISTR-MIN PIC S9(05)  VALUE ZERO COMP-3.           
029500        05  FILLER               PIC X(04)   VALUE LOW-VALUES.            
029600                                                                          
029700     03  W-WDE111KY-MAX.                                                  
029800        05  W-WDE111-IDDISTR-MAX PIC S9(05)  VALUE ZERO COMP-3.           
029900        05  FILLER               PIC X(04)   VALUE HIGH-VALUES.           
030000                                                                          
030100     03  W-WDE121KY-X.                                                    
030200        05  W-WDE121-IDPRODNR    PIC S9(07)  VALUE ZERO COMP-3.           
030300        05  W-WDE121-IDKOLLI     PIC S9(05)  VALUE ZERO COMP-3.           
030400                                                                          
030500     03  W-IDARTNR-X.                                                     
030600        05  W-IDARTNR            PIC S9(9)   VALUE ZERO COMP-3.           
030700                                                                          
030800     03  W-IDLEVNR-X.                                                     
030900        05  W-IDLEVNR            PIC X(5)    VALUE SPACE.                 
031000                                                                          
031100     03  W-IDDC-X.                                                        
031200        05  W-IDDC               PIC X(2)    VALUE SPACE.                 
031300                                                                          
031400     03  W-IDSKYLT-X.                                                     
031500        05  W-IDSKYLT            PIC X(3)    VALUE 'GB '.                 
031600                                                                          
031700*    --- STATUS-KOD FRÅN IMS                                              
031800 01  STATUS-WS                   PIC XX.                                  
031900     88  SEGMENT-FINNS                       VALUE '  '.                  
032000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
032100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
032200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
032300     88  IMS-EJ-OK                           VALUE 'XD'.                  
032400     SKIP2                                                                
032500 01  GOOD-STATUSCODES.                                                    
032600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
032700     SKIP3                                                                
032800 01  SSA1                        PIC X(64).                               
032900 01  SSA2                        PIC X(64).                               
033000 01  SSA3                        PIC X(64).                               
033100 01  SSA4                        PIC X(64).                               
033200     EJECT                                                                
033300*    --- IMS FUNKTIONSKODER                                               
033400*01  -COPY W0003                                                          
033500     EJECT                                                                
033600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE101'.                      
033700 01  DLI-IO-WDE101.                                                       
033800*    03  -COPY WDE101                                                     
033900                                                                          
034000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE111'.                      
034100 01  DLI-IO-WDE111.                                                       
034200*    03  -COPY WDE111                                                     
034300                                                                          
034400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE121'.                      
034500 01  DLI-IO-WDE121.                                                       
034600*    03  -COPY WDE121                                                     
034700                                                                          
034800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE131'.                      
034900 01  DLI-IO-WDE131.                                                       
035000*    03  -COPY WDE131                                                     
035100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
035200 01  DLI-IO-WDB201.                                                       
035300*    03  -COPY WDB201                                                     
035400                                                                          
035500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
035600 01  DLI-IO-WDB101.                                                       
035700*    03  -COPY WDB101                                                     
035800                                                                          
035900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
036000 01  DLI-IO-WDK601.                                                       
036100*    03  -COPY WDK601                                                     
036200     EJECT                                                                
036300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF106'.                      
036400 01  DLI-IO-WDF106.                                                       
036500*    03  -COPY WDF106                                                     
036600     EJECT                                                                
036700                                                                          
036800 LINKAGE SECTION.                                                         
036900*01  -COPY W476TRPD                                                       
037000                                                                          
037100*01  -COPY W0009   -PRE MSG-                                              
037200     EJECT                                                                
037300*01  -COPY W0009   -PRE ALT-                                              
037400     EJECT                                                                
037500*01  -COPY W0008  -PRE WDE1-                                              
037600     05  FILLER                  PIC X.                                   
037700*01  -COPY W0008  -PRE WDB2-                                              
037800     05  FILLER                  PIC X.                                   
037900*01  -COPY W0008  -PRE WDB1-                                              
038000     05  FILLER                  PIC X.                                   
038100*01  -COPY W0008  -PRE WDK6-                                              
038200     05  FILLER                  PIC X.                                   
038300*01  -COPY W0008  -PRE WDF1-                                              
038400     05  FILLER                  PIC X.                                   
038500                                                                          
038600 PROCEDURE DIVISION  USING TRPD-W476TRPD                                  
038700                           ALT-PCB                                        
038800                           WDE1-PCB                                       
038900                           WDB2-PCB                                       
039000                           WDB1-PCB                                       
039100                           WDK6-PCB                                       
039200                           WDF1-PCB.                                      
039300 MAIN SECTION.                                                            
039400     ENTRY 'DLITCBL' USING TRPD-W476TRPD                                  
039500                           ALT-PCB                                        
039600                           WDE1-PCB                                       
039700                           WDB2-PCB                                       
039800                           WDB1-PCB                                       
039900                           WDK6-PCB                                       
040000                           WDF1-PCB.                                      
040100                                                                          
040200     PERFORM A-INIT                                                       
040300     PERFORM IMS-GU-WDE101                                                
040400                                                                          
040500     PERFORM B-SPARA-DATA                                                 
040600                                                                          
040700     PERFORM C-INIT-ALLM                                                  
040800                                                                          
040900     PERFORM IMS-GU-WDE101                                                
041000                                                                          
041100     IF SEGMENT-FINNS                                                     
041200       PERFORM IMS-GNP-WDE111                                             
041300       MOVE SGMT-IDKUNDNR               TO W-WDE111-IDKUNDNR              
041400       MOVE SGMT-IDDISTR                TO W-WDE111-IDDISTR               
041500                                                                          
041600       MOVE SGMT-IDDISTR                TO W-WDB201-IDDISTR               
041700       MOVE SGMT-IDKUNDNR               TO W-WDB201-IDKUNDNR              
041800       PERFORM IMS-GU-WDB201                                              
041900       MOVE GMT-IDPARTNR                TO W-WDB101-IDPARTNR              
042000       MOVE GMT-IDFTG                   TO W-WDB101-IDFTG                 
042100       PERFORM IMS-GU-WDB101                                              
042200                                                                          
042300       PERFORM F-SKAPA-MANF-LISTA                                         
042400                                                                          
042500     END-IF                                                               
042600                                                                          
042700     PERFORM Z-FINIT                                                      
042800                                                                          
042900     MOVE ZERO TO RETURN-CODE                                             
043000     GOBACK                                                               
043100     .                                                                    
043200     EJECT                                                                
043300 A-INIT SECTION.                                                          
043400                                                                          
043500     MOVE '***---- INIT-MANF ***----'  TO WS-SEKTION                      
043600*                                                                         
043700     MOVE ZERO TO MANFTAB-ANTAL-POST                                      
043800     MOVE +1 TO INDX                                                      
043900     PERFORM UNTIL                                                        
044000      ( INDX > MANFTAB-MAX-ANTAL-POST )                                   
044100        MOVE SPACE     TO MANF-IDLEVNR (INDX)                             
044200        ADD +1    TO INDX                                                 
044300     END-PERFORM                                                          
044400                                                                          
044500     MOVE ZERO     TO TAB-IX                                              
044600                                                                          
044700     ACCEPT DAGENS-DATUM  FROM DATE                                       
044800     MOVE   DAGENS-DATUM  TO WS-AAMMDD                                    
044900                                                                          
045000     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
045100                                                                          
045200     MOVE TRPD-IDPRTLST   TO  W-IDPRTLST                                  
045300                              WS-PRT-IDPRTLST                             
045400     MOVE TRPD-IDSHIPM    TO  W-IDSHIPM                                   
045500     MOVE TRPD-PFDEF-OVR  TO  PRT-PFDEF-OVR                               
045600     MOVE TRPD-IDDISTR    TO  W-WDE111-IDDISTR                            
045700                              W-WDB201-IDDISTR                            
045800                              W-WDE111-IDDISTR-MIN                        
045900                              W-WDE111-IDDISTR-MAX                        
046000                              TEST-IDDISTR                                
046100     MOVE SPACES          TO RAD-HEAD                                     
046200                             RAD2-HEAD                                    
046300                             RAD3-HEAD                                    
046400                             RAD5-HEAD                                    
046500     .                                                                    
046600     EJECT                                                                
046700 B-SPARA-DATA SECTION.                                                    
046800     MOVE 'B-SPARA-DATA'       TO WS-SEKTION                              
046900                                                                          
047000     MOVE 1                    TO INDX                                    
047100     PERFORM IMS-GNP-WDE111                                               
047200     MOVE SGMT-IDKUNDNR        TO W-WDE111-IDKUNDNR                       
047300     MOVE SGMT-IDDISTR         TO W-WDE111-IDDISTR                        
047400     PERFORM UNTIL SEGMENT-SAKNAS                                         
047500                                                                          
047600       PERFORM IMS-GNP-WDE121                                             
047700       MOVE SKOLLI-IDPRODNR    TO W-WDE121-IDPRODNR                       
047800       MOVE SKOLLI-IDKOLLI     TO W-WDE121-IDKOLLI                        
047900       IF W-IDFAKT = ZERO                                                 
048000         MOVE SKOLLI-IDFAKT    TO W-IDFAKT                                
048100                                  MANF-RUB3-IDFAKT                        
048200       END-IF                                                             
048300       PERFORM UNTIL SEGMENT-SAKNAS                                       
048400         PERFORM IMS-GNP-WDE131                                           
048500         PERFORM UNTIL SEGMENT-SAKNAS                                     
048600           PERFORM S01-LAES-MANF                                          
048700           PERFORM S01-INDEX-NUMMER                                       
048800           IF INDX NOT = ZERO                                             
048900             MOVE ART-IDLEVNR      TO MANF-IDLEVNR(INDX)                  
049000           END-IF                                                         
049100           IF INDX > MANFTAB-ANTAL-POST                                   
049200             MOVE INDX             TO MANFTAB-ANTAL-POST                  
049300           END-IF                                                         
049400           IF INDX = MANFTAB-MAX-ANTAL-POST                               
049500            MOVE 'MANF TABELL FÖR LITEN ' TO FELTEXT                      
049600            CALL FELLOG                                                   
049700           END-IF                                                         
049800           MOVE SRAD-IDARTNR        TO W-IDARTNR                          
049900           MOVE SHIP-IDDC           TO W-IDDC                             
050000                                                                          
050100           ADD +1                   TO INDX                               
050200           PERFORM IMS-GNP-WDE131                                         
050300         END-PERFORM                                                      
050400         PERFORM IMS-GNP-WDE121                                           
050500         MOVE SKOLLI-IDPRODNR    TO W-WDE121-IDPRODNR                     
050600         MOVE SKOLLI-IDKOLLI     TO W-WDE121-IDKOLLI                      
050700       END-PERFORM                                                        
050800                                                                          
050900       PERFORM IMS-GNP-WDE111                                             
051000       MOVE SGMT-IDDISTR      TO W-WDE111-IDDISTR                         
051100       MOVE SGMT-IDKUNDNR     TO W-WDE111-IDKUNDNR                        
051200     END-PERFORM                                                          
051300                                                                          
051400     .                                                                    
051500     EJECT                                                                
051600 C-INIT-ALLM SECTION.                                                     
051700     MOVE 'C-INIT-ALLM'       TO WS-SEKTION                               
051800                                                                          
051900     MOVE ZERO  TO WS-SIDNR                                               
052000                                                                          
052100     MOVE +1    TO TAB-IX                                                 
052200                                                                          
052300     MOVE +45   TO WS-RADNR                                               
052400                                                                          
052500     IF WS-AA < 50                                                        
052600       MOVE 20                        TO WS-CENTURY                       
052700     ELSE                                                                 
052800       MOVE 19                        TO WS-CENTURY                       
052900     END-IF                                                               
053000                                                                          
053100     MOVE WS-CENTURY                  TO RUB-CC                           
053200     MOVE DAGENS-DATUM-MAANAD         TO RUB-MM                           
053300     MOVE DAGENS-DATUM-DAG            TO RUB-DD                           
053400     MOVE DAGENS-DATUM-AAR            TO RUB-AA                           
053500                                                                          
053600     MOVE WS-SIDNR                    TO SIDNR                            
053700     .                                                                    
053800     EJECT                                                                
053900 F-SKAPA-MANF-LISTA  SECTION.                                             
054000     MOVE 'F-SKAPA-MANF-LISTA'        TO WS-SEKTION                       
054100                                                                          
054200     IF MANFTAB-ANTAL-POST NOT = ZERO                                     
054300        IF MANFTAB-ANTAL-POST > +1                                        
054400          MOVE MANFTAB-ANTAL-POST     TO INTSOR-POST-ANTAL                
054500          MOVE MANFTAB-POST-LAENGD    TO INTSOR-POST-LAENGD               
054600          MOVE MANFTAB-IDLEVNR-LAENGD TO INTSOR-SORT-FAELT-LAENGD         
054700          CALL INTSOR USING MANF-TABELL (+1)                              
054800                            INTSOR-POST-LAENGD                            
054900                            INTSOR-POST-ANTAL                             
055000                            MANF-IDLEVNR (+1)                             
055100                            INTSOR-SORT-FAELT-LAENGD                      
055200        END-IF                                                            
055300        MOVE +99 TO WS-RADNR                                              
055400        MOVE +1  TO INDX                                                  
055500        PERFORM UNTIL                                                     
055600        ( INDX > MANFTAB-ANTAL-POST )                                     
055700          IF WS-RADNR > MAX-RADER                                         
055800            PERFORM S02-HUVUD-DEL-1                                       
055900            PERFORM FA-MANF-RUBRIK                                        
056000          END-IF                                                          
056100          MOVE SPACES                   TO ARBETS-RAD                     
056200          MOVE MANF-IDLEVNR (INDX)      TO RAD-IDLEVNR                    
056300                                           W-IDLEVNR                      
056400          PERFORM IMS-GU-WDF106                                           
056500          MOVE ADR-BELEV                TO RAD-ADR1                       
056600          MOVE PRT-AFTER-2              TO PRT-RADSKIP                    
056700          MOVE WS-SKIP2                 TO STYRTECKEN-RAD                 
056800          MOVE ARBETS-RAD               TO WS-RAD                         
056900                                           SEND-RAD                       
057000          PERFORM S05-SKRIV-EN-RAD                                        
057100          MOVE SPACES                   TO ARBETS-RAD                     
057200          MOVE ADR-ADLEV-RAD1           TO RAD-ADR1                       
057300          MOVE ADR-ADLEV-RAD2           TO RAD-ADR2                       
057400          MOVE PRT-AFTER-1              TO PRT-RADSKIP                    
057500          MOVE WS-SKIP1                 TO STYRTECKEN-RAD                 
057600          MOVE ARBETS-RAD               TO WS-RAD                         
057700                                           SEND-RAD                       
057800          PERFORM S05-SKRIV-EN-RAD                                        
057900          MOVE SPACES                   TO ARBETS-RAD                     
058000          MOVE ADR-ADLEV-ORT            TO RAD-ADR1                       
058100          MOVE ADR-ADLEVLND             TO RAD-ADR2                       
058200          MOVE PRT-AFTER-1              TO PRT-RADSKIP                    
058300          MOVE WS-SKIP1                 TO STYRTECKEN-RAD                 
058400          MOVE ARBETS-RAD               TO WS-RAD                         
058500                                           SEND-RAD                       
058600          PERFORM S05-SKRIV-EN-RAD                                        
058700          MOVE SPACES                   TO ARBETS-RAD                     
058800                                                                          
058900          ADD +4                        TO WS-RADNR                       
059000          ADD +1                        TO INDX                           
059100                                                                          
059200        END-PERFORM                                                       
059300        IF WS-RADNR > MAX-RADER - 5                                       
059400          PERFORM S02-HUVUD-DEL-1                                         
059500          PERFORM FA-MANF-RUBRIK                                          
059600        END-IF                                                            
059700        PERFORM FB-MANF-SLUT-RADER                                        
059800     END-IF                                                               
059900     .                                                                    
060000     EJECT                                                                
060100 FA-MANF-RUBRIK   SECTION.                                                
060200     MOVE '***-- FA-MANF-RUBRIK ***---'  TO WS-SEKTION                    
060300                                                                          
060400     MOVE SPACES                         TO ARBETS-RAD                    
060500     MOVE MANF-RUB3                      TO ARBETS-RAD                    
060600     MOVE ARBETS-RAD                     TO WS-RAD                        
060700                                            SEND-RAD                      
060800     MOVE PRT-AFTER-2                    TO PRT-RADSKIP                   
060900     MOVE WS-SKIP2                       TO STYRTECKEN-RAD                
061000     PERFORM S05-SKRIV-EN-RAD                                             
061100     MOVE SPACES                         TO ARBETS-RAD                    
061200     MOVE MANF-RUB1                      TO ARBETS-RAD                    
061300     MOVE ARBETS-RAD                     TO WS-RAD                        
061400                                            SEND-RAD                      
061500     MOVE PRT-AFTER-1                    TO PRT-RADSKIP                   
061600     MOVE WS-SKIP1                       TO STYRTECKEN-RAD                
061700     PERFORM S05-SKRIV-EN-RAD                                             
061800     MOVE SPACES                         TO ARBETS-RAD                    
061900     MOVE MANF-RUB2                      TO ARBETS-RAD                    
062000     MOVE ARBETS-RAD                     TO WS-RAD                        
062100                                            SEND-RAD                      
062200     MOVE PRT-AFTER-1                    TO PRT-RADSKIP                   
062300     MOVE WS-SKIP1                       TO STYRTECKEN-RAD                
062400     PERFORM S05-SKRIV-EN-RAD                                             
062500     ADD 4                               TO WS-RADNR                      
062600     .                                                                    
062700     EJECT                                                                
062800 FB-MANF-SLUT-RADER SECTION.                                              
062900     MOVE 'FB-MANF-SLUT-RADER'           TO WS-SEKTION                    
063000                                                                          
063100     MOVE SPACES                         TO ARBETS-RAD                    
063200     MOVE ' CERTIFIED TRUE AND CORRECT'  TO ARBETS-RAD                    
063300     MOVE PRT-AFTER-2                    TO PRT-RADSKIP                   
063400     MOVE WS-SKIP2                       TO STYRTECKEN-RAD                
063500     MOVE ARBETS-RAD                     TO WS-RAD                        
063600                                            SEND-RAD                      
063700     PERFORM S05-SKRIV-EN-RAD                                             
063800                                                                          
063900     MOVE SPACES                         TO ARBETS-RAD                    
064000     MOVE ' VOLVO CAR PARTS'             TO ARBETS-RAD                    
064100     MOVE PRT-AFTER-2                    TO PRT-RADSKIP                   
064200     MOVE WS-SKIP2                       TO STYRTECKEN-RAD                
064300     MOVE ARBETS-RAD                     TO WS-RAD                        
064400                                            SEND-RAD                      
064500     PERFORM S05-SKRIV-EN-RAD                                             
064600     .                                                                    
064700     EJECT                                                                
064800 Z-FINIT SECTION.                                                         
064900                                                                          
065000     MOVE 'S' TO POSTSUM-OPKOD                                            
065100     CALL POSTSUM USING POSTSUM-PARM                                      
065200     .                                                                    
065300     EJECT                                                                
065400 S01-LAES-MANF SECTION.                                                   
065500     MOVE 'S01-LAES-MANF'           TO WS-SEKTION                         
065600                                                                          
065700*      BEHÖVER NÅGOT REGISTER LÄSAS ?                                     
065800     MOVE SRAD-IDARTNR          TO W-IDARTNR                              
065900     MOVE 'GB '                 TO W-IDSKYLT                              
066000     PERFORM IMS-GU-WDK601                                                
066100     MOVE ART-IDLEVNR           TO W-IDLEVNR                              
066200     .                                                                    
066300     EJECT                                                                
066400 S01-INDEX-NUMMER  SECTION.                                               
066500     MOVE 'S01-INDEX-NUMMER'           TO WS-SEKTION                      
066600                                                                          
066700     MOVE +1                    TO INDX                                   
066800     PERFORM UNTIL INDX > MANFTAB-ANTAL-POST OR                           
066900       ART-IDLEVNR = MANF-IDLEVNR(INDX)                                   
067000       IF ART-IDLEVNR = MANF-IDLEVNR(INDX)                                
067100         MOVE -1                     TO INDX                              
067200       ELSE                                                               
067300         IF INDX > MANFTAB-ANTAL-POST                                     
067400           SUBTRACT 1                FROM INDX                            
067500         END-IF                                                           
067600       END-IF                                                             
067700       ADD 1                         TO INDX                              
067800                                                                          
067900     END-PERFORM                                                          
068000     .                                                                    
068100     EJECT                                                                
068200 S02-HUVUD-DEL-1     SECTION.                                             
068300     MOVE '***--- S02-HUVUD-DEL-1 ***---'  TO WS-SEKTION                  
068400*                                                                         
068500*    MOVE SHIP-IDDC TO WS-IDDC                                            
068600                                                                          
068700     MOVE ZERO                       TO WS-RADNR                          
068800     MOVE SPACES                     TO ARBETS-RAD                        
068900                                        WS-RAD                            
069000                                        SEND-RAD                          
069100     MOVE WS-PAGESKIP                TO STYRTECKEN-RAD                    
069200     PERFORM S90-PUT-DOC-LINE                                             
069300*    MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
069400*    PERFORM S90-PUT-DOC-LINE                                             
069500     MOVE WS-SKIP3                   TO STYRTECKEN-RAD                    
069600     PERFORM S90-PUT-DOC-LINE                                             
069700     MOVE BET-BEBETRAD-1             TO RAD1H-IMPORTER                    
069800     MOVE BET-BEBETRAD-2             TO RAD2H-IMPORTER                    
069900     MOVE BET-ADBETRAD-1             TO RAD3H-IMPORTER                    
070000     MOVE BET-ADBETRAD-2             TO RAD4H-IMPORTER                    
070100     MOVE BET-BELAND-SVE             TO RAD5H-IMPORTER                    
070200     MOVE 'INVOICE: '                TO RAD5-FILL                         
070300     MOVE W-IDFAKT                   TO RAD5-IDFAKT                       
070400     MOVE 'IMPORTER REF'             TO RAD1H-IMPORTER-TEXT               
070500     MOVE MANF-RUBRIK                TO RAD-TYP-IDSHIP                    
070600     MOVE RAD-HEAD                   TO WS-RAD                            
070700                                        SEND-RAD                          
070800     MOVE PRT-NYSIDA-RAD7            TO PRT-RADSKIP                       
070900     ADD 7                           TO WS-RADNR                          
071000     PERFORM S05-SKRIV-EN-RAD                                             
071100     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
071200     MOVE RAD2-HEAD                  TO ARBETS-RAD                        
071300                                        WS-RAD                            
071400                                        SEND-RAD                          
071500     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
071600     ADD 1                           TO WS-RADNR                          
071700     PERFORM S05-SKRIV-EN-RAD                                             
071800     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
071900                                                                          
072000     MOVE RAD3-HEAD                  TO ARBETS-RAD                        
072100                                        WS-RAD                            
072200                                        SEND-RAD                          
072300     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
072400     ADD 1                           TO WS-RADNR                          
072500     PERFORM S05-SKRIV-EN-RAD                                             
072600*    MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
072700                                                                          
072800     ADD 1                     TO WS-SIDNR                                
072900     MOVE SPACE                TO RAD1                                    
073000     MOVE BET-ADBETRAD-2       TO RAD4H-IMPORTER                          
073100     MOVE SHIP-TISKEPPN        TO RAD1-TIAAMMDD                           
073200     MOVE SGMT-IDDISTR         TO RAD1-IDDISTR                            
073300     MOVE SHIP-IDSHIPM         TO RAD1-IDSHIPM                            
073400     MOVE SHIP-IDTRPTNR        TO RAD1-IDTRPTNR                           
073500     MOVE SHIP-IDLBBET         TO RAD1-IDLBBET                            
073600     MOVE WS-SIDNR             TO RAD1-PAGE-NO                            
073700     MOVE RAD1                 TO ARBETS-RAD                              
073800                                  WS-RAD                                  
073900                                  SEND-RAD                                
074000     MOVE PRT-AFTER-1          TO PRT-RADSKIP                             
074100     MOVE WS-SKIP1             TO STYRTECKEN-RAD                          
074200     ADD 1                     TO WS-RADNR                                
074300     PERFORM S05-SKRIV-EN-RAD                                             
074400     MOVE RAD5-HEAD                  TO ARBETS-RAD                        
074500                                        WS-RAD                            
074600                                        SEND-RAD                          
074700     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
074800     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
074900     ADD 1                           TO WS-RADNR                          
075000     PERFORM S05-SKRIV-EN-RAD                                             
075100     .                                                                    
075200     EJECT                                                                
075300 S05-SKRIV-EN-RAD  SECTION.                                               
075400     MOVE 'S05-SKRIV-EN-RAD---'           TO WS-SEKTION                   
075500                                                                          
075600     PERFORM S90-PUT-DOC-LINE                                             
075700     IF TRPD-IDPGM = 'W4062200' OR                                        
075800        TRPD-IDPGM = 'W4063400'                                           
075900      CALL W006PRS1 USING PRT-SPOOL-OVR                                   
076000                          PRT-WRITE                                       
076100                          W-IDPRTLST                                      
076200                          ALT-PCB                                         
076300                          PRT-RADSKIP                                     
076400                          WS-RAD                                          
076500     END-IF                                                               
076600     .                                                                    
076700     EJECT                                                                
076800 S90-PUT-DOC-LINE SECTION.                                                
076900     MOVE 'S90-PUT-DOC-LINE---'           TO WS-SEKTION                   
077000     IF TRPD-IDPGM = 'W4063400' AND TRPD-KVCOPIES = '1'                   
077100       IF TRPD-FLSKRIV-ONDEM = YES OR JA                                  
077200         MOVE +1                          TO SEND-IDCOM                   
077300         MOVE 'PUT'                       TO SEND-KDFUNC                  
077400         MOVE LENGTH OF SEND-RAD-STYRTECKEN TO SEND-KVDLEN                
077500         CALL WZ01SEND USING SEND-CONTROL-AREA                            
077600                             SEND-KVDLEN                                  
077700                             SEND-RAD-STYRTECKEN                          
077800         IF SEND-KDRC > ZERO                                              
077900           MOVE SEND-KDRC                 TO KDRC-DISPLAY                 
078000           STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                   
078100           DELIMITED BY SIZE INTO FELTEXT-STR                             
078200           CALL ABEND USING RKOD-ABEND-WITH-DUMP                          
078300         END-IF                                                           
078400       END-IF                                                             
078500     END-IF                                                               
078600     .                                                                    
078700     EJECT                                                                
078800*****************IMS-SECTIONER*********************                       
078900                                                                          
079000 IMS-GU-WDE101  SECTION.                                                  
079100                                                                          
079200     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
079300          DELIMITED BY SIZE INTO SSA1                                     
079400     MOVE '  GE'            TO GOOD-STATUSCODES                           
079500     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
079600     MOVE WDE1-STATUS-CODE  TO STATUS-WS                                  
079700     PERFORM IMS-STATUSCHECK                                              
079800     .                                                                    
079900     EJECT                                                                
080000 IMS-GNP-WDE111 SECTION.                                                  
080100                                                                          
080200     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
080300          DELIMITED BY SIZE INTO SSA1                                     
080400     STRING 'WDE111  (WDE111KY>=' W-WDE111KY-MIN                          
080500                    '&WDE111KY<=' W-WDE111KY-MAX ')'                      
080600          DELIMITED BY SIZE INTO SSA2                                     
080700     MOVE '  GE'            TO GOOD-STATUSCODES                           
080800     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111 SSA1 SSA2              
080900     MOVE WDE1-STATUS-CODE  TO STATUS-WS                                  
081000     PERFORM IMS-STATUSCHECK                                              
081100     .                                                                    
081200     EJECT                                                                
081300 IMS-GNP-WDE121  SECTION.                                                 
081400                                                                          
081500     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
081600          DELIMITED BY SIZE INTO SSA1                                     
081700     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
081800          DELIMITED BY SIZE INTO SSA2                                     
081900     MOVE 'WDE121  '          TO SSA3                                     
082000     MOVE '  GE'            TO GOOD-STATUSCODES                           
082100     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE121 SSA1 SSA2 SSA3         
082200     MOVE WDE1-STATUS-CODE  TO STATUS-WS                                  
082300     PERFORM IMS-STATUSCHECK                                              
082400     .                                                                    
082500     EJECT                                                                
082600 IMS-GNP-WDE131  SECTION.                                                 
082700                                                                          
082800     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
082900          DELIMITED BY SIZE INTO SSA1                                     
083000     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
083100          DELIMITED BY SIZE INTO SSA2                                     
083200     STRING 'WDE121  (WDE121KY =' W-WDE121KY-X ')'                        
083300          DELIMITED BY SIZE INTO SSA3                                     
083400     MOVE 'WDE131  '          TO SSA4                                     
083500     MOVE '  GE'            TO GOOD-STATUSCODES                           
083600     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE131 SSA1 SSA2              
083700                                                   SSA3 SSA4              
083800     MOVE WDE1-STATUS-CODE  TO STATUS-WS                                  
083900     PERFORM IMS-STATUSCHECK                                              
084000     .                                                                    
084100     EJECT                                                                
084200 IMS-GU-WDB201 SECTION.                                                   
084300                                                                          
084400     STRING 'WDB201  (IDGMT    =' W-WDB201KY-X ')'                        
084500          DELIMITED BY SIZE INTO SSA1                                     
084600     MOVE '  GE'            TO GOOD-STATUSCODES                           
084700     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
084800     MOVE WDB2-STATUS-CODE  TO STATUS-WS                                  
084900     PERFORM IMS-STATUSCHECK                                              
085000     .                                                                    
085100     EJECT                                                                
085200 IMS-GU-WDB101 SECTION.                                                   
085300                                                                          
085400     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
085500          DELIMITED BY SIZE INTO SSA1                                     
085600     MOVE '  GE'            TO GOOD-STATUSCODES                           
085700     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
085800     MOVE WDB1-STATUS-CODE  TO STATUS-WS                                  
085900     PERFORM IMS-STATUSCHECK                                              
086000     .                                                                    
086100     EJECT                                                                
086200 IMS-GU-WDK601  SECTION.                                                  
086300                                                                          
086400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
086500       DELIMITED BY SIZE INTO SSA1                                        
086600     MOVE '    '            TO GOOD-STATUSCODES                           
086700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
086800     MOVE WDK6-STATUS-CODE  TO STATUS-WS                                  
086900     PERFORM IMS-STATUSCHECK                                              
087000     .                                                                    
087100 IMS-GU-WDF106 SECTION.                                                   
087200                                                                          
087300     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
087400           DELIMITED BY SIZE INTO SSA1                                    
087500     MOVE 'WDF106  '        TO SSA2                                       
087600     MOVE 'GE  '            TO GOOD-STATUSCODES                           
087700     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF106 SSA1 SSA2               
087800     MOVE WDF1-STATUS-CODE  TO STATUS-WS                                  
087900     PERFORM IMS-STATUSCHECK                                              
088000     .                                                                    
088100     EJECT                                                                
088200 IMS-STATUSCHECK SECTION.                                                 
088300                                                                          
088400     SET STATUS-IX TO 1                                                   
088500     SEARCH GOOD-STATUS                                                   
088600       AT END                                                             
088700         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
088800           DELIMITED BY SIZE INTO FELTEXT                                 
088900         CALL FELLOG                                                      
089000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
089100         CONTINUE                                                         
089200     END-SEARCH                                                           
089300     .                                                                    
089400     EJECT                                                                
