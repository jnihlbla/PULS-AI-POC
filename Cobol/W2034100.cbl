000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W2034100.                                                
000500*AUTHOR.         HENRIK ARONSSON.                                         
000600*DATE-WRITTEN.   MAJ 1992.                                                
000700                                                                          
000800*    REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        PROGRAMMETS UPPGIFT ÄR ATT LÄGGA UPP ARTIKEL FÖR                 
001200*        ANGIVET SDC SAMT VISA/UPPDATERA ARTIKELINFO.                     
001300*                                                                         
001400*        PROGRAM IS MODIFIED TO DISPLAY PART INFORMATION                  
001500*        FOR PARTS REFILLED TO CDC FROM NDC(CHINA REFILL PROJECT)         
001600*                                                                         
001700*        PROGRAMMET UPPDATERAR WDK7                                       
001800*                              WDK6                                       
001900*                              WLOIGA  (WDL7)                             
002000*        PROGRAMMET LÄSER      WLBENA  (WDD3)                             
002100*                              WDK6                                       
002200*                              WDL8                                       
002300*                              TP1ARTK (DB2)                              
002400*                              TP1KAMP (DB2)                              
002500*                                                                         
002600*                                                                         
002700*    INDATA.                                                              
002800*        TRANSAKTION: W2T341                                              
002900*        MID:         W2I34101                                            
003000*                                                                         
003100*    UTDATA.                                                              
003200*        MOD:         W2O34101                                            
003300*                                                                         
003400*                                                                         
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900 WORKING-STORAGE SECTION.                                                 
004000*    -COPY WY2000W1                                                       
004100     SKIP3                                                                
004200*    -COPY WY2000W3                                                       
004300     SKIP3                                                                
004400 77  IDPGM                       PIC X(08)   VALUE 'W2034100'.            
004500                                                                          
004600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004800                                                                          
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  YES                         PIC X       VALUE 'Y'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200 77  DEFINITIV                   PIC S9      VALUE +1 COMP-3.             
005300 77  IX                          PIC 9(2)    VALUE ZERO.                  
005400 77  IX1                         PIC 9(2)    VALUE ZERO.                  
005500 77  IX2                         PIC 9(2)    VALUE ZERO.                  
005600 77  INDX                        PIC 9(3)    VALUE ZERO.                  
005700 77  INDX-2                      PIC S9(4)   VALUE +0 COMP SYNC.          
005800 77  INDX-3                      PIC S9(4)   VALUE +0 COMP SYNC.          
005900 77  LEDTIX                      PIC 9(2)    VALUE ZERO.                  
006000 77  PB-IX                       PIC 9(2)    VALUE ZERO.                  
006100 77  DD-PLUS-SEX-MAN             PIC 9(6)    VALUE ZERO.                  
006200 77  PASSIV                      PIC X       VALUE 'P'.                   
006300 77  AKTIV                       PIC X       VALUE 'A'.                   
006400 77  WS-BALANCE                  PIC S9(7)    VALUE ZERO.                 
006500 77  WS-KVPB-SUM                 PIC S9(6)V9(1) COMP-3                    
006600                                              VALUE ZERO.                 
006700 77  WS-KVREFPKT                 PIC 9(7)    VALUE ZERO.                  
006800 77  WS-KVREFBER                 PIC 9(7)    VALUE ZERO.                  
006900 77  WS-TIREFPKT                 PIC 9(7)    VALUE ZERO.                  
007000 77  WS-TIREFPAF                 PIC 9(7)    VALUE ZERO.                  
007100 77  WS-IDDISTR                  PIC 9(5)    VALUE ZERO.                  
007200 77  FL-PRARTBES                 PIC X(1)    VALUE 'N'.                   
007300 77  WS-PRARTBES                 PIC S9(7)V9(2) VALUE ZERO COMP-3.        
007400 77  WS-PRMATRL                  PIC S9(7)V9(2) VALUE ZERO COMP-3.        
007500 77  WS-LEADTID-BEHOV            PIC S9(7)V9(2) VALUE ZERO COMP-3.        
007600 77  WS-IDPROJ                   PIC X(4)       VALUE SPACE.              
007700 77  WS-IDDC-SPAR                PIC X(2)       VALUE SPACE.              
007800 77  WS-IDDC-REF                 PIC X(2)       VALUE SPACE.              
007900 77  WS-IDDC-REF-SPAR            PIC X(2)       VALUE SPACE.              
008000 77  WS-SLACK-FLREFNYO           PIC X(1)    VALUE 'N'.                   
008100 77  WS-PRIS                     PIC S9(7)V9(2) VALUE ZERO COMP-3.        
008200 77  WS-GIT                      PIC S9(8)      VALUE ZERO COMP-3.        
008300 77  WS-STD-TEXT                 PIC X(4)       VALUE ' Std'.             
008400 77  WS-MTRL-TEXT                PIC X(4)       VALUE 'Mtrl'.             
008500 77  WS-PB-TEXT-CDC              PIC X(7)       VALUE 'tot/s  '.          
008600 77  WS-PB-TEXT-XDC              PIC X(7)       VALUE 'tot/s/r'.          
008700 77  WS-KVPB-PLAN                PIC 9(6)V9     VALUE ZERO.               
008800 77  DAGENS-AAAAMMDD             PIC 9(8)       VALUE ZERO.               
008900 77  DAGENS-DATUM-SEKEL          PIC 9(8)       VALUE ZERO.               
009000                                                                          
009100 77  SPRAK-IX                    PIC S9(9)  VALUE +0   COMP SYNC.         
009200 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +462 COMP SYNC.         
009300                                                                          
009400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
009500 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
009600                                                                          
009700 77  SPAR-KDERS                  PIC 9(3)    VALUE ZERO.                  
009800 77  WS-PRARTSTD                 PIC 9(7)V9(2) VALUE ZERO.                
009900 77  WS-IDPERSON-BUY             PIC 9(3)    VALUE ZERO.                  
010000                                                                          
010100 01  WS-TEREFMED.                                                         
010200     05 WS-COMMENT-1             PIC X(40)   VALUE SPACES.                
010300     05 WS-COMMENT-2             PIC X(32)   VALUE SPACES.                
010400                                                                          
010500 77  REFILL-PART-SW              PIC X       VALUE SPACE.                 
010600     88  REFILL-PART                         VALUE 'J'.                   
010700     88  NOT-REFILL-PART                     VALUE 'N'.                   
010800                                                                          
010900 77  ARTIKEL-ERS-SW              PIC X       VALUE 'N'.                   
011000     88  ARTIKEL-ERS-MAERKT                  VALUE 'J'.                   
011100                                                                          
011200 77  REFILL-ART-OK-SW            PIC X       VALUE 'J'.                   
011300     88  REFILL-ART-OK                       VALUE 'J'.                   
011400                                                                          
011500 77  WDK7-SW                     PIC X       VALUE 'N'.                   
011600     88  WDK7-UPPD                           VALUE 'J'.                   
011700                                                                          
011800 77  WDK626-SW                   PIC X       VALUE 'N'.                   
011900     88  WDK626-UPPD                         VALUE 'J'.                   
012000                                                                          
012100 77  WDK727-SW                   PIC X       VALUE 'N'.                   
012200     88  WDK727-UPPD                         VALUE 'J'.                   
012300                                                                          
012400 77  WDK6-SW                     PIC X       VALUE 'N'.                   
012500     88  WDK6-UPPD                           VALUE 'J'.                   
012600                                                                          
012700 77  WDK6-UPD-STAT-SW            PIC X       VALUE 'N'.                   
012800     88  WDK6-UPPD-OK                        VALUE 'J'.                   
012900                                                                          
013000 77  WDK7-UPD-STAT-SW            PIC X       VALUE 'N'.                   
013100     88  WDK7-UPPD-OK                        VALUE 'J'.                   
013200                                                                          
013300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
013400     88  INDATA-OK                           VALUE 'J'.                   
013500     88  INDATA-FEL                          VALUE 'N'.                   
013600                                                                          
013700 77  SW-CAMPAIGN                 PIC X       VALUE 'N'.                   
013800     88  CAMPAIGN-NEJ                        VALUE 'N'.                   
013900     88  CAMPAIGN-JA                         VALUE 'J'.                   
014000                                                                          
014100 77  BUY-SW                      PIC X       VALUE 'N'.                   
014200     88  BUY-NOT-LOCKED                      VALUE 'N'.                   
014300     88  BUY-LOCKED                          VALUE 'J'.                   
014400                                                                          
014500 77  TAB-SW                      PIC X       VALUE 'N'.                   
014600     88  TAB-NOT-LOCKED                      VALUE 'N'.                   
014700     88  TAB-LOCKED                          VALUE 'J'.                   
014800                                                                          
014900 77  SW-K711-EXISTS              PIC X       VALUE 'N'.                   
015000     88  K711-MISSING                        VALUE 'N'.                   
015100     88  K711-EXISTS                         VALUE 'J'.                   
015200                                                                          
015300 77  SW-K611-EXISTS              PIC X       VALUE 'N'.                   
015400     88  K611-MISSING                        VALUE 'N'.                   
015500     88  K611-EXISTS                         VALUE 'J'.                   
015600                                                                          
015700 77  SW-K626-EXISTS              PIC X       VALUE 'N'.                   
015800     88  K626-MISSING                        VALUE 'N'.                   
015900     88  K626-EXISTS                         VALUE 'J'.                   
016000                                                                          
016100 77  SW-K629-EXISTS              PIC X       VALUE 'N'.                   
016200     88  K629-MISSING                        VALUE 'N'.                   
016300     88  K629-EXISTS                         VALUE 'J'.                   
016400                                                                          
016500 77  SW-MESSAGE                  PIC X       VALUE 'N'.                   
016600     88  MESSAGE-NEJ                         VALUE 'N'.                   
016700     88  MESSAGE-JA                          VALUE 'J'.                   
016800                                                                          
016900 77  SW-KVPBJUST                 PIC X       VALUE 'N'.                   
017000     88  NO-KVPBJUST                         VALUE 'N'.                   
017100     88  KVPBJUST-EXIST                      VALUE 'J'.                   
017200                                                                          
017300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
017400     88  NYCKLAR-OK                          VALUE 'J'.                   
017500     88  NYCKLAR-FEL                         VALUE 'N'.                   
017600                                                                          
017700 77  SW-LYNK-PART                PIC X       VALUE 'N'.                   
017800     88  LYNK-PART                           VALUE 'J'.                   
017900                                                                          
018000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
018100     88  HELP-MID                            VALUE '0551'.                
018200     88  EGEN-MID                            VALUE '2341'.                
018300     88  GODK-MID                            VALUE '2341' '2342'          
018400                                                   '2343'.                
018500                                                                          
018600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
018700                                                                          
018800 01  WS-DATUM                    PIC 9(6)    VALUE ZERO.                  
018900 01  FILLER   REDEFINES WS-DATUM.                                         
019000     03 WS-DATUM-AAR             PIC 9(2).                                
019100     03 WS-DATUM-MAN             PIC 9(2).                                
019200     03 WS-DATUM-DAG             PIC 9(2).                                
019300                                                                          
019400 01  WS-TISTODAT-KAMP            PIC 9(6)    VALUE ZERO.                  
019500     EJECT                                                                
019600*      --- VALID IDDC CODES                                               
019700*                                                                         
019800*01    -COPY WWDCKONS                                                     
019900       EJECT                                                              
020000*01  -COPY WWDC99                                                         
020100     EJECT                                                                
020200                                                                          
020300*01  -COPY WWPRODSL                                                       
020400     EJECT                                                                
020500                                                                          
020600 01  WS.                                                                  
020700     03 WS-SECTION               PIC X(24)   VALUE SPACE.                 
020800     03 WS-TIPBJUST-AAVV         PIC 9(4)    VALUE ZERO.                  
020900*                                                                         
021000     03 WS-K626-TIPBJUST-1-JMF   PIC 9(4)    VALUE ZERO.                  
021100     03 WS-K626-TIPBJUST-2-JMF   PIC 9(4)    VALUE ZERO.                  
021200     03 WS-K626-KVPB-JUST-1      PIC S9(6)V9(1) VALUE ZERO COMP-3.        
021300     03 WS-K626-KVPB-JUST-2      PIC S9(6)V9(1) VALUE ZERO COMP-3.        
021400     03 WS-K626-TIAARP-1         PIC 9(4)    VALUE ZERO.                  
021500     03 WS-K626-TIAARP-2         PIC 9(4)    VALUE ZERO.                  
021600*                                                                         
021700     03 WS-K727-TIPBJUST-1-JMF   PIC 9(4)    VALUE ZERO.                  
021800     03 WS-K727-TIPBJUST-2-JMF   PIC 9(4)    VALUE ZERO.                  
021900     03 WS-K727-KVPB-JUST-1      PIC S9(6)V9(1) VALUE ZERO COMP-3.        
022000     03 WS-K727-KVPB-JUST-2      PIC S9(6)V9(1) VALUE ZERO COMP-3.        
022100     03 WS-K727-TIAARP-1         PIC 9(4)    VALUE ZERO.                  
022200     03 WS-K727-TIAARP-2         PIC 9(4)    VALUE ZERO.                  
022300*                                                                         
022400     03 WS-TIPBJUST-1            PIC 9(4)    VALUE ZERO.                  
022500     03 WS-TIPBJUST-2            PIC 9(4)    VALUE ZERO.                  
022600     03 WS-TIAARP-1              PIC 9(4)    VALUE ZERO.                  
022700     03 WS-TIAARP-2              PIC 9(4)    VALUE ZERO.                  
022800     03 WS-KVPB-JUST-1           PIC S9(6)V9(1) VALUE ZERO COMP-3.        
022900     03 WS-KVPB-JUST-2           PIC S9(6)V9(1) VALUE ZERO COMP-3.        
023000     03 DAGENS-AAVV              PIC 9(4)    VALUE ZERO.                  
023100*                                                                         
023200     03 FILLER                   PIC X(16)   VALUE                        
023300                                             'WS-DB2-SEKTION'.            
023400     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
023500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
023600 01  GENERELLA-SUBPROGRAM.                                                
023700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
023800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
023900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
024000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
024100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
024200     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
024300     03  W271REFL                PIC X(8)    VALUE 'W271REFL'.            
024400     03  W272REFL                PIC X(8)    VALUE 'W272REFL'.            
024500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
024600     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
024700     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
024800     03  W271UTUP                PIC X(8)    VALUE 'W271UTUP'.            
024900     03  W272UTUP                PIC X(8)    VALUE 'W272UTUP'.            
025000     03  W271UTIL                PIC X(8)    VALUE 'W271UTIL'.            
025100     EJECT                                                                
025200*    --- PARAMETRAR TILL ABEND                                            
025300                                                                          
025400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
025500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
025600     SKIP2                                                                
025700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
025800*01 -COPY WMEDAREA                                                        
025900     EJECT                                                                
026000*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
026100*01  -COPY WDATAREA                                                       
026200     EJECT                                                                
026300*    --- PARAMETRAR TILL SUBPROGRAM WDAGKONV                              
026400*01  -COPY WDAGAREA                                                       
026500     EJECT                                                                
026600*    --- PARAMETRAR TILL W271REFL                                         
026700*01 -COPY W271REFL -PRE W271-                                             
026800     EJECT                                                                
026900*    --- PARAMETRAR TILL W272REFL                                         
027000*01 -COPY W272REFL -PRE CDC-                                              
027100     EJECT                                                                
027200*    --- PARAMETRAR TILL W271UTUP                                         
027300*01 -COPY W271UTUP       -PRE W271-                                       
027400     EJECT                                                                
027500*    --- PARAMETRAR TILL W272UTUP                                         
027600*01 -COPY W272UTUP       -PRE W272-                                       
027700     EJECT                                                                
027800*    --- PARAMETRAR TILL W271UTIL                                         
027900*01 -COPY W271UTIL                                                        
028000     EJECT                                                                
028100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
028200*01  -COPY WMSGINIT                                                       
028300     EJECT                                                                
028400*   PARAMETRAR TILL SUBPROGRAM WDECEDIT                                   
028500*01  -COPY WDECAREA                                                       
028600     EJECT                                                                
028700 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
028800*   -COPY W005WDK7                                                        
028900     EJECT                                                                
029000 01  MESSAGE-CODES.                                                       
029100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
029200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
029300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
029400     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
029500     03  ARTIKEL-UTGANGEN        PIC X(3)    VALUE '018'.                 
029600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
029700     03  ARTIKEL-ERSATT          PIC X(3)    VALUE '220'.                 
029800     03  PRIS-SAKNAS             PIC X(3)    VALUE '301'.                 
029900     03  ARTIKEL-SAKNAS-SDC      PIC X(3)    VALUE '305'.                 
030000     03  DIREKTLEV               PIC X(3)    VALUE '306'.                 
030100     03  EJ-GODK-REFILL          PIC X(3)    VALUE '307'.                 
030200     03  END-O-I-REG             PIC X(3)    VALUE '308'.                 
030300     03  ERR-ONLY-FUTURE         PIC X(3)    VALUE '363'.                 
030400     03  ERR-MAX-1-YEAR          PIC X(3)    VALUE '364'.                 
030500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
030600     03  INF-NOT-REFILL          PIC X(3)    VALUE '957'.                 
030700     EJECT                                                                
030800                                                                          
030900 01  MEDDELANDE.                                                          
031000     03  MED-1                  PIC X(30)                                 
031100         VALUE 'KIT                           '.                          
031200     03  MED-2                  PIC X(30)                                 
031300         VALUE 'TPO                           '.                          
031400     03  MED-3                  PIC X(30)                                 
031500         VALUE 'CAMPAIGN                      '.                          
031600     03  MED-4                   PIC X(30)                                
031700         VALUE 'LYNK & CO PART                '.                          
031800     03  MED-5                   PIC X(30)                                
031900         VALUE 'LOCAL PART WRONG SETUP        '.                          
032000     EJECT                                                                
032100                                                                          
032200                                                                          
032300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
032400*                                                                         
032500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
032600     SKIP3                                                                
032700*01  MID -COPY W2I34101                                                   
032800     EJECT                                                                
032900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
033000     SKIP3                                                                
033100*01  -COPY WMSGAREA                                                       
033200     EJECT                                                                
033300     03  MOD REDEFINES MSG-AREA.                                          
033400*      05  -COPY W2O34101                                                 
033500     EJECT                                                                
033600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
033700     SKIP3                                                                
033800*01  -COPY WMFSAREA                                                       
033900     EJECT                                                                
034000*                                                                         
034100*01    -COPY WWBYT03                                                      
034200     EJECT                                                                
034300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
034400*                                                                         
034500     EJECT                                                                
034600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
034700     SKIP3                                                                
034800 01  NYCKLAR-TILL-DLI.                                                    
034900     03  W-IDARTNR-X.                                                     
035000         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
035100                                                                          
035200     03  W-IDDC-X.                                                        
035300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
035400                                                                          
035500     03  W-IDDC-K7-X.                                                     
035600         05  W-IDDC-K7           PIC X(2)    VALUE SPACE.                 
035700                                                                          
035800     03  W-IDDC-B6-X.                                                     
035900         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
036000                                                                          
036100     03  W-IDDC-B616-X.                                                   
036200         05  W-IDDC-B616         PIC X(2)    VALUE SPACE.                 
036300                                                                          
036400     03  W-IDDC-MIN-X.                                                    
036500         05  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                 
036600                                                                          
036700     03  W-IDDC-MAX-X.                                                    
036800         05  W-IDDC-MAX          PIC X(2)    VALUE SPACE.                 
036900                                                                          
037000     03  W-IDSKYLT-X.                                                     
037100         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
037200                                                                          
037300     03  W-KDSEGKEY-X.                                                    
037400         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
037500                                                                          
037600     03  W-KDNOTTYP-X.                                                    
037700         05  W-KDNOTTYP          PIC S9(01)  COMP-3 VALUE ZERO.           
037800                                                                          
037900     03  W-IDLEVNR-X.                                                     
038000         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
038100                                                                          
038200     03  W-IDLAND-X.                                                      
038300         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
038400                                                                          
038500     03  W-IDLEVNR-K7-X.                                                  
038600         05  W-IDLEVNR-K7        PIC X(5)    VALUE SPACE.                 
038700                                                                          
038800     03  W-DAPRLIST-X.                                                    
038900         05  W-DAPRLIST          PIC 9(8)    VALUE ZERO.                  
039000                                                                          
039100     03  W-WDGXKEY-2261-X.                                                
039200          05 W-2261-IDHTYP       PIC X(4)    VALUE '2261'.                
039300          05 W-2261-IDDC         PIC X(2)    VALUE SPACE.                 
039400          05 FILLER              PIC X(24)   VALUE LOW-VALUE.             
039500                                                                          
039600     03  W-WDGXKEY-X.                                                     
039700          05 W-2501-IDHTYP       PIC X(4)    VALUE '2501'.                
039800          05 W-2501-IDDC         PIC X(2)    VALUE SPACE.                 
039900          05 FILLER              PIC X(24)   VALUE LOW-VALUE.             
040000                                                                          
040100     03  W-IDREFTAB-X.                                                    
040200         05  W-2502-IDREFTAB     PIC X(1)    VALUE SPACE.                 
040300                                                                          
040400     03  W-KDSEGKEY-K722-X.                                               
040500         05  W-KDSEGKEY-K722     PIC X(1)    VALUE '1'.                   
040600                                                                          
040700     03  W-WDG3KEY.                                                       
040800         05  W-IDHTYP            PIC  X(4).                               
040900         05  W-NYCKEL-VALFRI     PIC  X(26).                              
041000     SKIP2                                                                
041100*    --- STATUS-KOD FRÅN IMS                                              
041200 01  STATUS-WS                   PIC XX.                                  
041300     88  SEGMENT-FINNS                       VALUE '  '.                  
041400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
041500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
041600     SKIP2                                                                
041700 01  GODK-STATUSKODER.                                                    
041800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
041900     SKIP3                                                                
042000 01  SSA1                        PIC X(64).                               
042100 01  SSA2                        PIC X(64).                               
042200 01  SSA3                        PIC X(64).                               
042300     EJECT                                                                
042400*    --- IMS FUNKTIONSKODER                                               
042500*01  -COPY W0003                                                          
042600     EJECT                                                                
042700                                                                          
042800 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
042900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
043000                                                                          
043100 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
043200 01  DB2-WS.                                                              
043300     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
043400         88  CURSOR-OK                      VALUE 000.                    
043500         88  LINES-FOUND                    VALUE 000.                    
043600         88  LINES-MISSING                  VALUE 100.                    
043700         88  RESOURCE-WRONG                 VALUE 904.                    
043800     03  GOOD-SQLCODECODES.                                               
043900         05  GOOD-SQLCODE OCCURS 5                                        
044000             INDEXED BY SQLCODE-IX PIC 9(3).                              
044100     EJECT                                                                
044200                                                                          
044300*    ---  DLI INPUT-OUTPUT AREA                                           
044400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
044500 01  DLI-IO-WDK601.                                                       
044600*    03  -COPY WDK601                                                     
044700     EJECT                                                                
044800                                                                          
044900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
045000 01  DLI-IO-WDK611.                                                       
045100*    03  -COPY WDK611                                                     
045200     EJECT                                                                
045300                                                                          
045400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK621'.                      
045500 01  DLI-IO-WDK621.                                                       
045600*    03  -COPY WDK621                                                     
045700     EJECT                                                                
045800                                                                          
045900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK625'.                      
046000 01  DLI-IO-WDK625.                                                       
046100*    03  -COPY WDK625                                                     
046200     EJECT                                                                
046300                                                                          
046400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK626'.                      
046500 01  DLI-IO-WDK626.                                                       
046600*    03  -COPY WDK626                                                     
046700     EJECT                                                                
046800                                                                          
046900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK629'.                      
047000 01  DLI-IO-WDK629.                                                       
047100*    03  -COPY WDK629                                                     
047200     EJECT                                                                
047300                                                                          
047400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK61129'.                    
047500 01  DLI-IO-WDK61129.                                                     
047600*    03  -COPY WDK611 -PRE K6-                                            
047700*    03  -COPY WDK629 -PRE K6-                                            
047800     EJECT                                                                
047900                                                                          
048000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK701'.                      
048100 01  DLI-IO-WDK701.                                                       
048200*    03  -COPY WDK701                                                     
048300     EJECT                                                                
048400                                                                          
048500 01  FILLER         PIC X(24) VALUE 'DLI-IO-K711'.                        
048600 01  DLI-IO-K711.                                                         
048700*    03  -COPY WDK711                                                     
048800     EJECT                                                                
048900                                                                          
049000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK712'.                      
049100 01  DLI-IO-WDK712.                                                       
049200*    03  -COPY WDK712                                                     
049300     EJECT                                                                
049400                                                                          
049500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK727'.                      
049600 01  DLI-IO-WDK727.                                                       
049700*    03  -COPY WDK727                                                     
049800     EJECT                                                                
049900                                                                          
050000 01  FILLER         PIC X(24) VALUE 'DLI-IO-OIGA11'.                      
050100 01  DLI-IO-OIGA11.                                                       
050200*    03  -COPY WDL711                                                     
050300     EJECT                                                                
050400                                                                          
050500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDGX2262'.                    
050600 01  DLI-IO-WDGX2262.                                                     
050700*    03  -COPY WDGX2262                                                   
050800     EJECT                                                                
050900                                                                          
051000 01  FILLER         PIC X(24) VALUE 'DLI-IO-BENA01'.                      
051100 01  DLI-IO-BENA01.                                                       
051200*    03  -COPY WDD301                                                     
051300     EJECT                                                                
051400                                                                          
051500 01  FILLER         PIC X(24) VALUE 'DLI-IO-BENA11'.                      
051600 01  DLI-IO-BENA11.                                                       
051700*    03  -COPY WDD311                                                     
051800     EJECT                                                                
051900                                                                          
052000 01  FILLER         PIC X(24) VALUE 'DLI-IO-250101'.                      
052100 01  DLI-IO-250101.                                                       
052200*    03  -COPY WDGX2501                                                   
052300     EJECT                                                                
052400                                                                          
052500 01  FILLER         PIC X(24) VALUE 'DLI-IO-250111'.                      
052600 01  DLI-IO-250111.                                                       
052700*    03  -COPY WDGX2502                                                   
052800     EJECT                                                                
052900                                                                          
053000 01  FILLER         PIC X(24) VALUE 'DLI-IO-LEVA01'.                      
053100 01  DLI-IO-LEVA01.                                                       
053200*    03  -COPY WDF101                                                     
053300     EJECT                                                                
053400                                                                          
053500 01  FILLER         PIC X(24) VALUE 'DLI-IO-LEVA16'.                      
053600 01  DLI-IO-LEVA16.                                                       
053700*    03  -COPY WDF116                                                     
053800     EJECT                                                                
053900                                                                          
054000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
054100 01   DLI-IO-AREA-B601.                                                   
054200*     03  -COPY WDB601                                                    
054300                                                                          
054400 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
054500 01   DLI-IO-AREA-B616.                                                   
054600*     03  -COPY WDB616                                                    
054700     EJECT                                                                
054800                                                                          
054900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711'.             
055000 01  DLI-IO-WDK711.                                                       
055100*    03  -COPY WDK711 -PRE K7-                                            
055200                                                                          
055300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK722'.             
055400 01  DLI-IO-WDK722.                                                       
055500*    03  -COPY WDK722                                                     
055600     EJECT                                                                
055700                                                                          
055800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDG302'.             
055900 01  DLI-IO-WDG302.                                                       
056000*    03  -COPY WDGX2207 -PRE 2207-                                        
056100     EJECT                                                                
056200                                                                          
056300 01  FILLER                      PIC X(16)  VALUE 'TP1KAMP-AREA'.         
056400                                                                          
056500*01  -COPY TP1KAMP -PRE TP1KAMP-                                          
056600     EJECT                                                                
056700 01  FILLER                      PIC X(16)  VALUE 'TP1ARTK-AREA'.         
056800                                                                          
056900*01  -COPY TP1ARTK -PRE TP1ARTK-                                          
057000     EJECT                                                                
057100     EXEC SQL INCLUDE TP1KAMP END-EXEC.                                   
057200     EJECT                                                                
057300     EXEC SQL INCLUDE TP1ARTK END-EXEC.                                   
057400     EJECT                                                                
057500                                                                          
057600 LINKAGE SECTION.                                                         
057700                                                                          
057800*01  -COPY W0009   -PRE MSG-                                              
057900     EJECT                                                                
058000*01  -COPY W0008  -PRE  USEA-                                             
058100     05  FILLER                          PIC X.                           
058200     EJECT                                                                
058300*01  -COPY W0008  -PRE  WDK6-                                             
058400     05  FILLER                          PIC X.                           
058500     EJECT                                                                
058600*01  -COPY W0008  -PRE  WDK7-                                             
058700     05  FILLER                          PIC X.                           
058800     EJECT                                                                
058900*01  -COPY W0008  -PRE  OIGA-                                             
059000     05  FILLER                          PIC X.                           
059100     EJECT                                                                
059200*01  -COPY W0008  -PRE  2261-                                             
059300     05  FILLER                          PIC X.                           
059400     EJECT                                                                
059500*01  -COPY W0008  -PRE  BENA-                                             
059600     05  FILLER                          PIC X.                           
059700     EJECT                                                                
059800*01  -COPY W0008  -PRE  LEVA-                                             
059900     05  FILLER                          PIC X.                           
060000     EJECT                                                                
060100*01  -COPY W0008  -PRE  2501-                                             
060200     05  FILLER                          PIC X.                           
060300     EJECT                                                                
060400*01  -COPY W0008      -PRE WDB6-                                          
060500     05  FILLER                          PIC X.                           
060600     EJECT                                                                
060700*01  -COPY W0008      -PRE WDK72-                                         
060800     05  FILLER                          PIC X.                           
060900     EJECT                                                                
061000*01  -COPY W0008      -PRE WDG3-                                          
061100     05  FILLER                          PIC X.                           
061200     EJECT                                                                
061300*****W271UTIL**********                                                   
061400 01  UTIL-WDK6-PCB                       PIC X.                           
061500 01  UTIL-WDK7-PCB                       PIC X.                           
061600 01  UTIL-WDB6-PCB                       PIC X.                           
061700*****W271REFL**********                                                   
061800 01  REFL1-2501-PCB                      PIC X.                           
061900 01  REFL1-WDB6-PCB                      PIC X.                           
062000 01  REFL1-WDK7-PCB                      PIC X.                           
062100 01  REFL1-UTIL-WDK6-PCB                 PIC X.                           
062200 01  REFL1-UTIL-WDK7-PCB                 PIC X.                           
062300 01  REFL1-UTIL-WDB6-PCB                 PIC X.                           
062400*****W272REFL**********                                                   
062500 01  REFL2-2501-PCB                      PIC X.                           
062600 01  REFL2-WDB6-PCB                      PIC X.                           
062700 01  REFL2-UTIL-WDK6-PCB                 PIC X.                           
062800 01  REFL2-UTIL-WDK7-PCB                 PIC X.                           
062900 01  REFL2-UTIL-WDB6-PCB                 PIC X.                           
063000*****W271UTUP**********                                                   
063100 01  UTUP1-WDK7-PCB                      PIC X.                           
063200 01  UTUP1-WDB6-PCB                      PIC X.                           
063300 01  UTUP1-UTIL-WDK6-PCB                 PIC X.                           
063400 01  UTUP1-UTIL-WDK7-PCB                 PIC X.                           
063500 01  UTUP1-UTIL-WDB6-PCB                 PIC X.                           
063600*****W272UTUP**********                                                   
063700 01  U2-WDK6-PCB                         PIC X.                           
063800 01  U2-WDB6-PCB                         PIC X.                           
063900 01  U2-PBTO-W222-WDK6-PCB               PIC X.                           
064000 01  U2-PBTO-W222-WDK7-PCB               PIC X.                           
064100 01  U2-PBTO-W222-ARTM-PCB               PIC X.                           
064200 01  U2-PBTO-W222-R1-2501-PCB            PIC X.                           
064300 01  U2-PBTO-W222-R1-WDB6R-PCB           PIC X.                           
064400 01  U2-PBTO-W222-R1-WDK7R-PCB           PIC X.                           
064500 01  U2-PBTO-W222-R1-UTIL-WDK6-PCB       PIC X.                           
064600 01  U2-PBTO-W222-R1-UTIL-WDK7-PCB       PIC X.                           
064700 01  U2-PBTO-W222-R1-UTIL-WDB6-PCB       PIC X.                           
064800 01  U2-PBTO-W222-WDB6-PCB               PIC X.                           
064900 01  U2-PBTO-W222-WDD7-PCB               PIC X.                           
065000 01  U2-PBTO-W222-WDK7E-PCB              PIC X.                           
065100 01  U2-PBTO-W222-U1-WDK7-PCB            PIC X.                           
065200 01  U2-PBTO-W222-U1-WDB6-PCB            PIC X.                           
065300 01  U2-PBTO-W222-U1-UTIL-WDK6-PCB       PIC X.                           
065400 01  U2-PBTO-W222-U1-UTIL-WDK7-PCB       PIC X.                           
065500 01  U2-PBTO-W222-U1-UTIL-WDB6-PCB       PIC X.                           
065600 01  U2-REFL2-2501-PCB                   PIC X.                           
065700 01  U2-REFL2-WDB6-PCB                   PIC X.                           
065800 01  U2-REFL2-UTIL-WDK6-PCB              PIC X.                           
065900 01  U2-REFL2-UTIL-WDK7-PCB              PIC X.                           
066000 01  U2-REFL2-UTIL-WDB6-PCB              PIC X.                           
066100 01  U2-W222-WDK6-PCB                    PIC X.                           
066200 01  U2-W222-WDK7-PCB                    PIC X.                           
066300 01  U2-W222-ARTM-PCB                    PIC X.                           
066400 01  U2-W222-2501-PCB                    PIC X.                           
066500 01  U2-W222-WDB6R-PCB                   PIC X.                           
066600 01  U2-W222-WDK7R-PCB                   PIC X.                           
066700 01  U2-W222-WDB6-PCB                    PIC X.                           
066800 01  U2-W222-WDD7-PCB                    PIC X.                           
066900 01  U2-W222-WDK7E-PCB                   PIC X.                           
067000 01  U2-W222-UTIL-WDK6-PCB               PIC X.                           
067100 01  U2-W222-UTIL-WDK7-PCB               PIC X.                           
067200 01  U2-W222-UTIL-WDB6-PCB               PIC X.                           
067300 01  U2-W222-U1-WDK7-PCB                 PIC X.                           
067400 01  U2-W222-U1-WDB6-PCB                 PIC X.                           
067500 01  U2-W222-U1-UTIL-WDK6-PCB            PIC X.                           
067600 01  U2-W222-U1-UTIL-WDK7-PCB            PIC X.                           
067700 01  U2-W222-U1-UTIL-WDB6-PCB            PIC X.                           
067800     EJECT                                                                
067900                                                                          
068000 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
068100                                 WDK6-PCB WDK7-PCB OIGA-PCB               
068200                                 2261-PCB                                 
068300                                 BENA-PCB LEVA-PCB 2501-PCB               
068400                                 WDB6-PCB WDK72-PCB WDG3-PCB              
068500                                 UTIL-WDK6-PCB  UTIL-WDK7-PCB             
068600                                 UTIL-WDB6-PCB                            
068700                                 REFL1-2501-PCB                           
068800                                 REFL1-WDB6-PCB                           
068900                                 REFL1-WDK7-PCB                           
069000                                 REFL1-UTIL-WDK6-PCB                      
069100                                 REFL1-UTIL-WDK7-PCB                      
069200                                 REFL1-UTIL-WDB6-PCB                      
069300                                 REFL2-2501-PCB                           
069400                                 REFL2-WDB6-PCB                           
069500                                 REFL2-UTIL-WDK6-PCB                      
069600                                 REFL2-UTIL-WDK7-PCB                      
069700                                 REFL2-UTIL-WDB6-PCB                      
069800                                 UTUP1-WDK7-PCB                           
069900                                 UTUP1-WDB6-PCB                           
070000                                 UTUP1-UTIL-WDK6-PCB                      
070100                                 UTUP1-UTIL-WDK7-PCB                      
070200                                 UTUP1-UTIL-WDB6-PCB                      
070300                                 U2-WDK6-PCB                              
070400                                 U2-WDB6-PCB                              
070500                                 U2-PBTO-W222-WDK6-PCB                    
070600                                 U2-PBTO-W222-WDK7-PCB                    
070700                                 U2-PBTO-W222-ARTM-PCB                    
070800                                 U2-PBTO-W222-R1-2501-PCB                 
070900                                 U2-PBTO-W222-R1-WDB6R-PCB                
071000                                 U2-PBTO-W222-R1-WDK7R-PCB                
071100                                 U2-PBTO-W222-R1-UTIL-WDK6-PCB            
071200                                 U2-PBTO-W222-R1-UTIL-WDK7-PCB            
071300                                 U2-PBTO-W222-R1-UTIL-WDB6-PCB            
071400                                 U2-PBTO-W222-WDB6-PCB                    
071500                                 U2-PBTO-W222-WDD7-PCB                    
071600                                 U2-PBTO-W222-WDK7E-PCB                   
071700                                 U2-PBTO-W222-U1-WDK7-PCB                 
071800                                 U2-PBTO-W222-U1-WDB6-PCB                 
071900                                 U2-PBTO-W222-U1-UTIL-WDK6-PCB            
072000                                 U2-PBTO-W222-U1-UTIL-WDK7-PCB            
072100                                 U2-PBTO-W222-U1-UTIL-WDB6-PCB            
072200                                 U2-REFL2-2501-PCB                        
072300                                 U2-REFL2-WDB6-PCB                        
072400                                 U2-REFL2-UTIL-WDK6-PCB                   
072500                                 U2-REFL2-UTIL-WDK7-PCB                   
072600                                 U2-REFL2-UTIL-WDB6-PCB                   
072700                                 U2-W222-WDK6-PCB                         
072800                                 U2-W222-WDK7-PCB                         
072900                                 U2-W222-ARTM-PCB                         
073000                                 U2-W222-2501-PCB                         
073100                                 U2-W222-WDB6R-PCB                        
073200                                 U2-W222-WDK7R-PCB                        
073300                                 U2-W222-WDB6-PCB                         
073400                                 U2-W222-WDD7-PCB                         
073500                                 U2-W222-WDK7E-PCB                        
073600                                 U2-W222-UTIL-WDK6-PCB                    
073700                                 U2-W222-UTIL-WDK7-PCB                    
073800                                 U2-W222-UTIL-WDB6-PCB                    
073900                                 U2-W222-U1-WDK7-PCB                      
074000                                 U2-W222-U1-WDB6-PCB                      
074100                                 U2-W222-U1-UTIL-WDK6-PCB                 
074200                                 U2-W222-U1-UTIL-WDK7-PCB                 
074300                                 U2-W222-U1-UTIL-WDB6-PCB.                
074400     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
074500                                 WDK6-PCB WDK7-PCB OIGA-PCB               
074600                                 2261-PCB                                 
074700                                 BENA-PCB LEVA-PCB 2501-PCB               
074800                                 WDB6-PCB WDK72-PCB WDG3-PCB              
074900                                 UTIL-WDK6-PCB  UTIL-WDK7-PCB             
075000                                 UTIL-WDB6-PCB                            
075100                                 UTUP1-WDK7-PCB                           
075200                                 UTUP1-WDB6-PCB                           
075300                                 UTUP1-UTIL-WDK6-PCB                      
075400                                 UTUP1-UTIL-WDK7-PCB                      
075500                                 UTUP1-UTIL-WDB6-PCB                      
075600                                 U2-WDK6-PCB                              
075700                                 U2-WDB6-PCB                              
075800                                 U2-PBTO-W222-WDK6-PCB                    
075900                                 U2-PBTO-W222-WDK7-PCB                    
076000                                 U2-PBTO-W222-ARTM-PCB                    
076100                                 U2-PBTO-W222-R1-2501-PCB                 
076200                                 U2-PBTO-W222-R1-WDB6R-PCB                
076300                                 U2-PBTO-W222-R1-WDK7R-PCB                
076400                                 U2-PBTO-W222-R1-UTIL-WDK6-PCB            
076500                                 U2-PBTO-W222-R1-UTIL-WDK7-PCB            
076600                                 U2-PBTO-W222-R1-UTIL-WDB6-PCB            
076700                                 U2-PBTO-W222-WDB6-PCB                    
076800                                 U2-PBTO-W222-WDD7-PCB                    
076900                                 U2-PBTO-W222-WDK7E-PCB                   
077000                                 U2-PBTO-W222-U1-WDK7-PCB                 
077100                                 U2-PBTO-W222-U1-WDB6-PCB                 
077200                                 U2-PBTO-W222-U1-UTIL-WDK6-PCB            
077300                                 U2-PBTO-W222-U1-UTIL-WDK7-PCB            
077400                                 U2-PBTO-W222-U1-UTIL-WDB6-PCB            
077500                                 U2-REFL2-2501-PCB                        
077600                                 U2-REFL2-WDB6-PCB                        
077700                                 U2-REFL2-UTIL-WDK6-PCB                   
077800                                 U2-REFL2-UTIL-WDK7-PCB                   
077900                                 U2-REFL2-UTIL-WDB6-PCB                   
078000                                 U2-W222-WDK6-PCB                         
078100                                 U2-W222-WDK7-PCB                         
078200                                 U2-W222-ARTM-PCB                         
078300                                 U2-W222-2501-PCB                         
078400                                 U2-W222-WDB6R-PCB                        
078500                                 U2-W222-WDK7R-PCB                        
078600                                 U2-W222-WDB6-PCB                         
078700                                 U2-W222-WDD7-PCB                         
078800                                 U2-W222-WDK7E-PCB                        
078900                                 U2-W222-UTIL-WDK6-PCB                    
079000                                 U2-W222-UTIL-WDK7-PCB                    
079100                                 U2-W222-UTIL-WDB6-PCB                    
079200                                 U2-W222-U1-WDK7-PCB                      
079300                                 U2-W222-U1-WDB6-PCB                      
079400                                 U2-W222-U1-UTIL-WDK6-PCB                 
079500                                 U2-W222-U1-UTIL-WDK7-PCB                 
079600                                 U2-W222-U1-UTIL-WDB6-PCB.                
079700                                                                          
079800     PERFORM IMS-GET-MSG                                                  
079900     IF SEGMENT-FINNS                                                     
080000       PERFORM A-INIT                                                     
080100       PERFORM B-KOLLA-NYCKLAR                                            
080200       IF NYCKLAR-OK                                                      
080300         IF MFS-UPDATE                                                    
080400           PERFORM G-KOLLA-INPUT                                          
080500           IF INDATA-OK                                                   
080600             IF  CDC-SE                                                   
080700             AND (WDK6-UPPD OR WDK626-UPPD)                               
080800               PERFORM I-UPPDATERA-CDC-REFILL                             
080900             ELSE                                                         
081000               PERFORM H-UPPDATERA                                        
081100             END-IF                                                       
081200           END-IF                                                         
081300         ELSE                                                             
081400           IF MFS-FIRST                                                   
081500             PERFORM C-FOERSTA-SIDA                                       
081600           ELSE                                                           
081700             PERFORM E-SAMMA-SIDA                                         
081800           END-IF                                                         
081900         END-IF                                                           
082000         IF INDATA-OK                                                     
082100           PERFORM F-LAES-VISA-INFO                                       
082200         END-IF                                                           
082300       END-IF                                                             
082400       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O34101 + 4                      
082500       PERFORM IMS-INSERT-MSG                                             
082600     END-IF                                                               
082700                                                                          
082800     MOVE ZERO TO RETURN-CODE                                             
082900     GOBACK                                                               
083000     .                                                                    
083100     EJECT                                                                
083200 A-INIT SECTION.                                                          
083300                                                                          
083400     IF MSG-DUBBLA-TRANSKODER                                             
083500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I34101                 
083600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
083700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
083800     ELSE                                                                 
083900       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W2I34101                   
084000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
084100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
084200     END-IF                                                               
084300                                                                          
084400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
084500     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
084600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
084700                                                                          
084800     MOVE LOW-VALUE  TO MSG-AREA                                          
084900     MOVE 'W2O341N1' TO MFS-IDMOD                                         
085000     MOVE '2341'     TO MOD-IDTRANS                                       
085100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
085200                                                                          
085300     IF EGEN-MID OR HELP-MID                                              
085400       CONTINUE                                                           
085500     ELSE                                                                 
085600       MOVE SPACE TO MFS-KDTRTYP                                          
085700       MOVE '7'   TO MFS-IDPFK                                            
085800     END-IF                                                               
085900                                                                          
086000     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM-SEKEL                
086100                                                                          
086200     ACCEPT DAGENS-DATUM FROM DATE                                        
086300                                                                          
086400     MOVE DAGENS-DATUM TO DAG-TIAAMMDD-FOM                                
086500     MOVE +182         TO DAG-KVKALDAG                                    
086600     MOVE 002          TO DAG-KDCALL                                      
086700     CALL WDAGKONV USING  DAG-KDCALL                                      
086800                          DAG-DATUM-AREA                                  
086900                          DAG-KDSVAR                                      
087000     IF DAG-KDSVAR = SPACE                                                
087100        MOVE DAG-TIAAMMDD-TOM TO DD-PLUS-SEX-MAN                          
087200     ELSE                                                                 
087300        MOVE 'FEL FRÅN WDAGKONV 1  I A-INIT SECTION I W20341' TO          
087400                                    FELTEXT                               
087500        PERFORM S99-ABEND                                                 
087600     END-IF                                                               
087700                                                                          
087800     MOVE 'IDAG  '            TO DAT-KDDATFORM                            
087900     CALL WDATKONV USING         DAT-KDDATFORM                            
088000                                 DAT-I-TIDATUM                            
088100                                 DAT-O-TIDATUM                            
088200                                 DAT-KDSVAR                               
088300     IF DAT-KDSVAR-OK                                                     
088400        MOVE DAT-TIAAVV-GRP     TO DAGENS-AAVV                            
088500     END-IF                                                               
088600                                                                          
088700     .                                                                    
088800     EJECT                                                                
088900 B-KOLLA-NYCKLAR SECTION.                                                 
089000                                                                          
089100     MOVE JA TO NYCKLAR-SW                                                
089200     MOVE SPACES            TO WS-IDDC-REF-SPAR                           
089300                                                                          
089400*      -- KONTROLL AV IDARTNR                                             
089500                                                                          
089600     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
089700                                                                          
089800     MOVE ALL '+' TO MSGI-WMSGINIT                                        
089900     MOVE '001'             TO MSGI-KDCALL                                
090000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
090100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
090200     MOVE '2341'            TO MSGI-IDTRANS                               
090300     IF EGEN-MID                                                          
090400       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
090500       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
090600     ELSE                                                                 
090700       IF  MID-IDARTNR-IN NUMERIC                                         
090800       AND MID-IDARTNR-IN > ZERO                                          
090900         MOVE MID-IDARTNR-IN                                              
091000                            TO MSGI-IDARTNR                               
091100       END-IF                                                             
091200     END-IF                                                               
091300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
091400                                                                          
091500*    -- KONTROLL AV SPRÅKKOD                                              
091600     IF MSGI-IDLAND-SPR = 'SE'                                            
091700       MOVE +1    TO SPRAK-IX                                             
091800       MOVE 'S  ' TO MED-IDSKYLT                                          
091900     ELSE                                                                 
092000       MOVE +2    TO SPRAK-IX                                             
092100       MOVE 'GB ' TO MED-IDSKYLT                                          
092200     END-IF                                                               
092300                                                                          
092400     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
092500     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
092600                                                                          
092700     IF MID-IDARTNR-IN = ALL '+'                                          
092800       CONTINUE                                                           
092900     ELSE                                                                 
093000       MOVE '7'         TO MFS-IDPFK                                      
093100       MOVE SPACE       TO MFS-KDTRTYP                                    
093200     END-IF                                                               
093300                                                                          
093400     IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                          
093500       CONTINUE                                                           
093600     ELSE                                                                 
093700       MOVE NEJ TO NYCKLAR-SW                                             
093800     END-IF                                                               
093900                                                                          
094000*  -- KONTROLL AV IDDC                                                    
094100                                                                          
094200     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
094300                                                                          
094400     IF NYCKLAR-OK                                                        
094500       IF MID-IDDC-IN NOT = ALL '+'                                       
094600         MOVE '7'            TO MFS-IDPFK                                 
094700         MOVE SPACE          TO MFS-KDTRTYP                               
094800*        MOVE MID-IDDC-IN    TO MSGI-IDDC-KEY                             
094900       END-IF                                                             
095000                                                                          
095100       MOVE MSGI-IDDC-KEY  TO W-IDDC-B6                                   
095200                                WS-IDDC                                   
095300       PERFORM IMS-GU-WDB601                                              
095400       IF DCS-KDDC > SPACE AND                                            
095500          NOT DCS-DDC                                                     
095600         MOVE MSGI-IDDC-KEY TO MOD-IDDC-UT                                
095700                                 W-IDDC                                   
095800                                 W-IDDC-K7                                
095900         MOVE WS-IDARTNR    TO W-IDARTNR                                  
096000         PERFORM BA-GET-REFILL-DISTRICT                                   
096100                                                                          
096200       ELSE                                                               
096300         MOVE NEJ            TO NYCKLAR-SW                                
096400         MOVE SPACES         TO MOD-IDDC-UT                               
096500       END-IF                                                             
096600       MOVE WS-IDARTNR     TO W-IDARTNR                                   
096700                                                                          
096800     END-IF                                                               
096900***LYNK PARTS ONLY IN EUROPE                                              
097000     MOVE NEJ     TO SW-LYNK-PART                                         
097100     PERFORM IMS-GU-WDK601                                                
097200     IF SEGMENT-FINNS                                                     
097310       IF  ART-KDPRODSL > 30                                              
097320       AND ART-KDPRODSL < 40                                              
097400       AND NDC                                                            
097500         MOVE NEJ          TO NYCKLAR-SW                                  
097600         MOVE JA           TO SW-LYNK-PART                                
097700       END-IF                                                             
097800     END-IF                                                               
097900                                                                          
098000     MOVE WS-IDARTNR       TO MOD-IDARTNR-UT                              
098100                                                                          
098200     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
098300     MOVE WS-IDDISTR       TO MOD-IDDISTR                                 
098400*                                                                         
098500     IF NYCKLAR-FEL                                                       
098600       IF LYNK-PART                                                       
098700         MOVE MED-4           TO MOD-TEMFSFEL                             
098800       ELSE                                                               
098900         MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                             
099000         CALL WMEDKONV USING MED-WMEDAREA                                 
099100         MOVE MED-MFSFEL      TO MOD-TEMFSFEL                             
099200       END-IF                                                             
099300       PERFORM MFS-RENSA-FAELT-UT                                         
099400     END-IF                                                               
099500     .                                                                    
099600     EJECT                                                                
099700 BA-GET-REFILL-DISTRICT SECTION.                                          
099800                                                                          
099900     IF CDC-SE                                                            
100000        PERFORM IMS-GU-WDK611                                             
100100        IF SEGMENT-FINNS                                                  
100200        AND CLAG-IDDC-REF NOT = SPACES                                    
100300           MOVE CLAG-IDDC-REF         TO W-IDDC-B616                      
100400           PERFORM IMS-GU-WDB616                                          
100500           IF SEGMENT-FINNS                                               
100600              MOVE REF-IDDISTR-REFILL TO WS-IDDISTR                       
100700           END-IF                                                         
100800        END-IF                                                            
100900     ELSE                                                                 
101000       PERFORM IMS-GU-WDK7-WDK711                                         
101100       IF SEGMENT-FINNS                                                   
101200          MOVE SLAG-IDDC-REF            TO WS-IDDC-REF-SPAR               
101300          IF SLAG-IDDC-REF NOT = SPACES                                   
101400             MOVE SLAG-IDDC-REF         TO W-IDDC-B616                    
101500             PERFORM IMS-GU-WDB616                                        
101600             IF SEGMENT-FINNS                                             
101700                MOVE REF-IDDISTR-REFILL TO WS-IDDISTR                     
101800             END-IF                                                       
101900          END-IF                                                          
102000       END-IF                                                             
102100     END-IF                                                               
102200     .                                                                    
102300     EJECT                                                                
102400 C-FOERSTA-SIDA SECTION.                                                  
102500                                                                          
102600     PERFORM MFS-RENSA-FAELT-IN                                           
102700     .                                                                    
102800     EJECT                                                                
102900 E-SAMMA-SIDA SECTION.                                                    
103000                                                                          
103100     IF MID-INPUT = ALL '+'                                               
103200       PERFORM MFS-RENSA-FAELT-IN                                         
103300     ELSE                                                                 
103400       IF EGEN-MID OR HELP-MID                                            
103500         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
103600         CALL WMEDKONV USING MED-WMEDAREA                                 
103700         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
103800         MOVE JA         TO SW-MESSAGE                                    
103900         PERFORM MFS-LAES-IN-IGEN                                         
104000                                                                          
104100         PERFORM EA-MID-INDATA-TILL-MOD                                   
104200       ELSE                                                               
104300         PERFORM MFS-RENSA-FAELT-IN                                       
104400       END-IF                                                             
104500     END-IF                                                               
104600     .                                                                    
104700 EA-MID-INDATA-TILL-MOD SECTION.                                          
104800* * * * * FÖR VARJE MID-FÄLT                                              
104900* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
105000* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
105100                                                                          
105200     IF MID-IDREFTAB = ALL '+'                                            
105300       MOVE MFS-RENSA-FAELT TO MOD-IDREFTAB-IN                            
105400     ELSE                                                                 
105500       MOVE MID-IDREFTAB    TO MOD-IDREFTAB-IN                            
105600     END-IF                                                               
105700                                                                          
105800     IF MID-FLWILSON = ALL '+'                                            
105900       MOVE MFS-RENSA-FAELT TO MOD-FLWILSON-IN                            
106000     ELSE                                                                 
106100       MOVE MID-FLWILSON    TO MOD-FLWILSON-IN                            
106200     END-IF                                                               
106300                                                                          
106400     IF MID-FLREFILL = ALL '+'                                            
106500       MOVE MFS-RENSA-FAELT TO MOD-FLREFILL-IN                            
106600     ELSE                                                                 
106700       MOVE MID-FLREFILL    TO MOD-FLREFILL-IN                            
106800     END-IF                                                               
106900                                                                          
107000     IF MID-FLREFBEO = ALL '+'                                            
107100       MOVE MFS-RENSA-FAELT TO MOD-FLREFBEO-IN                            
107200     ELSE                                                                 
107300       MOVE MID-FLREFBEO    TO MOD-FLREFBEO-IN                            
107400     END-IF                                                               
107500                                                                          
107600     IF MID-TIREFSTO  = ALL '+'                                           
107700       MOVE MFS-RENSA-FAELT TO MOD-TIREFSTO-IN                            
107800     ELSE                                                                 
107900       MOVE MID-TIREFSTO    TO MOD-TIREFSTO-IN                            
108000     END-IF                                                               
108100                                                                          
108200     IF MID-KVPB-JUST-1 = ALL '+'                                         
108300       MOVE MFS-RENSA-FAELT TO MOD-IN-KVPB-JUST-1                         
108400     ELSE                                                                 
108500       MOVE MID-KVPB-JUST-1 TO MOD-IN-KVPB-JUST-1                         
108600       INSPECT MOD-IN-KVPB-JUST-1                                         
108700               REPLACING LEADING ZERO BY SPACE                            
108800     END-IF                                                               
108900                                                                          
109000     IF MID-TIPBJUST-1 = ALL '+'                                          
109100       MOVE MFS-RENSA-FAELT TO MOD-IN-TIPBJUST-1                          
109200     ELSE                                                                 
109300       MOVE MID-TIPBJUST-1  TO MOD-IN-TIPBJUST-1                          
109400     END-IF                                                               
109500                                                                          
109600     IF MID-KVPB-JUST-2 = ALL '+'                                         
109700       MOVE MFS-RENSA-FAELT TO MOD-IN-KVPB-JUST-2                         
109800     ELSE                                                                 
109900       MOVE MID-KVPB-JUST-2 TO MOD-IN-KVPB-JUST-2                         
110000       INSPECT MOD-IN-KVPB-JUST-2                                         
110100               REPLACING LEADING ZERO BY SPACE                            
110200     END-IF                                                               
110300                                                                          
110400     IF MID-TIPBJUST-2 = ALL '+'                                          
110500       MOVE MFS-RENSA-FAELT TO MOD-IN-TIPBJUST-2                          
110600     ELSE                                                                 
110700       MOVE MID-TIPBJUST-2  TO MOD-IN-TIPBJUST-2                          
110800     END-IF                                                               
110900                                                                          
111000     IF MID-KVREFPKT = ALL '+'                                            
111100       MOVE MFS-RENSA-FAELT TO MOD-KVREFPKT-IN                            
111200     ELSE                                                                 
111300       MOVE MID-KVREFPKT    TO MOD-KVREFPKT-IN                            
111400       INSPECT MOD-KVREFPKT-IN REPLACING LEADING SPACE BY ZERO            
111500     END-IF                                                               
111600                                                                          
111700     IF MID-TIREFPKT = ALL '+'                                            
111800       MOVE MFS-RENSA-FAELT TO MOD-TIREFPKT-IN                            
111900     ELSE                                                                 
112000       MOVE MID-TIREFPKT    TO MOD-TIREFPKT-IN                            
112100     END-IF                                                               
112200                                                                          
112300     IF MID-KVREFBER = ALL '+'                                            
112400       MOVE MFS-RENSA-FAELT TO MOD-KVREFBER-IN                            
112500     ELSE                                                                 
112600       MOVE MID-KVREFBER    TO MOD-KVREFBER-IN                            
112700       INSPECT MOD-KVREFBER-IN REPLACING LEADING SPACE BY ZERO            
112800     END-IF                                                               
112900                                                                          
113000     IF MID-TIREFPAF = ALL '+'                                            
113100       MOVE MFS-RENSA-FAELT TO MOD-TIREFPAF-IN                            
113200     ELSE                                                                 
113300       MOVE MID-TIREFPAF    TO MOD-TIREFPAF-IN                            
113400     END-IF                                                               
113500                                                                          
113600     IF MID-IDPERSON-BUY = ALL '+'                                        
113700       MOVE MFS-RENSA-FAELT  TO MOD-IDPERSON-BUY-IN                       
113800     ELSE                                                                 
113900       MOVE MID-IDPERSON-BUY TO MOD-IDPERSON-BUY-IN                       
114000     END-IF                                                               
114100                                                                          
114200     IF MID-FLBUYUPD = ALL '+'                                            
114300       MOVE MFS-RENSA-FAELT  TO MOD-FLBUYUPD-IN                           
114400     ELSE                                                                 
114500       MOVE MID-FLBUYUPD     TO MOD-FLBUYUPD-IN                           
114600     END-IF                                                               
114700                                                                          
114800     IF MID-FLTABUPD = ALL '+'                                            
114900       MOVE MFS-RENSA-FAELT  TO MOD-FLTABUPD-IN                           
115000     ELSE                                                                 
115100       MOVE MID-FLTABUPD     TO MOD-FLTABUPD-IN                           
115200     END-IF                                                               
115300                                                                          
115400     IF MID-FLPB-FLYTT = ALL '+'                                          
115500       MOVE MFS-RENSA-FAELT  TO MOD-FLPB-FLYTT-IN                         
115600     ELSE                                                                 
115700       MOVE MID-FLPB-FLYTT   TO MOD-FLPB-FLYTT-IN                         
115800     END-IF                                                               
115900                                                                          
116000     IF MID-FLREFNYO = ALL '+'                                            
116100       MOVE MFS-RENSA-FAELT  TO MOD-FLREFNYO-IN                           
116200     ELSE                                                                 
116300       MOVE MID-FLREFNYO     TO MOD-FLREFNYO-IN                           
116400     END-IF                                                               
116500     .                                                                    
116600     EJECT                                                                
116700 F-LAES-VISA-INFO SECTION.                                                
116800                                                                          
116900     MOVE ZERO TO WS-LEADTID-BEHOV                                        
117000     IF CDC-SE                                                            
117100        PERFORM IMS-GU-WDK611                                             
117200        IF SEGMENT-FINNS                                                  
117300           IF CLAG-IDDC-REF    = SPACES                                   
117400              MOVE NEJ        TO INDATA-SW                                
117500           ELSE                                                           
117600              MOVE JA         TO INDATA-SW                                
117700              MOVE CLAG-IDDC-REF                                          
117800                              TO WS-IDDC-REF                              
117900           END-IF                                                         
118000        END-IF                                                            
118100        MOVE WS-PB-TEXT-CDC   TO MOD-PB-TEXT                              
118200     ELSE                                                                 
118300       IF NDC-CN OR NDC-US                                                
118400         PERFORM IMS-GU-WDK711                                            
118500         IF SEGMENT-FINNS                                                 
118600            IF K7-SLAG-IDDC-REF = SPACES                                  
118700               MOVE NEJ        TO INDATA-SW                               
118800            ELSE                                                          
118900               MOVE JA         TO INDATA-SW                               
119000               MOVE K7-SLAG-IDDC-REF TO WS-IDDC-REF                       
119100            END-IF                                                        
119200         END-IF                                                           
119300       ELSE                                                               
119400         IF  (WS-IDDC-REF-SPAR  = SPACES                                  
119500         AND  DCS-FLLPO         = NEJ)                                    
119600*CC*     OR  (WS-IDDC-REF-SPAR  > SPACES                                  
119700*CC*     AND  DCS-FLLPO         = JA )                                    
119800             MOVE NEJ          TO INDATA-SW                               
119900             MOVE MED-5        TO MOD-TEMFSFEL                            
120000             PERFORM MFS-RENSA-FAELT-UT                                   
120100         ELSE                                                             
120200            MOVE JA            TO INDATA-SW                               
120300         END-IF                                                           
120400       END-IF                                                             
120500       MOVE WS-PB-TEXT-XDC     TO MOD-PB-TEXT                             
120600     END-IF                                                               
120700*                                                                         
120800     IF INDATA-OK AND CDC-SE                                              
120900        PERFORM FE-GET-CDC-INFO                                           
121000     ELSE                                                                 
121100       IF INDATA-OK                                                       
121200         PERFORM FA-LAES-GRUNDDATA                                        
121300                                                                          
121400         IF SEGMENT-FINNS                                                 
121500           PERFORM S02-GET-BESPRIS                                        
121600           PERFORM FB-HAEMTA-BENAEMNING                                   
121700           MOVE MSGI-IDDC-KEY    TO W-IDDC                                
121800           PERFORM IMS-GHU-WDL7-OIGA11                                    
121900           IF SEGMENT-FINNS                                               
122000             IF DC-TIREFEFT > ZERO                                        
122100               MOVE DC-TIREFEFT          TO MOD-TIREFEFT                  
122200             ELSE                                                         
122300               MOVE MFS-RENSA-FAELT      TO MOD-TIREFEFT                  
122400             END-IF                                                       
122500             PERFORM FC-VISA-SDC-LAGERINFO                                
122600           ELSE                                                           
122700             PERFORM FC-VISA-SDC-LAGERINFO                                
122800           END-IF                                                         
122900           MOVE W-IDDC          TO W-2261-IDDC                            
123000           PERFORM IMS-GHU-WDGX2262                                       
123100           IF SEGMENT-FINNS                                               
123200             MOVE 2262-TEREFMED          TO MOD-TEREFMED                  
123300           ELSE                                                           
123400             MOVE MFS-RENSA-FAELT        TO MOD-TEREFMED                  
123500           END-IF                                                         
123600           MOVE MFS-ADD-LAES-IN-FAELT                                     
123700                             TO MOD-TEREFMED-ATTR                         
123800         ELSE                                                             
123900           MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                            
124000           CALL WMEDKONV    USING MED-WMEDAREA                            
124100           MOVE MED-MFSFEL     TO MOD-TEMFSFEL                            
124200           PERFORM MFS-RENSA-FAELT-UT                                     
124300         END-IF                                                           
124400       ELSE                                                               
124500           PERFORM MFS-RENSA-FAELT-UT                                     
124600           MOVE INF-NOT-REFILL TO MED-IDMFSINF                            
124700           CALL WMEDKONV    USING MED-WMEDAREA                            
124800           MOVE MED-MFSINF     TO MOD-TEMFSINF                            
124900           MOVE JA             TO SW-MESSAGE                              
125000       END-IF                                                             
125100     END-IF                                                               
125200     .                                                                    
125300     EJECT                                                                
125400 FA-LAES-GRUNDDATA SECTION.                                               
125500                                                                          
125600     PERFORM IMS-GU-WDK601                                                
125700     IF SEGMENT-FINNS                                                     
125800       PERFORM IMS-GNP-WDK611                                             
125900       IF SEGMENT-FINNS                                                   
126000         IF NDC-CN OR NDC-US                                              
126100            PERFORM FAA-MOVE-CN-IDANSK                                    
126200         ELSE                                                             
126300            MOVE CLAG-IDANSK  TO MOD-IDANSK                               
126400         END-IF                                                           
126500         MOVE CLAG-KDERS      TO MOD-KDERS                                
126600         IF NDC-CN OR NDC-NA                                              
126700           IF NDC-CN                                                      
126800             MOVE 'CN'   TO W-IDLAND                                      
126900           END-IF                                                         
127000           IF NDC-US                                                      
127100             MOVE 'US'   TO W-IDLAND                                      
127200           END-IF                                                         
127300           IF NDC-CA                                                      
127400             MOVE 'CA'   TO W-IDLAND                                      
127500           END-IF                                                         
127600           PERFORM IMS-GU-WDK712                                          
127700           IF SEGMENT-FINNS                                               
127800             MOVE WS-MTRL-TEXT TO MOD-PRICE-TEXT                          
127900             MOVE LART-PRMATRL TO MOD-PRARTSTD                            
128000                                    WS-PRMATRL                            
128100           ELSE                                                           
128200             MOVE WS-MTRL-TEXT TO MOD-PRICE-TEXT                          
128300             MOVE ZERO         TO MOD-PRARTSTD                            
128400                                    WS-PRMATRL                            
128500             MOVE 'NO MATERIALPRICE' TO MOD-TEMFSINF                      
128600           END-IF                                                         
128700         ELSE                                                             
128800           MOVE WS-STD-TEXT    TO MOD-PRICE-TEXT                          
128900           MOVE CLAG-PRARTSTD TO MOD-PRARTSTD                             
129000                                  WS-PRARTSTD                             
129100                                  WS-PRARTBES                             
129200         END-IF                                                           
129300                                                                          
129400         IF CLAG-KDERS > 0                                                
129500           IF CLAG-KDERS = +29 OR +52                                     
129600             MOVE ARTIKEL-UTGANGEN TO MED-IDMFSFEL                        
129700             CALL WMEDKONV USING MED-WMEDAREA                             
129800             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
129900           ELSE                                                           
130000             MOVE ARTIKEL-ERSATT TO MED-IDMFSFEL                          
130100             CALL WMEDKONV USING MED-WMEDAREA                             
130200             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
130300           END-IF                                                         
130400         END-IF                                                           
130500       ELSE                                                               
130600         MOVE MFS-RENSA-FAELT       TO MOD-BEART                          
130700                                       MOD-IDANSK                         
130800                                       MOD-KDERS                          
130900                                       MOD-KLASS                          
131000                                       MOD-REPPFAKT                       
131100                                       MOD-FLCDART                        
131200                                       MOD-ADLAGOMR                       
131300                                       MOD-ADGANG                         
131400                                       MOD-ADPLATS                        
131500                                       MOD-PRICE-TEXT                     
131600                                       MOD-PRARTSTD                       
131700       END-IF                                                             
131800     ELSE                                                                 
131900       MOVE MFS-RENSA-FAELT       TO MOD-BEART                            
132000                                     MOD-IDANSK                           
132100                                     MOD-KDERS                            
132200                                     MOD-KLASS                            
132300                                     MOD-REPPFAKT                         
132400                                     MOD-FLCDART                          
132500                                     MOD-ADLAGOMR                         
132600                                     MOD-ADGANG                           
132700                                     MOD-ADPLATS                          
132800                                     MOD-PRICE-TEXT                       
132900                                     MOD-PRARTSTD                         
133000     END-IF                                                               
133100                                                                          
133200     .                                                                    
133300     EJECT                                                                
133400 FAA-MOVE-CN-IDANSK SECTION.                                              
133500                                                                          
133600     IF WS-IDDC-REF = WC-CDC-SE                                           
133700        MOVE CLAG-IDANSK        TO MOD-IDANSK                             
133800     ELSE                                                                 
133900        MOVE WS-IDDC-REF        TO W-IDDC                                 
134000        PERFORM IMS-GU-WDK722                                             
134100        IF SEGMENT-FINNS                                                  
134200           MOVE XLAG-IDANSK     TO MOD-IDANSK                             
134300        ELSE                                                              
134400           MOVE MFS-RENSA-FAELT TO MOD-IDANSK                             
134500        END-IF                                                            
134600     END-IF                                                               
134700     .                                                                    
134800     EJECT                                                                
134900 FB-HAEMTA-BENAEMNING SECTION.                                            
135000                                                                          
135100     PERFORM IMS-GU-BENA01-BSEQ                                           
135200     IF SEGMENT-FINNS                                                     
135300       MOVE MED-IDSKYLT TO W-IDSKYLT                                      
135400       PERFORM IMS-GNP-BENA11                                             
135500       IF SEGMENT-FINNS                                                   
135600         MOVE TEXT-BEART        TO MOD-BEART                              
135700       ELSE                                                               
135800         MOVE MFS-RENSA-FAELT   TO MOD-BEART                              
135900       END-IF                                                             
136000     END-IF                                                               
136100     .                                                                    
136200     EJECT                                                                
136300 FC-VISA-SDC-LAGERINFO SECTION.                                           
136400                                                                          
136500     MOVE MSGI-IDDC-KEY       TO W-IDDC                                   
136600     PERFORM IMS-GU-WDK7-WDK711                                           
136700     IF SEGMENT-FINNS                                                     
136800        MOVE SLAG-IDREFTAB    TO MOD-IDREFTAB                             
136900        MOVE SLAG-FLWILSON    TO MOD-FLWILSON                             
137000        MOVE SLAG-FLREFILL    TO MOD-FLREFILL                             
137100        MOVE SLAG-FLREFNYO    TO MOD-FLREFNYO                             
137200        COMPUTE WS-BALANCE = SLAG-KVLS       -                            
137300                             SLAG-KVOKS-DAG  -                            
137400                             SLAG-KVOKS-BULK -                            
137500                             SLAG-KVROS-DAG  -                            
137600                             SLAG-KVROS-BULK -                            
137700                             SLAG-KVSPARR-KVAL                            
137800        MOVE WS-BALANCE       TO MOD-KVDISP                               
137900        MOVE SLAG-KVAKS-PAV   TO MOD-KVAKS-PAV                            
138000        MOVE SLAG-KVAKS-SDC   TO MOD-KVAKS-SDC                            
138100        MOVE SLAG-KVBEART     TO MOD-KVBEART                              
138200        MOVE SLAG-FLREFBEO    TO MOD-FLREFBEO                             
138300        MOVE SLAG-KDREFSTA    TO MOD-KDREFSTA                             
138400        MOVE SLAG-KVPB-REF    TO MOD-KVPB-REF                             
138500        MOVE SLAG-KVPBREOI    TO MOD-KVPBREOI                             
138600        COMPUTE WS-KVPB-SUM = SLAG-KVPB-REF +                             
138700                              SLAG-KVPBREOI                               
138800        MOVE WS-KVPB-SUM      TO MOD-KVPB-SUM                             
138900        MOVE SLAG-KVREFPKT    TO MOD-KVREFPKT                             
139000        MOVE SLAG-KVREFBER    TO MOD-KVREFBER                             
139100        MOVE SLAG-KVREFOVL    TO MOD-KVREFOVL                             
139200        MOVE SLAG-ADLAGOMR    TO MOD-ADLAGOMR                             
139300        MOVE SLAG-ADGANG      TO MOD-ADGANG                               
139400        MOVE SLAG-ADPLATS     TO MOD-ADPLATS                              
139500        MOVE SLAG-FLPB-FLYTT  TO MOD-FLPB-FLYTT                           
139600        MOVE SLAG-FLBUYUPD    TO MOD-FLBUYUPD                             
139700        MOVE SLAG-FLTABUPD    TO MOD-FLTABUPD                             
139800        MOVE SLAG-REPPFAKT    TO MOD-REPPFAKT                             
139900                                                                          
140000        IF SLAG-ADLAGOMR-CD = ZERO                                        
140100           MOVE 'N'                    TO MOD-FLCDART                     
140200        ELSE                                                              
140300           MOVE 'J'                    TO MOD-FLCDART                     
140400        END-IF                                                            
140500                                                                          
140600        IF SLAG-TIORDREG > ZERO                                           
140700           MOVE SLAG-TIORDREG          TO MOD-TIORDREG                    
140800        ELSE                                                              
140900           MOVE MFS-RENSA-FAELT        TO MOD-TIORDREG                    
141000        END-IF                                                            
141100                                                                          
141200        IF SLAG-TIREFSTO > ZERO                                           
141300          MOVE SLAG-TIREFSTO          TO MOD-TIREFSTO                     
141400        ELSE                                                              
141500          MOVE MFS-RENSA-FAELT        TO MOD-TIREFSTO                     
141600        END-IF                                                            
141700                                                                          
141800        IF SLAG-TIREFSTA > ZERO                                           
141900          MOVE SLAG-TIREFSTA          TO MOD-TIREFSTA                     
142000        ELSE                                                              
142100          MOVE MFS-RENSA-FAELT        TO MOD-TIREFSTA                     
142200        END-IF                                                            
142300                                                                          
142400        IF SLAG-TIREFMPB > ZERO                                           
142500          MOVE SLAG-TIREFMPB          TO MOD-TIREFMPB                     
142600        ELSE                                                              
142700          MOVE MFS-RENSA-FAELT        TO MOD-TIREFMPB                     
142800        END-IF                                                            
142900                                                                          
143000        IF SLAG-TIPBREOI > ZERO                                           
143100          MOVE SLAG-TIPBREOI          TO MOD-TIPBREOI                     
143200        ELSE                                                              
143300          MOVE MFS-RENSA-FAELT        TO MOD-TIPBREOI                     
143400        END-IF                                                            
143500                                                                          
143600        IF SLAG-TIREFPKT > ZERO                                           
143700          MOVE SLAG-TIREFPKT          TO MOD-TIREFPKT                     
143800        ELSE                                                              
143900          MOVE MFS-RENSA-FAELT        TO MOD-TIREFPKT                     
144000        END-IF                                                            
144100                                                                          
144200        IF SLAG-TIREFPAF > ZERO                                           
144300          MOVE SLAG-TIREFPAF          TO MOD-TIREFPAF                     
144400        ELSE                                                              
144500          MOVE MFS-RENSA-FAELT        TO MOD-TIREFPAF                     
144600        END-IF                                                            
144700                                                                          
144800        IF DCS-SDC                                                        
144900        OR DCS-NDC                                                        
145400          MOVE SLAG-IDPERSON-BUY      TO MOD-IDPERSON-BUY                 
145500        ELSE                                                              
145600          MOVE MFS-RENSA-FAELT        TO MOD-IDPERSON-BUY                 
145700        END-IF                                                            
145800                                                                          
145900        IF   SLAG-IDDC-REF = SPACE                                        
146000        AND (DCS-FLLPO = YES OR JA)                                       
146300            MOVE ZERO                  TO REF-KVDLTID-TOT                 
146400            IF SLAG-KVDAGAR-MANLT       > ZERO                            
146500               MOVE SLAG-KVDAGAR-MANLT TO MOD-LEDTID                      
146600            ELSE                                                          
146700              MOVE SLAG-IDLEVNR        TO W-IDLEVNR                       
146800               PERFORM IMS-GU-LEVA16                                      
146900               IF SEGMENT-FINNS                                           
147000                  MOVE NDC-KVDAGAR-TBT TO MOD-LEDTID                      
147100               ELSE                                                       
147200                  MOVE ZERO            TO MOD-LEDTID                      
147300               END-IF                                                     
147400            END-IF                                                        
147500        ELSE                                                              
147600          MOVE SLAG-IDDC-REF           TO W-IDDC-B616                     
147700          PERFORM IMS-GU-WDB616                                           
147800          MOVE REF-KVDLTID-TOT         TO MOD-LEDTID                      
147900        END-IF                                                            
148000*                                                                         
150600                                                                          
150700***     CALL W271REFL TO GET KLASS                                        
150800*                                                                         
150900        PERFORM S90-CALL-W271REFL                                         
151000                                                                          
151100        MOVE W271-REFL-KLASS TO MOD-KLASS                                 
151200        INSPECT MOD-KLASS REPLACING LEADING ZERO BY SPACE                 
151300                                                                          
151400        PERFORM IMS-GNP-WDK727                                            
151500        IF SEGMENT-FINNS                                                  
151600          MOVE PROG-KVPB-JUST(1)   TO MOD-KVPB-JUST-1                     
151700          MOVE PROG-KVPB-JUST(2)   TO MOD-KVPB-JUST-2                     
151800          MOVE PROG-TIPBJUST(1)    TO MOD-TIPBJUST-1                      
151900                                      WS-K727-TIPBJUST-1-JMF              
152000          MOVE PROG-TIPBJUST(2)    TO MOD-TIPBJUST-2                      
152100          IF PROG-KVPB-JUST(1) > ZERO                                     
152200            MOVE MFS-ADD-LYS-UPP-FAELT                                    
152300                                   TO MOD-KVREFPKT-ATTR                   
152400          END-IF                                                          
152500        ELSE                                                              
152600          MOVE MFS-RENSA-FAELT     TO MOD-KVPB-JUST-1                     
152700                                      MOD-KVPB-JUST-2                     
152800                                      MOD-TIPBJUST-1                      
152900                                      MOD-TIPBJUST-2                      
153000          MOVE ZERO                TO WS-K727-TIPBJUST-1-JMF              
153100        END-IF                                                            
153200                                                                          
153300     ELSE                                                                 
153400        MOVE ARTIKEL-SAKNAS-SDC TO MED-IDMFSFEL                           
153500        CALL WMEDKONV USING MED-WMEDAREA                                  
153600        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
153700        PERFORM FD-RENSA-SDC-INFO                                         
153800     END-IF                                                               
153900     .                                                                    
154000     EJECT                                                                
154100 FD-RENSA-SDC-INFO SECTION.                                               
154200                                                                          
154300     MOVE MFS-RENSA-FAELT TO MOD-IDREFTAB                                 
154400                             MOD-FLWILSON                                 
154500                             MOD-KVDISP                                   
154600                             MOD-KVBEART                                  
154700                             MOD-TIORDREG                                 
154800                             MOD-KVAKS-PAV                                
154900                             MOD-TIREFEFT                                 
155000                             MOD-KVAKS-SDC                                
155100                             MOD-KVREFPKT                                 
155200                             MOD-KVBEART                                  
155300                             MOD-TIREFPKT                                 
155400                             MOD-FLREFILL                                 
155500                             MOD-FLREFNYO                                 
155600                             MOD-KVREFBER                                 
155700                             MOD-FLREFBEO                                 
155800                             MOD-KVREFBER                                 
155900                             MOD-KVREFOVL                                 
156000                             MOD-TIREFSTO                                 
156100                             MOD-KDREFSTA                                 
156200                             MOD-TIREFSTA                                 
156300                             MOD-KVPB-SUM                                 
156400                             MOD-KVPB-REF                                 
156500                             MOD-KVPBREOI                                 
156600                             MOD-TIREFMPB                                 
156700                             MOD-TIPBREOI                                 
156800                             MOD-IDPERSON-BUY                             
156900                             MOD-FLBUYUPD                                 
157000                             MOD-FLTABUPD                                 
157100                             MOD-TEREFMED                                 
157200                             MOD-FLPB-FLYTT                               
157300                             MOD-KVPB-JUST-1                              
157400                             MOD-KVPB-JUST-2                              
157500                             MOD-TIPBJUST-1                               
157600                             MOD-TIPBJUST-2                               
157700     .                                                                    
157800     EJECT                                                                
157900 FE-GET-CDC-INFO SECTION.                                                 
158000                                                                          
158100     PERFORM IMS-GU-WDK611                                                
158200     IF SEGMENT-FINNS                                                     
158300***    GET FUTURE FORECAST FROM WDK626                                    
158400       PERFORM FEAD-VISA-FFC                                              
158500*                                                                         
158600       PERFORM IMS-GNP-WDK629                                             
158700       IF SEGMENT-FINNS                                                   
158800         PERFORM FEA-VISA-CDC-LAGERINFO                                   
158900                                                                          
159000         IF MESSAGE-NEJ                                                   
159100            IF CLAG-KVPB-SATS       > ZERO                                
159200               MOVE MED-1          TO MOD-TEMFSINF                        
159300            ELSE                                                          
159400              IF CLAG-KVPB-TPO      > ZERO                                
159500                MOVE MED-2         TO MOD-TEMFSINF                        
159600              END-IF                                                      
159700            END-IF                                                        
159800                                                                          
159900*---      CHECK IF ARTICLE IS PART OF CAMPAIGN                            
160000            PERFORM FEB-CHECK-CAMPAIGN                                    
160100            IF CAMPAIGN-JA                                                
160200               MOVE MED-3          TO MOD-TEMFSINF                        
160300            END-IF                                                        
160400         END-IF                                                           
160500                                                                          
160600         IF CLAG-KDERS > 0                                                
160700           IF CLAG-KDERS = +29 OR +52                                     
160800             MOVE ARTIKEL-UTGANGEN TO MED-IDMFSFEL                        
160900             CALL WMEDKONV      USING MED-WMEDAREA                        
161000             MOVE MED-MFSFEL       TO MOD-TEMFSFEL                        
161100           ELSE                                                           
161200             MOVE ARTIKEL-ERSATT   TO MED-IDMFSFEL                        
161300             CALL WMEDKONV      USING MED-WMEDAREA                        
161400             MOVE MED-MFSFEL       TO MOD-TEMFSFEL                        
161500           END-IF                                                         
161600         END-IF                                                           
161700       ELSE                                                               
161800          MOVE ARTIKEL-SAKNAS      TO MED-IDMFSFEL                        
161900          CALL WMEDKONV         USING MED-WMEDAREA                        
162000          MOVE MED-MFSFEL          TO MOD-TEMFSFEL                        
162100          PERFORM FD-RENSA-SDC-INFO                                       
162200          PERFORM FEC-RENSA-CDC-INFO                                      
162300       END-IF                                                             
162400     ELSE                                                                 
162500       MOVE ARTIKEL-SAKNAS         TO MED-IDMFSFEL                        
162600       CALL WMEDKONV            USING MED-WMEDAREA                        
162700       MOVE MED-MFSFEL             TO MOD-TEMFSFEL                        
162800       PERFORM FD-RENSA-SDC-INFO                                          
162900       PERFORM FEC-RENSA-CDC-INFO                                         
163000     END-IF                                                               
163100*                                                                         
163200     .                                                                    
163300     EJECT                                                                
163400 FEA-VISA-CDC-LAGERINFO  SECTION.                                         
163500                                                                          
163600     MOVE MSGI-IDDC-KEY     TO W-IDDC                                     
163700*                                                                         
163800     MOVE CLAG-KDERS        TO MOD-KDERS                                  
163900     MOVE 0.00              TO MOD-REPPFAKT                               
164000     MOVE WS-STD-TEXT       TO MOD-PRICE-TEXT                             
164100     MOVE CLAG-PRARTSTD     TO MOD-PRARTSTD                               
164200                               WS-PRARTSTD                                
164300                               WS-PRARTBES                                
164400     MOVE CREF-IDREFTAB     TO MOD-IDREFTAB                               
164500     MOVE CREF-FLWILSON     TO MOD-FLWILSON                               
164600*                                                                         
164700     COMPUTE WS-GIT          = CLAG-KVAKS-PAV    +                        
164800                               CLAG-KVAKS-T                               
164900*                                                                         
165000     MOVE WS-GIT            TO MOD-KVAKS-PAV                              
165100*    MOVE CLAG-KVTILLG-TOT  TO MOD-KVDISP                                 
165200     COMPUTE WS-BALANCE = CLAG-KVTILLG-TOT + CLAG-KVBEART                 
165300     MOVE WS-BALANCE        TO MOD-KVDISP                                 
165400     MOVE CLAG-KVAKS-CDC    TO MOD-KVAKS-SDC                              
165500     MOVE CLAG-KVBEART      TO MOD-KVBEART                                
165600     MOVE CREF-FLREFBEO     TO MOD-FLREFBEO                               
165700     MOVE CREF-KDREFSTA     TO MOD-KDREFSTA                               
165800     MOVE CLAG-KVPB-SEP     TO MOD-KVPB-REF                               
165900     MOVE CREF-KVREFPKT     TO MOD-KVREFPKT                               
166000     MOVE CLAG-KVQ          TO MOD-KVREFBER                               
166100     MOVE CREF-KVREFOVL     TO MOD-KVREFOVL                               
166200     MOVE CLAG-ADLAGOMR     TO MOD-ADLAGOMR                               
166300     MOVE CLAG-ADGANG       TO MOD-ADGANG                                 
166400     MOVE CLAG-ADPLATS      TO MOD-ADPLATS                                
166500     MOVE CREF-FLPB-FLYTT   TO MOD-FLPB-FLYTT                             
166600     MOVE MFS-RENSA-FAELT   TO MOD-KVPBREOI                               
166700     MOVE CREF-FLREFILL     TO MOD-FLREFILL                               
166800     MOVE CREF-FLREFNYO     TO MOD-FLREFNYO                               
166900     MOVE 'N'               TO MOD-FLCDART                                
167000     MOVE CREF-FLBUYUPD     TO MOD-FLBUYUPD                               
167100     MOVE CREF-FLTABUPD     TO MOD-FLTABUPD                               
167200                                                                          
167300     IF CREF-TIREFEFT > ZERO                                              
167400        MOVE CREF-TIREFEFT         TO MOD-TIREFEFT                        
167500     ELSE                                                                 
167600        MOVE MFS-RENSA-FAELT       TO MOD-TIREFEFT                        
167700     END-IF                                                               
167800                                                                          
167900     IF CREF-TIORDREG > ZERO                                              
168000        MOVE CREF-TIORDREG         TO MOD-TIORDREG                        
168100     ELSE                                                                 
168200        MOVE MFS-RENSA-FAELT       TO MOD-TIORDREG                        
168300     END-IF                                                               
168400                                                                          
168500     IF CREF-TIREFSTO > ZERO                                              
168600       MOVE CREF-TIREFSTO          TO MOD-TIREFSTO                        
168700     ELSE                                                                 
168800       MOVE MFS-RENSA-FAELT        TO MOD-TIREFSTO                        
168900     END-IF                                                               
169000                                                                          
169100     IF CREF-TIREFSTA > ZERO                                              
169200       MOVE CREF-TIREFSTA          TO MOD-TIREFSTA                        
169300     ELSE                                                                 
169400       MOVE MFS-RENSA-FAELT        TO MOD-TIREFSTA                        
169500     END-IF                                                               
169600                                                                          
169700     IF CLAG-TIPBDAT  > ZERO                                              
169800        MOVE 'AAVVD'               TO DAT-KDDATFORM                       
169900        MOVE CLAG-TIPBDAT          TO DAT-I-TIDATUM                       
170000        CALL WDATKONV  USING DAT-KDDATFORM DAT-I-TIDATUM                  
170100                             DAT-O-TIDATUM DAT-KDSVAR                     
170200        IF DAT-KDSVAR-OK                                                  
170300           MOVE DAT-TIAAMMDD       TO MOD-TIREFMPB                        
170400        ELSE                                                              
170500           MOVE MFS-RENSA-FAELT    TO MOD-TIREFMPB                        
170600        END-IF                                                            
170700     ELSE                                                                 
170800       MOVE MFS-RENSA-FAELT        TO MOD-TIREFMPB                        
170900     END-IF                                                               
171000                                                                          
171100     IF CLAG-DAPBPLAN > ZERO                                              
171200       MOVE CLAG-DAPBPLAN (3:6)    TO MOD-TIPBREOI                        
171300     ELSE                                                                 
171400       MOVE MFS-RENSA-FAELT        TO MOD-TIPBREOI                        
171500     END-IF                                                               
171600                                                                          
171700     IF CREF-TIREFPKT > ZERO                                              
171800       MOVE CREF-TIREFPKT          TO MOD-TIREFPKT                        
171900     ELSE                                                                 
172000       MOVE MFS-RENSA-FAELT        TO MOD-TIREFPKT                        
172100     END-IF                                                               
172200                                                                          
172300     IF CREF-TIREFPAF > ZERO                                              
172400       MOVE CREF-TIREFPAF          TO MOD-TIREFPAF                        
172500     ELSE                                                                 
172600       MOVE MFS-RENSA-FAELT        TO MOD-TIREFPAF                        
172700     END-IF                                                               
172800                                                                          
172900     IF CREF-IDPERSON-BUY > ZERO                                          
173000       MOVE CREF-IDPERSON-BUY      TO MOD-IDPERSON-BUY                    
173100     ELSE                                                                 
173200       MOVE MFS-RENSA-FAELT        TO MOD-IDPERSON-BUY                    
173300     END-IF                                                               
173400                                                                          
173500*--- GET TOTAL LEADTIME FOR BOAT/TRUCK                                    
173600     MOVE CLAG-IDDC-REF            TO W-IDDC-B616                         
173700     PERFORM IMS-GU-WDB616                                                
173800     IF SEGMENT-FINNS                                                     
173900        MOVE REF-KVDLTID-TOT       TO MOD-LEDTID                          
174000     ELSE                                                                 
174100        MOVE +1                    TO MOD-LEDTID                          
174200     END-IF                                                               
174300                                                                          
174400*--- GET NOTES FROM WDK625                                                
174500     PERFORM FEAA-GET-NOTE-WDK625                                         
174600     MOVE WS-TEREFMED              TO MOD-TEREFMED                        
174700                                                                          
174800*--- GET PART DESCRIPTION                                                 
174900     PERFORM FB-HAEMTA-BENAEMNING                                         
175000*--- GET PROCURER INFORMATION                                             
175100     PERFORM FEAC-GET-PROC-INFO                                           
175200     PERFORM FEAB-GET-INFO-W272REFL                                       
175300     PERFORM S04-GET-FC-TOT                                               
175400                                                                          
175500     .                                                                    
175600     EJECT                                                                
175700 FEAA-GET-NOTE-WDK625  SECTION.                                           
175800                                                                          
175900     MOVE 1                  TO W-KDNOTTYP                                
176000     PERFORM IMS-GU-WDK625                                                
176100     IF SEGMENT-FINNS                                                     
176200        MOVE NOT-TEARTNOT                                                 
176300                             TO WS-COMMENT-1                              
176400     ELSE                                                                 
176500        MOVE SPACES          TO WS-COMMENT-1                              
176600     END-IF                                                               
176700*                                                                         
176800     MOVE 2                  TO W-KDNOTTYP                                
176900     PERFORM IMS-GU-WDK625                                                
177000     IF SEGMENT-FINNS                                                     
177100        MOVE NOT-TEARTNOT (1:32)                                          
177200                             TO WS-COMMENT-2                              
177300     ELSE                                                                 
177400        MOVE SPACES          TO WS-COMMENT-2                              
177500     END-IF                                                               
177600     .                                                                    
177700     EJECT                                                                
177800 FEAB-GET-INFO-W272REFL  SECTION.                                         
177900                                                                          
178000     MOVE MSGI-IDDC-KEY      TO W-IDDC                                    
178100*                                                                         
178200     INITIALIZE CDC-REFL-W272REFL                                         
178300     MOVE W-IDARTNR          TO CDC-REFL-IDARTNR                          
178400     MOVE W-IDDC             TO CDC-REFL-IDDC                             
178500     MOVE CREF-IDDC-REF      TO CDC-REFL-IDDC-REF                         
178600     MOVE CREF-IDREFTAB      TO CDC-REFL-IDREFTAB                         
178700     MOVE CREF-FLREFBEO      TO CDC-REFL-FLREFBEO                         
178800     MOVE CREF-FLWILSON      TO CDC-REFL-FLWILSON                         
178900     MOVE CLAG-PRARTSTD      TO CDC-REFL-PRARTBES                         
179000     MOVE CREF-FLFLYG        TO CDC-REFL-FLFLYG                           
179100     MOVE DAGENS-DATUM       TO CDC-REFL-BINNDAY-TIAAMMDD                 
179200*                                                                         
179300     MOVE CREF-TIREFPKT      TO TMP1-YYMMDD                               
179400     MOVE DAGENS-DATUM       TO TMP2-YYMMDD                               
179500     PERFORM WY2000P1                                                     
179600     IF  TMP1-YYMMDD         >= TMP2-YYMMDD                               
179700       MOVE CREF-KVREFPKT    TO CDC-REFL-IN-KVREFPKT                      
179800     ELSE                                                                 
179900       MOVE ZERO             TO CDC-REFL-IN-KVREFPKT                      
180000     END-IF                                                               
180100                                                                          
180200     MOVE CREF-TIREFPAF      TO TMP1-YYMMDD                               
180300     MOVE DAGENS-DATUM       TO TMP2-YYMMDD                               
180400     PERFORM WY2000P1                                                     
180500     IF TMP1-YYMMDD          >= TMP2-YYMMDD                               
180600       MOVE CLAG-KVQ         TO CDC-REFL-IN-KVREFBER                      
180700     ELSE                                                                 
180800       MOVE ZERO             TO CDC-REFL-IN-KVREFBER                      
180900     END-IF                                                               
181000******get leadtime demand for CDC************                             
181100     MOVE 2                     TO W272-UTUP-KDCALL                       
181200     MOVE W-IDARTNR             TO W272-UTUP-IDARTNR                      
181300     MOVE W-IDDC                TO W272-UTUP-IDDC                         
181400     IF CREF-IDDC-REF = SPACE                                             
181500       CALL FELLOG                                                        
181600     ELSE                                                                 
181700       MOVE CREF-IDDC-REF       TO W272-UTUP-IDDC-REF                     
181800     END-IF                                                               
181900                                                                          
182000     CALL W272UTUP USING W272-UTUP-W272UTUP                               
182100                   U2-WDK6-PCB                                            
182200                   U2-WDB6-PCB                                            
182300                   U2-PBTO-W222-WDK6-PCB                                  
182400                   U2-PBTO-W222-WDK7-PCB                                  
182500                   U2-PBTO-W222-ARTM-PCB                                  
182600                   U2-PBTO-W222-R1-2501-PCB                               
182700                   U2-PBTO-W222-R1-WDB6R-PCB                              
182800                   U2-PBTO-W222-R1-WDK7R-PCB                              
182900                   U2-PBTO-W222-WDB6-PCB                                  
183000                   U2-PBTO-W222-WDD7-PCB                                  
183100                   U2-PBTO-W222-WDK7E-PCB                                 
183200                   U2-PBTO-W222-R1-UTIL-WDK6-PCB                          
183300                   U2-PBTO-W222-R1-UTIL-WDK7-PCB                          
183400                   U2-PBTO-W222-R1-UTIL-WDB6-PCB                          
183500                   U2-PBTO-W222-U1-WDK7-PCB                               
183600                   U2-PBTO-W222-U1-WDB6-PCB                               
183700                   U2-PBTO-W222-U1-UTIL-WDK6-PCB                          
183800                   U2-PBTO-W222-U1-UTIL-WDK7-PCB                          
183900                   U2-PBTO-W222-U1-UTIL-WDB6-PCB                          
184000                   U2-REFL2-2501-PCB                                      
184100                   U2-REFL2-WDB6-PCB                                      
184200                   U2-REFL2-UTIL-WDK6-PCB                                 
184300                   U2-REFL2-UTIL-WDK7-PCB                                 
184400                   U2-REFL2-UTIL-WDB6-PCB                                 
184500                   U2-W222-WDK6-PCB                                       
184600                   U2-W222-WDK7-PCB                                       
184700                   U2-W222-ARTM-PCB                                       
184800                   U2-W222-2501-PCB                                       
184900                   U2-W222-WDB6R-PCB                                      
185000                   U2-W222-WDK7R-PCB                                      
185100                   U2-W222-WDB6-PCB                                       
185200                   U2-W222-WDD7-PCB                                       
185300                   U2-W222-WDK7E-PCB                                      
185400                   U2-W222-UTIL-WDK6-PCB                                  
185500                   U2-W222-UTIL-WDK7-PCB                                  
185600                   U2-W222-UTIL-WDB6-PCB                                  
185700                   U2-W222-U1-WDK7-PCB                                    
185800                   U2-W222-U1-WDB6-PCB                                    
185900                   U2-W222-U1-UTIL-WDK6-PCB                               
186000                   U2-W222-U1-UTIL-WDK7-PCB                               
186100                   U2-W222-U1-UTIL-WDB6-PCB                               
186200                                                                          
186300     IF W272-UTUP-KDSVAR-OK                                               
186400        MOVE W272-UTUP-LEADTID-BEHOV TO CDC-REFL-IN-LEADTID-BEHOV         
186500     ELSE                                                                 
186600        DISPLAY 'W272UTUP-ERROR :' W272-UTUP-TEXT                         
186700        CALL FELLOG                                                       
186800     END-IF                                                               
186900*                                                                         
187000     CALL W272REFL        USING CDC-REFL-W272REFL                         
187100                                REFL2-2501-PCB                            
187200                                REFL2-WDB6-PCB                            
187300                                REFL2-UTIL-WDK6-PCB                       
187400                                REFL2-UTIL-WDK7-PCB                       
187500                                REFL2-UTIL-WDB6-PCB                       
187600*                                                                         
187700     MOVE CDC-REFL-KLASS     TO MOD-KLASS                                 
187800     INSPECT MOD-KLASS REPLACING LEADING ZERO BY SPACE                    
187900     .                                                                    
188000     EJECT                                                                
188100 FEAC-GET-PROC-INFO      SECTION.                                         
188200                                                                          
188300     MOVE CLAG-IDDC-REF      TO W-IDDC                                    
188400     PERFORM IMS-GU-WDK722                                                
188500     IF SEGMENT-FINNS                                                     
188600        MOVE XLAG-IDANSK     TO MOD-IDANSK                                
188700     ELSE                                                                 
188800        MOVE MFS-RENSA-FAELT TO MOD-IDANSK                                
188900     END-IF                                                               
189000     .                                                                    
189100     EJECT                                                                
189200 FEAD-VISA-FFC SECTION.                                                   
189300                                                                          
189400     PERFORM IMS-GNP-WDK626                                               
189500     IF SEGMENT-FINNS                                                     
189600        MOVE JUST-KVPB-JUST(1)   TO MOD-KVPB-JUST-1                       
189700        MOVE JUST-KVPB-JUST(2)   TO MOD-KVPB-JUST-2                       
189800        MOVE JUST-TIPBJUST(1)    TO MOD-TIPBJUST-1                        
189900                                    WS-K626-TIPBJUST-1-JMF                
190000        MOVE JUST-TIPBJUST(2)    TO MOD-TIPBJUST-2                        
190100        IF JUST-KVPB-JUST(1) > ZERO                                       
190200          MOVE MFS-ADD-LYS-UPP-FAELT                                      
190300                                 TO MOD-KVREFPKT-ATTR                     
190400        ELSE                                                              
190500          MOVE MFS-RENSA-FAELT   TO MOD-KVPB-JUST-1                       
190600                                    MOD-TIPBJUST-1                        
190700        END-IF                                                            
190800        IF JUST-KVPB-JUST(2) > ZERO                                       
190900           CONTINUE                                                       
191000        ELSE                                                              
191100          MOVE MFS-RENSA-FAELT   TO MOD-KVPB-JUST-2                       
191200                                    MOD-TIPBJUST-2                        
191300        END-IF                                                            
191400     ELSE                                                                 
191500        MOVE MFS-RENSA-FAELT     TO MOD-KVPB-JUST-1                       
191600                                    MOD-KVPB-JUST-2                       
191700                                    MOD-TIPBJUST-1                        
191800                                    MOD-TIPBJUST-2                        
191900        MOVE ZERO                TO WS-K626-TIPBJUST-1-JMF                
192000     END-IF                                                               
192100     .                                                                    
192200     EJECT                                                                
192300 FEB-CHECK-CAMPAIGN      SECTION.                                         
192400                                                                          
192500     MOVE NEJ                     TO SW-CAMPAIGN                          
192600                                                                          
192700     PERFORM DB2-DCL-OPN-TP1ARTK-CRS                                      
192800                                                                          
192900     IF SQLCODE-WS  = ZERO                                                
193000       PERFORM FEBA-READ-TP1ARTK                                          
193100     END-IF                                                               
193200                                                                          
193300     IF SQLCODE-WS  = ZERO                                                
193400                                                                          
193500        PERFORM UNTIL SQLCODE      > ZERO                                 
193600          IF TP1KAMP-TISTODAT-KAMP = ZERO                                 
193700             COMPUTE WS-TISTODAT-KAMP =                                   
193800                     TP1KAMP-TISTADAT-KAMP + 50000                        
193900          ELSE                                                            
194000             MOVE TP1KAMP-TISTODAT-KAMP                                   
194100                                  TO WS-TISTODAT-KAMP                     
194200          END-IF                                                          
194300                                                                          
194400          MOVE DAGENS-DATUM       TO WS-DATUM                             
194500                                                                          
194600          IF WS-DATUM-MAN          > 06                                   
194700             SUBTRACT 600       FROM WS-DATUM                             
194800          ELSE                                                            
194900             ADD 600              TO WS-DATUM                             
195000             SUBTRACT 10000     FROM WS-DATUM                             
195100          END-IF                                                          
195200                                                                          
195300          MOVE WS-TISTODAT-KAMP   TO TMP1-YYMMDD                          
195400          MOVE WS-DATUM           TO TMP2-YYMMDD                          
195500          PERFORM WY2000P1                                                
195600          IF TMP1-YYMMDD          >= TMP2-YYMMDD                          
195700             MOVE JA              TO SW-CAMPAIGN                          
195800             MOVE +100            TO SQLCODE                              
195900          END-IF                                                          
196000                                                                          
196100          IF SQLCODE = ZERO                                               
196200             PERFORM FEBA-READ-TP1ARTK                                    
196300          END-IF                                                          
196400       END-PERFORM                                                        
196500     END-IF                                                               
196600     PERFORM DB2-CLOSE-TP1ARTK-CRS                                        
196700     .                                                                    
196800     EJECT                                                                
196900 FEBA-READ-TP1ARTK      SECTION.                                          
197000***  READ AND SHOW TP1ARTK AND TP1KAMP                                    
197100     PERFORM DB2-FETCH-TP1ARTK-CRS                                        
197200     .                                                                    
197300     EJECT                                                                
197400 FEC-RENSA-CDC-INFO     SECTION.                                          
197500                                                                          
197600     MOVE MFS-RENSA-FAELT    TO MOD-BEART                                 
197700                                MOD-IDANSK                                
197800                                MOD-KDERS                                 
197900                                MOD-KLASS                                 
198000                                MOD-REPPFAKT                              
198100                                MOD-FLCDART                               
198200                                MOD-ADLAGOMR                              
198300                                MOD-ADGANG                                
198400                                MOD-ADPLATS                               
198500                                MOD-PRICE-TEXT                            
198600                                MOD-PRARTSTD                              
198700     .                                                                    
198800     EJECT                                                                
198900 G-KOLLA-INPUT SECTION.                                                   
199000                                                                          
199100     MOVE JA                 TO INDATA-SW                                 
199200     MOVE NEJ                TO ARTIKEL-ERS-SW                            
199300                                                                          
199400     PERFORM IMS-GU-WDK601                                                
199500                                                                          
199600     IF SEGMENT-FINNS                                                     
199700       PERFORM IMS-GNP-WDK611                                             
199800     END-IF                                                               
199900*                                                                         
200000     IF SEGMENT-SAKNAS AND CDC-SE                                         
200100        MOVE NEJ            TO INDATA-SW                                  
200200        MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                               
200300        CALL WMEDKONV    USING MED-WMEDAREA                               
200400        MOVE MED-MFSFEL     TO MOD-TEMFSFEL                               
200500        PERFORM MFS-RENSA-FAELT-UT                                        
200600     END-IF                                                               
200700*                                                                         
200800     IF SEGMENT-FINNS                                                     
200900       IF CLAG-KDERS > +9                                                 
201000         MOVE JA TO ARTIKEL-ERS-SW                                        
201100         MOVE CLAG-KDERS TO SPAR-KDERS                                    
201200       END-IF                                                             
201300                                                                          
201400       IF CDC-SE                                                          
201500          IF CLAG-IDDC-REF   = SPACES                                     
201600             MOVE NEJ            TO INDATA-SW                             
201700             MOVE INF-NOT-REFILL TO MED-IDMFSINF                          
201800             CALL WMEDKONV    USING MED-WMEDAREA                          
201900             MOVE MED-MFSINF     TO MOD-TEMFSINF                          
202000             MOVE JA             TO SW-MESSAGE                            
202100          ELSE                                                            
202200             PERFORM IMS-GNP-WDK626                                       
202300             IF SEGMENT-FINNS                                             
202400                MOVE JA         TO SW-K626-EXISTS                         
202500             END-IF                                                       
202600          END-IF                                                          
202700       ELSE                                                               
202800          PERFORM IMS-GU-WDK7-WDK701                                      
202900          IF SEGMENT-SAKNAS                                               
203000            IF ARTIKEL-ERS-MAERKT                                         
203100*------- DET SKALL EJ GÅ ATT LÄGGA UPP                                    
203200*------- ARTIKEL SOM ÄR ERS.MÄRKT                                         
203300*------- MEN MAN KAN UPPDATERA VISSA ITEM                                 
203400              MOVE NEJ TO INDATA-SW                                       
203500            END-IF                                                        
203600          END-IF                                                          
203700       END-IF                                                             
203800                                                                          
203900       IF INDATA-OK AND CDC-SE                                            
204000          PERFORM GD-CDC-DATA-INPUT                                       
204100       ELSE                                                               
204200         IF INDATA-OK                                                     
204300           IF MID-INPUT = ALL '+' AND SEGMENT-FINNS                       
204400             MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                    
204500             CALL WMEDKONV USING MED-WMEDAREA                             
204600             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
204700             PERFORM MFS-ROER-EJ-FAELT-IN                                 
204800             PERFORM MFS-ROER-EJ-FAELT-UT                                 
204900             MOVE NEJ TO INDATA-SW                                        
205000           ELSE                                                           
205100             IF SEGMENT-SAKNAS                                            
205200               PERFORM S01-KOLLA-OM-REFILL-OK                             
205300               IF REFILL-ART-OK                                           
205400                 CONTINUE                                                 
205500               ELSE                                                       
205600                 MOVE NEJ TO INDATA-SW                                    
205700               END-IF                                                     
205800             END-IF                                                       
205900             IF INDATA-OK                                                 
206000               IF ARTIKEL-ERS-MAERKT                                      
206100                 PERFORM GA-HUVUDFLAGGOR-ERSATT-ARTIKEL                   
206200               ELSE                                                       
206300                 PERFORM GB-HUVUDFLAGGOR-VANLIG-ARTIKEL                   
206400               END-IF                                                     
206500*              IF INDATA-OK                                               
206600                 PERFORM GC-KOLLA-OVR-DATAELEMENT                         
206700*              END-IF                                                     
206800             END-IF                                                       
206900             IF INDATA-FEL                                                
207000               PERFORM MFS-ROER-EJ-FAELT-IN                               
207100               PERFORM MFS-ROER-EJ-FAELT-UT                               
207200             END-IF                                                       
207300                                                                          
207400           END-IF                                                         
207500         END-IF                                                           
207600       END-IF                                                             
207700     ELSE                                                                 
207800       MOVE NEJ TO INDATA-SW                                              
207900     END-IF                                                               
208000     .                                                                    
208100     EJECT                                                                
208200                                                                          
208300 GA-HUVUDFLAGGOR-ERSATT-ARTIKEL SECTION.                                  
208400                                                                          
208500       IF MID-FLREFILL = ALL '+'                                          
208600         MOVE MFS-RENSA-FAELT    TO MOD-FLREFILL-IN                       
208700       ELSE                                                               
208800         MOVE NEJ TO INDATA-SW                                            
208900         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLREFILL-IN-ATTR                  
209000       END-IF                                                             
209100                                                                          
209200       IF MID-FLREFBEO = ALL '+'                                          
209300         MOVE MFS-RENSA-FAELT    TO MOD-FLREFBEO-IN                       
209400       ELSE                                                               
209500         MOVE NEJ TO INDATA-SW                                            
209600         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLREFBEO-IN-ATTR                  
209700       END-IF                                                             
209800                                                                          
209900       IF MID-TIREFSTO = ALL '+'                                          
210000         MOVE MFS-RENSA-FAELT    TO MOD-TIREFSTO-IN                       
210100       ELSE                                                               
210200         MOVE NEJ TO INDATA-SW                                            
210300         MOVE MFS-NUM-FAELT-FEL  TO MOD-TIREFSTO-IN-ATTR                  
210400       END-IF                                                             
210500                                                                          
210600       IF INDATA-FEL                                                      
210700         IF SPAR-KDERS > ZERO                                             
210800           IF SPAR-KDERS = +29 OR +52                                     
210900             MOVE ARTIKEL-UTGANGEN TO MED-IDMFSFEL                        
211000             CALL WMEDKONV USING MED-WMEDAREA                             
211100             MOVE MED-MFSFEL TO MOD-TEMFSINF                              
211200             MOVE JA         TO SW-MESSAGE                                
211300           ELSE                                                           
211400             MOVE ARTIKEL-ERSATT TO MED-IDMFSFEL                          
211500             CALL WMEDKONV USING MED-WMEDAREA                             
211600             MOVE MED-MFSFEL TO MOD-TEMFSINF                              
211700             MOVE JA         TO SW-MESSAGE                                
211800           END-IF                                                         
211900         END-IF                                                           
212000       END-IF                                                             
212100       .                                                                  
212200       EJECT                                                              
212300                                                                          
212400                                                                          
212500 GB-HUVUDFLAGGOR-VANLIG-ARTIKEL SECTION.                                  
212600                                                                          
212700       IF MID-FLREFILL = ALL '+'                                          
212800         MOVE MFS-RENSA-FAELT TO MOD-FLREFILL-IN                          
212900       ELSE                                                               
213000         IF MID-FLREFILL = JA OR YES OR NEJ                               
213100           IF MID-FLREFILL = JA OR YES                                    
213200             MOVE JA   TO MID-FLREFILL                                    
213300             PERFORM S01-KOLLA-OM-REFILL-OK                               
213400             IF REFILL-ART-OK                                             
213500               MOVE JA TO WDK7-SW                                         
213600               MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLREFILL-IN-ATTR          
213700             ELSE                                                         
213800               MOVE NEJ TO INDATA-SW                                      
213900               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
214000               MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLREFILL-IN-ATTR          
214100             END-IF                                                       
214200           ELSE                                                           
214300             MOVE JA TO WDK7-SW                                           
214400             MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLREFILL-IN-ATTR            
214500           END-IF                                                         
214600         ELSE                                                             
214700           MOVE NEJ TO INDATA-SW                                          
214800           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
214900           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLREFILL-IN-ATTR              
215000         END-IF                                                           
215100       END-IF                                                             
215200                                                                          
215300       IF MID-FLREFBEO = ALL '+'                                          
215400         MOVE MFS-RENSA-FAELT TO MOD-FLREFBEO-IN                          
215500       ELSE                                                               
215600                                                                          
215700         PERFORM IMS-GU-WDK7-WDK711                                       
215800         IF SEGMENT-FINNS                                                 
216000           IF SLAG-IDDC-REF NOT = SPACES                                  
216100*----                                                                     
216200*---- EJ LOKAL ARTIKEL                                                    
216300*----                                                                     
216400             IF MID-FLREFBEO = JA                                         
216500             OR MID-FLREFBEO = YES                                        
216600             OR MID-FLREFBEO = NEJ                                        
216700             OR MID-FLREFBEO = 'S'                                        
216800               IF MID-FLREFBEO = YES                                      
216900                 MOVE JA TO MID-FLREFBEO                                  
217000               END-IF                                                     
217100               MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLREFBEO-IN-ATTR          
217200             ELSE                                                         
217300               MOVE NEJ TO INDATA-SW                                      
217400               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
217500               MOVE MFS-ALFA-FAELT-FEL TO MOD-FLREFBEO-IN-ATTR            
217600             END-IF                                                       
217700           ELSE                                                           
217800*----                                                                     
217900*---- LOKAL ARTIKEL                                                       
218000*----                                                                     
218100             IF MID-FLREFBEO = NEJ                                        
218200               MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLREFBEO-IN-ATTR          
218300             ELSE                                                         
218400               MOVE NEJ TO INDATA-SW                                      
218500               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
218600               MOVE MFS-ALFA-FAELT-FEL TO MOD-FLREFBEO-IN-ATTR            
218700             END-IF                                                       
218800           END-IF                                                         
218900           MOVE JA TO WDK7-SW                                             
219000         ELSE                                                             
219100           MOVE NEJ TO INDATA-SW                                          
219200           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
219300           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLREFBEO-IN-ATTR                
219400         END-IF                                                           
219500       END-IF                                                             
219600                                                                          
219700                                                                          
219800*--- KOLLA BEORDRINGSSTOPP                                                
219900                                                                          
220000       IF MID-TIREFSTO = ALL '+'                                          
220100         MOVE MFS-RENSA-FAELT TO MOD-TIREFSTO-IN                          
220200       ELSE                                                               
220300         IF MID-TIREFSTO NUMERIC                                          
220400           IF MID-TIREFSTO = ZERO                                         
220500             MOVE MFS-NUM-FAELT-RAETT TO                                  
220600                                      MOD-TIREFSTO-IN-ATTR                
220700           ELSE                                                           
220800             MOVE 'AAMMDD'     TO DAT-KDDATFORM                           
220900             MOVE MID-TIREFSTO TO DAT-I-TIDATUM                           
221000             CALL WDATKONV USING DAT-KDDATFORM                            
221100                                 DAT-I-TIDATUM                            
221200                                 DAT-O-TIDATUM DAT-KDSVAR                 
221300                                                                          
221400             IF DAT-KDSVAR-OK                                             
221500                MOVE MID-TIREFSTO    TO TMP1-YYMMDD                       
221600                MOVE DAGENS-DATUM    TO TMP2-YYMMDD                       
221700                MOVE DD-PLUS-SEX-MAN TO TMP3-YYMMDD                       
221800                PERFORM WY2000Q1                                          
221900                IF TMP1-YYMMDD >= TMP2-YYMMDD                             
222000                AND TMP1-YYMMDD < TMP3-YYMMDD                             
222100                  MOVE MFS-NUM-FAELT-RAETT TO                             
222200                                       MOD-TIREFSTO-IN-ATTR               
222300                ELSE                                                      
222400                  MOVE NEJ TO INDATA-SW                                   
222500                  MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL               
222600                  MOVE MFS-NUM-FAELT-FEL TO                               
222700                                        MOD-TIREFSTO-IN-ATTR              
222800                END-IF                                                    
222900             ELSE                                                         
223000               MOVE NEJ TO INDATA-SW                                      
223100               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
223200               MOVE MFS-NUM-FAELT-FEL TO                                  
223300                                      MOD-TIREFSTO-IN-ATTR                
223400             END-IF                                                       
223500           END-IF                                                         
223600           MOVE JA TO WDK7-SW                                             
223700         ELSE                                                             
223800           MOVE NEJ TO INDATA-SW                                          
223900           MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                     
224000           MOVE MFS-NUM-FAELT-FEL     TO                                  
224100                                      MOD-TIREFSTO-IN-ATTR                
224200         END-IF                                                           
224300       END-IF                                                             
224400       .                                                                  
224500       EJECT                                                              
224600                                                                          
224700 GC-KOLLA-OVR-DATAELEMENT SECTION.                                        
224800                                                                          
224900     MOVE ZERO                        TO WS-TIAARP-1                      
225000                                         WS-TIAARP-2                      
225100                                         WS-K727-TIAARP-1                 
225200                                         WS-K727-TIAARP-2                 
225300                                         WS-K727-TIPBJUST-1-JMF           
225400                                         WS-K727-TIPBJUST-2-JMF           
225500*                                                                         
225600     MOVE NEJ                         TO BUY-SW                           
225700                                         TAB-SW                           
225800                                         SW-K711-EXISTS                   
225900                                         SW-KVPBJUST                      
226000     PERFORM S02-GET-BESPRIS                                              
226100     PERFORM IMS-GU-WDK7-WDK711                                           
226200     IF SEGMENT-FINNS                                                     
226300        MOVE SLAG-FLBUYUPD            TO BUY-SW                           
226400        MOVE SLAG-FLTABUPD            TO TAB-SW                           
226500        MOVE JA                       TO SW-K711-EXISTS                   
226600        PERFORM IMS-GNP-WDK727                                            
226700        IF SEGMENT-FINNS                                                  
226800           MOVE PROG-KVPB-JUST (1)    TO WS-K727-KVPB-JUST-1              
226900           MOVE PROG-TIPBJUST  (1)    TO WS-K727-TIPBJUST-1-JMF           
227000           MOVE PROG-KVPB-JUST (2)    TO WS-K727-KVPB-JUST-2              
227100           MOVE PROG-TIPBJUST  (2)    TO WS-K727-TIPBJUST-2-JMF           
227200           IF PROG-KVPB-JUST (1)       > ZERO                             
227300              MOVE JA                 TO SW-KVPBJUST                      
227400              MOVE WS-K727-TIPBJUST-1-JMF                                 
227500                                      TO DAT-I-TIDATUM                    
227600              MOVE 'AAVV  '           TO DAT-KDDATFORM                    
227700              CALL WDATKONV USING DAT-KDDATFORM                           
227800                                  DAT-I-TIDATUM                           
227900                                  DAT-O-TIDATUM                           
228000                                  DAT-KDSVAR                              
228100              IF DAT-KDSVAR-OK                                            
228200                 MOVE DAT-TIAARP      TO WS-K727-TIAARP-1                 
228300              END-IF                                                      
228400           END-IF                                                         
228500           IF PROG-KVPB-JUST (2)       > ZERO                             
228600              MOVE WS-K727-TIPBJUST-2-JMF                                 
228700                                      TO DAT-I-TIDATUM                    
228800              MOVE 'AAVV  '           TO DAT-KDDATFORM                    
228900              CALL WDATKONV USING DAT-KDDATFORM                           
229000                                  DAT-I-TIDATUM                           
229100                                  DAT-O-TIDATUM                           
229200                                  DAT-KDSVAR                              
229300              IF DAT-KDSVAR-OK                                            
229400                 MOVE DAT-TIAARP      TO WS-K727-TIAARP-2                 
229500              END-IF                                                      
229600           END-IF                                                         
229610        ELSE                                                              
229620          IF MID-KVPB-JUST-1 = ZERO                                       
229630          OR MID-TIPBJUST-1 = ZERO                                        
229631          OR MID-KVPB-JUST-2 = ZERO                                       
229632          OR MID-TIPBJUST-2 = ZERO                                        
229633             MOVE NEJ                 TO INDATA-SW                        
229634             MOVE ERR-CORR-HILITE-FLDS                                    
229635                                    TO MED-IDMFSFEL                       
229636             MOVE MFS-ALFA-FAELT-FEL                                      
229637                                    TO MOD-IN-KVPB-JUST-1-ATTR            
229638                                       MOD-IN-TIPBJUST-1-ATTR             
229639                                       MOD-IN-KVPB-JUST-2-ATTR            
229640                                       MOD-IN-TIPBJUST-2-ATTR             
229641          ELSE                                                            
229642            IF MID-KVPB-JUST-1 = ALL '+'                                  
229643            OR MID-TIPBJUST-1 = ALL '+'                                   
229644              IF MID-KVPB-JUST-2 = ZERO                                   
229645              OR MID-TIPBJUST-2 = ZERO                                    
229646                MOVE NEJ              TO INDATA-SW                        
229647                MOVE ERR-CORR-HILITE-FLDS                                 
229648                                       TO MED-IDMFSFEL                    
229649                MOVE MFS-ALFA-FAELT-FEL                                   
229650                                       TO MOD-IN-KVPB-JUST-2-ATTR         
229651                                          MOD-IN-TIPBJUST-2-ATTR          
229654              END-IF                                                      
229655            END-IF                                                        
229660          END-IF                                                          
229700        END-IF                                                            
229800     END-IF                                                               
229900                                                                          
230000*------KOLLA IDREFTAB                                                     
230100     IF MID-IDREFTAB = ALL '+'                                            
230200       MOVE MFS-RENSA-FAELT           TO MOD-IDREFTAB-IN                  
230300     ELSE                                                                 
230400        IF TAB-LOCKED                                                     
230500          MOVE NEJ                    TO INDATA-SW                        
230600          MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                     
230700          MOVE MFS-ALFA-FAELT-FEL     TO MOD-IDREFTAB-IN-ATTR             
230800        ELSE                                                              
230900          MOVE DCS-IDDC               TO W-2501-IDDC                      
231000          MOVE MID-IDREFTAB           TO W-2502-IDREFTAB                  
231100          PERFORM IMS-GU-2502                                             
231200          IF SEGMENT-FINNS                                                
231300             IF MID-IDREFTAB NUMERIC                                      
231400               MOVE JA                TO WDK7-SW                          
231500               MOVE MFS-ALFA-FAELT-RAETT                                  
231600                                      TO MOD-IDREFTAB-IN-ATTR             
231700             ELSE                                                         
231800               MOVE NEJ               TO INDATA-SW                        
231900               MOVE ERR-CORR-HILITE-FLDS                                  
232000                                      TO MED-IDMFSFEL                     
232100               MOVE MFS-ALFA-FAELT-FEL                                    
232200                                      TO MOD-IDREFTAB-IN-ATTR             
232300             END-IF                                                       
232400          ELSE                                                            
232500             MOVE NEJ                 TO INDATA-SW                        
232600             MOVE ERR-CORR-HILITE-FLDS                                    
232700                                      TO MED-IDMFSFEL                     
232800             MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDREFTAB-IN-ATTR             
232900          END-IF                                                          
233000        END-IF                                                            
233100     END-IF                                                               
233200                                                                          
233300*------KOLLA FLWILSON                                                     
233400     IF MID-FLWILSON = ALL '+'                                            
233500       MOVE MFS-RENSA-FAELT           TO MOD-FLWILSON-IN                  
233600     ELSE                                                                 
233700       IF MID-FLWILSON = JA OR NEJ                                        
233800         MOVE JA                      TO WDK7-SW                          
233900         MOVE MFS-ALFA-FAELT-RAETT    TO MOD-FLWILSON-IN-ATTR             
234000       ELSE                                                               
234100         MOVE NEJ                     TO INDATA-SW                        
234200         MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                     
234300         MOVE MFS-ALFA-FAELT-FEL      TO MOD-FLWILSON-IN-ATTR             
234400       END-IF                                                             
234500     END-IF                                                               
234600                                                                          
234700*------KOLLA FLPB-FLYTT                                                   
234800     IF MID-FLPB-FLYTT = ALL '+'                                          
234900        MOVE MFS-RENSA-FAELT          TO MOD-FLPB-FLYTT-IN                
235000     ELSE                                                                 
235100       IF MID-KVPB-JUST-1 NOT = ALL '+'                                   
235200       OR MID-KVPB-JUST-2 NOT = ALL '+'                                   
235300*      OR KVPBJUST-EXIST                                                  
235400         MOVE NEJ                     TO INDATA-SW                        
235500         MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                     
235600         MOVE MFS-ALFA-FAELT-FEL      TO MOD-FLPB-FLYTT-IN-ATTR           
235700       ELSE                                                               
235800         IF MID-FLPB-FLYTT = JA OR YES OR NEJ                             
235900           IF MID-FLPB-FLYTT = YES                                        
236000             MOVE JA                  TO MID-FLPB-FLYTT                   
236100           END-IF                                                         
236200           MOVE JA                    TO WDK7-SW                          
236300           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-FLPB-FLYTT-IN-ATTR           
236400         ELSE                                                             
236500           MOVE NEJ                   TO INDATA-SW                        
236600           MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                     
236700           MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLPB-FLYTT-IN-ATTR           
236800         END-IF                                                           
236900       END-IF                                                             
237000     END-IF                                                               
237100                                                                          
237200*------KOLLA FLREFNYO                                                     
237300     IF MID-FLREFNYO = ALL '+'                                            
237400       MOVE MFS-RENSA-FAELT           TO MOD-FLREFNYO-IN                  
237500     ELSE                                                                 
237600       IF MID-FLREFNYO = JA OR YES OR NEJ                                 
237700         IF MID-FLREFNYO = YES                                            
237800           MOVE JA                    TO MID-FLREFNYO                     
237900         END-IF                                                           
238000         MOVE JA                      TO WDK7-SW                          
238100         MOVE MFS-ALFA-FAELT-RAETT    TO MOD-FLREFNYO-IN-ATTR             
238200       ELSE                                                               
238300         MOVE NEJ                     TO INDATA-SW                        
238400         MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                     
238500         MOVE MFS-ALFA-FAELT-FEL      TO MOD-FLREFNYO-IN-ATTR             
238600       END-IF                                                             
238700     END-IF                                                               
238800                                                                          
238900*------KOLLA PÅFYLLNADSPUNKT                                              
239000     IF MID-KVREFPKT = ALL '+'                                            
239100       MOVE MFS-RENSA-FAELT           TO MOD-KVREFPKT-IN                  
239200     ELSE                                                                 
239300       MOVE MID-KVREFPKT              TO WS-KVREFPKT                      
239400       INSPECT WS-KVREFPKT REPLACING LEADING SPACE BY ZERO                
239500       IF  WS-KVREFPKT NUMERIC                                            
239600       AND WS-KVREFPKT > ZERO                                             
239700         MOVE MFS-NUM-FAELT-RAETT     TO MOD-KVREFPKT-IN-ATTR             
239800         MOVE JA                      TO WDK7-SW                          
239900       ELSE                                                               
240000         MOVE NEJ                     TO INDATA-SW                        
240100         MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                     
240200         MOVE MFS-NUM-FAELT-FEL       TO MOD-KVREFPKT-IN-ATTR             
240300       END-IF                                                             
240400     END-IF                                                               
240500                                                                          
240600     IF MID-TIREFPKT = ALL '+'                                            
240700        IF MID-KVREFPKT NOT = ALL '+'                                     
240800           MOVE NEJ                   TO INDATA-SW                        
240900           MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                     
241000           MOVE MFS-NUM-FAELT-FEL     TO MOD-TIREFPKT-IN-ATTR             
241100        ELSE                                                              
241200           MOVE MFS-RENSA-FAELT       TO MOD-TIREFPKT-IN                  
241300        END-IF                                                            
241400     ELSE                                                                 
241500       IF MID-TIREFPKT NUMERIC                                            
241600         IF MID-TIREFPKT = ZERO                                           
241700            MOVE MFS-NUM-FAELT-FEL    TO MOD-TIREFPKT-IN-ATTR             
241800         ELSE                                                             
241900           MOVE MID-TIREFPKT          TO TMP1-YYMMDD                      
242000           MOVE DAGENS-DATUM          TO TMP2-YYMMDD                      
242100           PERFORM WY2000P1                                               
242200           IF TMP1-YYMMDD >= TMP2-YYMMDD                                  
242300             MOVE 'AAMMDD'            TO DAT-KDDATFORM                    
242400             MOVE MID-TIREFPKT        TO DAT-I-TIDATUM                    
242500             CALL WDATKONV USING DAT-KDDATFORM                            
242600                                 DAT-I-TIDATUM                            
242700                                 DAT-O-TIDATUM DAT-KDSVAR                 
242800                                                                          
242900             IF DAT-KDSVAR-OK                                             
243000                MOVE MFS-NUM-FAELT-RAETT                                  
243100                                      TO MOD-TIREFPKT-IN-ATTR             
243200*--------------- KONTROLLERA ATT KVREFPKT > NOLL                          
243300                IF MID-KVREFPKT > ZERO                                    
243400                   MOVE JA            TO WDK7-SW                          
243500                ELSE                                                      
243600                   MOVE NEJ           TO INDATA-SW                        
243700                   MOVE ERR-CORR-HILITE-FLDS                              
243800                                      TO MED-IDMFSFEL                     
243900                   MOVE MFS-NUM-FAELT-FEL                                 
244000                                      TO MOD-KVREFPKT-IN-ATTR             
244100               END-IF                                                     
244200             ELSE                                                         
244300               MOVE NEJ               TO INDATA-SW                        
244400               MOVE ERR-CORR-HILITE-FLDS                                  
244500                                      TO MED-IDMFSFEL                     
244600               MOVE MFS-NUM-FAELT-FEL TO MOD-TIREFPKT-IN-ATTR             
244700                                                                          
244800             END-IF                                                       
244900           ELSE                                                           
245000             MOVE NEJ                 TO INDATA-SW                        
245100             MOVE ERR-CORR-HILITE-FLDS                                    
245200                                      TO MED-IDMFSFEL                     
245300             MOVE MFS-NUM-FAELT-FEL   TO MOD-TIREFPKT-IN-ATTR             
245400                                                                          
245500           END-IF                                                         
245600         END-IF                                                           
245700       ELSE                                                               
245800         MOVE NEJ                     TO INDATA-SW                        
245900         MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                     
246000         MOVE MFS-NUM-FAELT-FEL       TO MOD-TIREFPKT-IN-ATTR             
246100       END-IF                                                             
246200     END-IF                                                               
246300*                                                                         
246400*----- OM MANUELL BESTÄLLNINGSPUNKT ÄR IFYLLD SÅ FÅR INTE                 
246500*      BESTÄLLNINGSPRIS VARA 0 ,FÖR KINA FÅR INTE PRMATRL VARA 0          
246600*                                                                         
246700     IF (    INDATA-OK                                                    
246800         AND MID-TIREFPKT   NOT = ALL '+'                                 
246900         AND WS-PRIS            = ZERO )                                  
247000         MOVE NEJ                     TO INDATA-SW                        
247100         MOVE PRIS-SAKNAS             TO MED-IDMFSFEL                     
247200     END-IF                                                               
247300                                                                          
247400*------KOLLA PÅFYLLNADSKVANT                                              
247500     IF MID-KVREFBER = ALL '+'                                            
247600        MOVE MFS-RENSA-FAELT          TO MOD-KVREFBER-IN                  
247700     ELSE                                                                 
247800        MOVE MID-KVREFBER             TO WS-KVREFBER                      
247900        INSPECT WS-KVREFBER REPLACING LEADING SPACE BY ZERO               
248000        IF  MID-KVREFBER NUMERIC                                          
248100        AND MID-KVREFBER > ZERO                                           
248200            MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVREFBER-IN-ATTR             
248300            MOVE JA                   TO WDK7-SW                          
248400        ELSE                                                              
248500            MOVE NEJ                  TO INDATA-SW                        
248600            MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                     
248700            MOVE MFS-NUM-FAELT-FEL    TO MOD-KVREFBER-IN-ATTR             
248800        END-IF                                                            
248900     END-IF                                                               
249000                                                                          
249100     IF MID-TIREFPAF = ALL '+'                                            
249200       IF MID-KVREFBER NOT = ALL '+'                                      
249300         MOVE NEJ                     TO INDATA-SW                        
249400         MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                     
249500         MOVE MFS-NUM-FAELT-FEL       TO MOD-TIREFPAF-IN-ATTR             
249600       ELSE                                                               
249700         MOVE MFS-RENSA-FAELT         TO MOD-TIREFPAF-IN                  
249800       END-IF                                                             
249900     ELSE                                                                 
250000       IF MID-TIREFPAF NUMERIC                                            
250100         IF MID-TIREFPAF = ZERO                                           
250200            MOVE MFS-NUM-FAELT-FEL    TO MOD-TIREFPAF-IN-ATTR             
250300         ELSE                                                             
250400           MOVE MID-TIREFPAF          TO TMP1-YYMMDD                      
250500           MOVE DAGENS-DATUM          TO TMP2-YYMMDD                      
250600           PERFORM WY2000P1                                               
250700           IF TMP1-YYMMDD >= TMP2-YYMMDD                                  
250800             MOVE 'AAMMDD'            TO DAT-KDDATFORM                    
250900             MOVE MID-TIREFPAF        TO DAT-I-TIDATUM                    
251000             CALL WDATKONV USING DAT-KDDATFORM                            
251100                                 DAT-I-TIDATUM                            
251200                                 DAT-O-TIDATUM DAT-KDSVAR                 
251300                                                                          
251400             IF DAT-KDSVAR-OK                                             
251500               MOVE MFS-NUM-FAELT-RAETT                                   
251600                                      TO MOD-TIREFPAF-IN-ATTR             
251700*--------------- KONTROLLERA ATT KVREFBER > NOLL                          
251800               IF MID-KVREFBER > ZERO                                     
251900                 MOVE JA              TO WDK7-SW                          
252000               ELSE                                                       
252100                 MOVE NEJ             TO INDATA-SW                        
252200                 MOVE ERR-CORR-HILITE-FLDS                                
252300                                      TO MED-IDMFSFEL                     
252400                 MOVE MFS-NUM-FAELT-FEL                                   
252500                                      TO MOD-KVREFBER-IN-ATTR             
252600               END-IF                                                     
252700             ELSE                                                         
252800               MOVE NEJ               TO INDATA-SW                        
252900               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
253000               MOVE MFS-NUM-FAELT-FEL TO MOD-TIREFPAF-IN-ATTR             
253100                                                                          
253200             END-IF                                                       
253300           ELSE                                                           
253400             MOVE NEJ                 TO INDATA-SW                        
253500             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
253600             MOVE MFS-NUM-FAELT-FEL   TO MOD-TIREFPAF-IN-ATTR             
253700           END-IF                                                         
253800         END-IF                                                           
253900       ELSE                                                               
254000         MOVE NEJ                     TO INDATA-SW                        
254100         MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                     
254200         MOVE MFS-NUM-FAELT-FEL       TO MOD-TIREFPAF-IN-ATTR             
254300       END-IF                                                             
254400     END-IF                                                               
254500*                                                                         
254600*----- OM MANUELL BESTÄLLNINGSKVANT ÄR IFYLLD SÅ FÅR INTE                 
254700*      BESTÄLLNINGSPRIS VARA 0,FÖR KINA FÅR INTE PRMATRL VARA 0           
254800*                                                                         
254900     IF (    INDATA-OK                                                    
255000         AND MID-TIREFPAF   NOT = ALL '+'                                 
255100         AND WS-PRIS            = ZERO )                                  
255200         MOVE NEJ                     TO INDATA-SW                        
255300         MOVE PRIS-SAKNAS             TO MED-IDMFSFEL                     
255400     END-IF                                                               
255500                                                                          
255600                                                                          
255700*--- KOLLA IDPERSON-BUY                                                   
255800                                                                          
255900     IF MID-IDPERSON-BUY = ALL '+'                                        
256000       MOVE MFS-RENSA-FAELT           TO MOD-IDPERSON-BUY-IN              
256100     ELSE                                                                 
256200       IF BUY-LOCKED                                                      
256300         MOVE NEJ TO INDATA-SW                                            
256400         MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                     
256500         MOVE MFS-NUM-FAELT-FEL       TO MOD-IDPERSON-BUY-IN-ATTR         
256600       ELSE                                                               
256700         MOVE MID-IDPERSON-BUY        TO WS-IDPERSON-BUY                  
256800         INSPECT WS-IDPERSON-BUY REPLACING LEADING SPACE BY ZERO          
256900         IF WS-IDPERSON-BUY NUMERIC                                       
257500           MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDPERSON-BUY-IN-ATTR         
257600           MOVE JA                    TO WDK7-SW                          
257700         ELSE                                                             
257800           MOVE NEJ                   TO INDATA-SW                        
257900           MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                     
258000           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDPERSON-BUY-IN-ATTR         
258100         END-IF                                                           
258200       END-IF                                                             
258300     END-IF                                                               
258400                                                                          
258500*------KOLLA FLBUYUPD                                                     
258600     IF MID-FLBUYUPD = ALL '+'                                            
258700       MOVE MFS-RENSA-FAELT           TO MOD-FLBUYUPD-IN                  
258800     ELSE                                                                 
258900       IF MID-FLBUYUPD = JA OR YES OR NEJ                                 
259000         IF MID-FLBUYUPD = YES                                            
259100           MOVE JA                    TO MID-FLBUYUPD                     
259200         END-IF                                                           
259300         MOVE JA                      TO WDK7-SW                          
259400         MOVE MFS-ALFA-FAELT-RAETT    TO MOD-FLBUYUPD-IN-ATTR             
259500       ELSE                                                               
259600         MOVE NEJ                     TO INDATA-SW                        
259700         MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                     
259800         MOVE MFS-ALFA-FAELT-FEL      TO MOD-FLBUYUPD-IN-ATTR             
259900       END-IF                                                             
260000     END-IF                                                               
260100                                                                          
260200*------KOLLA FLTABUPD                                                     
260300     IF MID-FLTABUPD = ALL '+'                                            
260400       MOVE MFS-RENSA-FAELT           TO MOD-FLTABUPD-IN                  
260500     ELSE                                                                 
260600       IF MID-FLTABUPD = JA OR YES OR NEJ                                 
260700         IF MID-FLTABUPD = YES                                            
260800           MOVE JA                    TO MID-FLTABUPD                     
260900         END-IF                                                           
261000         MOVE JA                      TO WDK7-SW                          
261100         MOVE MFS-ALFA-FAELT-RAETT    TO MOD-FLTABUPD-IN-ATTR             
261200       ELSE                                                               
261300         MOVE NEJ                     TO INDATA-SW                        
261400         MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                     
261500         MOVE MFS-ALFA-FAELT-FEL      TO MOD-FLTABUPD-IN-ATTR             
261600       END-IF                                                             
261700     END-IF                                                               
261800                                                                          
261900*TEREFMED                                                                 
262000                                                                          
262100     IF MID-TEREFMED = ALL '+'                                            
262200       MOVE MFS-ALFA-FAELT-RAETT      TO MOD-TEREFMED-ATTR                
262300     ELSE                                                                 
262400       MOVE MFS-ALFA-FAELT-RAETT      TO MOD-TEREFMED-ATTR                
262500     END-IF                                                               
262600                                                                          
262700     IF INDATA-OK                                                         
262800        IF (MID-KVPB-JUST-1 NOT = ALL '+'                                 
262900        OR  MID-TIPBJUST-1  NOT = ALL '+'                                 
263000        OR  MID-KVPB-JUST-2 NOT = ALL '+'                                 
263100        OR  MID-TIPBJUST-2  NOT = ALL '+')                                
263200            PERFORM GCA-FORECAST-FUTURE-CONTROLS                          
263300        END-IF                                                            
263400     END-IF                                                               
263500                                                                          
263600     IF INDATA-FEL                                                        
263700        CALL WMEDKONV              USING MED-WMEDAREA                     
263800        MOVE MED-MFSFEL               TO MOD-TEMFSFEL                     
263900     END-IF                                                               
264000     .                                                                    
264100     EJECT                                                                
264200                                                                          
264300 GCA-FORECAST-FUTURE-CONTROLS SECTION.                                    
264400                                                                          
264500     MOVE NEJ                         TO WDK727-SW                        
264600     PERFORM GCAA-GENERAL-CONTROLS                                        
264700     IF INDATA-OK                                                         
264800          PERFORM GCAB-KVPBJUST-CONTROL                                   
264900          IF INDATA-OK                                                    
265000             PERFORM GCAC-TIPBJUST-CONTROL                                
265100          END-IF                                                          
265200     END-IF                                                               
265300     .                                                                    
265400     EJECT                                                                
265500 GCAA-GENERAL-CONTROLS SECTION.                                           
265600                                                                          
265700     IF (MID-KVPB-JUST-1 NOT = ALL '+'                                    
265800     OR  MID-TIPBJUST-1  NOT = ALL '+'                                    
265900     OR  MID-KVPB-JUST-2 NOT = ALL '+'                                    
266000     OR  MID-TIPBJUST-2  NOT = ALL '+')                                   
266100         IF K711-MISSING                                                  
266200            MOVE NEJ                  TO INDATA-SW                        
266300            MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                     
266400            MOVE MFS-ALFA-FAELT-FEL   TO MOD-IN-KVPB-JUST-1-ATTR          
266500                                         MOD-IN-KVPB-JUST-2-ATTR          
266600                                         MOD-IN-TIPBJUST-1-ATTR           
266700                                         MOD-IN-TIPBJUST-2-ATTR           
266800            MOVE MFS-RENSA-FAELT      TO MOD-IN-KVPB-JUST-1               
266900                                         MOD-IN-TIPBJUST-1                
267000                                         MOD-IN-KVPB-JUST-2               
267100                                         MOD-IN-TIPBJUST-2                
267200         END-IF                                                           
267300     END-IF                                                               
267400*                                                                         
267500     IF   MID-KVPB-JUST-1 NOT = ALL '+'                                   
267600     AND (MID-KVPB-JUST-2 NOT = ALL '+'                                   
267700     OR   MID-TIPBJUST-2  NOT = ALL '+')                                  
267800         MOVE NEJ                     TO INDATA-SW                        
267900         MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                     
268000         MOVE MFS-ALFA-FAELT-FEL      TO MOD-IN-KVPB-JUST-1-ATTR          
268100                                         MOD-IN-KVPB-JUST-2-ATTR          
268200                                         MOD-IN-TIPBJUST-2-ATTR           
268300     ELSE                                                                 
268400       IF  MID-KVPB-JUST-2 NOT = ALL '+'                                  
268500       AND MID-TIPBJUST-1  NOT = ALL '+'                                  
268600            MOVE NEJ                  TO INDATA-SW                        
268700            MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                     
268800            MOVE MFS-ALFA-FAELT-FEL   TO MOD-IN-TIPBJUST-1-ATTR           
268900                                         MOD-IN-KVPB-JUST-2-ATTR          
269000       END-IF                                                             
269100     END-IF                                                               
269200*                                                                         
269300*    IF  ARTIKEL-ERS-MAERKT                                               
269400*    AND (MID-KVPB-JUST-1 = ZERO                                          
269500*    OR   MID-KVPB-JUST-2 = ZERO)                                         
269600*      CONTINUE                                                           
269700*    ELSE                                                                 
269800       IF ARTIKEL-ERS-MAERKT                                              
269900       AND (MID-KVPB-JUST-1 NOT = ALL '+'                                 
270000       OR MID-KVPB-JUST-2 NOT = ALL '+' )                                 
270100            MOVE NEJ                   TO INDATA-SW                       
270200            MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                    
270300            MOVE MFS-ALFA-FAELT-FEL    TO MOD-IN-KVPB-JUST-1-ATTR         
270400                                          MOD-IN-KVPB-JUST-2-ATTR         
270500                                          MOD-IN-TIPBJUST-1-ATTR          
270600                                          MOD-IN-TIPBJUST-2-ATTR          
270700       END-IF                                                             
270800*    END-IF                                                               
270900*                                                                         
271000     IF  K711-EXISTS                                                      
271100     AND SLAG-FLPB-FLYTT  = JA                                            
271200     AND (MID-KVPB-JUST-1 NOT = ALL '+'                                   
271300     OR   MID-KVPB-JUST-2 NOT = ALL '+')                                  
271310       IF MID-KVPB-JUST-1 = ZERO                                          
271320       OR MID-KVPB-JUST-2 = ZERO                                          
271330          CONTINUE                                                        
271340       ELSE                                                               
271400          MOVE NEJ                     TO INDATA-SW                       
271500          MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                    
271600          MOVE MFS-ALFA-FAELT-FEL      TO MOD-IN-KVPB-JUST-1-ATTR         
271700                                          MOD-IN-KVPB-JUST-2-ATTR         
271800                                          MOD-IN-TIPBJUST-1-ATTR          
271900                                          MOD-IN-TIPBJUST-2-ATTR          
271910       END-IF                                                             
272000     END-IF                                                               
272100     .                                                                    
272200     EJECT                                                                
272300 GCAB-KVPBJUST-CONTROL SECTION.                                           
272400                                                                          
272500     IF MID-KVPB-JUST-1 NOT    = ALL '+'                                  
272600       IF (MID-KVPB-JUST-1     = ZERO                                     
272700       AND WS-K727-KVPB-JUST-2 > ZERO)                                    
272800         MOVE NEJ                  TO INDATA-SW                           
272900         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
273000         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IN-KVPB-JUST-1-ATTR             
273100                                      MOD-IN-TIPBJUST-1-ATTR              
273200       ELSE                                                               
273300         IF MID-KVPB-JUST-1 = ZERO                                        
273400           MOVE ZERO               TO WS-KVPB-JUST-1                      
273500                                      WS-TIPBJUST-1                       
273600                                      MID-TIPBJUST-1                      
273700           MOVE JA                 TO WDK727-SW                           
273800         ELSE                                                             
273900           MOVE MID-KVPB-JUST-1    TO DEC-IDFRIDATA                       
274000           MOVE 6                  TO DEC-KVHELTAL                        
274100           MOVE 1                  TO DEC-KVDECIMAL                       
274200           CALL WDECEDIT        USING DEC-WDECAREA                        
274300           IF DEC-KDSVAR-OK                                               
274400              MOVE DEC-IDEDITDATA  TO WS-KVPB-JUST-1                      
274500              MOVE JA              TO WDK727-SW                           
274600           ELSE                                                           
274700              MOVE MFS-NUM-FAELT-FEL                                      
274800                                   TO MOD-IN-KVPB-JUST-1-ATTR             
274900              MOVE NEJ             TO INDATA-SW                           
275000              MOVE ERR-CORR-HILITE-FLDS                                   
275100                                   TO MED-IDMFSFEL                        
275200           END-IF                                                         
275300         END-IF                                                           
275400       END-IF                                                             
275500     ELSE                                                                 
275600       IF MID-KVPB-JUST-2 NOT = ALL '+'                                   
275700         IF MID-KVPB-JUST-2   = ZERO                                      
275800           MOVE ZERO                   TO WS-KVPB-JUST-2                  
275900                                          WS-TIPBJUST-2                   
276000                                          MID-TIPBJUST-2                  
276100           MOVE JA                     TO WDK727-SW                       
276200         ELSE                                                             
276300          IF WS-K727-KVPB-JUST-1 NOT > ZERO                               
276400             MOVE NEJ                  TO INDATA-SW                       
276500             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
276600             MOVE MFS-ALFA-FAELT-FEL   TO MOD-IN-KVPB-JUST-2-ATTR         
276700                                          MOD-IN-TIPBJUST-2-ATTR          
276800          ELSE                                                            
276900             MOVE MID-KVPB-JUST-2      TO DEC-IDFRIDATA                   
277000             MOVE 6                    TO DEC-KVHELTAL                    
277100             MOVE 1                    TO DEC-KVDECIMAL                   
277200             CALL WDECEDIT          USING DEC-WDECAREA                    
277300             IF DEC-KDSVAR-OK                                             
277400                MOVE DEC-IDEDITDATA    TO WS-KVPB-JUST-2                  
277500                MOVE JA                TO WDK727-SW                       
277600             ELSE                                                         
277700                MOVE MFS-NUM-FAELT-FEL TO                                 
277800                                    MOD-IN-KVPB-JUST-2-ATTR               
277900                MOVE NEJ               TO INDATA-SW                       
278000                MOVE ERR-CORR-HILITE-FLDS                                 
278100                                       TO MED-IDMFSFEL                    
278200             END-IF                                                       
278300          END-IF                                                          
278400         END-IF                                                           
278500       END-IF                                                             
278600     END-IF                                                               
278700     .                                                                    
278800     EJECT                                                                
278900 GCAC-TIPBJUST-CONTROL SECTION.                                           
279000                                                                          
279100*****PB-JUST-1                                                            
279200     IF MID-KVPB-JUST-1 = ALL '+'                                         
279300        MOVE MFS-RENSA-FAELT          TO MOD-IN-KVPB-JUST-1               
279400        MOVE MFS-NUM-FAELT-RAETT      TO MOD-IN-KVPB-JUST-1-ATTR          
279500     ELSE                                                                 
279600        MOVE MFS-ROER-EJ-FAELT        TO MOD-IN-KVPB-JUST-1               
279700     END-IF                                                               
279800                                                                          
279900********TIPBJUST-1                                                        
280000     IF   MID-TIPBJUST-1 = ALL '+'                                        
280100     AND (WS-K727-KVPB-JUST-1 NOT > ZERO                                  
280200     AND  WS-KVPB-JUST-1          > ZERO)                                 
280300          MOVE NEJ                    TO INDATA-SW                        
280400          MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                     
280500          MOVE MFS-ALFA-FAELT-FEL     TO MOD-IN-KVPB-JUST-1-ATTR          
280600                                         MOD-IN-TIPBJUST-1-ATTR           
280700     ELSE                                                                 
280800        IF MID-TIPBJUST-1 = ALL '+'                                       
280900           MOVE MFS-RENSA-FAELT       TO MOD-IN-KVPB-JUST-1               
281000           MOVE MFS-NUM-FAELT-RAETT   TO MOD-IN-TIPBJUST-1-ATTR           
281100        END-IF                                                            
281200     END-IF                                                               
281300     IF INDATA-OK                                                         
281400       IF MID-TIPBJUST-1 NOT = ALL '+'                                    
281500        IF MID-TIPBJUST-1      = ALL ZERO                                 
281600           IF MID-KVPB-JUST-1  = ZERO                                     
281700              MOVE MFS-NUM-FAELT-RAETT                                    
281800                                      TO MOD-IN-TIPBJUST-1-ATTR           
281900           ELSE                                                           
282000              MOVE NEJ                TO INDATA-SW                        
282100              MOVE ERR-CORR-HILITE-FLDS                                   
282200                                      TO MED-IDMFSFEL                     
282300              MOVE MFS-ALFA-FAELT-FEL TO MOD-IN-TIPBJUST-1-ATTR           
282400                                         MOD-IN-KVPB-JUST-1-ATTR          
282500                                                                          
282600           END-IF                                                         
282700        ELSE                                                              
282800           IF MID-TIPBJUST-1 NUMERIC                                      
282900              IF  WS-K727-KVPB-JUST-1 NOT > ZERO                          
283000              AND WS-KVPB-JUST-1      NOT > ZERO                          
283100                  MOVE NEJ            TO INDATA-SW                        
283200                  MOVE ERR-CORR-HILITE-FLDS                               
283300                                      TO MED-IDMFSFEL                     
283400                  MOVE MFS-ALFA-FAELT-FEL                                 
283500                                      TO MOD-IN-KVPB-JUST-1-ATTR          
283600                                         MOD-IN-TIPBJUST-1-ATTR           
283700              ELSE                                                        
283800                  MOVE MID-TIPBJUST-1 TO WS-TIPBJUST-1                    
283900                  MOVE WS-TIPBJUST-1  TO DAT-I-TIDATUM                    
284000                  MOVE 'AAVV  '       TO DAT-KDDATFORM                    
284100                  CALL WDATKONV USING DAT-KDDATFORM                       
284200                                      DAT-I-TIDATUM                       
284300                                      DAT-O-TIDATUM                       
284400                                      DAT-KDSVAR                          
284500                  IF DAT-KDSVAR-OK                                        
284600                     MOVE DAT-TIAARP         TO WS-TIAARP-1               
284700                     MOVE DAT-TIAAVV-GRP     TO WS-TIPBJUST-AAVV          
284800                     MOVE WS-TIPBJUST-AAVV   TO TMP1-YYWW                 
284900                     MOVE DAGENS-AAVV        TO TMP2-YYWW                 
285000                     PERFORM WY2000P3                                     
285100                     IF TMP1-YYWW > TMP2-YYWW                             
285200                        MOVE MFS-NUM-FAELT-RAETT                          
285300                                       TO MOD-IN-TIPBJUST-1-ATTR          
285400***CHECK THAT THE DATE IS WITHIN A YEAR                                   
285500                        ADD +100       TO TMP2-YYWW                       
285600                        IF TMP1-YYWW    < TMP2-YYWW                       
285700                           MOVE MFS-NUM-FAELT-RAETT                       
285800                                       TO MOD-IN-TIPBJUST-1-ATTR          
285900                           MOVE JA     TO WDK727-SW                       
286000                        ELSE                                              
286100                           MOVE MFS-NUM-FAELT-FEL                         
286200                                       TO MOD-IN-TIPBJUST-1-ATTR          
286300                           MOVE NEJ    TO INDATA-SW                       
286400                           MOVE ERR-MAX-1-YEAR                            
286500                                       TO MED-MFSFEL                      
286600                        END-IF                                            
286700                     ELSE                                                 
286800                        MOVE MFS-NUM-FAELT-FEL                            
286900                                       TO MOD-IN-TIPBJUST-1-ATTR          
287000                        MOVE NEJ       TO INDATA-SW                       
287100                        MOVE ERR-ONLY-FUTURE                              
287200                                       TO MED-MFSFEL                      
287300                     END-IF                                               
287400                     IF WS-K727-TIPBJUST-2-JMF > ZERO                     
287500                        IF WS-TIPBJUST-1 >                                
287600                           WS-K727-TIPBJUST-2-JMF                         
287700                           MOVE MFS-NUM-FAELT-FEL                         
287800                                       TO MOD-IN-TIPBJUST-1-ATTR          
287900                           MOVE NEJ    TO INDATA-SW                       
288000                           MOVE ERR-CORR-HILITE-FLDS                      
288100                                       TO MED-IDMFSFEL                    
288200                        END-IF                                            
288300                     END-IF                                               
288400                  ELSE                                                    
288500                     MOVE MFS-NUM-FAELT-FEL                               
288600                                       TO MOD-IN-TIPBJUST-1-ATTR          
288700                     MOVE NEJ          TO INDATA-SW                       
288800                     MOVE ERR-CORR-HILITE-FLDS                            
288900                                       TO MED-IDMFSFEL                    
289000                  END-IF                                                  
289100              END-IF                                                      
289200           ELSE                                                           
289300              MOVE NEJ               TO INDATA-SW                         
289400              MOVE MFS-NUM-FAELT-FEL TO MOD-IN-TIPBJUST-1-ATTR            
289500              MOVE ERR-CORR-HILITE-FLDS                                   
289600                                     TO MED-IDMFSFEL                      
289700           END-IF                                                         
289800        END-IF                                                            
289900        MOVE MFS-ROER-EJ-FAELT       TO MOD-IN-TIPBJUST-1                 
290000       END-IF                                                             
290100     END-IF                                                               
290200                                                                          
290300*PB-JUSTERING-2                                                           
290400                                                                          
290500     IF MID-KVPB-JUST-2 = ALL '+'                                         
290600        MOVE MFS-RENSA-FAELT          TO                                  
290700                       MOD-IN-KVPB-JUST-2                                 
290800        MOVE MFS-NUM-FAELT-RAETT      TO                                  
290900                       MOD-IN-KVPB-JUST-2-ATTR                            
291000     END-IF                                                               
291100                                                                          
291200                                                                          
291300********TIPBJUST-2                                                        
291400     IF   MID-TIPBJUST-2 = ALL '+'                                        
291500     AND (WS-K727-KVPB-JUST-2 NOT > ZERO                                  
291600     AND  WS-KVPB-JUST-2          > ZERO)                                 
291700          MOVE NEJ                    TO INDATA-SW                        
291800          MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                     
291900          MOVE MFS-ALFA-FAELT-FEL     TO MOD-IN-KVPB-JUST-2-ATTR          
292000                                         MOD-IN-TIPBJUST-2-ATTR           
292100     ELSE                                                                 
292200        IF MID-TIPBJUST-2 = ALL '+'                                       
292300           MOVE MFS-RENSA-FAELT       TO MOD-IN-KVPB-JUST-2               
292400           MOVE MFS-NUM-FAELT-RAETT   TO MOD-IN-TIPBJUST-2-ATTR           
292500        END-IF                                                            
292600     END-IF                                                               
292700     IF INDATA-OK                                                         
292800       IF MID-TIPBJUST-2 NOT = ALL '+'                                    
292900        IF MID-TIPBJUST-2 = ALL ZERO                                      
293000           IF MID-KVPB-JUST-2  = ZERO                                     
293100              MOVE MFS-NUM-FAELT-RAETT                                    
293200                                      TO MOD-IN-TIPBJUST-2-ATTR           
293300           ELSE                                                           
293400              MOVE NEJ                TO INDATA-SW                        
293500              MOVE ERR-CORR-HILITE-FLDS                                   
293600                                      TO MED-IDMFSFEL                     
293700              MOVE MFS-ALFA-FAELT-FEL                                     
293800                                      TO MOD-IN-KVPB-JUST-2-ATTR          
293900                                         MOD-IN-TIPBJUST-2-ATTR           
294000           END-IF                                                         
294100        ELSE                                                              
294200           IF MID-TIPBJUST-2 NUMERIC                                      
294300              IF  WS-K727-KVPB-JUST-2 NOT > ZERO                          
294400              AND WS-KVPB-JUST-2      NOT > ZERO                          
294500                  MOVE NEJ            TO INDATA-SW                        
294600                  MOVE ERR-CORR-HILITE-FLDS                               
294700                                      TO MED-IDMFSFEL                     
294800                  MOVE MFS-ALFA-FAELT-FEL                                 
294900                                      TO MOD-IN-KVPB-JUST-2-ATTR          
295000                                         MOD-IN-TIPBJUST-2-ATTR           
295100              ELSE                                                        
295200                  MOVE MID-TIPBJUST-2     TO WS-TIPBJUST-2                
295300                  MOVE WS-TIPBJUST-2      TO DAT-I-TIDATUM                
295400                  MOVE 'AAVV  '           TO DAT-KDDATFORM                
295500                  CALL WDATKONV USING DAT-KDDATFORM                       
295600                                      DAT-I-TIDATUM                       
295700                                      DAT-O-TIDATUM                       
295800                                      DAT-KDSVAR                          
295900                  IF DAT-KDSVAR-OK                                        
296000                     MOVE DAT-TIAARP         TO WS-TIAARP-2               
296100                     MOVE DAT-TIAAVV-GRP     TO WS-TIPBJUST-AAVV          
296200                     MOVE WS-TIPBJUST-AAVV   TO TMP1-YYWW                 
296300                     MOVE DAGENS-AAVV        TO TMP2-YYWW                 
296400                     PERFORM WY2000P3                                     
296500                     IF TMP1-YYWW > TMP2-YYWW                             
296600                        MOVE MFS-NUM-FAELT-RAETT                          
296700                                       TO MOD-IN-TIPBJUST-2-ATTR          
296800***CHECK THAT THE DATE IS WITHIN A YEAR                                   
296900                        ADD +100       TO TMP2-YYWW                       
297000                        IF TMP1-YYWW    < TMP2-YYWW                       
297100                           MOVE MFS-NUM-FAELT-RAETT                       
297200                                       TO MOD-IN-TIPBJUST-2-ATTR          
297300                           MOVE JA     TO WDK727-SW                       
297400                        ELSE                                              
297500                           MOVE MFS-NUM-FAELT-FEL                         
297600                                       TO MOD-IN-TIPBJUST-2-ATTR          
297700                           MOVE NEJ    TO INDATA-SW                       
297800                           MOVE ERR-MAX-1-YEAR                            
297900                                       TO MED-MFSFEL                      
298000                        END-IF                                            
298100                     ELSE                                                 
298200                        MOVE MFS-NUM-FAELT-FEL                            
298300                                       TO MOD-IN-TIPBJUST-2-ATTR          
298400                        MOVE NEJ       TO INDATA-SW                       
298500                        MOVE ERR-ONLY-FUTURE                              
298600                                       TO MED-MFSFEL                      
298700                     END-IF                                               
298800                     IF WS-TIPBJUST-2 <                                   
298900                        WS-K727-TIPBJUST-1-JMF                            
299000                        MOVE MFS-NUM-FAELT-FEL                            
299100                                       TO MOD-IN-TIPBJUST-2-ATTR          
299200                        MOVE NEJ       TO INDATA-SW                       
299300                        MOVE ERR-CORR-HILITE-FLDS                         
299400                                       TO MED-IDMFSFEL                    
299500                     END-IF                                               
299600                  ELSE                                                    
299700                     MOVE MFS-NUM-FAELT-FEL                               
299800                                       TO MOD-IN-TIPBJUST-2-ATTR          
299900                     MOVE NEJ          TO INDATA-SW                       
300000                     MOVE ERR-CORR-HILITE-FLDS                            
300100                                       TO MED-IDMFSFEL                    
300200                  END-IF                                                  
300300              END-IF                                                      
300400           ELSE                                                           
300500              MOVE NEJ                 TO INDATA-SW                       
300600              MOVE MFS-NUM-FAELT-FEL                                      
300700                                       TO MOD-IN-TIPBJUST-2-ATTR          
300800              MOVE ERR-CORR-HILITE-FLDS                                   
300900                                       TO MED-MFSFEL                      
301000           END-IF                                                         
301100        END-IF                                                            
301200        MOVE MFS-ROER-EJ-FAELT         TO MOD-IN-TIPBJUST-2               
301300       END-IF                                                             
301400     END-IF                                                               
301500                                                                          
301600*****COMMON CHECK                                                         
301700     IF INDATA-OK                                                         
301800        IF  MID-TIPBJUST-1 NOT = ALL '+'                                  
301900        AND MID-TIPBJUST-2 NOT = ALL '+'                                  
302000            MOVE NEJ                TO INDATA-SW                          
302100            MOVE ERR-CORR-HILITE-FLDS                                     
302200                                    TO MED-IDMFSFEL                       
302300            MOVE MFS-ALFA-FAELT-FEL                                       
302400                                    TO MOD-IN-TIPBJUST-1-ATTR             
302500                                       MOD-IN-TIPBJUST-2-ATTR             
302600        END-IF                                                            
302700     END-IF                                                               
302800     .                                                                    
302900     EJECT                                                                
303000                                                                          
303100 GD-CDC-DATA-INPUT  SECTION.                                              
303200                                                                          
303300     IF MID-INPUT = ALL '+' AND SEGMENT-FINNS                             
303400       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
303500       CALL WMEDKONV USING MED-WMEDAREA                                   
303600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
303700       PERFORM MFS-ROER-EJ-FAELT-IN                                       
303800       PERFORM MFS-ROER-EJ-FAELT-UT                                       
303900       MOVE NEJ TO INDATA-SW                                              
304000     ELSE                                                                 
304100       IF INDATA-OK                                                       
304200         IF ARTIKEL-ERS-MAERKT                                            
304300           PERFORM GA-HUVUDFLAGGOR-ERSATT-ARTIKEL                         
304400         ELSE                                                             
304500           PERFORM GDB-SETFLAG-ARTICLE                                    
304600         END-IF                                                           
304700         PERFORM GDC-KOLLA-OVR-DATAELEMENT                                
304800         PERFORM GDD-VALIDATE-FUT-FORECAST                                
304900       END-IF                                                             
305000       IF INDATA-FEL                                                      
305100         PERFORM MFS-ROER-EJ-FAELT-IN                                     
305200         PERFORM MFS-ROER-EJ-FAELT-UT                                     
305300       END-IF                                                             
305400     END-IF                                                               
305500       .                                                                  
305600       EJECT                                                              
305700 GDB-SETFLAG-ARTICLE SECTION.                                             
305800                                                                          
305900       IF MID-FLREFILL = ALL '+'                                          
306000         MOVE MFS-RENSA-FAELT            TO MOD-FLREFILL-IN               
306100       ELSE                                                               
306200         IF MID-FLREFILL = JA OR YES                                      
306300            MOVE JA                      TO MID-FLREFILL                  
306400            PERFORM S01-KOLLA-OM-REFILL-OK                                
306500            IF REFILL-ART-OK                                              
306600               MOVE JA                   TO WDK6-SW                       
306700               MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLREFILL-IN-ATTR          
306800            ELSE                                                          
306900               MOVE NEJ                  TO INDATA-SW                     
307000               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
307100               MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLREFILL-IN-ATTR          
307200            END-IF                                                        
307300         ELSE                                                             
307400           MOVE NEJ TO INDATA-SW                                          
307500           MOVE ERR-CORR-HILITE-FLDS     TO MED-IDMFSFEL                  
307600           MOVE MFS-ALFA-FAELT-FEL       TO MOD-FLREFILL-IN-ATTR          
307700         END-IF                                                           
307800       END-IF                                                             
307900                                                                          
308000       IF MID-FLREFBEO = ALL '+'                                          
308100         MOVE MFS-RENSA-FAELT            TO MOD-FLREFBEO-IN               
308200       ELSE                                                               
308300                                                                          
308400           IF CLAG-IDDC-REF > SPACES                                      
308500*----                                                                     
308600*---- EJ LOKAL ARTIKEL                                                    
308700*----                                                                     
308800             IF MID-FLREFBEO = JA                                         
308900             OR MID-FLREFBEO = YES                                        
309000             OR MID-FLREFBEO = NEJ                                        
309100             OR MID-FLREFBEO = 'S'                                        
309200               IF MID-FLREFBEO = YES                                      
309300                 MOVE JA TO MID-FLREFBEO                                  
309400               END-IF                                                     
309500               MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLREFBEO-IN-ATTR          
309600             ELSE                                                         
309700               MOVE NEJ TO INDATA-SW                                      
309800               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
309900               MOVE MFS-ALFA-FAELT-FEL TO MOD-FLREFBEO-IN-ATTR            
310000             END-IF                                                       
310100           ELSE                                                           
310200*----                                                                     
310300*---- LOKAL ARTIKEL                                                       
310400*----                                                                     
310500             IF MID-FLREFBEO = NEJ                                        
310600               MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLREFBEO-IN-ATTR          
310700             ELSE                                                         
310800               MOVE NEJ TO INDATA-SW                                      
310900               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
311000               MOVE MFS-ALFA-FAELT-FEL TO MOD-FLREFBEO-IN-ATTR            
311100             END-IF                                                       
311200           END-IF                                                         
311300           MOVE JA TO WDK6-SW                                             
311400       END-IF                                                             
311500                                                                          
311600                                                                          
311700*--- KOLLA BEORDRINGSSTOPP                                                
311800                                                                          
311900       IF MID-TIREFSTO = ALL '+'                                          
312000         MOVE MFS-RENSA-FAELT TO MOD-TIREFSTO-IN                          
312100       ELSE                                                               
312200         IF MID-TIREFSTO NUMERIC                                          
312300           IF MID-TIREFSTO = ZERO                                         
312400             MOVE MFS-NUM-FAELT-RAETT TO                                  
312500                                      MOD-TIREFSTO-IN-ATTR                
312600           ELSE                                                           
312700             MOVE 'AAMMDD'     TO DAT-KDDATFORM                           
312800             MOVE MID-TIREFSTO TO DAT-I-TIDATUM                           
312900             CALL WDATKONV USING DAT-KDDATFORM                            
313000                                 DAT-I-TIDATUM                            
313100                                 DAT-O-TIDATUM DAT-KDSVAR                 
313200                                                                          
313300             IF DAT-KDSVAR-OK                                             
313400                MOVE MID-TIREFSTO    TO TMP1-YYMMDD                       
313500                MOVE DAGENS-DATUM    TO TMP2-YYMMDD                       
313600                MOVE DD-PLUS-SEX-MAN TO TMP3-YYMMDD                       
313700                PERFORM WY2000Q1                                          
313800                IF TMP1-YYMMDD >= TMP2-YYMMDD                             
313900                AND TMP1-YYMMDD < TMP3-YYMMDD                             
314000                  MOVE MFS-NUM-FAELT-RAETT TO                             
314100                                       MOD-TIREFSTO-IN-ATTR               
314200                ELSE                                                      
314300                  MOVE NEJ TO INDATA-SW                                   
314400                  MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL               
314500                  MOVE MFS-NUM-FAELT-FEL TO                               
314600                                        MOD-TIREFSTO-IN-ATTR              
314700                END-IF                                                    
314800             ELSE                                                         
314900               MOVE NEJ TO INDATA-SW                                      
315000               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
315100               MOVE MFS-NUM-FAELT-FEL TO                                  
315200                                      MOD-TIREFSTO-IN-ATTR                
315300             END-IF                                                       
315400           END-IF                                                         
315500           MOVE JA TO WDK6-SW                                             
315600         ELSE                                                             
315700           MOVE NEJ TO INDATA-SW                                          
315800           MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                     
315900           MOVE MFS-NUM-FAELT-FEL     TO                                  
316000                                      MOD-TIREFSTO-IN-ATTR                
316100         END-IF                                                           
316200       END-IF                                                             
316300       .                                                                  
316400       EJECT                                                              
316500                                                                          
316600 GDC-KOLLA-OVR-DATAELEMENT SECTION.                                       
316700                                                                          
316800     MOVE CLAG-PRARTSTD       TO WS-PRIS                                  
316900     MOVE NEJ                 TO BUY-SW                                   
317000     MOVE NEJ                 TO TAB-SW                                   
317100                                 SW-K629-EXISTS                           
317200     PERFORM IMS-GU-WDK61129                                              
317300     IF SEGMENT-FINNS                                                     
317400       MOVE JA                TO SW-K629-EXISTS                           
317500       MOVE K6-CREF-FLBUYUPD  TO BUY-SW                                   
317600       MOVE K6-CREF-FLTABUPD  TO TAB-SW                                   
317700     END-IF                                                               
317800                                                                          
317900*------KOLLA IDREFTAB                                                     
318000     IF MID-IDREFTAB = ALL '+'                                            
318100       MOVE MFS-RENSA-FAELT TO MOD-IDREFTAB-IN                            
318200     ELSE                                                                 
318300        IF TAB-LOCKED                                                     
318400          MOVE NEJ TO INDATA-SW                                           
318500          MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                       
318600          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDREFTAB-IN-ATTR                 
318700        ELSE                                                              
318800          MOVE DCS-IDDC TO W-2501-IDDC                                    
318900          MOVE MID-IDREFTAB TO W-2502-IDREFTAB                            
319000          PERFORM IMS-GU-2502                                             
319100          IF SEGMENT-FINNS                                                
319200             IF MID-IDREFTAB NUMERIC                                      
319300               MOVE JA TO WDK6-SW                                         
319400               MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDREFTAB-IN-ATTR          
319500             ELSE                                                         
319600               MOVE NEJ TO INDATA-SW                                      
319700               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
319800               MOVE MFS-ALFA-FAELT-FEL TO MOD-IDREFTAB-IN-ATTR            
319900             END-IF                                                       
320000          ELSE                                                            
320100             MOVE NEJ TO INDATA-SW                                        
320200             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
320300             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDREFTAB-IN-ATTR              
320400          END-IF                                                          
320500        END-IF                                                            
320600     END-IF                                                               
320700                                                                          
320800*------KOLLA FLWILSON                                                     
320900     IF MID-FLWILSON = ALL '+'                                            
321000       MOVE MFS-RENSA-FAELT TO MOD-FLWILSON-IN                            
321100     ELSE                                                                 
321200       IF MID-FLWILSON = JA OR NEJ                                        
321300         MOVE JA TO WDK6-SW                                               
321400         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLWILSON-IN-ATTR                
321500       ELSE                                                               
321600         MOVE NEJ TO INDATA-SW                                            
321700         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
321800         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLWILSON-IN-ATTR                
321900       END-IF                                                             
322000     END-IF                                                               
322100                                                                          
322200*------KOLLA FLPB-FLYTT                                                   
322300     IF MID-FLPB-FLYTT = ALL '+'                                          
322400       MOVE MFS-RENSA-FAELT TO MOD-FLPB-FLYTT-IN                          
322500     ELSE                                                                 
322600       IF MID-FLPB-FLYTT = JA OR YES OR NEJ                               
322700         IF MID-FLPB-FLYTT = YES                                          
322800           MOVE JA TO MID-FLPB-FLYTT                                      
322900         END-IF                                                           
323000         MOVE JA TO WDK6-SW                                               
323100         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLPB-FLYTT-IN-ATTR              
323200       ELSE                                                               
323300         MOVE NEJ TO INDATA-SW                                            
323400         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
323500         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLPB-FLYTT-IN-ATTR              
323600       END-IF                                                             
323700     END-IF                                                               
323800                                                                          
323900     IF MID-FLREFNYO = ALL '+'                                            
324000       MOVE MFS-RENSA-FAELT TO MOD-FLREFNYO-IN                            
324100     ELSE                                                                 
324200       IF MID-FLREFNYO = JA OR YES OR NEJ                                 
324300         IF MID-FLREFNYO = YES                                            
324400           MOVE JA TO MID-FLREFNYO                                        
324500         END-IF                                                           
324600         MOVE JA TO WDK6-SW                                               
324700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLREFNYO-IN-ATTR                
324800       ELSE                                                               
324900         MOVE NEJ TO INDATA-SW                                            
325000         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
325100         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLREFNYO-IN-ATTR                
325200       END-IF                                                             
325300     END-IF                                                               
325400                                                                          
325500*------KOLLA PÅFYLLNADSPUNKT                                              
325600     IF MID-KVREFPKT = ALL '+'                                            
325700       MOVE MFS-RENSA-FAELT TO MOD-KVREFPKT-IN                            
325800     ELSE                                                                 
325900       MOVE MID-KVREFPKT TO WS-KVREFPKT                                   
326000       INSPECT WS-KVREFPKT REPLACING LEADING SPACE BY ZERO                
326100       IF WS-KVREFPKT NUMERIC AND WS-KVREFPKT > ZERO                      
326200         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVREFPKT-IN-ATTR                 
326300       ELSE                                                               
326400         MOVE NEJ TO INDATA-SW                                            
326500         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
326600         MOVE MFS-NUM-FAELT-FEL   TO MOD-KVREFPKT-IN-ATTR                 
326700       END-IF                                                             
326800       MOVE JA TO WDK6-SW                                                 
326900     END-IF                                                               
327000                                                                          
327100     IF MID-TIREFPKT = ALL '+'                                            
327200       IF MID-KVREFPKT NOT = ALL '+'                                      
327300         MOVE NEJ TO INDATA-SW                                            
327400         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
327500         MOVE MFS-NUM-FAELT-FEL   TO MOD-TIREFPKT-IN-ATTR                 
327600       ELSE                                                               
327700         MOVE MFS-RENSA-FAELT TO MOD-TIREFPKT-IN                          
327800       END-IF                                                             
327900     ELSE                                                                 
328000       IF MID-TIREFPKT NUMERIC                                            
328100         IF MID-TIREFPKT = ZERO                                           
328200           MOVE MFS-NUM-FAELT-FEL TO                                      
328300                                    MOD-TIREFPKT-IN-ATTR                  
328400         ELSE                                                             
328500           MOVE MID-TIREFPKT   TO TMP1-YYMMDD                             
328600           MOVE DAGENS-DATUM   TO TMP2-YYMMDD                             
328700           PERFORM WY2000P1                                               
328800           IF TMP1-YYMMDD >= TMP2-YYMMDD                                  
328900             MOVE 'AAMMDD'     TO DAT-KDDATFORM                           
329000             MOVE MID-TIREFPKT TO DAT-I-TIDATUM                           
329100             CALL WDATKONV USING DAT-KDDATFORM                            
329200                                 DAT-I-TIDATUM                            
329300                                 DAT-O-TIDATUM DAT-KDSVAR                 
329400                                                                          
329500             IF DAT-KDSVAR-OK                                             
329600               MOVE MFS-NUM-FAELT-RAETT TO                                
329700                                     MOD-TIREFPKT-IN-ATTR                 
329800*--------------- KONTROLLERA ATT KVREFPKT > NOLL                          
329900               IF MID-KVREFPKT > ZERO                                     
330000                 CONTINUE                                                 
330100               ELSE                                                       
330200                 MOVE NEJ TO INDATA-SW                                    
330300                 MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                
330400                 MOVE MFS-NUM-FAELT-FEL TO                                
330500                                   MOD-KVREFPKT-IN-ATTR                   
330600               END-IF                                                     
330700             ELSE                                                         
330800               MOVE NEJ TO INDATA-SW                                      
330900               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
331000               MOVE MFS-NUM-FAELT-FEL TO                                  
331100                                     MOD-TIREFPKT-IN-ATTR                 
331200             END-IF                                                       
331300           ELSE                                                           
331400             MOVE NEJ TO INDATA-SW                                        
331500             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
331600             MOVE MFS-NUM-FAELT-FEL TO                                    
331700                                    MOD-TIREFPKT-IN-ATTR                  
331800           END-IF                                                         
331900         END-IF                                                           
332000       ELSE                                                               
332100         MOVE NEJ TO INDATA-SW                                            
332200         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
332300         MOVE MFS-NUM-FAELT-FEL     TO                                    
332400                                    MOD-TIREFPKT-IN-ATTR                  
332500       END-IF                                                             
332600       MOVE JA TO WDK6-SW                                                 
332700     END-IF                                                               
332800*                                                                         
332900*----- OM MANUELL BESTÄLLNINGSPUNKT ÄR IFYLLD SÅ FÅR INTE                 
333000*      BESTÄLLNINGSPRIS VARA 0 ,FÖR KINA FÅR INTE PRMATRL VARA 0          
333100*                                                                         
333200     IF (    INDATA-OK                                                    
333300         AND MID-TIREFPKT   NOT = ALL '+'                                 
333400         AND WS-PRIS            = ZERO )                                  
333500       MOVE NEJ             TO INDATA-SW                                  
333600       MOVE PRIS-SAKNAS     TO MED-IDMFSFEL                               
333700     END-IF                                                               
333800                                                                          
333900*------KOLLA PÅFYLLNADSKVANT                                              
334000     IF MID-KVREFBER = ALL '+'                                            
334100       MOVE MFS-RENSA-FAELT TO MOD-KVREFBER-IN                            
334200     ELSE                                                                 
334300       MOVE MID-KVREFBER TO WS-KVREFBER                                   
334400       INSPECT WS-KVREFBER REPLACING LEADING SPACE BY ZERO                
334500       IF MID-KVREFBER NUMERIC AND MID-KVREFBER > ZERO                    
334600         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVREFBER-IN-ATTR                 
334700       ELSE                                                               
334800         MOVE NEJ TO INDATA-SW                                            
334900         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
335000         MOVE MFS-NUM-FAELT-FEL   TO MOD-KVREFBER-IN-ATTR                 
335100       END-IF                                                             
335200       MOVE JA TO WDK6-SW                                                 
335300     END-IF                                                               
335400                                                                          
335500     IF MID-TIREFPAF = ALL '+'                                            
335600       IF MID-KVREFBER NOT = ALL '+'                                      
335700         MOVE NEJ TO INDATA-SW                                            
335800         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
335900         MOVE MFS-NUM-FAELT-FEL   TO MOD-TIREFPAF-IN-ATTR                 
336000       ELSE                                                               
336100         MOVE MFS-RENSA-FAELT TO MOD-TIREFPAF-IN                          
336200       END-IF                                                             
336300     ELSE                                                                 
336400       IF MID-TIREFPAF NUMERIC                                            
336500         IF MID-TIREFPAF = ZERO                                           
336600           MOVE MFS-NUM-FAELT-FEL TO                                      
336700                                    MOD-TIREFPAF-IN-ATTR                  
336800         ELSE                                                             
336900           MOVE MID-TIREFPAF   TO TMP1-YYMMDD                             
337000           MOVE DAGENS-DATUM   TO TMP2-YYMMDD                             
337100           PERFORM WY2000P1                                               
337200           IF TMP1-YYMMDD >= TMP2-YYMMDD                                  
337300             MOVE 'AAMMDD'     TO DAT-KDDATFORM                           
337400             MOVE MID-TIREFPAF TO DAT-I-TIDATUM                           
337500             CALL WDATKONV USING DAT-KDDATFORM                            
337600                                 DAT-I-TIDATUM                            
337700                                 DAT-O-TIDATUM DAT-KDSVAR                 
337800                                                                          
337900             IF DAT-KDSVAR-OK                                             
338000               MOVE MFS-NUM-FAELT-RAETT TO                                
338100                                     MOD-TIREFPAF-IN-ATTR                 
338200*--------------- KONTROLLERA ATT KVREFBER > NOLL                          
338300               IF MID-KVREFBER > ZERO                                     
338400                 CONTINUE                                                 
338500               ELSE                                                       
338600                 MOVE NEJ TO INDATA-SW                                    
338700                 MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                
338800                 MOVE MFS-NUM-FAELT-FEL TO                                
338900                                   MOD-KVREFBER-IN-ATTR                   
339000               END-IF                                                     
339100             ELSE                                                         
339200               MOVE NEJ TO INDATA-SW                                      
339300               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
339400               MOVE MFS-NUM-FAELT-FEL TO                                  
339500                                     MOD-TIREFPAF-IN-ATTR                 
339600             END-IF                                                       
339700           ELSE                                                           
339800             MOVE NEJ TO INDATA-SW                                        
339900             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
340000             MOVE MFS-NUM-FAELT-FEL TO                                    
340100                                    MOD-TIREFPAF-IN-ATTR                  
340200           END-IF                                                         
340300         END-IF                                                           
340400       ELSE                                                               
340500         MOVE NEJ TO INDATA-SW                                            
340600         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
340700         MOVE MFS-NUM-FAELT-FEL     TO                                    
340800                                    MOD-TIREFPAF-IN-ATTR                  
340900       END-IF                                                             
341000       MOVE JA TO WDK6-SW                                                 
341100     END-IF                                                               
341200*                                                                         
341300*----- OM MANUELL BESTÄLLNINGSKVANT ÄR IFYLLD SÅ FÅR INTE                 
341400*      BESTÄLLNINGSPRIS VARA 0,FÖR KINA FÅR INTE PRMATRL VARA 0           
341500*                                                                         
341600     IF (    INDATA-OK                                                    
341700         AND MID-TIREFPAF   NOT = ALL '+'                                 
341800         AND WS-PRIS            = ZERO )                                  
341900       MOVE NEJ            TO INDATA-SW                                   
342000       MOVE PRIS-SAKNAS    TO MED-IDMFSFEL                                
342100     END-IF                                                               
342200                                                                          
342300*--- KOLLA IDPERSON-BUY                                                   
342400                                                                          
342500     IF MID-IDPERSON-BUY = ALL '+'                                        
342600       MOVE MFS-RENSA-FAELT TO MOD-IDPERSON-BUY-IN                        
342700     ELSE                                                                 
342800       IF BUY-LOCKED                                                      
342900         MOVE NEJ TO INDATA-SW                                            
343000         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
343100         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPERSON-BUY-IN-ATTR              
343200       ELSE                                                               
343300         MOVE MID-IDPERSON-BUY                                            
343400                              TO WS-IDPERSON-BUY                          
343500         INSPECT WS-IDPERSON-BUY REPLACING LEADING SPACE BY ZERO          
343600         IF WS-IDPERSON-BUY NUMERIC                                       
343700           MOVE MFS-NUM-FAELT-RAETT                                       
343800                              TO MOD-IDPERSON-BUY-IN-ATTR                 
343900           MOVE JA TO WDK6-SW                                             
344000         ELSE                                                             
344100           MOVE NEJ TO INDATA-SW                                          
344200           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
344300           MOVE MFS-NUM-FAELT-FEL                                         
344400                              TO MOD-IDPERSON-BUY-IN-ATTR                 
344500         END-IF                                                           
344600       END-IF                                                             
344700     END-IF                                                               
344800                                                                          
344900     IF MID-FLBUYUPD = ALL '+'                                            
345000       MOVE MFS-RENSA-FAELT TO MOD-FLBUYUPD-IN                            
345100     ELSE                                                                 
345200       IF MID-FLBUYUPD = JA OR YES OR NEJ                                 
345300         IF MID-FLBUYUPD = YES                                            
345400           MOVE JA TO MID-FLBUYUPD                                        
345500         END-IF                                                           
345600         MOVE JA TO WDK6-SW                                               
345700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLBUYUPD-IN-ATTR                
345800       ELSE                                                               
345900         MOVE NEJ TO INDATA-SW                                            
346000         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
346100         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLBUYUPD-IN-ATTR                
346200       END-IF                                                             
346300     END-IF                                                               
346400                                                                          
346500     IF MID-FLTABUPD = ALL '+'                                            
346600       MOVE MFS-RENSA-FAELT TO MOD-FLTABUPD-IN                            
346700     ELSE                                                                 
346800       IF MID-FLTABUPD = JA OR YES OR NEJ                                 
346900         IF MID-FLTABUPD = YES                                            
347000           MOVE JA TO MID-FLTABUPD                                        
347100         END-IF                                                           
347200         MOVE JA TO WDK6-SW                                               
347300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLTABUPD-IN-ATTR                
347400       ELSE                                                               
347500         MOVE NEJ TO INDATA-SW                                            
347600         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
347700         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLTABUPD-IN-ATTR                
347800       END-IF                                                             
347900     END-IF                                                               
348000                                                                          
348100*---   FOR UPDATE OF NOTES                                                
348200                                                                          
348300     IF MID-TEREFMED = ALL '+'                                            
348400       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEREFMED-ATTR                     
348500     ELSE                                                                 
348600       MOVE JA TO WDK6-SW                                                 
348700       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEREFMED-ATTR                     
348800     END-IF                                                               
348900                                                                          
349000     IF INDATA-FEL                                                        
349100       CALL WMEDKONV USING MED-WMEDAREA                                   
349200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
349300     END-IF                                                               
349400     .                                                                    
349500     EJECT                                                                
349600 GDD-VALIDATE-FUT-FORECAST SECTION.                                       
349700                                                                          
349800     MOVE ZERO                   TO WS-TIAARP-1                           
349900                                    WS-TIAARP-2                           
350000                                    WS-K626-TIAARP-1                      
350100                                    WS-K626-TIAARP-2                      
350200                                    WS-K626-TIPBJUST-1-JMF                
350300                                    WS-K626-TIPBJUST-2-JMF                
350400*                                                                         
350500     MOVE NEJ                    TO SW-KVPBJUST                           
350600                                                                          
350700     IF K626-EXISTS                                                       
350800        MOVE JUST-KVPB-JUST (1)  TO WS-K626-KVPB-JUST-1                   
350900        MOVE JUST-TIPBJUST  (1)  TO WS-K626-TIPBJUST-1-JMF                
351000        MOVE JUST-KVPB-JUST (2)  TO WS-K626-KVPB-JUST-2                   
351100        MOVE JUST-TIPBJUST  (2)  TO WS-K626-TIPBJUST-2-JMF                
351200        IF JUST-KVPB-JUST   (1)   > ZERO                                  
351300           MOVE JA               TO SW-KVPBJUST                           
351400           MOVE WS-K626-TIPBJUST-1-JMF                                    
351500                                 TO DAT-I-TIDATUM                         
351600           MOVE 'AAVV  '         TO DAT-KDDATFORM                         
351700           CALL WDATKONV USING DAT-KDDATFORM                              
351800                               DAT-I-TIDATUM                              
351900                               DAT-O-TIDATUM                              
352000                               DAT-KDSVAR                                 
352100           IF DAT-KDSVAR-OK                                               
352200              MOVE DAT-TIAARP    TO WS-K626-TIAARP-1                      
352300           END-IF                                                         
352400        END-IF                                                            
352500        IF JUST-KVPB-JUST (2)     > ZERO                                  
352600           MOVE WS-K626-TIPBJUST-2-JMF                                    
352700                                 TO DAT-I-TIDATUM                         
352800           MOVE 'AAVV  '         TO DAT-KDDATFORM                         
352900           CALL WDATKONV USING DAT-KDDATFORM                              
353000                               DAT-I-TIDATUM                              
353100                               DAT-O-TIDATUM                              
353200                               DAT-KDSVAR                                 
353300           IF DAT-KDSVAR-OK                                               
353400              MOVE DAT-TIAARP    TO WS-K626-TIAARP-2                      
353500           END-IF                                                         
353600        END-IF                                                            
353700     END-IF                                                               
353800                                                                          
353900     IF INDATA-OK                                                         
354000        IF (MID-KVPB-JUST-1 NOT = ALL '+'                                 
354100        OR  MID-TIPBJUST-1  NOT = ALL '+'                                 
354200        OR  MID-KVPB-JUST-2 NOT = ALL '+'                                 
354300        OR  MID-TIPBJUST-2  NOT = ALL '+')                                
354400            PERFORM GDDA-FORECAST-FUTURE-CONTROLS                         
354500        END-IF                                                            
354600     END-IF                                                               
354700                                                                          
354800     IF INDATA-FEL                                                        
354900        CALL WMEDKONV              USING MED-WMEDAREA                     
355000        MOVE MED-MFSFEL               TO MOD-TEMFSFEL                     
355100     END-IF                                                               
355200     .                                                                    
355300     EJECT                                                                
355400 GDDA-FORECAST-FUTURE-CONTROLS SECTION.                                   
355500                                                                          
355600     MOVE NEJ                         TO WDK626-SW                        
355700     PERFORM GDDAA-GENERAL-CONTROLS                                       
355800     IF INDATA-OK                                                         
355900          PERFORM GDDAB-KVPBJUST-CONTROL                                  
356000          IF INDATA-OK                                                    
356100             PERFORM GDDAC-TIPBJUST-CONTROL                               
356200          END-IF                                                          
356300     END-IF                                                               
356400     .                                                                    
356500     EJECT                                                                
356600 GDDAA-GENERAL-CONTROLS SECTION.                                          
356700                                                                          
356800     IF (MID-KVPB-JUST-1 NOT = ALL '+'                                    
356900     OR  MID-TIPBJUST-1  NOT = ALL '+'                                    
357000     OR  MID-KVPB-JUST-2 NOT = ALL '+'                                    
357100     OR  MID-TIPBJUST-2  NOT = ALL '+')                                   
357200         IF K629-MISSING                                                  
357300            MOVE NEJ                  TO INDATA-SW                        
357400            MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                     
357500            MOVE MFS-ALFA-FAELT-FEL   TO MOD-IN-KVPB-JUST-1-ATTR          
357600                                         MOD-IN-KVPB-JUST-2-ATTR          
357700                                         MOD-IN-TIPBJUST-1-ATTR           
357800                                         MOD-IN-TIPBJUST-2-ATTR           
357900            MOVE MFS-RENSA-FAELT      TO MOD-IN-KVPB-JUST-1               
358000                                         MOD-IN-TIPBJUST-1                
358100                                         MOD-IN-KVPB-JUST-2               
358200                                         MOD-IN-TIPBJUST-2                
358300         END-IF                                                           
358400     END-IF                                                               
358500*                                                                         
358600     IF   MID-KVPB-JUST-1 NOT = ALL '+'                                   
358700     AND (MID-KVPB-JUST-2 NOT = ALL '+'                                   
358800     OR   MID-TIPBJUST-2  NOT = ALL '+')                                  
358900         MOVE NEJ                     TO INDATA-SW                        
359000         MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                     
359100         MOVE MFS-ALFA-FAELT-FEL      TO MOD-IN-KVPB-JUST-1-ATTR          
359200                                         MOD-IN-KVPB-JUST-2-ATTR          
359300                                         MOD-IN-TIPBJUST-2-ATTR           
359400     ELSE                                                                 
359500       IF  MID-KVPB-JUST-2 NOT = ALL '+'                                  
359600       AND MID-TIPBJUST-1  NOT = ALL '+'                                  
359700            MOVE NEJ                  TO INDATA-SW                        
359800            MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                     
359900            MOVE MFS-ALFA-FAELT-FEL   TO MOD-IN-TIPBJUST-1-ATTR           
360000                                         MOD-IN-KVPB-JUST-2-ATTR          
360100       END-IF                                                             
360200     END-IF                                                               
360300*                                                                         
360400     IF  ARTIKEL-ERS-MAERKT                                               
360500     AND (MID-KVPB-JUST-1 NOT = ALL '+'                                   
360600     OR   MID-KVPB-JUST-2 NOT = ALL '+' )                                 
360700          MOVE NEJ                     TO INDATA-SW                       
360800          MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                    
360900          MOVE MFS-ALFA-FAELT-FEL      TO MOD-IN-KVPB-JUST-1-ATTR         
361000                                          MOD-IN-KVPB-JUST-2-ATTR         
361100                                          MOD-IN-TIPBJUST-1-ATTR          
361200                                          MOD-IN-TIPBJUST-2-ATTR          
361300     END-IF                                                               
361400*                                                                         
361500     IF  K629-EXISTS                                                      
361600     AND K6-CREF-FLPB-FLYTT = JA                                          
361700     AND (MID-KVPB-JUST-1 NOT = ALL '+'                                   
361800     OR   MID-KVPB-JUST-2 NOT = ALL '+')                                  
361900          MOVE NEJ                     TO INDATA-SW                       
362000          MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                    
362100          MOVE MFS-ALFA-FAELT-FEL      TO MOD-IN-KVPB-JUST-1-ATTR         
362200                                          MOD-IN-KVPB-JUST-2-ATTR         
362300                                          MOD-IN-TIPBJUST-1-ATTR          
362400                                          MOD-IN-TIPBJUST-2-ATTR          
362500     END-IF                                                               
362600     .                                                                    
362700     EJECT                                                                
362800 GDDAB-KVPBJUST-CONTROL SECTION.                                          
362900                                                                          
363000     IF MID-KVPB-JUST-1 NOT    = ALL '+'                                  
363100       IF (MID-KVPB-JUST-1     = ZERO                                     
363200       AND WS-K626-KVPB-JUST-2 > ZERO)                                    
363300         MOVE NEJ                  TO INDATA-SW                           
363400         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
363500         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IN-KVPB-JUST-1-ATTR             
363600                                      MOD-IN-TIPBJUST-1-ATTR              
363700       ELSE                                                               
363800         IF MID-KVPB-JUST-1 = ZERO                                        
363900           MOVE ZERO               TO WS-KVPB-JUST-1                      
364000                                      WS-TIPBJUST-1                       
364100                                      MID-TIPBJUST-1                      
364200           MOVE JA                 TO WDK626-SW                           
364300         ELSE                                                             
364400           MOVE MID-KVPB-JUST-1    TO DEC-IDFRIDATA                       
364500           MOVE 6                  TO DEC-KVHELTAL                        
364600           MOVE 1                  TO DEC-KVDECIMAL                       
364700           CALL WDECEDIT        USING DEC-WDECAREA                        
364800           IF DEC-KDSVAR-OK                                               
364900              MOVE DEC-IDEDITDATA  TO WS-KVPB-JUST-1                      
365000              MOVE JA              TO WDK626-SW                           
365100           ELSE                                                           
365200              MOVE MFS-NUM-FAELT-FEL                                      
365300                                   TO MOD-IN-KVPB-JUST-1-ATTR             
365400              MOVE NEJ             TO INDATA-SW                           
365500              MOVE ERR-CORR-HILITE-FLDS                                   
365600                                   TO MED-IDMFSFEL                        
365700           END-IF                                                         
365800         END-IF                                                           
365900       END-IF                                                             
366000     ELSE                                                                 
366100       IF MID-KVPB-JUST-2 NOT = ALL '+'                                   
366200         IF MID-KVPB-JUST-2   = ZERO                                      
366300           MOVE ZERO                   TO WS-KVPB-JUST-2                  
366400                                          WS-TIPBJUST-2                   
366500                                          MID-TIPBJUST-2                  
366600           MOVE JA                     TO WDK626-SW                       
366700         ELSE                                                             
366800          IF WS-K626-KVPB-JUST-1 NOT > ZERO                               
366900             MOVE NEJ                  TO INDATA-SW                       
367000             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
367100             MOVE MFS-ALFA-FAELT-FEL   TO MOD-IN-KVPB-JUST-2-ATTR         
367200                                          MOD-IN-TIPBJUST-2-ATTR          
367300          ELSE                                                            
367400             MOVE MID-KVPB-JUST-2      TO DEC-IDFRIDATA                   
367500             MOVE 6                    TO DEC-KVHELTAL                    
367600             MOVE 1                    TO DEC-KVDECIMAL                   
367700             CALL WDECEDIT          USING DEC-WDECAREA                    
367800             IF DEC-KDSVAR-OK                                             
367900                MOVE DEC-IDEDITDATA    TO WS-KVPB-JUST-2                  
368000                MOVE JA                TO WDK626-SW                       
368100             ELSE                                                         
368200                MOVE MFS-NUM-FAELT-FEL TO                                 
368300                                          MOD-IN-KVPB-JUST-2-ATTR         
368400                MOVE NEJ               TO INDATA-SW                       
368500                MOVE ERR-CORR-HILITE-FLDS                                 
368600                                       TO MED-IDMFSFEL                    
368700             END-IF                                                       
368800          END-IF                                                          
368900         END-IF                                                           
369000       END-IF                                                             
369100     END-IF                                                               
369200     .                                                                    
369300     EJECT                                                                
369400 GDDAC-TIPBJUST-CONTROL SECTION.                                          
369500                                                                          
369600*****PB-JUST-1                                                            
369700     IF MID-KVPB-JUST-1 = ALL '+'                                         
369800        MOVE MFS-RENSA-FAELT          TO MOD-IN-KVPB-JUST-1               
369900        MOVE MFS-NUM-FAELT-RAETT      TO MOD-IN-KVPB-JUST-1-ATTR          
370000     ELSE                                                                 
370100        MOVE MFS-ROER-EJ-FAELT        TO MOD-IN-KVPB-JUST-1               
370200     END-IF                                                               
370300                                                                          
370400********TIPBJUST-1                                                        
370500     IF   MID-TIPBJUST-1 = ALL '+'                                        
370600     AND (WS-K626-KVPB-JUST-1 NOT > ZERO                                  
370700     AND  WS-KVPB-JUST-1          > ZERO)                                 
370800          MOVE NEJ                    TO INDATA-SW                        
370900          MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                     
371000          MOVE MFS-ALFA-FAELT-FEL     TO MOD-IN-KVPB-JUST-1-ATTR          
371100                                         MOD-IN-TIPBJUST-1-ATTR           
371200     ELSE                                                                 
371300        IF MID-TIPBJUST-1 = ALL '+'                                       
371400           MOVE MFS-RENSA-FAELT       TO MOD-IN-KVPB-JUST-1               
371500           MOVE MFS-NUM-FAELT-RAETT   TO MOD-IN-TIPBJUST-1-ATTR           
371600        END-IF                                                            
371700     END-IF                                                               
371800     IF INDATA-OK                                                         
371900       IF MID-TIPBJUST-1 NOT = ALL '+'                                    
372000        IF MID-TIPBJUST-1      = ALL ZERO                                 
372100           IF MID-KVPB-JUST-1  = ZERO                                     
372200              MOVE MFS-NUM-FAELT-RAETT                                    
372300                                      TO MOD-IN-TIPBJUST-1-ATTR           
372400           ELSE                                                           
372500              MOVE NEJ                TO INDATA-SW                        
372600              MOVE ERR-CORR-HILITE-FLDS                                   
372700                                      TO MED-IDMFSFEL                     
372800              MOVE MFS-ALFA-FAELT-FEL TO MOD-IN-TIPBJUST-1-ATTR           
372900                                         MOD-IN-KVPB-JUST-1-ATTR          
373000                                                                          
373100           END-IF                                                         
373200        ELSE                                                              
373300           IF MID-TIPBJUST-1 NUMERIC                                      
373400              IF  WS-K626-KVPB-JUST-1 NOT > ZERO                          
373500              AND WS-KVPB-JUST-1      NOT > ZERO                          
373600                  MOVE NEJ            TO INDATA-SW                        
373700                  MOVE ERR-CORR-HILITE-FLDS                               
373800                                      TO MED-IDMFSFEL                     
373900                  MOVE MFS-ALFA-FAELT-FEL                                 
374000                                      TO MOD-IN-KVPB-JUST-1-ATTR          
374100                                         MOD-IN-TIPBJUST-1-ATTR           
374200              ELSE                                                        
374300                  MOVE MID-TIPBJUST-1 TO WS-TIPBJUST-1                    
374400                  MOVE WS-TIPBJUST-1  TO DAT-I-TIDATUM                    
374500                  MOVE 'AAVV  '       TO DAT-KDDATFORM                    
374600                  CALL WDATKONV USING DAT-KDDATFORM                       
374700                                      DAT-I-TIDATUM                       
374800                                      DAT-O-TIDATUM                       
374900                                      DAT-KDSVAR                          
375000                  IF DAT-KDSVAR-OK                                        
375100                     MOVE DAT-TIAARP         TO WS-TIAARP-1               
375200                     MOVE DAT-TIAAVV-GRP     TO WS-TIPBJUST-AAVV          
375300                     MOVE WS-TIPBJUST-AAVV   TO TMP1-YYWW                 
375400                     MOVE DAGENS-AAVV        TO TMP2-YYWW                 
375500                     PERFORM WY2000P3                                     
375600                     IF TMP1-YYWW > TMP2-YYWW                             
375700                        MOVE MFS-NUM-FAELT-RAETT                          
375800                                       TO MOD-IN-TIPBJUST-1-ATTR          
375900***CHECK THAT THE DATE IS WITHIN A YEAR                                   
376000                        ADD +100       TO TMP2-YYWW                       
376100                        IF TMP1-YYWW    < TMP2-YYWW                       
376200                           MOVE MFS-NUM-FAELT-RAETT                       
376300                                       TO MOD-IN-TIPBJUST-1-ATTR          
376400                           MOVE JA     TO WDK626-SW                       
376500                        ELSE                                              
376600                           MOVE MFS-NUM-FAELT-FEL                         
376700                                       TO MOD-IN-TIPBJUST-1-ATTR          
376800                           MOVE NEJ    TO INDATA-SW                       
376900                           MOVE ERR-MAX-1-YEAR                            
377000                                       TO MED-MFSFEL                      
377100                        END-IF                                            
377200                     ELSE                                                 
377300                        MOVE MFS-NUM-FAELT-FEL                            
377400                                       TO MOD-IN-TIPBJUST-1-ATTR          
377500                        MOVE NEJ       TO INDATA-SW                       
377600                        MOVE ERR-ONLY-FUTURE                              
377700                                       TO MED-MFSFEL                      
377800                     END-IF                                               
377900                     IF WS-K626-TIPBJUST-2-JMF > ZERO                     
378000                        IF WS-TIPBJUST-1 >                                
378100                           WS-K626-TIPBJUST-2-JMF                         
378200                           MOVE MFS-NUM-FAELT-FEL                         
378300                                       TO MOD-IN-TIPBJUST-1-ATTR          
378400                           MOVE NEJ    TO INDATA-SW                       
378500                           MOVE ERR-CORR-HILITE-FLDS                      
378600                                       TO MED-IDMFSFEL                    
378700                        END-IF                                            
378800                     END-IF                                               
378900                  ELSE                                                    
379000                     MOVE MFS-NUM-FAELT-FEL                               
379100                                       TO MOD-IN-TIPBJUST-1-ATTR          
379200                     MOVE NEJ          TO INDATA-SW                       
379300                     MOVE ERR-CORR-HILITE-FLDS                            
379400                                       TO MED-IDMFSFEL                    
379500                  END-IF                                                  
379600              END-IF                                                      
379700           ELSE                                                           
379800              MOVE NEJ               TO INDATA-SW                         
379900              MOVE MFS-NUM-FAELT-FEL TO MOD-IN-TIPBJUST-1-ATTR            
380000              MOVE ERR-CORR-HILITE-FLDS                                   
380100                                     TO MED-IDMFSFEL                      
380200           END-IF                                                         
380300        END-IF                                                            
380400        MOVE MFS-ROER-EJ-FAELT       TO MOD-IN-TIPBJUST-1                 
380500       END-IF                                                             
380600     END-IF                                                               
380700                                                                          
380800*PB-JUSTERING-2                                                           
380900                                                                          
381000     IF MID-KVPB-JUST-2 = ALL '+'                                         
381100        MOVE MFS-RENSA-FAELT          TO                                  
381200                       MOD-IN-KVPB-JUST-2                                 
381300        MOVE MFS-NUM-FAELT-RAETT      TO                                  
381400                       MOD-IN-KVPB-JUST-2-ATTR                            
381500     END-IF                                                               
381600                                                                          
381700                                                                          
381800********TIPBJUST-2                                                        
381900     IF   MID-TIPBJUST-2 = ALL '+'                                        
382000     AND (WS-K626-KVPB-JUST-2 NOT > ZERO                                  
382100     AND  WS-KVPB-JUST-2          > ZERO)                                 
382200          MOVE NEJ                    TO INDATA-SW                        
382300          MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                     
382400          MOVE MFS-ALFA-FAELT-FEL     TO MOD-IN-KVPB-JUST-2-ATTR          
382500                                         MOD-IN-TIPBJUST-2-ATTR           
382600     ELSE                                                                 
382700        IF MID-TIPBJUST-2 = ALL '+'                                       
382800           MOVE MFS-RENSA-FAELT       TO MOD-IN-KVPB-JUST-2               
382900           MOVE MFS-NUM-FAELT-RAETT   TO MOD-IN-TIPBJUST-2-ATTR           
383000        END-IF                                                            
383100     END-IF                                                               
383200     IF INDATA-OK                                                         
383300       IF MID-TIPBJUST-2 NOT = ALL '+'                                    
383400        IF MID-TIPBJUST-2 = ALL ZERO                                      
383500           IF MID-KVPB-JUST-2  = ZERO                                     
383600              MOVE MFS-NUM-FAELT-RAETT                                    
383700                                      TO MOD-IN-TIPBJUST-2-ATTR           
383800           ELSE                                                           
383900              MOVE NEJ                TO INDATA-SW                        
384000              MOVE ERR-CORR-HILITE-FLDS                                   
384100                                      TO MED-IDMFSFEL                     
384200              MOVE MFS-ALFA-FAELT-FEL                                     
384300                                      TO MOD-IN-KVPB-JUST-2-ATTR          
384400                                         MOD-IN-TIPBJUST-2-ATTR           
384500           END-IF                                                         
384600        ELSE                                                              
384700           IF MID-TIPBJUST-2 NUMERIC                                      
384800              IF  WS-K626-KVPB-JUST-2 NOT > ZERO                          
384900              AND WS-KVPB-JUST-2      NOT > ZERO                          
385000                  MOVE NEJ            TO INDATA-SW                        
385100                  MOVE ERR-CORR-HILITE-FLDS                               
385200                                      TO MED-IDMFSFEL                     
385300                  MOVE MFS-ALFA-FAELT-FEL                                 
385400                                      TO MOD-IN-KVPB-JUST-2-ATTR          
385500                                         MOD-IN-TIPBJUST-2-ATTR           
385600              ELSE                                                        
385700                  MOVE MID-TIPBJUST-2     TO WS-TIPBJUST-2                
385800                  MOVE WS-TIPBJUST-2      TO DAT-I-TIDATUM                
385900                  MOVE 'AAVV  '           TO DAT-KDDATFORM                
386000                  CALL WDATKONV USING DAT-KDDATFORM                       
386100                                      DAT-I-TIDATUM                       
386200                                      DAT-O-TIDATUM                       
386300                                      DAT-KDSVAR                          
386400                  IF DAT-KDSVAR-OK                                        
386500                     MOVE DAT-TIAARP         TO WS-TIAARP-2               
386600                     MOVE DAT-TIAAVV-GRP     TO WS-TIPBJUST-AAVV          
386700                     MOVE WS-TIPBJUST-AAVV   TO TMP1-YYWW                 
386800                     MOVE DAGENS-AAVV        TO TMP2-YYWW                 
386900                     PERFORM WY2000P3                                     
387000                     IF TMP1-YYWW > TMP2-YYWW                             
387100                        MOVE MFS-NUM-FAELT-RAETT                          
387200                                       TO MOD-IN-TIPBJUST-2-ATTR          
387300***CHECK THAT THE DATE IS WITHIN A YEAR                                   
387400                        ADD +100       TO TMP2-YYWW                       
387500                        IF TMP1-YYWW    < TMP2-YYWW                       
387600                           MOVE MFS-NUM-FAELT-RAETT                       
387700                                       TO MOD-IN-TIPBJUST-2-ATTR          
387800                           MOVE JA     TO WDK626-SW                       
387900                        ELSE                                              
388000                           MOVE MFS-NUM-FAELT-FEL                         
388100                                       TO MOD-IN-TIPBJUST-2-ATTR          
388200                           MOVE NEJ    TO INDATA-SW                       
388300                           MOVE ERR-MAX-1-YEAR                            
388400                                       TO MED-MFSFEL                      
388500                        END-IF                                            
388600                     ELSE                                                 
388700                        MOVE MFS-NUM-FAELT-FEL                            
388800                                       TO MOD-IN-TIPBJUST-2-ATTR          
388900                        MOVE NEJ       TO INDATA-SW                       
389000                        MOVE ERR-ONLY-FUTURE                              
389100                                       TO MED-MFSFEL                      
389200                     END-IF                                               
389300                     IF WS-TIPBJUST-2 <                                   
389400                        WS-K626-TIPBJUST-1-JMF                            
389500                        MOVE MFS-NUM-FAELT-FEL                            
389600                                       TO MOD-IN-TIPBJUST-2-ATTR          
389700                        MOVE NEJ       TO INDATA-SW                       
389800                        MOVE ERR-CORR-HILITE-FLDS                         
389900                                       TO MED-IDMFSFEL                    
390000                     END-IF                                               
390100                  ELSE                                                    
390200                     MOVE MFS-NUM-FAELT-FEL                               
390300                                       TO MOD-IN-TIPBJUST-2-ATTR          
390400                     MOVE NEJ          TO INDATA-SW                       
390500                     MOVE ERR-CORR-HILITE-FLDS                            
390600                                       TO MED-IDMFSFEL                    
390700                  END-IF                                                  
390800              END-IF                                                      
390900           ELSE                                                           
391000              MOVE NEJ                 TO INDATA-SW                       
391100              MOVE MFS-NUM-FAELT-FEL                                      
391200                                       TO MOD-IN-TIPBJUST-2-ATTR          
391300              MOVE ERR-CORR-HILITE-FLDS                                   
391400                                       TO MED-MFSFEL                      
391500           END-IF                                                         
391600        END-IF                                                            
391700        MOVE MFS-ROER-EJ-FAELT         TO MOD-IN-TIPBJUST-2               
391800       END-IF                                                             
391900     END-IF                                                               
392000                                                                          
392100*****COMMON CHECK                                                         
392200     IF INDATA-OK                                                         
392300        IF  MID-TIPBJUST-1 NOT = ALL '+'                                  
392400        AND MID-TIPBJUST-2 NOT = ALL '+'                                  
392500            MOVE NEJ                TO INDATA-SW                          
392600            MOVE ERR-CORR-HILITE-FLDS                                     
392700                                    TO MED-IDMFSFEL                       
392800            MOVE MFS-ALFA-FAELT-FEL                                       
392900                                    TO MOD-IN-TIPBJUST-1-ATTR             
393000                                       MOD-IN-TIPBJUST-2-ATTR             
393100        END-IF                                                            
393200     END-IF                                                               
393300     .                                                                    
393400     EJECT                                                                
393500                                                                          
393600 H-UPPDATERA SECTION.                                                     
393700                                                                          
393800     PERFORM IMS-GHU-WDK7-WDK711                                          
393900     IF WDK7-UPPD                                                         
394000       IF SEGMENT-FINNS                                                   
394100         IF (NDC-NA OR NDC-CN)                                            
394200         AND (SLAG-IDDC-REF = SPACE)                                      
394300           CONTINUE                                                       
394400         ELSE                                                             
394500           PERFORM HA-FLYTTA-INMATADE-FAELT                               
394600           PERFORM IMS-REPL-WDK7-WDK711                                   
394700           MOVE JA         TO WDK7-UPD-STAT-SW                            
394800         END-IF                                                           
394900       ELSE                                                               
395000         MOVE ALL '+'      TO WDK7-W005WDK7                               
395100         MOVE 'WDK711'     TO WDK7-IDSEGM                                 
395200         MOVE W-IDARTNR    TO WDK7-IDARTNR-KFB                            
395300         MOVE DCS-IDDC     TO WDK7-IDDC-KFB                               
395400                              WDK7-IDDC                                   
395500         PERFORM S03-FLYTTA-INMATADE-VAERDEN                              
395600         CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB                       
395700                                           WDK6-PCB WDK7-PCB              
395800         PERFORM IMS-GHU-WDK7-WDK711                                      
395900         PERFORM HA-FLYTTA-INMATADE-FAELT                                 
396000         PERFORM IMS-REPL-WDK7-WDK711                                     
396100         MOVE JA           TO WDK7-UPD-STAT-SW                            
396200       END-IF                                                             
396300     END-IF                                                               
396400                                                                          
396500     IF WDK727-UPPD                                                       
396600       PERFORM HB-FLYTTA-FAELT-WDK727                                     
396700     END-IF                                                               
396800                                                                          
396900     IF WDK7-UPPD                                                         
397000     OR WDK727-UPPD                                                       
397100        PERFORM HC-UPD-REFL1-OUTPUT                                       
397200     END-IF                                                               
397300                                                                          
397400     IF MID-TEREFMED NOT = ALL '+'                                        
397500       MOVE W-IDDC        TO W-2261-IDDC                                  
397600       PERFORM IMS-GHU-WDGX2262                                           
397700       MOVE MID-TEREFMED         TO 2262-TEREFMED                         
397800       IF SEGMENT-FINNS                                                   
397900         PERFORM IMS-REPL-WDGX2262                                        
398000       ELSE                                                               
398100         MOVE W-IDARTNR          TO 2262-IDARTNR                          
398200         PERFORM IMS-ISRT-WDGX2262                                        
398300       END-IF                                                             
398400       MOVE JA                   TO WDK7-UPD-STAT-SW                      
398500     END-IF                                                               
398600                                                                          
398700     IF WDK7-UPPD-OK                                                      
398800        MOVE INF-UPDATE-DONE TO MED-IDMFSINF                              
398900        CALL WMEDKONV USING MED-WMEDAREA                                  
399000        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
399100        MOVE JA         TO SW-MESSAGE                                     
399200        PERFORM MFS-FORM-ATTR                                             
399300        PERFORM MFS-RENSA-FAELT-IN                                        
399400     END-IF                                                               
399500     .                                                                    
399600     EJECT                                                                
399700 HA-FLYTTA-INMATADE-FAELT SECTION.                                        
399800                                                                          
399900     IF MID-IDREFTAB  = ALL '+'                                           
400000       CONTINUE                                                           
400100     ELSE                                                                 
400200       MOVE MID-IDREFTAB     TO SLAG-IDREFTAB                             
400300       MOVE JA               TO SLAG-FLTABUPD                             
400400     END-IF                                                               
400500                                                                          
400600     IF MID-FLWILSON  = ALL '+'                                           
400700       CONTINUE                                                           
400800     ELSE                                                                 
400900       MOVE MID-FLWILSON     TO SLAG-FLWILSON                             
401000     END-IF                                                               
401100                                                                          
401200     IF MID-KVREFPKT  = ALL '+'                                           
401300       CONTINUE                                                           
401400     ELSE                                                                 
401500       MOVE MID-KVREFPKT     TO SLAG-KVREFPKT                             
401600     END-IF                                                               
401700                                                                          
401800     IF MID-TIREFPKT  = ALL '+'                                           
401900       MOVE ZERO             TO WS-TIREFPKT                               
402000     ELSE                                                                 
402100       MOVE MID-TIREFPKT     TO WS-TIREFPKT                               
402200     END-IF                                                               
402300                                                                          
402400                                                                          
402500     IF MID-FLREFILL = ALL '+'                                            
402600       CONTINUE                                                           
402700     ELSE                                                                 
402800       MOVE MID-FLREFILL    TO SLAG-FLREFILL                              
402900       IF MID-FLREFILL = NEJ                                              
403000         MOVE PASSIV        TO SLAG-KDREFSTA                              
403100         MOVE DAGENS-DATUM  TO SLAG-TIREFSTA                              
403200         MOVE ZERO          TO SLAG-KVPB-REF                              
403300                               SLAG-KVPB-HIST                             
403400                               SLAG-KVPBREOI                              
403500                               SLAG-KVPBREOI-HIST                         
403600                               SLAG-KVREFBER                              
403700                               SLAG-KVREFPKT                              
403800                               SLAG-KVREFOVL                              
403900         MOVE ZERO          TO SLAG-TIREFMPB                              
404000                               SLAG-TIREFPAF                              
404100                               SLAG-TIREFPKT                              
404200                               SLAG-TIPBREOI                              
404300       END-IF                                                             
404400     END-IF                                                               
404500                                                                          
404600     IF MID-KVREFBER  = ALL '+'                                           
404700       CONTINUE                                                           
404800     ELSE                                                                 
404900       MOVE MID-KVREFBER     TO SLAG-KVREFBER                             
405000     END-IF                                                               
405100                                                                          
405200     IF MID-FLREFBEO = ALL '+'                                            
405300       CONTINUE                                                           
405400     ELSE                                                                 
405500*---     OM FL-AUTOMATISK-BEORDRING SÄTTS TILL JA, SLÄCKS                 
405600*---     EV FL-VÄNTA-TILLS-NY-OI                                          
405700       IF MID-FLREFBEO = JA                                               
405800       AND (SLAG-FLREFBEO = NEJ                                           
405900       OR   SLAG-FLREFBEO = 'S')                                          
406000           MOVE JA TO WS-SLACK-FLREFNYO                                   
406100       END-IF                                                             
406200       MOVE MID-FLREFBEO    TO SLAG-FLREFBEO                              
406300     END-IF                                                               
406400                                                                          
406500     IF MID-TIREFPAF  = ALL '+'                                           
406600       MOVE ZERO             TO WS-TIREFPAF                               
406700     ELSE                                                                 
406800       MOVE MID-TIREFPAF     TO WS-TIREFPAF                               
406900     END-IF                                                               
407000                                                                          
407100     IF MID-TIREFSTO = ALL '+'                                            
407200       CONTINUE                                                           
407300     ELSE                                                                 
407400       MOVE JA TO WS-SLACK-FLREFNYO                                       
407500       MOVE MID-TIREFSTO     TO SLAG-TIREFSTO                             
407600     END-IF                                                               
407700                                                                          
407800     IF MID-IDPERSON-BUY = ALL '+'                                        
407900       CONTINUE                                                           
408000     ELSE                                                                 
408100       MOVE MID-IDPERSON-BUY TO SLAG-IDPERSON-BUY                         
408200       IF MID-IDPERSON-BUY NOT = ZERO                                     
408300         MOVE JA             TO SLAG-FLBUYUPD                             
408400       END-IF                                                             
408500     END-IF                                                               
408600                                                                          
408700     IF MID-FLBUYUPD = ALL '+'                                            
408800       CONTINUE                                                           
408900     ELSE                                                                 
409000       MOVE MID-FLBUYUPD     TO SLAG-FLBUYUPD                             
409100     END-IF                                                               
409200                                                                          
409300     IF MID-FLTABUPD = ALL '+'                                            
409400       CONTINUE                                                           
409500     ELSE                                                                 
409600       MOVE MID-FLTABUPD     TO SLAG-FLTABUPD                             
409700     END-IF                                                               
409800                                                                          
409900     IF MID-FLPB-FLYTT = ALL '+'                                          
410000       CONTINUE                                                           
410100     ELSE                                                                 
410200       MOVE MID-FLPB-FLYTT   TO SLAG-FLPB-FLYTT                           
410300     END-IF                                                               
410400                                                                          
410500     IF MID-FLREFNYO = ALL '+'                                            
410600       CONTINUE                                                           
410700     ELSE                                                                 
410800       MOVE MID-FLREFNYO     TO SLAG-FLREFNYO                             
410900     END-IF                                                               
411000                                                                          
411100     IF WS-SLACK-FLREFNYO = JA                                            
411200       MOVE NEJ              TO SLAG-FLREFNYO                             
411300     END-IF                                                               
411400     .                                                                    
411500     EJECT                                                                
411600                                                                          
411700 HB-FLYTTA-FAELT-WDK727 SECTION.                                          
411800                                                                          
411900     IF MID-KVPB-JUST-1   NOT = ALL '+'                                   
412000     OR MID-TIPBJUST-1    NOT = ALL '+'                                   
412100     OR MID-KVPB-JUST-2   NOT = ALL '+'                                   
412200     OR MID-TIPBJUST-2    NOT = ALL '+'                                   
412300        PERFORM IMS-GHNP-WDK727                                           
412400        IF SEGMENT-FINNS                                                  
412500           IF MID-KVPB-JUST-1 NOT   = ALL '+'                             
412600              MOVE WS-KVPB-JUST-1  TO PROG-KVPB-JUST(1)                   
412700           END-IF                                                         
412800           IF MID-KVPB-JUST-2 NOT   = ALL '+'                             
412900              MOVE WS-KVPB-JUST-2  TO PROG-KVPB-JUST(2)                   
413000           END-IF                                                         
413100           IF MID-TIPBJUST-1 NOT    = ALL '+'                             
413200              MOVE MID-TIPBJUST-1  TO PROG-TIPBJUST(1)                    
413300           END-IF                                                         
413400           IF MID-TIPBJUST-2 NOT    = ALL '+'                             
413500              MOVE MID-TIPBJUST-2  TO PROG-TIPBJUST(2)                    
413600           END-IF                                                         
413700           IF  PROG-KVPB-JUST(1)    = ZERO                                
413800           AND PROG-KVPB-JUST(2)    = ZERO                                
413900               PERFORM IMS-DLET-WDK727                                    
414000           ELSE                                                           
414100               PERFORM IMS-REPL-WDK727                                    
414200           END-IF                                                         
414300                                                                          
414400           PERFORM IMS-GHU-WDK7-WDK711                                    
414500           IF SLAG-KDREFSTA = 'P'                                         
414600              MOVE AKTIV           TO SLAG-KDREFSTA                       
414700              MOVE DAGENS-DATUM    TO SLAG-TIREFSTA                       
414800              PERFORM IMS-REPL-WDK7-WDK711                                
414900           END-IF                                                         
415000        ELSE                                                              
415100           MOVE ALL '+'            TO WDK7-W005WDK7                       
415200           MOVE 'WDK727'           TO WDK7-IDSEGM                         
415300           MOVE W-IDARTNR          TO WDK7-IDARTNR-KFB                    
415400           MOVE DCS-IDDC           TO WDK7-IDDC-KFB                       
415500           IF MID-KVPB-JUST-1   NOT = ALL '+'                             
415600              MOVE WS-KVPB-JUST-1  TO WDK7-KVPB-JUST(1)                   
415700           END-IF                                                         
415800           IF MID-KVPB-JUST-2   NOT = ALL '+'                             
415900              MOVE WS-KVPB-JUST-2  TO WDK7-KVPB-JUST(2)                   
416000           END-IF                                                         
416100           IF MID-TIPBJUST-1    NOT = ALL '+'                             
416200              MOVE WS-TIPBJUST-1   TO WDK7-TIPBJUST(1)                    
416300           END-IF                                                         
416400           IF MID-TIPBJUST-2    NOT = ALL '+'                             
416500              MOVE WS-TIPBJUST-2   TO WDK7-TIPBJUST(2)                    
416600           END-IF                                                         
416700           CALL W005WDK7        USING WDK7-W005WDK7                       
416800                                      WDB6-PCB                            
416900                                      WDK6-PCB                            
417000                                      WDK7-PCB                            
417100           PERFORM IMS-GHU-WDK7-WDK711                                    
417200           IF SLAG-KDREFSTA   = 'P'                                       
417300              MOVE AKTIV           TO SLAG-KDREFSTA                       
417400              MOVE DAGENS-DATUM    TO SLAG-TIREFSTA                       
417500              PERFORM IMS-REPL-WDK7-WDK711                                
417600           END-IF                                                         
417700        END-IF                                                            
417800        MOVE JA                    TO WDK7-UPD-STAT-SW                    
417900     END-IF                                                               
418000     .                                                                    
418100     EJECT                                                                
418200 HC-UPD-REFL1-OUTPUT SECTION.                                             
418300***                                                                       
418400*  OM DATUM FÖR MANUELL PÅFYLLNADS-PUNKT ELLER -KVANTITET                 
418500*  ÄR SATT BERÄKNA ÖVERLAGERPUNKTEN (-KVREFOVL)                           
418600***                                                                       
418700     IF (WDK727-UPPD                                                      
418800     OR  MID-IDREFTAB > SPACES                                            
418900     OR  WS-TIREFPKT  > ZERO                                              
419000     OR  WS-TIREFPAF  > ZERO )                                            
419100                                                                          
419200         PERFORM IMS-GHU-WDK7-WDK711                                      
419300         IF SEGMENT-FINNS                                                 
419400            IF (NDC-NA OR NDC-CN)                                         
419500            AND (SLAG-IDDC-REF = SPACE)                                   
419600              CONTINUE                                                    
419700            ELSE                                                          
419800***           CALL W271REFL TO REFILLING PT & QTY                         
419900*                                                                         
420000              PERFORM S90-CALL-W271REFL                                   
420100*                                                                         
420200              IF WS-TIREFPKT > ZERO                                       
420300                 MOVE WS-TIREFPKT      TO SLAG-TIREFPKT                   
420400              END-IF                                                      
420500                                                                          
420600              IF WS-TIREFPAF > ZERO                                       
420700                 MOVE WS-TIREFPAF      TO SLAG-TIREFPAF                   
420800              END-IF                                                      
420900*                                                                         
421000              MOVE SLAG-TIREFPKT       TO TMP1-YYMMDD                     
421100              MOVE DAGENS-DATUM        TO TMP2-YYMMDD                     
421200              PERFORM WY2000P1                                            
421300              IF TMP1-YYMMDD >= TMP2-YYMMDD                               
421400                                                                          
421500*                                                                         
421600*--- INGEN UPPDATERING AV KVREFPKT PGA MANUELLT DATUM ÄR SATT             
421700                 CONTINUE                                                 
421800              ELSE                                                        
421900                 MOVE W271-REFL-KVREFPKT    TO SLAG-KVREFPKT              
422000              END-IF                                                      
422100                                                                          
422200              MOVE SLAG-TIREFPAF       TO TMP1-YYMMDD                     
422300              MOVE DAGENS-DATUM        TO TMP2-YYMMDD                     
422400              PERFORM WY2000P1                                            
422500              IF TMP1-YYMMDD >= TMP2-YYMMDD                               
422600*                                                                         
422700*--- INGEN UPPDATERING AV KVREFBER PGA MANUELLT DATUM ÄR SATT             
422800                 CONTINUE                                                 
422900              ELSE                                                        
423000                 MOVE W271-REFL-KVREFBER    TO SLAG-KVREFBER              
423100              END-IF                                                      
423200              MOVE W271-REFL-KVREFOVL  TO SLAG-KVREFOVL                   
423300*                                                                         
423400              PERFORM IMS-REPL-WDK7-WDK711                                
423500            END-IF                                                        
423600         END-IF                                                           
423700     END-IF                                                               
423800                                                                          
423900     .                                                                    
424000     EJECT                                                                
424100                                                                          
424200 I-UPPDATERA-CDC-REFILL SECTION.                                          
424300                                                                          
424400     IF WDK6-UPPD                                                         
424500       MOVE MSGI-IDDC-KEY      TO W-IDDC                                  
424600                                                                          
424700       PERFORM IMS-GHU-WDK61129                                           
424800       IF SEGMENT-FINNS                                                   
424900         PERFORM IA-FLYTTA-INMATADE-FAELT                                 
425000         PERFORM IMS-REPL-WDK61129                                        
425100         PERFORM IB-UPD-NOTES-WDK625                                      
425200         MOVE JA               TO WDK6-UPD-STAT-SW                        
425300       END-IF                                                             
425400     END-IF                                                               
425500*                                                                         
425600     IF WDK626-UPPD                                                       
425700        PERFORM IC-UPDATE-WDK626                                          
425800     END-IF                                                               
425900*                                                                         
426000     IF WDK6-UPPD-OK                                                      
426100        MOVE INF-UPDATE-DONE   TO MED-IDMFSINF                            
426200        CALL WMEDKONV       USING MED-WMEDAREA                            
426300        MOVE MED-MFSINF        TO MOD-TEMFSINF                            
426400        MOVE JA                TO SW-MESSAGE                              
426500        PERFORM MFS-FORM-ATTR                                             
426600        PERFORM MFS-RENSA-FAELT-IN                                        
426700     END-IF                                                               
426800     .                                                                    
426900     EJECT                                                                
427000                                                                          
427100 IA-FLYTTA-INMATADE-FAELT SECTION.                                        
427200                                                                          
427300*--- BELOW IS FOR UPDATE OF WDK6                                          
427400     IF MID-IDREFTAB  = ALL '+'                                           
427500       CONTINUE                                                           
427600     ELSE                                                                 
427700       MOVE 'J'              TO K6-CREF-FLTABUPD                          
427800       MOVE MID-IDREFTAB     TO K6-CREF-IDREFTAB                          
427900     END-IF                                                               
428000                                                                          
428100     IF MID-FLWILSON  = ALL '+'                                           
428200       CONTINUE                                                           
428300     ELSE                                                                 
428400       MOVE MID-FLWILSON     TO K6-CREF-FLWILSON                          
428500     END-IF                                                               
428600                                                                          
428700     IF MID-KVREFPKT  = ALL '+'                                           
428800       CONTINUE                                                           
428900     ELSE                                                                 
429000       MOVE MID-KVREFPKT     TO K6-CREF-KVREFPKT                          
429100     END-IF                                                               
429200                                                                          
429300     IF MID-TIREFPKT  = ALL '+'                                           
429400       MOVE ZERO             TO WS-TIREFPKT                               
429500     ELSE                                                                 
429600       MOVE MID-TIREFPKT     TO K6-CREF-TIREFPKT                          
429700                                WS-TIREFPKT                               
429800     END-IF                                                               
429900                                                                          
430000                                                                          
430100     IF MID-FLREFILL = ALL '+'                                            
430200       CONTINUE                                                           
430300     ELSE                                                                 
430400       MOVE MID-FLREFILL    TO K6-CREF-FLREFILL                           
430500     END-IF                                                               
430600                                                                          
430700     IF MID-KVREFBER  = ALL '+'                                           
430800       CONTINUE                                                           
430900     ELSE                                                                 
431000       MOVE MID-KVREFBER    TO K6-CLAG-KVQ                                
431100     END-IF                                                               
431200                                                                          
431300     IF MID-FLREFBEO = ALL '+'                                            
431400       CONTINUE                                                           
431500     ELSE                                                                 
431600*---     OM FL-AUTOMATISK-BEORDRING SÄTTS TILL JA, SLÄCKS                 
431700*---     EV FL-VÄNTA-TILLS-NY-OI                                          
431800       IF MID-FLREFBEO = JA                                               
431900       AND (K6-CREF-FLREFBEO = NEJ                                        
432000       OR   K6-CREF-FLREFBEO = 'S')                                       
432100           MOVE JA TO WS-SLACK-FLREFNYO                                   
432200       END-IF                                                             
432300       MOVE MID-FLREFBEO    TO K6-CREF-FLREFBEO                           
432400     END-IF                                                               
432500                                                                          
432600     IF MID-TIREFPAF  = ALL '+'                                           
432700       MOVE ZERO             TO WS-TIREFPAF                               
432800     ELSE                                                                 
432900       MOVE MID-TIREFPAF     TO K6-CREF-TIREFPAF                          
433000                                WS-TIREFPAF                               
433100     END-IF                                                               
433200                                                                          
433300     IF MID-TIREFSTO = ALL '+'                                            
433400       CONTINUE                                                           
433500     ELSE                                                                 
433600       MOVE JA TO WS-SLACK-FLREFNYO                                       
433700       MOVE MID-TIREFSTO     TO K6-CREF-TIREFSTO                          
433800     END-IF                                                               
433900                                                                          
434000     IF MID-IDPERSON-BUY = ALL '+'                                        
434100     OR DCS-NDC-NA                                                        
434200       CONTINUE                                                           
434300     ELSE                                                                 
434400       MOVE MID-IDPERSON-BUY TO K6-CREF-IDPERSON-BUY                      
434500       IF MID-IDPERSON-BUY NOT = ZERO                                     
434600         MOVE JA             TO K6-CREF-FLBUYUPD                          
434700       END-IF                                                             
434800     END-IF                                                               
434900                                                                          
435000     IF MID-FLBUYUPD = ALL '+'                                            
435100       CONTINUE                                                           
435200     ELSE                                                                 
435300       MOVE MID-FLBUYUPD     TO K6-CREF-FLBUYUPD                          
435400     END-IF                                                               
435500                                                                          
435600     IF MID-FLTABUPD = ALL '+'                                            
435700       CONTINUE                                                           
435800     ELSE                                                                 
435900       MOVE MID-FLTABUPD     TO K6-CREF-FLTABUPD                          
436000     END-IF                                                               
436100                                                                          
436200     IF MID-FLPB-FLYTT = ALL '+'                                          
436300       CONTINUE                                                           
436400     ELSE                                                                 
436500       MOVE MID-FLPB-FLYTT   TO K6-CREF-FLPB-FLYTT                        
436600     END-IF                                                               
436700                                                                          
436800     IF MID-FLREFNYO = ALL '+'                                            
436900       CONTINUE                                                           
437000     ELSE                                                                 
437100       MOVE MID-FLREFNYO     TO K6-CREF-FLREFNYO                          
437200     END-IF                                                               
437300                                                                          
437400     IF WS-SLACK-FLREFNYO = JA                                            
437500        MOVE NEJ             TO K6-CREF-FLREFNYO                          
437600     END-IF                                                               
437700                                                                          
437800     PERFORM IAA-MANUELL-PAFYLLN                                          
437900     .                                                                    
438000     EJECT                                                                
438100 IAA-MANUELL-PAFYLLN SECTION.                                             
438200***                                                                       
438300*  OM DATUM FÖR MANUELL PÅFYLLNADS-PUNKT ELLER -KVANTITET                 
438400*  ÄR SATT BERÄKNA ÖVERLAGERPUNKTEN (-KVREFOVL)                           
438500***                                                                       
438600                                                                          
438700     IF (   WS-TIREFPKT > ZERO                                            
438800         OR WS-TIREFPAF > ZERO)                                           
438900                                                                          
439000        INITIALIZE CDC-REFL-W272REFL                                      
439100        MOVE W-IDARTNR          TO CDC-REFL-IDARTNR                       
439200        MOVE W-IDDC             TO CDC-REFL-IDDC                          
439300        MOVE K6-CREF-IDDC-REF   TO CDC-REFL-IDDC-REF                      
439400        MOVE K6-CREF-IDREFTAB   TO CDC-REFL-IDREFTAB                      
439500        MOVE K6-CREF-FLREFBEO   TO CDC-REFL-FLREFBEO                      
439600        MOVE K6-CREF-FLWILSON   TO CDC-REFL-FLWILSON                      
439700        MOVE K6-CLAG-PRARTSTD   TO CDC-REFL-PRARTBES                      
439800        MOVE K6-CREF-FLFLYG     TO CDC-REFL-FLFLYG                        
439900        MOVE DAGENS-DATUM       TO CDC-REFL-BINNDAY-TIAAMMDD              
440000*                                                                         
440100        MOVE MFS-RENSA-FAELT    TO MOD-KVPBREOI                           
440200*                                                                         
440300        MOVE K6-CREF-TIREFPKT   TO TMP1-YYMMDD                            
440400        MOVE DAGENS-DATUM       TO TMP2-YYMMDD                            
440500        PERFORM WY2000P1                                                  
440600        IF  TMP1-YYMMDD         >= TMP2-YYMMDD                            
440700          MOVE K6-CREF-KVREFPKT                                           
440800                                TO CDC-REFL-IN-KVREFPKT                   
440900        ELSE                                                              
441000          MOVE ZERO             TO CDC-REFL-IN-KVREFPKT                   
441100        END-IF                                                            
441200                                                                          
441300        MOVE K6-CREF-TIREFPAF   TO TMP1-YYMMDD                            
441400        MOVE DAGENS-DATUM       TO TMP2-YYMMDD                            
441500        PERFORM WY2000P1                                                  
441600        IF TMP1-YYMMDD          >= TMP2-YYMMDD                            
441700          MOVE K6-CLAG-KVQ      TO CDC-REFL-IN-KVREFBER                   
441800        ELSE                                                              
441900          MOVE ZERO             TO CDC-REFL-IN-KVREFBER                   
442000        END-IF                                                            
442100*                                                                         
442200        CALL W272REFL       USING CDC-REFL-W272REFL                       
442300                                  REFL2-2501-PCB                          
442400                                  REFL2-WDB6-PCB                          
442500                                  REFL2-UTIL-WDK6-PCB                     
442600                                  REFL2-UTIL-WDK7-PCB                     
442700                                  REFL2-UTIL-WDB6-PCB                     
442800*                                                                         
442900        MOVE CDC-REFL-KVREFOVL TO K6-CREF-KVREFOVL                        
443000     END-IF                                                               
443100     .                                                                    
443200     EJECT                                                                
443300 IB-UPD-NOTES-WDK625    SECTION.                                          
443400                                                                          
443500     IF MID-TEREFMED            = ALL '+'                                 
443600        CONTINUE                                                          
443700     ELSE                                                                 
443800        MOVE MID-TEREFMED      TO WS-TEREFMED                             
443900        MOVE 1                 TO W-KDNOTTYP                              
444000        PERFORM IMS-GHU-WDK625                                            
444100        MOVE SPACES            TO NOT-TEARTNOT                            
444200        IF WS-COMMENT-1     NOT > SPACES                                  
444300          IF SEGMENT-FINNS                                                
444400             PERFORM IMS-DLET-WDK625                                      
444500          END-IF                                                          
444600        ELSE                                                              
444700          MOVE WS-COMMENT-1    TO NOT-TEARTNOT                            
444800          IF SEGMENT-FINNS                                                
444900             PERFORM IMS-REPL-WDK625                                      
445000          ELSE                                                            
445100            IF SEGMENT-SAKNAS                                             
445200               MOVE  1         TO NOT-KDNOTTYP                            
445300               PERFORM IMS-ISRT-WDK625                                    
445400            END-IF                                                        
445500          END-IF                                                          
445600        END-IF                                                            
445700                                                                          
445800        MOVE 2                 TO W-KDNOTTYP                              
445900        PERFORM IMS-GHU-WDK625                                            
446000        MOVE SPACES            TO NOT-TEARTNOT                            
446100        IF WS-COMMENT-2     NOT > SPACES                                  
446200          IF SEGMENT-FINNS                                                
446300             PERFORM IMS-DLET-WDK625                                      
446400          END-IF                                                          
446500        ELSE                                                              
446600          MOVE WS-COMMENT-2    TO NOT-TEARTNOT  (1:32)                    
446700          IF SEGMENT-FINNS                                                
446800             PERFORM IMS-REPL-WDK625                                      
446900          ELSE                                                            
447000            IF SEGMENT-SAKNAS                                             
447100               MOVE  2         TO NOT-KDNOTTYP                            
447200               PERFORM IMS-ISRT-WDK625                                    
447300            END-IF                                                        
447400          END-IF                                                          
447500        END-IF                                                            
447600     END-IF                                                               
447700     .                                                                    
447800     EJECT                                                                
447900 IC-UPDATE-WDK626 SECTION.                                                
448000                                                                          
448100     IF MID-KVPB-JUST-1   NOT = ALL '+'                                   
448200     OR MID-TIPBJUST-1    NOT = ALL '+'                                   
448300     OR MID-KVPB-JUST-2   NOT = ALL '+'                                   
448400     OR MID-TIPBJUST-2    NOT = ALL '+'                                   
448500        PERFORM IMS-GHU-WDK626                                            
448600        IF SEGMENT-FINNS                                                  
448700           IF MID-KVPB-JUST-1 NOT   = ALL '+'                             
448800              MOVE WS-KVPB-JUST-1  TO JUST-KVPB-JUST(1)                   
448900           END-IF                                                         
449000           IF MID-KVPB-JUST-2 NOT   = ALL '+'                             
449100              MOVE WS-KVPB-JUST-2  TO JUST-KVPB-JUST(2)                   
449200           END-IF                                                         
449300           IF MID-TIPBJUST-1 NOT    = ALL '+'                             
449400              MOVE MID-TIPBJUST-1  TO JUST-TIPBJUST(1)                    
449500           END-IF                                                         
449600           IF MID-TIPBJUST-2 NOT    = ALL '+'                             
449700              MOVE MID-TIPBJUST-2  TO JUST-TIPBJUST(2)                    
449800           END-IF                                                         
449900           PERFORM IMS-REPL-WDK626                                        
450000        ELSE                                                              
450100           MOVE ZERO               TO JUST-REPBJUST                       
450200                                      JUST-TIPBJUST-CENTR                 
450300                                      JUST-DAMANSEA                       
450400                                      JUST-DASPSEA                        
450500                                                                          
450600           MOVE WS-KVPB-JUST-1     TO JUST-KVPB-JUST(1)                   
450700           MOVE WS-KVPB-JUST-2     TO JUST-KVPB-JUST(2)                   
450800           MOVE WS-TIPBJUST-1      TO JUST-TIPBJUST(1)                    
450900           MOVE WS-TIPBJUST-2      TO JUST-TIPBJUST(2)                    
451000*                                                                         
451100           MOVE +1                 TO PB-IX                               
451200           PERFORM UNTIL PB-IX > 12                                       
451300             MOVE +1               TO JUST-RESEASON (PB-IX)               
451400             ADD  +1               TO PB-IX                               
451500           END-PERFORM                                                    
451600*                                                                         
451700           PERFORM IMS-ISRT-WDK626                                        
451800        END-IF                                                            
451900*                                                                         
452000        PERFORM ICA-UPDATE-EVENT-2207                                     
452100*                                                                         
452200        PERFORM IMS-GHU-WDK61129                                          
452300        IF K6-CREF-KDREFSTA = 'P'                                         
452400           MOVE AKTIV              TO K6-CREF-KDREFSTA                    
452500           MOVE DAGENS-DATUM       TO K6-CREF-TIREFSTA                    
452600           PERFORM IMS-REPL-WDK61129                                      
452700        END-IF                                                            
452800        MOVE JA                    TO WDK6-UPD-STAT-SW                    
452900     END-IF                                                               
453000     .                                                                    
453100     EJECT                                                                
453200 ICA-UPDATE-EVENT-2207  SECTION.                                          
453300                                                                          
453400     IF WS-TIPBJUST-1  > 0                                                
453500        MOVE 2207               TO W-IDHTYP                               
453600        MOVE LOW-VALUE          TO W-NYCKEL-VALFRI                        
453700        MOVE LOW-VALUE          TO DLI-IO-WDG302                          
453800        MOVE W-IDARTNR          TO 2207-IDARTNR                           
453900        MOVE WS-TIPBJUST-1      TO 2207-TIPBJUST                          
454000        PERFORM IMS-ISRT-2207                                             
454100     END-IF                                                               
454200     IF WS-TIPBJUST-2  > 0                                                
454300        MOVE 2207               TO W-IDHTYP                               
454400        MOVE LOW-VALUE          TO W-NYCKEL-VALFRI                        
454500        MOVE LOW-VALUE          TO DLI-IO-WDG302                          
454600        MOVE WS-TIPBJUST-2      TO 2207-TIPBJUST                          
454700        MOVE W-IDARTNR          TO 2207-IDARTNR                           
454800        PERFORM IMS-ISRT-2207                                             
454900     END-IF                                                               
455000     .                                                                    
455100     EJECT                                                                
455200 S01-KOLLA-OM-REFILL-OK SECTION.                                          
455300                                                                          
455400     IF CLAG-FLREFILL = NEJ AND DCS-SDC                                   
455500*                                                                         
455600       MOVE ART-KDPRODSL         TO TEST-KDPRODSL                         
455700       IF (DCS-FLEXCP1-REFBER = JA                                        
455800       AND KDPRODSL-BIMA                                                  
455900       AND NOT (BYT03-OBJEKT                                              
456000       OR     ART-KDSORT = 'SW'                                           
456100       OR     CLAG-FLLSRDEL = NEJ))                                       
456200          CONTINUE                                                        
456300       ELSE                                                               
456400         MOVE EJ-GODK-REFILL TO MED-IDMFSFEL                              
456500         CALL WMEDKONV USING MED-WMEDAREA                                 
456600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
456700         MOVE NEJ TO REFILL-ART-OK-SW                                     
456800       END-IF                                                             
456900     END-IF                                                               
457000                                                                          
457100     .                                                                    
457200     EJECT                                                                
457300                                                                          
457400                                                                          
457500 S02-GET-BESPRIS  SECTION.                                                
457600     IF DCS-CHINA OR DCS-NDC-NA                                           
457700       PERFORM S02A-GET-BESPRIS                                           
457800     ELSE                                                                 
457900       PERFORM S02B-GET-BESPRIS                                           
458000     END-IF                                                               
458100     .                                                                    
458200     EJECT                                                                
458300                                                                          
458400 S02A-GET-BESPRIS SECTION.                                                
458500*    -- WDK712                                                            
458600     MOVE DCS-IDLANDX2    TO W-IDLAND                                     
458700     PERFORM IMS-GU-WDK712                                                
458800     IF SEGMENT-FINNS                                                     
458900       MOVE LART-PRMATRL     TO WS-PRARTBES                               
459000                                WS-PRIS                                   
459100     ELSE                                                                 
459200       MOVE ZERO             TO WS-PRARTBES                               
459300                                WS-PRIS                                   
459400     END-IF                                                               
459500     .                                                                    
459600     EJECT                                                                
459700                                                                          
459800 S02B-GET-BESPRIS SECTION.                                                
459900                                                                          
460000     MOVE CLAG-PRARTSTD         TO  WS-PRARTBES                           
460100                                                                          
460200     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-AAAAMMDD                   
460300     COMPUTE W-DAPRLIST = 99999999 - DAGENS-AAAAMMDD                      
460400     PERFORM IMS-GNP-WDK621                                               
460500     IF SEGMENT-SAKNAS                                                    
460600       MOVE CLAG-PRARTSTD       TO  WS-PRIS                               
460700     ELSE                                                                 
460800       MOVE NEJ                 TO FL-PRARTBES                            
460900       PERFORM UNTIL  SEGMENT-SAKNAS                                      
461000         IF PRL-SUINLEV-PR > ZERO                                         
461100           MOVE PRL-PRARTBES-PR TO WS-PRIS                                
461200           SET SEGMENT-SAKNAS   TO TRUE                                   
461300         ELSE                                                             
461400           IF FL-PRARTBES = NEJ                                           
461500             MOVE PRL-PRARTBES-PR                                         
461600                                TO WS-PRIS                                
461700             MOVE JA            TO FL-PRARTBES                            
461800           END-IF                                                         
461900           PERFORM IMS-GNP-WDK621                                         
462000         END-IF                                                           
462100       END-PERFORM                                                        
462200     END-IF                                                               
462300     .                                                                    
462400     EJECT                                                                
462500 S03-FLYTTA-INMATADE-VAERDEN SECTION.                                     
462600                                                                          
462700     IF MID-IDREFTAB  NOT = ALL '+'                                       
462800       MOVE MID-IDREFTAB     TO WDK7-IDREFTAB                             
462900     END-IF                                                               
463000                                                                          
463100     IF MID-FLWILSON  NOT = ALL '+'                                       
463200       MOVE MID-FLWILSON     TO WDK7-FLWILSON                             
463300     END-IF                                                               
463400                                                                          
463500     IF MID-KVREFPKT  NOT = ALL '+'                                       
463600       MOVE MID-KVREFPKT     TO WDK7-KVREFPKT                             
463700     END-IF                                                               
463800                                                                          
463900     IF MID-TIREFPKT  NOT = ALL '+'                                       
464000       MOVE MID-TIREFPKT     TO WDK7-TIREFPKT                             
464100     END-IF                                                               
464200                                                                          
464300     IF MID-KVREFBER  NOT = ALL '+'                                       
464400       MOVE MID-KVREFBER     TO WDK7-KVREFBER                             
464500     END-IF                                                               
464600                                                                          
464700     IF MID-FLREFBEO NOT = ALL '+'                                        
464800       MOVE MID-FLREFBEO    TO WDK7-FLREFBEO                              
464900     END-IF                                                               
465000                                                                          
465100     IF MID-TIREFPAF  NOT = ALL '+'                                       
465200       MOVE MID-TIREFPAF     TO WDK7-TIREFPAF                             
465300     END-IF                                                               
465400                                                                          
465500     IF MID-TIREFSTO NOT = ALL '+'                                        
465600       MOVE MID-TIREFSTO     TO WDK7-TIREFSTO                             
465700     END-IF                                                               
465800                                                                          
465900     IF MID-IDPERSON-BUY = ALL '+'                                        
466000       CONTINUE                                                           
466100     ELSE                                                                 
466200       MOVE MID-IDPERSON-BUY TO WDK7-IDPERSON-BUY                         
466300     END-IF                                                               
466400                                                                          
466500     IF MID-FLBUYUPD NOT = ALL '+'                                        
466600       MOVE MID-FLBUYUPD     TO WDK7-FLBUYUPD                             
466700     END-IF                                                               
466800                                                                          
466900     IF MID-FLTABUPD NOT = ALL '+'                                        
467000       MOVE MID-FLTABUPD     TO WDK7-FLTABUPD                             
467100     END-IF                                                               
467200                                                                          
467300     IF MID-FLPB-FLYTT NOT = ALL '+'                                      
467400       MOVE MID-FLPB-FLYTT   TO WDK7-FLPB-FLYTT                           
467500     END-IF                                                               
467600                                                                          
467700     IF MID-FLREFNYO NOT = ALL '+'                                        
467800       MOVE MID-FLREFNYO     TO WDK7-FLREFNYO                             
467900     END-IF                                                               
468000                                                                          
468100     IF MID-FLREFILL NOT = ALL '+'                                        
468200       MOVE MID-FLREFILL TO WDK7-FLREFILL                                 
468300     ELSE                                                                 
468400       PERFORM S01-KOLLA-OM-REFILL-OK                                     
468500       IF REFILL-ART-OK                                                   
468600         MOVE JA         TO WDK7-FLREFILL                                 
468700       ELSE                                                               
468800         MOVE NEJ        TO WDK7-FLREFILL                                 
468900       END-IF                                                             
469000     END-IF                                                               
469100     .                                                                    
469200     EJECT                                                                
469300 S04-GET-FC-TOT       SECTION.                                            
469400                                                                          
469500     INITIALIZE UTIL-W271UTIL                                             
469600     MOVE W-IDARTNR                 TO UTIL-IDARTNR                       
469700     MOVE 003                       TO UTIL-KDCALL                        
469800                                                                          
469900     CALL W271UTIL USING UTIL-W271UTIL                                    
470000                         UTIL-WDK6-PCB                                    
470100                         UTIL-WDK7-PCB                                    
470200                         UTIL-WDB6-PCB                                    
470300                                                                          
470400     IF UTIL-KDSVAR-OK                                                    
470500        MOVE UTIL-KVPB-TOT          TO MOD-KVPB-SUM                       
470600     ELSE                                                                 
470700        STRING 'FEL FRÅN W271UTIL '   UTIL-KDSVAR                         
470800        DELIMITED BY SIZE INTO FELTEXT                                    
470900        CALL FELLOG                                                       
471000     END-IF                                                               
471100     .                                                                    
471200     EJECT                                                                
471300 S90-CALL-W271REFL SECTION.                                               
471400                                                                          
471500     INITIALIZE    W271-REFL-W271REFL                                     
471600                                                                          
471700     MOVE DCS-IDDC            TO W271-REFL-IDDC                           
471800                                                                          
471900     PERFORM IMS-GU-WDK601                                                
472000     IF SEGMENT-FINNS                                                     
472100                                                                          
472200       PERFORM IMS-GNP-WDK611                                             
472300                                                                          
472400       IF SEGMENT-FINNS                                                   
472500         PERFORM S02-GET-BESPRIS                                          
472600                                                                          
472700***LAGT PRMATRL I WS-PRARTBES                                             
472800         MOVE WS-PRARTBES     TO W271-REFL-PRARTBES                       
472900                                                                          
473000         MOVE W-IDARTNR       TO W271-REFL-IDARTNR                        
473100         MOVE SLAG-IDREFTAB   TO W271-REFL-IDREFTAB                       
473200         MOVE SLAG-FLWILSON   TO W271-REFL-FLWILSON                       
473300         MOVE SLAG-IDLEVNR    TO W271-REFL-IN-IDLEVNR-DC                  
473400         MOVE SLAG-IDDC-REF   TO W271-REFL-IDDC-REF                       
473500         MOVE SLAG-FLFLYG     TO W271-REFL-FLFLYG                         
473600                                                                          
473700         MOVE SLAG-IDLEVNR    TO W-IDLEVNR                                
473800         MOVE 1 TO INDX                                                   
473900         PERFORM UNTIL INDX > 12                                          
474000            MOVE SLAG-RESEASON(INDX) TO W271-REFL-RESEASON(INDX)          
474100            ADD 1 TO INDX                                                 
474200         END-PERFORM                                                      
474300*                                                                         
474400         IF WS-TIREFPKT > ZERO                                            
474500            MOVE WS-TIREFPKT     TO TMP1-YYMMDD                           
474600         ELSE                                                             
474700            MOVE SLAG-TIREFPKT   TO TMP1-YYMMDD                           
474800         END-IF                                                           
474900                                                                          
475000         MOVE DAGENS-DATUM       TO TMP2-YYMMDD                           
475100         PERFORM WY2000P1                                                 
475200         IF TMP1-YYMMDD >= TMP2-YYMMDD                                    
475300           MOVE SLAG-KVREFPKT    TO W271-REFL-IN-KVREFPKT                 
475400         ELSE                                                             
475500           MOVE ZERO             TO W271-REFL-IN-KVREFPKT                 
475600         END-IF                                                           
475700                                                                          
475800         IF WS-TIREFPAF > ZERO                                            
475900            MOVE WS-TIREFPAF     TO TMP1-YYMMDD                           
476000         ELSE                                                             
476100            MOVE SLAG-TIREFPAF   TO TMP1-YYMMDD                           
476200         END-IF                                                           
476300                                                                          
476400         MOVE DAGENS-DATUM       TO TMP2-YYMMDD                           
476500         PERFORM WY2000P1                                                 
476600         IF TMP1-YYMMDD >= TMP2-YYMMDD                                    
476700           MOVE SLAG-KVREFBER    TO W271-REFL-IN-KVREFBER                 
476800         ELSE                                                             
476900           MOVE ZERO             TO W271-REFL-IN-KVREFBER                 
477000         END-IF                                                           
477100                                                                          
477200         IF DCS-SDC                                                       
477300            MOVE ZERO TO W271-REFL-NDC-KVDAGAR-TBT-DC                     
477400         END-IF                                                           
477900         IF DCS-NDC                                                       
478000            IF SLAG-IDDC-REF NOT = SPACES                                 
478300               MOVE ZERO TO W271-REFL-NDC-KVDAGAR-TBT-DC                  
478400            ELSE                                                          
478500              IF SLAG-KVDAGAR-MANLT > ZERO                                
478600                MOVE SLAG-KVDAGAR-MANLT                                   
478700                             TO W271-REFL-NDC-KVDAGAR-TBT-DC              
478800                                MOD-LEDTID                                
478900              ELSE                                                        
479000                 MOVE SLAG-IDLEVNR TO W-IDLEVNR                           
479100                 PERFORM IMS-GU-LEVA16                                    
479200                 IF SEGMENT-FINNS                                         
479300                    MOVE NDC-KVDAGAR-TBT TO                               
479400                         W271-REFL-NDC-KVDAGAR-TBT-DC                     
479500                 ELSE                                                     
479600                    MOVE 1 TO W271-REFL-NDC-KVDAGAR-TBT-DC                
479700                 END-IF                                                   
479800              END-IF                                                      
479900            END-IF                                                        
480000         END-IF                                                           
480100                                                                          
480200         PERFORM S100-CALL-W271UTUP                                       
480300                                                                          
480400         CALL W271REFL USING W271-REFL-W271REFL                           
480500                             REFL1-2501-PCB                               
480600                             REFL1-WDB6-PCB                               
480700                             REFL1-WDK7-PCB                               
480800                             REFL1-UTIL-WDK6-PCB                          
480900                             REFL1-UTIL-WDK7-PCB                          
481000                             REFL1-UTIL-WDB6-PCB                          
481100                                                                          
481200       END-IF                                                             
481300     END-IF                                                               
481400     .                                                                    
481500     EJECT                                                                
481600 S100-CALL-W271UTUP SECTION.                                              
481700                                                                          
481800***  THIS SECTION CALLS W271UTUP TO GET LEAD TIME                         
481900***  ADJUSTED DEMAND - ( LEAD TIME ADJUSTED FROM CURRENT WEEK)            
482000*                                                                         
482100     INITIALIZE W271-UTUP-W271UTUP                                        
482200     MOVE W-IDARTNR              TO W271-UTUP-IDARTNR                     
482300     MOVE SLAG-IDDC                 TO W271-UTUP-IDDC                     
482400     MOVE SLAG-IDDC-REF             TO W271-UTUP-IDDC-REF                 
482500     MOVE NEJ                       TO W271-UTUP-FLSIM                    
482600     MOVE DAGENS-DATUM              TO W271-UTUP-TIAAMMDD                 
482700     MOVE W271-REFL-NDC-KVDAGAR-TBT-DC                                    
482800                                    TO W271-UTUP-LEADTIME                 
482900     MOVE 004                       TO W271-UTUP-KDCALL                   
483000*                                                                         
483100     CALL W271UTUP USING W271-UTUP-W271UTUP                               
483200                         UTUP1-WDK7-PCB                                   
483300                         UTUP1-WDB6-PCB                                   
483400                         UTUP1-UTIL-WDK6-PCB                              
483500                         UTUP1-UTIL-WDK7-PCB                              
483600                         UTUP1-UTIL-WDB6-PCB                              
483700     IF W271-UTUP-KDSVAR-OK                                               
483800        MOVE W271-UTUP-LEADTID-BEHOV TO W271-REFL-IN-LEADTID-BEHOV        
483900     ELSE                                                                 
484000        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
484100     END-IF                                                               
484200     .                                                                    
484300     EJECT                                                                
484400                                                                          
484500                                                                          
484600 S99-ABEND SECTION.                                                       
484700                                                                          
484800     SKIP2                                                                
484900     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
485000     .                                                                    
485100     EJECT                                                                
485200                                                                          
485300                                                                          
485400 MFS-RENSA-FAELT-UT SECTION.                                              
485500                                                                          
485600*    --- ALLA UTDATA-FÄLT                                                 
485700     MOVE MFS-RENSA-FAELT TO MOD-BEART                                    
485800                             MOD-IDANSK                                   
485900                             MOD-PRICE-TEXT                               
486000                             MOD-PRARTSTD                                 
486100                             MOD-REPPFAKT                                 
486200                             MOD-FLCDART                                  
486300                             MOD-ADLAGOMR                                 
486400                             MOD-ADGANG                                   
486500                             MOD-ADPLATS                                  
486600                             MOD-KDERS                                    
486700                             MOD-KLASS                                    
486800                             MOD-KVDISP                                   
486900                             MOD-IDREFTAB                                 
487000                             MOD-FLWILSON                                 
487100                             MOD-TIORDREG                                 
487200                             MOD-KVAKS-PAV                                
487300                             MOD-TIREFEFT                                 
487400                             MOD-KVAKS-SDC                                
487500                             MOD-KVREFPKT                                 
487600                             MOD-KVBEART                                  
487700                             MOD-TIREFPKT                                 
487800                             MOD-FLREFILL                                 
487900                             MOD-FLREFNYO                                 
488000                             MOD-KVREFBER                                 
488100                             MOD-FLREFBEO                                 
488200                             MOD-TIREFPAF                                 
488300                             MOD-KVREFOVL                                 
488400                             MOD-TIREFSTO                                 
488500                             MOD-LEDTID                                   
488600                             MOD-KDREFSTA                                 
488700                             MOD-TIREFSTA                                 
488800                             MOD-KVPB-SUM                                 
488900                             MOD-KVPB-REF                                 
489000                             MOD-KVPBREOI                                 
489100                             MOD-TIREFMPB                                 
489200                             MOD-TIPBREOI                                 
489300                             MOD-IDPERSON-BUY                             
489400                             MOD-FLBUYUPD                                 
489500                             MOD-FLTABUPD                                 
489600                             MOD-FLPB-FLYTT                               
489700                             MOD-KVPB-JUST-1                              
489800                             MOD-KVPB-JUST-2                              
489900                             MOD-TIPBJUST-1                               
490000                             MOD-TIPBJUST-2                               
490100                             MOD-TEMFSINF                                 
490200     .                                                                    
490300     SKIP2                                                                
490400 MFS-RENSA-FAELT-IN SECTION.                                              
490500                                                                          
490600*    --- ALLA INDATA-FÄLT                                                 
490700     MOVE MFS-RENSA-FAELT TO MOD-IDREFTAB-IN                              
490800                             MOD-FLWILSON-IN                              
490900                             MOD-KVREFPKT-IN                              
491000                             MOD-TIREFPKT-IN                              
491100                             MOD-FLREFILL-IN                              
491200                             MOD-KVREFBER-IN                              
491300                             MOD-FLREFBEO-IN                              
491400                             MOD-TIREFPAF-IN                              
491500                             MOD-TIREFSTO-IN                              
491600                             MOD-IDPERSON-BUY-IN                          
491700                             MOD-FLBUYUPD-IN                              
491800                             MOD-FLTABUPD-IN                              
491900                             MOD-FLPB-FLYTT-IN                            
492000                             MOD-FLREFNYO-IN                              
492100                             MOD-IN-KVPB-JUST-1                           
492200                             MOD-IN-KVPB-JUST-2                           
492300                             MOD-IN-TIPBJUST-1                            
492400                             MOD-IN-TIPBJUST-2                            
492500                             MOD-TEREFMED                                 
492600     .                                                                    
492700     EJECT                                                                
492800 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
492900                                                                          
493000*    --- ALLA UTDATA-FÄLT                                                 
493100     MOVE MFS-ROER-EJ-FAELT TO MOD-BEART                                  
493200                               MOD-IDANSK                                 
493300                               MOD-PRICE-TEXT                             
493400                               MOD-PRARTSTD                               
493500                               MOD-REPPFAKT                               
493600                               MOD-FLCDART                                
493700                               MOD-ADLAGOMR                               
493800                               MOD-ADGANG                                 
493900                               MOD-ADPLATS                                
494000                               MOD-KDERS                                  
494100                               MOD-KLASS                                  
494200                               MOD-IDREFTAB                               
494300                               MOD-FLWILSON                               
494400                               MOD-KVDISP                                 
494500                               MOD-TIORDREG                               
494600                               MOD-KVAKS-PAV                              
494700                               MOD-TIREFEFT                               
494800                               MOD-KVAKS-SDC                              
494900                               MOD-KVREFPKT                               
495000                               MOD-KVBEART                                
495100                               MOD-TIREFPKT                               
495200                               MOD-FLREFILL                               
495300                               MOD-FLREFNYO                               
495400                               MOD-KVREFBER                               
495500                               MOD-FLREFBEO                               
495600                               MOD-TIREFPAF                               
495700                               MOD-KVREFOVL                               
495800                               MOD-TIREFSTO                               
495900                               MOD-LEDTID                                 
496000                               MOD-KDREFSTA                               
496100                               MOD-TIREFSTA                               
496200                               MOD-KVPB-SUM                               
496300                               MOD-KVPB-REF                               
496400                               MOD-KVPBREOI                               
496500                               MOD-TIREFMPB                               
496600                               MOD-TIPBREOI                               
496700                               MOD-IDPERSON-BUY                           
496800                               MOD-FLBUYUPD                               
496900                               MOD-FLTABUPD                               
497000                               MOD-FLPB-FLYTT                             
497100                               MOD-TEREFMED                               
497200                               MOD-KVPB-JUST-1                            
497300                               MOD-KVPB-JUST-2                            
497400                               MOD-TIPBJUST-1                             
497500                               MOD-TIPBJUST-2                             
497600                               MOD-TEMFSINF                               
497700     .                                                                    
497800     SKIP2                                                                
497900 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
498000                                                                          
498100*    --- ALLA INDATA-FÄLT                                                 
498200     MOVE MFS-ROER-EJ-FAELT TO MOD-IDREFTAB-IN                            
498300                               MOD-FLWILSON-IN                            
498400                               MOD-KVREFPKT-IN                            
498500                               MOD-TIREFPKT-IN                            
498600                               MOD-FLREFILL-IN                            
498700                               MOD-KVREFBER-IN                            
498800                               MOD-FLREFBEO-IN                            
498900                               MOD-TIREFPAF-IN                            
499000                               MOD-TIREFSTO-IN                            
499100                               MOD-FLBUYUPD-IN                            
499200                               MOD-FLTABUPD-IN                            
499300                               MOD-FLPB-FLYTT-IN                          
499400                               MOD-FLREFNYO-IN                            
499500                               MOD-IN-KVPB-JUST-1                         
499600                               MOD-IN-KVPB-JUST-2                         
499700                               MOD-IN-TIPBJUST-1                          
499800                               MOD-IN-TIPBJUST-2                          
499900                               MOD-TEREFMED                               
500000     .                                                                    
500100     EJECT                                                                
500200 MFS-FORM-ATTR SECTION.                                                   
500300                                                                          
500400*    --- ALLA INDATA-FÄLT                                                 
500500     MOVE MFS-FORMATETS-ATTR TO MOD-IDREFTAB-IN-ATTR                      
500600                                MOD-FLWILSON-IN-ATTR                      
500700                                MOD-KVREFPKT-IN-ATTR                      
500800                                MOD-TIREFPKT-IN-ATTR                      
500900                                MOD-FLREFILL-IN-ATTR                      
501000                                MOD-KVREFBER-IN-ATTR                      
501100                                MOD-FLREFBEO-IN-ATTR                      
501200                                MOD-TIREFPAF-IN-ATTR                      
501300                                MOD-TIREFSTO-IN-ATTR                      
501400                                MOD-IDPERSON-BUY-IN-ATTR                  
501500                                MOD-FLBUYUPD-IN-ATTR                      
501600                                MOD-FLTABUPD-IN-ATTR                      
501700                                MOD-FLPB-FLYTT-IN-ATTR                    
501800                                MOD-FLREFNYO-IN-ATTR                      
501900                                MOD-IN-KVPB-JUST-1-ATTR                   
502000                                MOD-IN-KVPB-JUST-2-ATTR                   
502100                                MOD-IN-TIPBJUST-1-ATTR                    
502200                                MOD-IN-TIPBJUST-2-ATTR                    
502300                                MOD-TEREFMED-ATTR                         
502400     .                                                                    
502500     SKIP2                                                                
502600 MFS-LAES-IN-IGEN SECTION.                                                
502700                                                                          
502800*    --- ALLA INDATA-FÄLT                                                 
502900     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDREFTAB-IN-ATTR                   
503000                                   MOD-FLWILSON-IN-ATTR                   
503100                                   MOD-KVREFPKT-IN-ATTR                   
503200                                   MOD-TIREFPKT-IN-ATTR                   
503300                                   MOD-FLREFILL-IN-ATTR                   
503400                                   MOD-KVREFBER-IN-ATTR                   
503500                                   MOD-FLREFBEO-IN-ATTR                   
503600                                   MOD-TIREFPAF-IN-ATTR                   
503700                                   MOD-TIREFSTO-IN-ATTR                   
503800                                   MOD-FLBUYUPD-IN-ATTR                   
503900                                   MOD-FLTABUPD-IN-ATTR                   
504000                                   MOD-FLPB-FLYTT-IN-ATTR                 
504100                                   MOD-FLREFNYO-IN-ATTR                   
504200                                   MOD-IN-KVPB-JUST-1-ATTR                
504300                                   MOD-IN-KVPB-JUST-2-ATTR                
504400                                   MOD-IN-TIPBJUST-1-ATTR                 
504500                                   MOD-IN-TIPBJUST-2-ATTR                 
504600                                   MOD-TEREFMED-ATTR                      
504700     .                                                                    
504800     EJECT                                                                
504900* --- IMS SEKTIONER ---                                                   
505000     SKIP3                                                                
505100 IMS-GET-MSG SECTION.                                                     
505200                                                                          
505300     MOVE '  QC' TO GODK-STATUSKODER                                      
505400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
505500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
505600     PERFORM IMS-STATUSKONTROLL                                           
505700     .                                                                    
505800     SKIP3                                                                
505900 IMS-INSERT-MSG SECTION.                                                  
506000                                                                          
506100     MOVE 'N'              TO MFS-KDHUVOMR                                
506200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
506300     MOVE SPACE TO GODK-STATUSKODER                                       
506400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
506500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
506600     PERFORM IMS-STATUSKONTROLL                                           
506700     .                                                                    
506800     EJECT                                                                
506900 IMS-GU-WDK601      SECTION.                                              
507000                                                                          
507100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
507200          DELIMITED BY SIZE INTO SSA1                                     
507300     MOVE '  GE' TO GODK-STATUSKODER                                      
507400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
507500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
507600     PERFORM IMS-STATUSKONTROLL                                           
507700     .                                                                    
507800     EJECT                                                                
507900 IMS-GNP-WDK611      SECTION.                                             
508000                                                                          
508100     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
508200          DELIMITED BY SIZE INTO SSA1                                     
508300     MOVE '  GE' TO GODK-STATUSKODER                                      
508400     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
508500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
508600     PERFORM IMS-STATUSKONTROLL                                           
508700     .                                                                    
508800     EJECT                                                                
508900 IMS-GNP-WDK621      SECTION.                                             
509000                                                                          
509100     STRING 'WDK621  (DAPRLIST=>' W-DAPRLIST-X ')'                        
509200          DELIMITED BY SIZE INTO SSA1                                     
509300     MOVE '  GE' TO GODK-STATUSKODER                                      
509400     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK621 SSA1                   
509500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
509600     PERFORM IMS-STATUSKONTROLL                                           
509700     .                                                                    
509800     EJECT                                                                
509900 IMS-GU-WDK611       SECTION.                                             
510000                                                                          
510100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
510200          DELIMITED BY SIZE INTO SSA1                                     
510300     MOVE 'WDK611  '       TO SSA2                                        
510400     MOVE '  GE' TO GODK-STATUSKODER                                      
510500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
510600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
510700     PERFORM IMS-STATUSKONTROLL                                           
510800     .                                                                    
510900     EJECT                                                                
511000 IMS-GNP-WDK626  SECTION.                                                 
511100                                                                          
511200     MOVE 'WDK626 ' TO SSA1                                               
511300     MOVE '  GE' TO GODK-STATUSKODER                                      
511400     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK626 SSA1                   
511500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
511600     PERFORM IMS-STATUSKONTROLL                                           
511700     .                                                                    
511800     EJECT                                                                
511900 IMS-GNP-WDK629     SECTION.                                              
512000                                                                          
512100     MOVE 'WDK629  '       TO SSA1                                        
512200     MOVE '  GE'   TO GODK-STATUSKODER                                    
512300     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK629 SSA1                   
512400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
512500     PERFORM IMS-STATUSKONTROLL                                           
512600     .                                                                    
512700     EJECT                                                                
512800 IMS-GU-WDK61129   SECTION.                                               
512900                                                                          
513000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
513100          DELIMITED BY SIZE INTO SSA1                                     
513200     STRING 'WDK611  *D(KDSEGKEY =' W-KDSEGKEY-X ')'                      
513300          DELIMITED BY SIZE INTO SSA2                                     
513400     MOVE 'WDK629  '       TO SSA3                                        
513500     MOVE '  GE' TO GODK-STATUSKODER                                      
513600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK61129                       
513700                           SSA1 SSA2 SSA3                                 
513800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
513900     PERFORM IMS-STATUSKONTROLL                                           
514000     .                                                                    
514100     SKIP3                                                                
514200 IMS-GHU-WDK61129   SECTION.                                              
514300                                                                          
514400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
514500          DELIMITED BY SIZE INTO SSA1                                     
514600     STRING 'WDK611  *D(KDSEGKEY =' W-KDSEGKEY-X ')'                      
514700          DELIMITED BY SIZE INTO SSA2                                     
514800     MOVE 'WDK629  '       TO SSA3                                        
514900     MOVE '  GE' TO GODK-STATUSKODER                                      
515000     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK61129                      
515100                            SSA1 SSA2 SSA3                                
515200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
515300     PERFORM IMS-STATUSKONTROLL                                           
515400     .                                                                    
515500     SKIP3                                                                
515600 IMS-REPL-WDK61129  SECTION.                                              
515700                                                                          
515800     MOVE '  ' TO GODK-STATUSKODER                                        
515900     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK61129                     
516000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
516100     PERFORM IMS-STATUSKONTROLL                                           
516200     .                                                                    
516300     SKIP3                                                                
516400 IMS-GU-WDK625      SECTION.                                              
516500                                                                          
516600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
516700          DELIMITED BY SIZE INTO SSA1                                     
516800     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
516900          DELIMITED BY SIZE INTO SSA2                                     
517000     STRING 'WDK625  (KDNOTTYP =' W-KDNOTTYP-X ')'                        
517100          DELIMITED BY SIZE INTO SSA3                                     
517200     MOVE '  GE' TO GODK-STATUSKODER                                      
517300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK625                         
517400                           SSA1 SSA2 SSA3                                 
517500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
517600     PERFORM IMS-STATUSKONTROLL                                           
517700     .                                                                    
517800     SKIP3                                                                
517900 IMS-GHU-WDK625     SECTION.                                              
518000                                                                          
518100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
518200          DELIMITED BY SIZE INTO SSA1                                     
518300     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
518400          DELIMITED BY SIZE INTO SSA2                                     
518500     STRING 'WDK625  (KDNOTTYP =' W-KDNOTTYP-X ')'                        
518600          DELIMITED BY SIZE INTO SSA3                                     
518700     MOVE '  GE' TO GODK-STATUSKODER                                      
518800     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK625                        
518900                           SSA1 SSA2 SSA3                                 
519000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
519100     PERFORM IMS-STATUSKONTROLL                                           
519200     .                                                                    
519300     SKIP3                                                                
519400 IMS-DLET-WDK625     SECTION.                                             
519500                                                                          
519600     MOVE '  ' TO GODK-STATUSKODER                                        
519700     CALL CBLTDLI USING DLET WDK6-PCB DLI-IO-WDK625                       
519800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
519900     PERFORM IMS-STATUSKONTROLL                                           
520000     .                                                                    
520100     SKIP3                                                                
520200 IMS-REPL-WDK625     SECTION.                                             
520300                                                                          
520400     MOVE '  ' TO GODK-STATUSKODER                                        
520500     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK625                       
520600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
520700     PERFORM IMS-STATUSKONTROLL                                           
520800     .                                                                    
520900     SKIP3                                                                
521000 IMS-ISRT-WDK625     SECTION.                                             
521100                                                                          
521200     MOVE 'WDK625   ' TO SSA1                                             
521300     MOVE '  ' TO GODK-STATUSKODER                                        
521400     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK625 SSA1                  
521500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
521600     PERFORM IMS-STATUSKONTROLL                                           
521700     .                                                                    
521800     SKIP3                                                                
521900 IMS-GHU-WDK626 SECTION.                                                  
522000                                                                          
522100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
522200          DELIMITED BY SIZE INTO SSA1                                     
522300     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
522400          DELIMITED BY SIZE INTO SSA2                                     
522500     STRING 'WDK626    '                                                  
522600          DELIMITED BY SIZE INTO SSA3                                     
522700     MOVE '  GE' TO GODK-STATUSKODER                                      
522800     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK626 SSA1 SSA2 SSA3         
522900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
523000     PERFORM IMS-STATUSKONTROLL                                           
523100     .                                                                    
523200     EJECT                                                                
523300 IMS-ISRT-WDK626 SECTION.                                                 
523400                                                                          
523500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
523600          DELIMITED BY SIZE INTO SSA1                                     
523700     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
523800          DELIMITED BY SIZE INTO SSA2                                     
523900     MOVE 'WDK626 '           TO SSA3                                     
524000     MOVE '  II' TO GODK-STATUSKODER                                      
524100     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK626 SSA1 SSA2 SSA3        
524200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
524300     PERFORM IMS-STATUSKONTROLL                                           
524400     .                                                                    
524500     EJECT                                                                
524600 IMS-REPL-WDK626 SECTION.                                                 
524700                                                                          
524800     MOVE '  ' TO GODK-STATUSKODER                                        
524900     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK626                       
525000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
525100     PERFORM IMS-STATUSKONTROLL                                           
525200     .                                                                    
525300     EJECT                                                                
525400 IMS-DLET-WDK626 SECTION.                                                 
525500                                                                          
525600     MOVE '  ' TO GODK-STATUSKODER                                        
525700     CALL CBLTDLI USING DLET WDK6-PCB DLI-IO-WDK626                       
525800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
525900     PERFORM IMS-STATUSKONTROLL                                           
526000     .                                                                    
526100     EJECT                                                                
526200 IMS-GU-WDK7-WDK701 SECTION.                                              
526300                                                                          
526400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
526500          DELIMITED BY SIZE INTO SSA1                                     
526600     MOVE '  GE' TO GODK-STATUSKODER                                      
526700     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
526800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
526900     PERFORM IMS-STATUSKONTROLL                                           
527000     .                                                                    
527100     SKIP3                                                                
527200 IMS-GU-WDK7-WDK711 SECTION.                                              
527300                                                                          
527400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
527500          DELIMITED BY SIZE INTO SSA1                                     
527600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
527700          DELIMITED BY SIZE INTO SSA2                                     
527800     MOVE '  GE' TO GODK-STATUSKODER                                      
527900     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-K711 SSA1 SSA2                 
528000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
528100     PERFORM IMS-STATUSKONTROLL                                           
528200     .                                                                    
528300     SKIP3                                                                
528400 IMS-GHU-WDK7-WDK711 SECTION.                                             
528500                                                                          
528600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
528700          DELIMITED BY SIZE INTO SSA1                                     
528800     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
528900          DELIMITED BY SIZE INTO SSA2                                     
529000     MOVE '  GE' TO GODK-STATUSKODER                                      
529100     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-K711 SSA1 SSA2                
529200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
529300     PERFORM IMS-STATUSKONTROLL                                           
529400     .                                                                    
529500     SKIP3                                                                
529600 IMS-GHNP-WDK7-WDK711 SECTION.                                            
529700                                                                          
529800     STRING 'WDK711  (IDDC    >=' W-IDDC-MIN-X                            
529900                    '&IDDC    <=' W-IDDC-MAX-X ')'                        
530000          DELIMITED BY SIZE INTO SSA1                                     
530100     MOVE '  GE' TO GODK-STATUSKODER                                      
530200     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-K711 SSA1                    
530300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
530400     PERFORM IMS-STATUSKONTROLL                                           
530500     .                                                                    
530600     SKIP3                                                                
530700 IMS-REPL-WDK7-WDK711 SECTION.                                            
530800                                                                          
530900     MOVE '  ' TO GODK-STATUSKODER                                        
531000     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-K711                         
531100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
531200     PERFORM IMS-STATUSKONTROLL                                           
531300     .                                                                    
531400     EJECT                                                                
531500 IMS-GNP-WDK727 SECTION.                                                  
531600                                                                          
531700     MOVE 'WDK727 ' TO SSA1                                               
531800     MOVE '  GEGP' TO GODK-STATUSKODER                                    
531900     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK727 SSA1                   
532000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
532100     PERFORM IMS-STATUSKONTROLL                                           
532200     .                                                                    
532300     SKIP3                                                                
532400 IMS-GHNP-WDK727 SECTION.                                                 
532500                                                                          
532600     MOVE 'WDK727 ' TO SSA1                                               
532700     MOVE '  GEGP' TO GODK-STATUSKODER                                    
532800     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK727 SSA1                  
532900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
533000     PERFORM IMS-STATUSKONTROLL                                           
533100     .                                                                    
533200     SKIP3                                                                
533300 IMS-REPL-WDK727 SECTION.                                                 
533400                                                                          
533500     MOVE '  ' TO GODK-STATUSKODER                                        
533600     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK727                       
533700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
533800     PERFORM IMS-STATUSKONTROLL                                           
533900     .                                                                    
534000     EJECT                                                                
534100 IMS-DLET-WDK727 SECTION.                                                 
534200                                                                          
534300     MOVE '  ' TO GODK-STATUSKODER                                        
534400     CALL CBLTDLI USING DLET WDK7-PCB DLI-IO-WDK727                       
534500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
534600     PERFORM IMS-STATUSKONTROLL                                           
534700     .                                                                    
534800     EJECT                                                                
534900 IMS-GHU-WDL7-OIGA11 SECTION.                                             
535000                                                                          
535100     STRING 'WLOIGA01(IDARTNR  =' W-IDARTNR-X ')'                         
535200          DELIMITED BY SIZE INTO SSA1                                     
535300     STRING 'WLOIGA11(IDDC     =' W-IDDC-X ')'                            
535400          DELIMITED BY SIZE INTO SSA2                                     
535500     MOVE '  GE' TO GODK-STATUSKODER                                      
535600     CALL CBLTDLI USING GHU OIGA-PCB DLI-IO-OIGA11 SSA1 SSA2              
535700     MOVE OIGA-STATUS-CODE TO STATUS-WS                                   
535800     PERFORM IMS-STATUSKONTROLL                                           
535900     .                                                                    
536000     SKIP3                                                                
536100 IMS-REPL-WDL7-OIGA11 SECTION.                                            
536200                                                                          
536300     MOVE '  ' TO GODK-STATUSKODER                                        
536400     CALL CBLTDLI USING REPL OIGA-PCB DLI-IO-OIGA11                       
536500     MOVE OIGA-STATUS-CODE TO STATUS-WS                                   
536600     PERFORM IMS-STATUSKONTROLL                                           
536700     .                                                                    
536800     EJECT                                                                
536900 IMS-GHU-WDGX2262 SECTION.                                                
537000                                                                          
537100     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2261-X ')'                    
537200          DELIMITED BY SIZE INTO SSA1                                     
537300     STRING 'WDGX2262(IDARTNR  =' W-IDARTNR-X ')'                         
537400          DELIMITED BY SIZE INTO SSA2                                     
537500     MOVE '  GE' TO GODK-STATUSKODER                                      
537600     CALL CBLTDLI USING GHU 2261-PCB DLI-IO-WDGX2262 SSA1 SSA2            
537700     MOVE 2261-STATUS-CODE TO STATUS-WS                                   
537800     PERFORM IMS-STATUSKONTROLL                                           
537900     .                                                                    
538000     SKIP3                                                                
538100 IMS-ISRT-WDGX2262   SECTION.                                             
538200                                                                          
538300     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2261-X ')'                    
538400          DELIMITED BY SIZE INTO SSA1                                     
538500     MOVE 'WDGX2262 ' TO SSA2                                             
538600     MOVE '  ' TO GODK-STATUSKODER                                        
538700     CALL CBLTDLI USING ISRT 2261-PCB DLI-IO-WDGX2262 SSA1 SSA2           
538800     MOVE 2261-STATUS-CODE TO STATUS-WS                                   
538900     PERFORM IMS-STATUSKONTROLL                                           
539000     .                                                                    
539100     SKIP3                                                                
539200 IMS-REPL-WDGX2262 SECTION.                                               
539300                                                                          
539400     MOVE '  ' TO GODK-STATUSKODER                                        
539500     CALL CBLTDLI USING REPL 2261-PCB DLI-IO-WDGX2262                     
539600     MOVE 2261-STATUS-CODE TO STATUS-WS                                   
539700     PERFORM IMS-STATUSKONTROLL                                           
539800     .                                                                    
539900     EJECT                                                                
540000 IMS-GU-BENA01-BSEQ SECTION.                                              
540100                                                                          
540200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
540300          DELIMITED BY SIZE INTO SSA1                                     
540400     MOVE '  GE' TO GODK-STATUSKODER                                      
540500     CALL CBLTDLI USING GU BENA-PCB DLI-IO-BENA01 SSA1                    
540600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
540700     PERFORM IMS-STATUSKONTROLL                                           
540800     .                                                                    
540900     SKIP3                                                                
541000 IMS-GNP-BENA11 SECTION.                                                  
541100                                                                          
541200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
541300          DELIMITED BY SIZE INTO SSA1                                     
541400     MOVE '  GE' TO GODK-STATUSKODER                                      
541500     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-BENA11 SSA1                   
541600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
541700     PERFORM IMS-STATUSKONTROLL                                           
541800     .                                                                    
541900     EJECT                                                                
542000                                                                          
542100                                                                          
542200 IMS-GU-2502 SECTION.                                                     
542300                                                                          
542400     STRING 'WL250101(WDGXKEY  =' W-WDGXKEY-X ')'                         
542500          DELIMITED BY SIZE INTO SSA1                                     
542600     STRING 'WL250111(IDREFTAB =' W-IDREFTAB-X ')'                        
542700          DELIMITED BY SIZE INTO SSA2                                     
542800     MOVE '  GE' TO GODK-STATUSKODER                                      
542900     CALL CBLTDLI USING GU 2501-PCB DLI-IO-250111 SSA1 SSA2               
543000     MOVE 2501-STATUS-CODE TO STATUS-WS                                   
543100     PERFORM IMS-STATUSKONTROLL                                           
543200     .                                                                    
543300     EJECT                                                                
543400                                                                          
543500                                                                          
543600 IMS-GU-LEVA16 SECTION.                                                   
543700                                                                          
543800     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
543900          DELIMITED BY SIZE INTO SSA1                                     
544000     STRING 'WLLEVA16(IDDC     =' W-IDDC-X ')'                            
544100          DELIMITED BY SIZE INTO SSA2                                     
544200     MOVE '  GE' TO GODK-STATUSKODER                                      
544300     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-LEVA16 SSA1 SSA2               
544400     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
544500     PERFORM IMS-STATUSKONTROLL                                           
544600     .                                                                    
544700     EJECT                                                                
544800 IMS-GU-WDB601    SECTION.                                                
544900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
545000          DELIMITED BY SIZE INTO SSA1                                     
545100     MOVE '  GE' TO GODK-STATUSKODER                                      
545200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
545300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
545400     PERFORM IMS-STATUSKONTROLL                                           
545500     IF SEGMENT-SAKNAS                                                    
545600        MOVE SPACE TO DCS-KDDC                                            
545700     END-IF                                                               
545800     .                                                                    
545900     EJECT                                                                
546000 IMS-GU-WDB616    SECTION.                                                
546100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
546200          DELIMITED BY SIZE INTO SSA1                                     
546300     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
546400          DELIMITED BY SIZE INTO SSA2                                     
546500     MOVE '  '                TO GODK-STATUSKODER                         
546600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B616 SSA1 SSA2            
546700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
546800     PERFORM IMS-STATUSKONTROLL                                           
546900     .                                                                    
547000     EJECT                                                                
547100 IMS-GU-WDK711    SECTION.                                                
547200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
547300          DELIMITED BY SIZE INTO SSA1                                     
547400     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
547500          DELIMITED BY SIZE INTO SSA2                                     
547600     MOVE '  GE' TO GODK-STATUSKODER                                      
547700     CALL CBLTDLI USING GU WDK72-PCB DLI-IO-WDK711                        
547800          SSA1 SSA2                                                       
547900     MOVE WDK72-STATUS-CODE TO STATUS-WS                                  
548000     PERFORM IMS-STATUSKONTROLL                                           
548100     .                                                                    
548200                                                                          
548300 IMS-GU-WDK712 SECTION.                                                   
548400                                                                          
548500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
548600          DELIMITED BY SIZE INTO SSA1                                     
548700     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
548800          DELIMITED BY SIZE INTO SSA2                                     
548900     MOVE '  GE' TO GODK-STATUSKODER                                      
549000     CALL CBLTDLI USING GU WDK72-PCB DLI-IO-WDK712 SSA1 SSA2              
549100     MOVE WDK72-STATUS-CODE TO STATUS-WS                                  
549200     PERFORM IMS-STATUSKONTROLL                                           
549300     .                                                                    
549400     EJECT                                                                
549500                                                                          
549600 IMS-GU-WDK722 SECTION.                                                   
549700                                                                          
549800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
549900          DELIMITED BY SIZE INTO SSA1                                     
550000     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
550100          DELIMITED BY SIZE INTO SSA2                                     
550200     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-K722-X ')'                   
550300          DELIMITED BY SIZE INTO SSA3                                     
550400     MOVE '  GE'              TO GODK-STATUSKODER                         
550500     CALL CBLTDLI USING GU WDK72-PCB DLI-IO-WDK722 SSA1 SSA2              
550600                                                        SSA3              
550700     MOVE WDK72-STATUS-CODE   TO STATUS-WS                                
550800     PERFORM IMS-STATUSKONTROLL                                           
550900     .                                                                    
551000     EJECT                                                                
551100 IMS-ISRT-2207 SECTION.                                                   
551200     STRING 'WDG301  (WDG3KEY  =' W-WDG3KEY ')'                           
551300            DELIMITED BY SIZE INTO SSA1                                   
551400     MOVE   'WDG302   '       TO SSA2                                     
551500     MOVE '  '                TO GODK-STATUSKODER                         
551600     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-WDG302 SSA1 SSA2             
551700     MOVE WDG3-STATUS-CODE    TO STATUS-WS                                
551800     PERFORM IMS-STATUSKONTROLL                                           
551900     .                                                                    
552000     EJECT                                                                
552100 IMS-STATUSKONTROLL SECTION.                                              
552200                                                                          
552300     SET STATUS-IX TO 1                                                   
552400     SEARCH GODK-STATUS                                                   
552500       AT END                                                             
552600         STRING 'OTILLÅTEN STATUSKOD FRÅN IMS: ' STATUS-WS                
552700         DELIMITED BY SIZE INTO FELTEXT                                   
552800         CALL FELLOG                                                      
552900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
553000         CONTINUE                                                         
553100     END-SEARCH                                                           
553200     .                                                                    
553300     EJECT                                                                
553400 DB2-DCL-OPN-TP1ARTK-CRS  SECTION.                                        
553500     MOVE 'DB2-DCL-OPN-TP1ARTK   ' TO  WS-DB2-SEKTION                     
553600                                                                          
553700     MOVE 000100  TO GOOD-SQLCODECODES                                    
553800                                                                          
553900     EXEC SQL                                                             
554000         DECLARE TP1ARTK-CRS CURSOR FOR                                   
554100           SELECT  A.IDKAMP                                               
554200                  ,A.IDARTNR                                              
554300                  ,B.TISTADAT_KAMP                                        
554400                  ,B.TISTODAT_KAMP                                        
554500                                                                          
554600           FROM    TP1ARTK A                                              
554700                  ,TP1KAMP B                                              
554800                                                                          
554900           WHERE   A.IDARTNR = :W-IDARTNR                                 
555000               AND A.IDKAMP  =  B.IDKAMP                                  
555100                                                                          
555200           ORDER BY B.IDKAMP                                              
555300     END-EXEC                                                             
555400                                                                          
555500     MOVE 000100  TO GOOD-SQLCODECODES                                    
555600     EXEC SQL OPEN TP1ARTK-CRS END-EXEC                                   
555700     .                                                                    
555800     SKIP3                                                                
555900 DB2-FETCH-TP1ARTK-CRS  SECTION.                                          
556000     MOVE 'DB2-FETCH-TP1ARTK   ' TO  WS-DB2-SEKTION                       
556100     SKIP2                                                                
556200     MOVE 000100  TO GOOD-SQLCODECODES                                    
556300     EXEC SQL                                                             
556400         FETCH TP1ARTK-CRS INTO                                           
556500                    :TP1KAMP-IDKAMP                                       
556600                   ,:TP1ARTK-IDARTNR                                      
556700                   ,:TP1KAMP-TISTADAT-KAMP                                
556800                   ,:TP1KAMP-TISTODAT-KAMP                                
556900     END-EXEC                                                             
557000                                                                          
557100     MOVE SQLCODE TO SQLCODE-WS                                           
557200     PERFORM DB2-STATUS-CHECK                                             
557300     .                                                                    
557400     SKIP3                                                                
557500 DB2-CLOSE-TP1ARTK-CRS  SECTION.                                          
557600     MOVE 'DB2-CLOSE-TP1ARTK   ' TO  WS-DB2-SEKTION                       
557700                                                                          
557800     EXEC SQL CLOSE TP1ARTK-CRS END-EXEC                                  
557900     .                                                                    
558000     EJECT                                                                
558100 DB2-STATUS-CHECK  SECTION.                                               
558200                                                                          
558300     SET SQLCODE-IX TO 1                                                  
558400     SEARCH GOOD-SQLCODE                                                  
558500       AT END                                                             
558600*         STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
558700*         DELIMITED BY SIZE INTO ERROR-TEXT                               
558800          CALL FELLOG                                                     
558900       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
559000     END-SEARCH                                                           
559100     .                                                                    
559200     EJECT                                                                
559300*    -COPY WY2000P1                                                       
559400     EJECT                                                                
559500*    -COPY WY2000Q1                                                       
559600     EJECT                                                                
559700*    -COPY WY2000P3                                                       
