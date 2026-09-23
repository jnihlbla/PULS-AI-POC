000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4028200.                                                
000400 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000500 DATE-WRITTEN.   90/05/17.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        BILD 4282 - FRÅGA PÅ ORDERBEKRÄFTELSE ORDER.                     
001100*        PROGRAMMET VISAR ALLA ORDERBEKRÄFTELSER FÖR                      
001200*        EN ORDER.                                                        
001300*                                                                         
001400*        PROGRAMMET ANROPAR W218ETA FÖR HÄMTNING                          
001500*        AV ETA-DATUM/NDC-LAGER.                                          
001600*       (ESTIMATED TIME AVAILABLE)                                        
001700*                                                                         
001800*        PROGRAMMET ÄR EN FRÅGE-MPP                                       
001900*        PROGRAMMET LÄSER      WLORQM (WDQ1)                              
002000*        PROGRAMMET LÄSER      WLORQI (WDQ2)                              
002100*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002200*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
002300*        PROGRAMMET LÄSER      WLSATB (WDJ1)                              
002400*        PROGRAMMET LÄSER      WLPROC (WDE8)                              
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSAKTION: W4T282                                              
002800*        MID:         W4I28201                                            
002900*                                                                         
003000*    UTDATA.                                                              
003100*        MOD:         W4O28201                                            
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700 WORKING-STORAGE SECTION.                                                 
003800*    -COPY WY2000W1                                                       
003900     SKIP3                                                                
004000 77  IDPGM                       PIC X(08)   VALUE 'W4028200'.            
004100                                                                          
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400 77  FL-HUVUD-FINNS              PIC X.                                   
004500 77  ASTERIX                     PIC X       VALUE '*'.                   
004600                                                                          
004700*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004800 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004900 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
005000 77  MAX-INDX-KOD-61             PIC S9(4)  VALUE +13   COMP SYNC.        
005100                                                                          
005200 77  TABELL-IX                   PIC S9(3)  VALUE +0    COMP SYNC.        
005300 77  TAB-INDX                    PIC S9(3)  VALUE +0    COMP SYNC.        
005400 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005500                                                                          
005600*01  -COPY WWDCKONS                                                       
005700                                                                          
005800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005900 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006000 77  IDORDNR7-WS                 PIC X(7)    VALUE SPACE.                 
006100 77  WS-BEART-NUM                PIC 9(15)   VALUE ZERO.                  
006200 77  WS-IDARTNR-NUM              PIC 9(9)    VALUE ZERO.                  
006300 77  WS-IDARTNR-SPAR             PIC 9(9)    VALUE ZERO.                  
006400 77  WS1-IDDC                    PIC X(2)    VALUE ZERO.                  
006500 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
006600 77  WS-IDDISTR-NUM              PIC 9(4)    VALUE ZERO.                  
006700 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
006800 77  WS-IDKUNDNR-NUM             PIC 9(6)    VALUE ZERO.                  
006900 77  WS-IDORDER-SPAR             PIC S9(7)   VALUE +0 COMP-3.             
007000 77  WS-KDORDBEK-NUM             PIC 9(2)    VALUE ZERO.                  
007100 77  WS-KDORDBEK-SPAR            PIC 9(2)    VALUE ZERO.                  
007200*--- SPARADE NYCKLAR VID EVENTUELL BACKNING VID KDORDBEK = 61             
007300                                                                          
007400 77  SPAR-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
007500 77  SPAR-IDORDER-NEXT           PIC 9(7)    VALUE ZERO.                  
007600 77  SPAR-IDARTNR-NEXT           PIC 9(9)    VALUE ZERO.                  
007700 77  SPAR-IDLOPNR-NEXT           PIC 9(3)    VALUE ZERO.                  
007800 77  SPAR-IDSEKVNR-NEXT          PIC 9(3)    VALUE ZERO.                  
007900 77  SPAR-IDDC-NEXT              PIC X(2)    VALUE SPACE.                 
008000 77  SPAR-KDORDBEK-NEXT          PIC 9(2)    VALUE ZERO.                  
008100 01  W-SPAR-IDKUNDRF.                                                     
008200     03  W-SPAR-IDORDNR7         PIC X(7)    VALUE '+++++++'.             
008300     03  FILLER                  PIC X(3)    VALUE '+++'.                 
008400                                                                          
008500                                                                          
008600 01  ARTIKEL-REKSIFFRA.                                                   
008700     03 WS-IDARTNR               PIC X(9)    VALUE SPACE.                 
008800     03 WS-STRECK                PIC X(1)    VALUE '-'.                   
008900     03 WS-REKNR                 PIC X(1)    VALUE SPACE.                 
009000                                                                          
009100 01  KOD.                                                                 
009200     03 WS-KDORDBEK              PIC X(2)    VALUE SPACE.                 
009300     03 WS-ASTERIX               PIC X       VALUE SPACE.                 
009400                                                                          
009500******* NYCKLAR TILL BILD 4281 *************************                  
009600                                                                          
009700 77  WS-KDFRAKT                  PIC X(2)    VALUE SPACE.                 
009800 77  WS-KDORDKL                  PIC X(1)    VALUE SPACE.                 
009900 77  WS-KDFRAKT-NUM              PIC 9(2)    VALUE ZERO.                  
010000 77  WS-KDORDKL-NUM              PIC 9(1)    VALUE ZERO.                  
010100                                                                          
010200********************************************************                  
010300                                                                          
010400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
010500     88  NYCKLAR-OK                          VALUE 'J'.                   
010600     88  NYCKLAR-FEL                         VALUE 'N'.                   
010700                                                                          
010800 77  ALLT-SW                     PIC X       VALUE 'J'.                   
010900     88  ALLT-OK                             VALUE 'J'.                   
011000                                                                          
011100 77  PROFORMA-SW                 PIC X       VALUE 'N'.                   
011200     88  PROFORMA                            VALUE 'J'.                   
011300                                                                          
011400 77  IFYLLT-SW                   PIC X       VALUE 'J'.                   
011500     88  ALLT-IFYLLT                         VALUE 'J'.                   
011600                                                                          
012000 77  RAKNA-UPP-INDX-SW           PIC X       VALUE 'N'.                   
012100     88  RAKNA-UPP-INDX                      VALUE 'J'.                   
012200                                                                          
012300 77  KEY-KOLL                    PIC X       VALUE '3'.                   
012400     88  BASNR-IFYLLT                        VALUE '3'.                   
012500     88  ARTNR-IFYLLT                        VALUE '4'.                   
012600     88  KDORDBEK-IFYLLT                     VALUE '5'.                   
012700                                                                          
012800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
012900     88  EGEN-MID                            VALUE '4282'.                
013000     88  GODK-MID                            VALUE '4281' '4282'          
013100                                                   '4283' '4284'.         
013200                                                                          
013300 77  GODK-KDORDBEK               PIC X(2)    VALUE SPACE.                 
013400     88 NOLL-KOD                             VALUE '0 ' '00' ' 0'         
013500                                                   '  '.                  
013600     88 GODK-KOD                             VALUE '10' '15' '16'         
013700                                                   '20' '21' '22'         
013800                                                   '26'                   
013900                                                   '30' '31' '32'         
014000                                                   '33' '34'              
014100                                                   '40' '41' '42'         
014200                                                   '43' '44' '51'         
014300                                                   '52' '53' '54'         
014400                                                   '55' '56' '57'         
014500                                                   '58' '59'              
014600                                                   '61' '65' '66'         
014700                                                   '67' '70' '71'         
014800                                                   '72' '73' '74'         
014900                                                   '75' '76' '80'         
015000                                                   '81' '82' '83'         
015100                                                   '84'                   
015200                                                   '85' '87' '90'         
015300                                                   '91' '92' '95'         
015400                                                   '96' '97' '98'         
015500                                                   '99'.                  
015600     EJECT                                                                
015700*  --- FÖR ATT FÅ RÄTT SEKEL VID ANROP TILL W218ETA                       
015800 01  WS-ETA-DATUM.                                                        
015900     03  WS-ETA-DATUM-AAR        PIC 9(2).                                
016000     03  WS-ETA-DATUM-MAANAD     PIC 9(2).                                
016100     03  WS-ETA-DATUM-DAG        PIC 9(2).                                
016200     EJECT                                                                
016300*  --- FÖR ATT SPARA ARTNR I FRÅN WDJ1 FÖR ATT KOLLA BLÄDDRING            
016400                                                                          
016500 01  SPAR-TABELL.                                                         
016600    03 TABELL OCCURS 6 TIMES.                                             
016700      05  TAB-IDARTNR        PIC S9(9)           COMP-3.                  
016800                                                                          
016900 01  OBKR-SEG-SPAR.                                                       
017000*    03  -COPY WDQ101 -L                                                  
017100                                                                          
017200 01  WS-IDKUNDRF                 PIC X(10).                               
017300 01  FILLER REDEFINES WS-IDKUNDRF.                                        
017400     03  WS-IDORDNR5             PIC 9(5).                                
017500     03  FILLER                  PIC X(5).                                
017600 01  FILLER REDEFINES WS-IDKUNDRF.                                        
017700     03  WS-IDORDNR7             PIC 9(7).                                
017800     03  FILLER                  PIC X(3).                                
017900 01  FILLER REDEFINES WS-IDKUNDRF.                                        
018000     03  FILLER                  PIC X(5).                                
018100     03  BLANK-TECKEN            PIC X(2).                                
018200     03  FILLER                  PIC X(3).                                
018300                                                                          
018400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
018500 01  GENERELLA-SUBPROGRAM.                                                
018600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
018700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
018800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
018900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
019000     EJECT                                                                
019100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
019200*01 -COPY WMSGINIT                                                        
019300     EJECT                                                                
019400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
019500*   -COPY WMEDAREA                                                        
019600     SKIP3                                                                
019700 01  MESSAGE-CODES.                                                       
019800     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
019900     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
020000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
020100     EJECT                                                                
020200*    --- GEMENSAMMA SUBPROGRAM                                            
020300 01  GEMENSAMMA-SUBPROGRAM.                                               
020400     03  W218ETA                 PIC X(8)    VALUE 'W218ETA '.            
020500*        HÄMTA TIBERANK                                                   
020600     EJECT                                                                
020700*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
020800 01 FILLER                       PIC X(8)    VALUE 'W218LETA'.            
020900*   -COPY W218LETA -PRE ETA-.                                             
021000     EJECT                                                                
021100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
021200*                                                                         
021300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
021400     SKIP3                                                                
021500*01  MID -COPY W4I28201                                                   
021600     EJECT                                                                
021700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
021800     SKIP3                                                                
021900*01  -COPY WMSGAREA                                                       
022000     EJECT                                                                
022100     03  MOD REDEFINES MSG-AREA.                                          
022200*      05  -COPY W4O28201                                                 
022300     EJECT                                                                
022400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
022500     SKIP3                                                                
022600*01  -COPY WMFSAREA                                                       
022700     EJECT                                                                
022800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022900*                                                                         
023000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023100     SKIP3                                                                
023200 01  NYCKLAR-TILL-DLI.                                                    
023300                                                                          
023400     03  W-IDARTNR-WDK6-X.                                                
023500         05  W-IDARTNR-WDK6          PIC S9(9) VALUE ZERO COMP-3.         
023600                                                                          
023700******* NYCKEL FÖR ATT LÄSA WDQ1 MED BAS-NYCKELN *********                
023800                                                                          
023900     03  W-WDQ101KY-BAS-MIN-X.                                            
024000         05  W-IDORDER-BAS-MIN   PIC S9(7)    VALUE ZERO COMP-3.          
024100         05  W-IDARTNR-BAS-MIN   PIC S9(9)    VALUE ZERO COMP-3.          
024200         05  W-IDLOPNR-BAS-MIN   PIC S9(3)    VALUE ZERO COMP-3.          
024300         05  W-IDSEKVNR-BAS-MIN  PIC S9(3)    VALUE ZERO COMP-3.          
024400         05  FILLER              PIC  X(2)    VALUE SPACE.                
024500         05  W-KDORDBEK-BAS-MIN  PIC  9(2)    VALUE ZERO.                 
024600                                                                          
024700     03  W-WDQ101KY-BAS-MAX-X.                                            
024800         05  W-IDORDER-BAS-MAX   PIC S9(7)    VALUE ZERO COMP-3.          
024900         05  W-IDARTNR-BAS-MAX   PIC S9(9)    VALUE ZERO COMP-3.          
025000         05  W-IDLOPNR-BAS-MAX   PIC S9(3)    VALUE ZERO COMP-3.          
025100         05  W-IDSEKVNR-BAS-MAX  PIC S9(3)    VALUE ZERO COMP-3.          
025200         05  FILLER              PIC  X(2)    VALUE SPACE.                
025300         05  W-KDORDBEK-BAS-MAX  PIC  9(2)    VALUE ZERO.                 
025400                                                                          
025500******* NYCKEL FÖR ATT LÄSA ERSÄTTNINGAR VID KOD 41 ******                
025600                                                                          
025700     03  W-WDQ101KY-41-MIN-X.                                             
025800         05  W-IDORDER-41-MIN    PIC S9(7)    VALUE ZERO COMP-3.          
025900         05  W-IDARTNR-41-MIN    PIC S9(9)    VALUE ZERO COMP-3.          
026000         05  W-IDLOPNR-41-MIN    PIC S9(3)    VALUE ZERO COMP-3.          
026100         05  W-IDSEKVNR-41-MIN   PIC S9(3)    VALUE ZERO COMP-3.          
026200         05  FILLER              PIC  X(2)    VALUE SPACE.                
026300         05  W-KDORDBEK-41-MIN   PIC  9(2)    VALUE ZERO.                 
026400                                                                          
026500     03  W-IDDC-41-MIN-X.                                                 
026600         05  W-IDDC-41-MIN       PIC X(2)     VALUE SPACE.                
026700                                                                          
026800     03  W-WDQ101KY-41-MAX-X.                                             
026900         05  W-IDORDER-41-MAX    PIC S9(7)    VALUE ZERO COMP-3.          
027000         05  W-IDARTNR-41-MAX    PIC S9(9)    VALUE ZERO COMP-3.          
027100         05  W-IDLOPNR-41-MAX    PIC S9(3)    VALUE ZERO COMP-3.          
027200         05  W-IDSEKVNR-41-MAX   PIC S9(3)    VALUE ZERO COMP-3.          
027300         05  FILLER              PIC  X(2)    VALUE SPACE.                
027400         05  W-KDORDBEK-41-MAX   PIC  9(2)    VALUE ZERO.                 
027500                                                                          
027600     03  W-IDDC-41-MAX-X.                                                 
027700         05  W-IDDC-41-MAX       PIC X(2)     VALUE SPACE.                
027800                                                                          
027900** NYCKEL FÖR LÄSNING AV WDQ1 MED BAS + IDC + KDORDBEK ********           
028000                                                                          
028100     03  W-WDQ101KY-MAX-MIN-X.                                            
028200         05  W-IDORDER-MAX-MIN   PIC S9(7)    VALUE ZERO COMP-3.          
028300         05  W-IDARTNR-MAX-MIN   PIC S9(9)    VALUE ZERO COMP-3.          
028400         05  W-IDLOPNR-MAX-MIN   PIC S9(3)    VALUE ZERO COMP-3.          
028500         05  W-IDSEKVNR-MAX-MIN  PIC S9(3)    VALUE ZERO COMP-3.          
028600         05  FILLER              PIC  X(2)    VALUE SPACE.                
028700         05  FILLER              PIC  9(2)    VALUE ZERO.                 
028800     03  W-IDDC-MAX-MIN-X.                                                
028900         05  W-IDDC-MAX-MIN      PIC X(2)     VALUE SPACE.                
029000     03  W-KDORDBEK-MAX-MIN-X.                                            
029100         05  W-KDORDBEK-MAX-MIN      PIC  9(2)    VALUE ZERO.             
029200                                                                          
029300     03  W-WDQ101KY-MAX-MAX-X.                                            
029400         05  W-IDORDER-MAX-MAX   PIC S9(7)    VALUE ZERO COMP-3.          
029500         05  W-IDARTNR-MAX-MAX   PIC S9(9)    VALUE ZERO COMP-3.          
029600         05  W-IDLOPNR-MAX-MAX   PIC S9(3)    VALUE ZERO COMP-3.          
029700         05  W-IDSEKVNR-MAX-MAX  PIC S9(3)    VALUE ZERO COMP-3.          
029800         05  FILLER              PIC  X(2)    VALUE SPACE.                
029900         05  FILLER              PIC  9(2)    VALUE ZERO.                 
030000     03  W-IDDC-MAX-MAX-X.                                                
030100         05  W-IDDC-MAX-MAX      PIC X(2)     VALUE SPACE.                
030200     03  W-KDORDBEK-MAX-MAX-X.                                            
030300         05  W-KDORDBEK-MAX-MAX      PIC  9(2)    VALUE ZERO.             
030400                                                                          
030500** NYCKEL FÖR LÄSNING AV WDQ1 MED BAS + IDDC + ARTNR ******               
030600                                                                          
030700     03  W-WDQ101KY-MIN-MIN-X.                                            
030800         05  W-IDORDER-MIN-MIN   PIC S9(7)    VALUE ZERO COMP-3.          
030900         05  W-IDARTNR-MIN-MIN   PIC S9(9)    VALUE ZERO COMP-3.          
031000         05  W-IDLOPNR-MIN-MIN   PIC S9(3)    VALUE ZERO COMP-3.          
031100         05  W-IDSEKVNR-MIN-MIN  PIC S9(3)    VALUE ZERO COMP-3.          
031200         05  FILLER              PIC  X(2)    VALUE SPACE.                
031300         05  W-KDORDBEK-MIN-MIN  PIC  9(2)    VALUE ZERO.                 
031400     03  W-IDDC-MIN-MIN-X.                                                
031500         05  W-IDDC-MIN-MIN      PIC X(2)     VALUE SPACE.                
031600                                                                          
031700     03  W-WDQ101KY-MIN-MAX-X.                                            
031800         05  W-IDORDER-MIN-MAX   PIC S9(7)    VALUE ZERO COMP-3.          
031900         05  W-IDARTNR-MIN-MAX   PIC S9(9)    VALUE ZERO COMP-3.          
032000         05  W-IDLOPNR-MIN-MAX   PIC S9(3)    VALUE ZERO COMP-3.          
032100         05  W-IDSEKVNR-MIN-MAX  PIC S9(3)    VALUE ZERO COMP-3.          
032200         05  FILLER              PIC  X(2)    VALUE SPACE.                
032300         05  W-KDORDBEK-MIN-MAX  PIC  9(2)    VALUE ZERO.                 
032400     03  W-IDDC-MIN-MAX-X.                                                
032500         05  W-IDDC-MIN-MAX      PIC X(2)     VALUE SPACE.                
032600                                                                          
032700******* ?????????????              *********************                  
032800                                                                          
032900     03  W-KDORDBEK-40-X.                                                 
033000         05  W-KDORDBEK-40           PIC  9(2)    VALUE 40.               
033100                                                                          
033200     03  W-KDORDBEK-15-X.                                                 
033300         05  W-KDORDBEK-40           PIC  9(2)    VALUE 15.               
033400                                                                          
033500******* SEKUNDÄRNYCKEL TILL WDQ201 ***********************                
033600                                                                          
033700     03  W-IDGMTREF-X.                                                    
033800         05  W-IDDISTR           PIC S9(5)    VALUE ZERO COMP-3.          
033900         05  W-IDKUNDNR          PIC S9(7)    VALUE ZERO COMP-3.          
034000         05  W-IDKUNDREF         PIC X(10)    VALUE SPACE.                
034100                                                                          
034200******* NYCKEL FÖR ATT LÄSA WDJ1C1 ***********************                
034300                                                                          
034400     03  W-WDJ1CSEQ-X.                                                    
034500         05  W-IDLEVNR           PIC  X(5)    VALUE SPACE.                
034600         05  W-BELEVART          PIC  X(30)   VALUE SPACE.                
034700         05  W-IDARTNR           PIC S9(9)    VALUE +0   COMP-3.          
034800                                                                          
034900     03  W-IDLEVNR-X.                                                     
035000         05  W-IDLEVNR-SATB      PIC  X(5)    VALUE '1002 '.              
035100                                                                          
035200     03  W-IDSYSTEM-X.                                                    
035300         05  W-IDSYSTEM          PIC  X(4)    VALUE 'PROF'.               
035400                                                                          
035500******* DIREKTNYCKEL TILL WDD301 *************************                
035600                                                                          
035700     03  W-IDARTNR-X.                                                     
035800         05  W-IDARTNR-WDD301   PIC S9(9)   VALUE ZERO COMP-3.            
035900                                                                          
036000******* DIREKTNYCKEL TILL WDD311 *************************                
036100                                                                          
036200     03  W-IDSKYLT-X.                                                     
036300         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
036400                                                                          
036500******* DIREKTNYCKEL TILL WDB601 *************************                
036600     03  W-IDDC-B6-X.                                                     
036700         05 W-IDDC-B6                  PIC X(2).                          
036800                                                                          
036900*    --- STATUS-KOD FRÅN IMS                                              
037000 01  STATUS-WS                   PIC XX.                                  
037100     88  SEGMENT-FINNS                       VALUE '  '.                  
037200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
037300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
037400     88  BASEN-SLUT                          VALUE 'GB'.                  
037500     SKIP2                                                                
037600 01  GODK-STATUSKODER.                                                    
037700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
037800     SKIP3                                                                
037900 01  SSA1                        PIC X(200).                              
038000 01  SSA2                        PIC X(64).                               
038100 01  SSA3                        PIC X(64).                               
038200     EJECT                                                                
038300*    --- IMS FUNKTIONSKODER                                               
038400*01  -COPY W0003                                                          
038500     EJECT                                                                
038600*    ---  DLI INPUT-OUTPUT AREA                                           
038700                                                                          
038800 01  FILLER                   PIC X(16)   VALUE 'IO-AREA-ORQI01'.         
038900 01  DLI-IO-ORQI01.                                                       
039000*    03  WLORQI01    -COPY WDQ201                                         
039100     EJECT                                                                
039200                                                                          
039300 01  FILLER                   PIC X(16)   VALUE 'IO-AREA-ORQM01'.         
039400 01  DLI-IO-ORQM01.                                                       
039500*    03  WLORQM01    -COPY WDQ101                                         
039600     EJECT                                                                
039700                                                                          
039800 01  FILLER                   PIC X(16)   VALUE 'IO-AREA-BENA11'.         
039900 01  DLI-IO-BENA11.                                                       
040000*    03  WLBENA11    -COPY WDD311                                         
040100     EJECT                                                                
040200                                                                          
040300 01  FILLER                   PIC X(16)   VALUE 'IO-AREA-J01&-11'.        
040400 01  DLI-IO-SATB11.                                                       
040500*    03  WLSATB11    -COPY WDJ111      -PRE SATB-                         
040600*    03  WLSATB01    -COPY WDJ101      -PRE SATB-                         
040700     EJECT                                                                
040800                                                                          
040900 01  FILLER                   PIC X(16)   VALUE 'IO-AREA-PROC01'.         
041000 01  DLI-IO-PROC01.                                                       
041100*    03  WLPROC01    -COPY WDE801                                         
041200     EJECT                                                                
041300                                                                          
041400 01  FILLER                   PIC X(16)   VALUE 'IO-AREA-ARTC11'.         
041500 01  DLI-IO-ARTC11.                                                       
041600*    03  WLARTC11    -COPY WDK611                                         
041700     EJECT                                                                
041800 01  FILLER                      PIC X(12) VALUE 'DUMMY-ARTC'.            
041900 01  ETA-ARTC-PCB                PIC X.                                   
042000 01  FILLER                      PIC X(12) VALUE 'DUMMY-LEVA'.            
042100 01  ETA-LEVA-PCB                PIC X.                                   
042200                                                                          
042300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
042400 01   DLI-IO-AREA-B601.                                                   
042500*     03  -COPY WDB601                                                    
042600                                                                          
042700 LINKAGE SECTION.                                                         
042800                                                                          
042900*01  -COPY W0009      -PRE MSG-                                           
043000     EJECT                                                                
043100*01  -COPY W0008      -PRE USEA-                                          
043200     05  FILLER                  PIC X.                                   
043300     EJECT                                                                
043400*01  -COPY W0008      -PRE ORQM-                                          
043500     05  FILLER                  PIC X.                                   
043600     EJECT                                                                
043700*01  -COPY W0008      -PRE ORQI-                                          
043800     05  FILLER                  PIC X.                                   
043900     EJECT                                                                
044000*01  -COPY W0008      -PRE BENA-                                          
044100     05  FILLER                  PIC X.                                   
044200     EJECT                                                                
044300*01  -COPY W0008      -PRE ARTC-                                          
044400     05  FILLER                  PIC X.                                   
044500     EJECT                                                                
044600*01  -COPY W0008      -PRE SATB-                                          
044700     05  FILLER                  PIC X.                                   
044800     EJECT                                                                
044900*01  -COPY W0008      -PRE WDB6-                                          
045000     05  FILLER                  PIC X.                                   
045100     EJECT                                                                
045200*01  -COPY W0008      -PRE PROC-                                          
045300     05  FILLER                  PIC X.                                   
045400     EJECT                                                                
045500 01  ETA-WDK7-PCB                PIC X.                                   
045600 01  ETA-INLC-PCB                PIC X.                                   
045700 01  ETA-WDB6-PCB                PIC X.                                   
045800 01  ETA-WDD9-PCB                PIC X.                                   
045900     EJECT                                                                
046000 PROCEDURE DIVISION  USING MSG-PCB  USEA-PCB ORQM-PCB ORQI-PCB            
046100                           BENA-PCB ARTC-PCB SATB-PCB WDB6-PCB            
046200                           PROC-PCB                                       
046300                           ETA-WDK7-PCB                                   
046400                           ETA-INLC-PCB                                   
046500                           ETA-WDB6-PCB ETA-WDD9-PCB.                     
046600     ENTRY 'DLITCBL' USING MSG-PCB  USEA-PCB ORQM-PCB ORQI-PCB            
046700                           BENA-PCB ARTC-PCB SATB-PCB WDB6-PCB            
046800                           PROC-PCB                                       
046900                           ETA-WDK7-PCB                                   
047000                           ETA-INLC-PCB                                   
047100                           ETA-WDB6-PCB ETA-WDD9-PCB.                     
047200                                                                          
047300     PERFORM IMS-GET-MSG                                                  
047400     IF SEGMENT-FINNS                                                     
047500       PERFORM A-INIT                                                     
047600       PERFORM B-KOLLA-NYCKLAR                                            
047700       IF NYCKLAR-OK                                                      
047800           IF MFS-FIRST                                                   
047900             PERFORM C-FOERSTA-SIDA                                       
048000           ELSE                                                           
048100             IF MFS-NEXT                                                  
048200               PERFORM D-NAESTA-SIDA                                      
048300             ELSE                                                         
048400               PERFORM E-SAMMA-SIDA                                       
048500             END-IF                                                       
048600           END-IF                                                         
048700           PERFORM H-LAES-HUVUD                                           
048800           IF FL-HUVUD-FINNS = JA                                         
048900             IF PROFORMA                                                  
049000               PERFORM FA-INIT-PROF-NYCKLAR                               
049100             ELSE                                                         
049200               PERFORM F-INIT-NYCKLAR                                     
049300             END-IF                                                       
049400             PERFORM G-LAES-VISA-INFO                                     
049500           ELSE                                                           
049600              MOVE '701' TO MED-IDMFSFEL                                  
049700              CALL WMEDKONV USING MED-WMEDAREA                            
049800              MOVE MED-MFSFEL TO MOD-TEMFSFEL                             
049900              PERFORM MFS-RENSA-FAELT-UT                                  
050000              MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                        
050100           END-IF                                                         
050200       END-IF                                                             
050300       PERFORM IMS-INSERT-MSG                                             
050400     END-IF                                                               
050500                                                                          
050600     MOVE ZERO TO RETURN-CODE                                             
050700     GOBACK                                                               
050800     .                                                                    
050900     EJECT                                                                
051000 A-INIT SECTION.                                                          
051100                                                                          
051200     IF MSG-DUBBLA-TRANSKODER                                             
051300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I28201                 
051400       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
051500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
051600     ELSE                                                                 
051700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I28201                  
051800       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
051900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
052000     END-IF                                                               
052100                                                                          
052200     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
052300     MOVE MSG-IDPFK TO MFS-IDPFK                                          
052400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
052500                                                                          
052600     MOVE LOW-VALUE TO MSG-AREA                                           
052700     MOVE 'W4O28201' TO MFS-IDMOD                                         
052800     MOVE '4282' TO MOD-IDTRANS                                           
052900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
053000     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O28201 + 4                        
053100                                                                          
053200     IF NOT EGEN-MID                                                      
053300       MOVE SPACE TO MFS-KDTRTYP                                          
053400       MOVE '7' TO MFS-IDPFK                                              
053500       MOVE MFS-RENSA-FAELT TO MOD-PROFORMA                               
053600       MOVE ALL '+' TO MID-PROFORMA                                       
053700     END-IF                                                               
053800                                                                          
053900     MOVE +1 TO TABELL-IX                                                 
054000     PERFORM 6 TIMES                                                      
054100       MOVE ZERO TO TAB-IDARTNR(TABELL-IX)                                
054200       ADD +1 TO TABELL-IX                                                
054300     END-PERFORM                                                          
054400     ACCEPT DAGENS-DATUM FROM DATE                                        
054500     .                                                                    
054600     EJECT                                                                
054700 B-KOLLA-NYCKLAR SECTION.                                                 
054800                                                                          
054900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
055000     MOVE '001'             TO MSGI-KDCALL                                
055100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
055200     MOVE '4282'            TO MSGI-IDTRANS                               
055300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
055400     IF EGEN-MID                                                          
055500        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
055600        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
055700        MOVE MID-IDORDNR7-IN   TO W-SPAR-IDORDNR7                         
055800        MOVE W-SPAR-IDKUNDRF   TO MSGI-IDKUNDRF                           
055900        MOVE MID-IDARTNR-IN    TO MSGI-IDARTNR                            
056000        MOVE MID-KDFRAKT-IN    TO MSGI-KDFRAKT                            
056100     END-IF                                                               
056200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
056300     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
056400                                                                          
056500     MOVE JA TO NYCKLAR-SW                                                
056600                ALLT-SW                                                   
056700                                                                          
056800     MOVE LOW-VALUE    TO                                                 
056900                         W-WDQ101KY-BAS-MIN-X                             
057000                         W-WDQ101KY-41-MIN-X                              
057100                         W-WDQ101KY-MIN-MIN-X                             
057200                         W-WDQ101KY-MAX-MIN-X                             
057300                         W-IDGMTREF-X                                     
057400                         W-IDARTNR-X                                      
057500                         W-IDSKYLT-X                                      
057600                         W-IDDC-41-MIN-X                                  
057700                         W-IDDC-MAX-MIN-X                                 
057800                         W-IDDC-MIN-MIN-X                                 
057900     MOVE HIGH-VALUE TO                                                   
058000                         W-WDQ101KY-BAS-MAX-X                             
058100                         W-WDQ101KY-41-MAX-X                              
058200                         W-WDQ101KY-MAX-MAX-X                             
058300                         W-WDQ101KY-MIN-MAX-X                             
058400                         W-IDDC-41-MAX-X                                  
058500                         W-IDDC-MIN-MAX-X                                 
058600                         W-IDDC-MAX-MAX-X                                 
058700                                                                          
058800     PERFORM BA-KOLLA-DISTRIKT                                            
058900     PERFORM BB-KOLLA-KUNDNR                                              
059000     PERFORM BC-KOLLA-KUNDRF                                              
059100     PERFORM BI-KOLLA-PROFORMA                                            
059200     PERFORM BD-KOLLA-IDDC                                                
059300     MOVE      MFS-RENSA-FAELT TO MOD-IDARTNR-IN                          
059400     MOVE      MFS-RENSA-FAELT TO MOD-KDORDBEK-IN                         
059500     IF ALLT-IFYLLT AND GODK-MID                                          
059600       PERFORM BE-KOLLA-ARTNR                                             
059700       IF ALLT-IFYLLT                                                     
059800         PERFORM BF-KOLLA-ORDBEK                                          
059900       END-IF                                                             
060000     END-IF                                                               
060100                                                                          
060200     PERFORM BG-KOLLA-FRAKTKOD                                            
060300     PERFORM BH-KOLLA-ORDERKLASS                                          
060400                                                                          
060500     IF NYCKLAR-FEL                                                       
060600       IF ALLT-OK                                                         
060700         MOVE      ERR-WRONG-KEY TO MED-IDMFSFEL                          
060800         CALL      WMEDKONV USING MED-WMEDAREA                            
060900         MOVE      MED-MFSFEL TO MOD-TEMFSFEL                             
061000         PERFORM MFS-RENSA-FAELT-UT                                       
061100       ELSE                                                               
061200         MOVE      '001'         TO MED-IDMFSFEL                          
061300         CALL      WMEDKONV USING MED-WMEDAREA                            
061400         MOVE      MED-MFSFEL TO MOD-TEMFSFEL                             
061500         PERFORM MFS-RENSA-FAELT-UT                                       
061600       END-IF                                                             
061700     END-IF                                                               
061800     .                                                                    
061900     EJECT                                                                
062000 BA-KOLLA-DISTRIKT SECTION.                                               
062100                                                                          
062200     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
062300                                                                          
062400     IF MID-IDDISTR-IN     NOT = ALL '+'                                  
062500       INSPECT WS-IDDISTR  REPLACING LEADING SPACE BY ZERO                
062600       MOVE '7'         TO MFS-IDPFK                                      
062700       MOVE SPACE       TO MFS-KDTRTYP                                    
062800     END-IF                                                               
062900                                                                          
063000     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
063100       MOVE MSGI-IDDISTR   TO WS-IDDISTR-NUM                              
063200       MOVE WS-IDDISTR-NUM TO W-IDDISTR                                   
063300     ELSE                                                                 
063400       MOVE NEJ TO NYCKLAR-SW                                             
063500     END-IF                                                               
063600                                                                          
063700     IF WS-IDDISTR-NUM         = ZERO                                     
063800       MOVE '   0'             TO MOD-IDDISTR-UT                          
063900     ELSE                                                                 
064000       MOVE MSGI-IDDISTR       TO MOD-IDDISTR-UT                          
064100       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
064200     END-IF                                                               
064300                                                                          
064400     .                                                                    
064500     EJECT                                                                
064600 BB-KOLLA-KUNDNR SECTION.                                                 
064700                                                                          
064800     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
064900                                                                          
065000     IF MID-IDKUNDNR-IN   NOT = ALL '+'                                   
065100       MOVE '7'           TO MFS-IDPFK                                    
065200       MOVE SPACE         TO MFS-KDTRTYP                                  
065300     END-IF                                                               
065400                                                                          
065500     IF MSGI-IDKUNDNR NUMERIC                                             
065600       MOVE MSGI-IDKUNDNR     TO WS-IDKUNDNR-NUM                          
065700       MOVE WS-IDKUNDNR-NUM   TO W-IDKUNDNR                               
065800     ELSE                                                                 
065900       MOVE NEJ TO NYCKLAR-SW                                             
066000     END-IF                                                               
066100                                                                          
066200     MOVE MSGI-IDKUNDNR     TO MOD-IDKUNDNR-UT                            
066300     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
066400     IF MOD-IDKUNDNR-UT     = SPACE                                       
066500       MOVE '     0'        TO MOD-IDKUNDNR-UT                            
066600     END-IF                                                               
066700     .                                                                    
066800     EJECT                                                                
066900 BC-KOLLA-KUNDRF SECTION.                                                 
067000                                                                          
067100     MOVE MFS-RENSA-FAELT TO MOD-IDORDNR7-IN                              
067200                                                                          
067300     IF MID-IDORDNR7-IN     NOT = ALL '+'                                 
067400       MOVE '7'             TO MFS-IDPFK                                  
067500       MOVE SPACE           TO MFS-KDTRTYP                                
067600     END-IF                                                               
067700                                                                          
067800     IF MSGI-IDKUNDRF(1:7) NUMERIC                                        
067900       MOVE MSGI-IDKUNDRF(1:7)   TO W-IDKUNDREF                           
068000       MOVE '3' TO KEY-KOLL                                               
068100     ELSE                                                                 
068200       MOVE NEJ TO NYCKLAR-SW                                             
068300     END-IF                                                               
068400                                                                          
068500     MOVE MSGI-IDKUNDRF(1:7)   TO MOD-IDORDNR7-UT                         
068600     INSPECT MOD-IDORDNR7-UT REPLACING LEADING ZERO BY SPACE              
068700     IF MOD-IDORDNR7-UT = SPACE                                           
068800       MOVE '      0' TO MOD-IDORDNR7-UT                                  
068900     END-IF                                                               
069000                                                                          
069100     IF MID-IDORDNR7-IN = ALL '+' AND                                     
069200        MID-IDORDNR7-UT = SPACE   AND                                     
069300        MOD-IDORDNR7-UT NOT = '      0'                                   
069400       MOVE NEJ TO IFYLLT-SW                                              
069500     END-IF                                                               
069600     .                                                                    
069700     EJECT                                                                
069800 BD-KOLLA-IDDC SECTION.                                                   
069900                                                                          
070000     MOVE MFS-RENSA-FAELT   TO MOD-IDDC-IN                                
070100     IF MID-IDDC-IN = ALL '+'                                             
070200       IF PROFORMA                                                        
070300         MOVE WC-CDC-SE     TO W-IDDC-B6                                  
070400       ELSE                                                               
070500         IF MID-IDDC-UT = SPACE                                           
070600           MOVE ' 0'        TO W-IDDC-B6                                  
070700         ELSE                                                             
070800           MOVE MID-IDDC-UT TO W-IDDC-B6                                  
070900         END-IF                                                           
071000       END-IF                                                             
071100     ELSE                                                                 
071200       IF PROFORMA                                                        
071300         MOVE WC-CDC-SE     TO W-IDDC-B6                                  
071400       ELSE                                                               
071500         MOVE MID-IDDC-IN   TO W-IDDC-B6                                  
071600       END-IF                                                             
071700       MOVE '7'             TO MFS-IDPFK                                  
071800       MOVE SPACE           TO MFS-KDTRTYP                                
071900     END-IF                                                               
072000     PERFORM IMS-GU-WDB601                                                
072100                                                                          
072200     MOVE  W-IDDC-B6       TO WS1-IDDC                                    
072300     INSPECT WS1-IDDC REPLACING ALL SPACES BY ZEROS                       
072400                                                                          
072500     IF PROFORMA                                                          
072600       CONTINUE                                                           
072700     ELSE                                                                 
072800                                                                          
072900       IF DCS-KDDC = SPACE OR DCS-CDC-TR                                  
073000          IF WS1-IDDC = ZERO                                              
073100             CONTINUE                                                     
073200           ELSE                                                           
073300             MOVE MSGI-IDDC   TO W-IDDC-B6                                
073400             PERFORM IMS-GU-WDB601                                        
073500           END-IF                                                         
073600       END-IF                                                             
073700     END-IF                                                               
073800                                                                          
073900     MOVE W-IDDC-B6        TO MOD-IDDC-UT                                 
074000                                                                          
074100     IF NYCKLAR-OK AND WS1-IDDC NOT = ZERO                                
074200       MOVE W-IDDC-B6 TO W-IDDC-MIN-MIN                                   
074300                         W-IDDC-MIN-MAX                                   
074400                         W-IDDC-MAX-MIN                                   
074500                         W-IDDC-MAX-MAX                                   
074600     END-IF                                                               
074700     .                                                                    
074800     EJECT                                                                
074900 BE-KOLLA-ARTNR SECTION.                                                  
075000                                                                          
075100     IF W-IDTRANS = '4281'                                                
075200        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                            
075300     ELSE                                                                 
075400        IF MID-IDARTNR-IN = ALL '+'                                       
075500            MOVE MID-IDARTNR-UT TO WS-IDARTNR                             
075600            INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO            
075700        ELSE                                                              
075800          MOVE MID-IDARTNR-IN TO WS-IDARTNR                               
075900          INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO              
076000          MOVE '7'      TO MFS-IDPFK                                      
076100        END-IF                                                            
076200        IF WS-IDARTNR NUMERIC                                             
076300          IF WS-IDARTNR = ZERO                                            
076400            CONTINUE                                                      
076500          ELSE                                                            
076600            MOVE '4' TO KEY-KOLL                                          
076700            MOVE WS-IDARTNR TO WS-IDARTNR-NUM                             
076800            MOVE WS-IDARTNR-NUM TO W-IDARTNR                              
076900                                   W-IDARTNR-WDD301                       
077000                                   W-IDARTNR-MIN-MIN                      
077100                                   W-IDARTNR-MIN-MAX                      
077200                                   W-IDARTNR-MAX-MIN                      
077300                                   W-IDARTNR-MAX-MAX                      
077400                                   WS-IDARTNR-SPAR                        
077500            MOVE JA TO ALLT-SW                                            
077600            MOVE JA TO IFYLLT-SW                                          
077700          END-IF                                                          
077800        ELSE                                                              
077900          MOVE WS-IDARTNR TO MOD-IDARTNR-UT                               
078000          MOVE NEJ TO NYCKLAR-SW                                          
078100        END-IF                                                            
078200                                                                          
078300        IF GODK-MID AND WS-IDARTNR NOT = ZERO                             
078400          MOVE WS-IDARTNR TO MOD-IDARTNR-UT                               
078500          INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE          
078600        ELSE                                                              
078700          MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                          
078800        END-IF                                                            
078900     END-IF                                                               
079000     .                                                                    
079100     EJECT                                                                
079200 BF-KOLLA-ORDBEK SECTION.                                                 
079300                                                                          
079400     IF W-IDTRANS = '4281'                                                
079500        MOVE MFS-RENSA-FAELT TO MOD-KDORDBEK-UT                           
079600     ELSE                                                                 
079700        IF MID-KDORDBEK-IN = ALL '+'                                      
079800          MOVE MID-KDORDBEK-UT TO GODK-KDORDBEK                           
079900        ELSE                                                              
080000          MOVE MID-KDORDBEK-IN TO GODK-KDORDBEK                           
080100          MOVE '7'      TO MFS-IDPFK                                      
080200        END-IF                                                            
080300                                                                          
080400        IF GODK-KOD AND GODK-MID                                          
080500          MOVE '5' TO KEY-KOLL                                            
080600          MOVE GODK-KDORDBEK TO WS-KDORDBEK-SPAR                          
080700                                W-KDORDBEK-MAX-MIN                        
080800                                W-KDORDBEK-MAX-MAX                        
080900                                MOD-KDORDBEK-UT                           
081000                                WS-KDORDBEK-NUM                           
081100        ELSE                                                              
081200          IF NOLL-KOD                                                     
081300            MOVE MFS-RENSA-FAELT TO MOD-KDORDBEK-UT                       
081400          ELSE                                                            
081500            MOVE GODK-KDORDBEK TO MOD-KDORDBEK-UT                         
081600            MOVE NEJ TO NYCKLAR-SW                                        
081700          END-IF                                                          
081800        END-IF                                                            
081900     END-IF                                                               
082000     .                                                                    
082100     EJECT                                                                
082200 BG-KOLLA-FRAKTKOD SECTION.                                               
082300                                                                          
082400     MOVE MSGI-KDFRAKT    TO MOD-KDFRAKT-UT                               
082500     .                                                                    
082600     EJECT                                                                
082700 BH-KOLLA-ORDERKLASS SECTION.                                             
082800                                                                          
082900     MOVE MFS-RENSA-FAELT TO MOD-KDORDKL-IN                               
083000                                                                          
083100     IF MID-KDORDKL-IN = ALL '+'                                          
083200       MOVE MFS-RENSA-FAELT TO MOD-KDORDKL-UT                             
083300     ELSE                                                                 
083400       MOVE MID-KDORDKL-IN TO MOD-KDORDKL-UT                              
083500       INSPECT MOD-KDORDKL-UT REPLACING LEADING ZERO BY SPACE             
083600     END-IF                                                               
083700     .                                                                    
083800     EJECT                                                                
083900                                                                          
084000 BI-KOLLA-PROFORMA SECTION.                                               
084100                                                                          
084200     MOVE MFS-RENSA-FAELT TO MOD-PROFORMA                                 
084300     MOVE MFS-RENSA-FAELT TO MOD-PROFORMA-SPAR                            
084400                                                                          
084500     IF MID-PROFORMA = ALL '+'                                            
084600           MOVE NEJ TO MOD-PROFORMA                                       
084700                       MOD-PROFORMA-SPAR                                  
084800                       PROFORMA-SW                                        
084900     ELSE                                                                 
085000        IF MID-PROFORMA = MID-PROFORMA-SPAR                               
085100           CONTINUE                                                       
085200        ELSE                                                              
085300           MOVE '7' TO MFS-IDPFK                                          
085400           MOVE SPACE TO MFS-KDTRTYP                                      
085500        END-IF                                                            
085600                                                                          
085700        IF MID-PROFORMA = NEJ                                             
085800           MOVE NEJ TO MOD-PROFORMA                                       
085900                       MOD-PROFORMA-SPAR                                  
086000        ELSE                                                              
086100           IF MID-PROFORMA = 'J' OR 'Y'                                   
086200              MOVE JA           TO MOD-PROFORMA                           
086300                                   MOD-PROFORMA-SPAR                      
086400                                   PROFORMA-SW                            
086500           ELSE                                                           
086600              MOVE MFS-ALFA-FAELT-FEL TO MOD-PROFORMA-ATTR                
086700              MOVE MFS-ROER-EJ-FAELT  TO MOD-PROFORMA                     
086800              MOVE NEJ TO NYCKLAR-SW                                      
086900              MOVE NEJ TO ALLT-SW                                         
087000           END-IF                                                         
087100        END-IF                                                            
087200     END-IF                                                               
087300     .                                                                    
087400     EJECT                                                                
087500 C-FOERSTA-SIDA SECTION.                                                  
087600                                                                          
087700     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
087800     CALL WMEDKONV USING MED-WMEDAREA                                     
087900     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
088000     .                                                                    
088100 D-NAESTA-SIDA SECTION.                                                   
088200                                                                          
088300     IF MID-IDORDER-NEXT > ZERO                                           
088400       MOVE MID-IDORDER-NEXT  TO W-IDORDER-BAS-MIN                        
088500                                   W-IDORDER-BAS-MAX                      
088600                                   W-IDORDER-MIN-MIN                      
088700                                   W-IDORDER-MIN-MAX                      
088800                                   W-IDORDER-MAX-MIN                      
088900                                   W-IDORDER-MAX-MAX                      
089000                                   W-IDORDER-41-MIN                       
089100                                   W-IDORDER-41-MAX                       
089200       MOVE MID-IDARTNR-NEXT  TO W-IDARTNR                                
089300                                   W-IDARTNR-BAS-MIN                      
089400                                   W-IDARTNR-MIN-MIN                      
089500                                   W-IDARTNR-MIN-MAX                      
089600                                   W-IDARTNR-MAX-MIN                      
089700                                   W-IDARTNR-MAX-MAX                      
089800*                                  W-IDARTNR-MIN                          
089900*                                  W-IDARTNR-MAX                          
090000                                   W-IDARTNR-WDD301                       
090100                                   W-IDARTNR-41-MIN                       
090200                                   W-IDARTNR-41-MAX                       
090300       MOVE MID-IDLOPNR-NEXT  TO W-IDLOPNR-BAS-MIN                        
090400                                   W-IDLOPNR-MIN-MIN                      
090500                                   W-IDLOPNR-MAX-MIN                      
090600                                   W-IDLOPNR-41-MIN                       
090700                                   W-IDLOPNR-41-MIN                       
090800       MOVE MID-IDSEKVNR-NEXT TO W-IDSEKVNR-BAS-MIN                       
090900                                   W-IDSEKVNR-MIN-MIN                     
091000                                   W-IDSEKVNR-MAX-MIN                     
091100                                   W-IDSEKVNR-41-MIN                      
091200       MOVE MID-IDDC-NEXT TO                                              
091300                                   W-IDDC-MIN-MIN                         
091400                                   W-IDDC-MIN-MAX                         
091500                                   W-IDDC-MAX-MIN                         
091600                                   W-IDDC-MAX-MAX                         
091700                                   W-IDDC-41-MIN                          
091800                                   W-IDDC-41-MAX                          
091900       MOVE MID-KDORDBEK-NEXT TO W-KDORDBEK-BAS-MIN                       
092000                                   W-KDORDBEK-MIN-MIN                     
092100                                   W-KDORDBEK-MAX-MIN                     
092200                                   W-KDORDBEK-MAX-MAX                     
092300     END-IF                                                               
092400     MOVE JA TO ALLT-SW                                                   
092500     .                                                                    
092600     EJECT                                                                
092700 E-SAMMA-SIDA SECTION.                                                    
092800                                                                          
092900     IF MID-IDORDER-ENTER > ZERO                                          
093000       MOVE MID-IDORDER-ENTER TO W-IDORDER-BAS-MIN                        
093100                                   W-IDORDER-BAS-MAX                      
093200                                   W-IDORDER-MIN-MIN                      
093300                                   W-IDORDER-MIN-MAX                      
093400                                   W-IDORDER-MAX-MIN                      
093500                                   W-IDORDER-MAX-MAX                      
093600       MOVE MID-IDARTNR-ENTER TO W-IDARTNR                                
093700                                   W-IDARTNR-BAS-MIN                      
093800                                   W-IDARTNR-MIN-MIN                      
093900                                   W-IDARTNR-MIN-MAX                      
094000                                   W-IDARTNR-MAX-MIN                      
094100                                   W-IDARTNR-MAX-MAX                      
094200                                   W-IDARTNR-WDD301                       
094300       MOVE MID-IDLOPNR-ENTER TO W-IDLOPNR-BAS-MIN                        
094400                                   W-IDLOPNR-MIN-MIN                      
094500                                   W-IDLOPNR-MAX-MIN                      
094600       MOVE MID-IDSEKVNR-ENTER TO W-IDSEKVNR-BAS-MIN                      
094700                                   W-IDSEKVNR-MIN-MIN                     
094800                                   W-IDSEKVNR-MAX-MIN                     
094900       MOVE MID-IDDC-ENTER TO                                             
095000                                   W-IDDC-MIN-MIN                         
095100                                   W-IDDC-MIN-MAX                         
095200                                   W-IDDC-MAX-MIN                         
095300                                   W-IDDC-MAX-MAX                         
095400       MOVE MID-KDORDBEK-ENTER TO W-KDORDBEK-BAS-MIN                      
095500                                   W-KDORDBEK-MIN-MIN                     
095600                                   W-KDORDBEK-MAX-MIN                     
095700                                   W-KDORDBEK-MAX-MAX                     
095800     END-IF                                                               
095900     MOVE JA TO ALLT-SW                                                   
096000     .                                                                    
096100     EJECT                                                                
096200 F-INIT-NYCKLAR SECTION.                                                  
096300                                                                          
096400     MOVE OHUV-IDORDER TO W-IDORDER-BAS-MIN                               
096500                          W-IDORDER-BAS-MAX                               
096600                          W-IDORDER-MIN-MIN                               
096700                          W-IDORDER-MIN-MAX                               
096800                          W-IDORDER-MAX-MIN                               
096900                          W-IDORDER-MAX-MAX                               
097000                          WS-IDORDER-SPAR                                 
097100     MOVE OHUV-IDSKYLT TO W-IDSKYLT                                       
097200     .                                                                    
097300     EJECT                                                                
097400 FA-INIT-PROF-NYCKLAR SECTION.                                            
097500                                                                          
097600     MOVE PHUV-IDORDER TO W-IDORDER-BAS-MIN                               
097700                          W-IDORDER-BAS-MAX                               
097800                          W-IDORDER-MIN-MIN                               
097900                          W-IDORDER-MIN-MAX                               
098000                          W-IDORDER-MAX-MIN                               
098100                          W-IDORDER-MAX-MAX                               
098200                          WS-IDORDER-SPAR                                 
098300     MOVE PHUV-IDSKYLT TO W-IDSKYLT                                       
098400     .                                                                    
098500                                                                          
098600     EJECT                                                                
098700 G-LAES-VISA-INFO SECTION.                                                
098800                                                                          
098900     IF BASNR-IFYLLT                                                      
099000       IF PROFORMA                                                        
099100          PERFORM IMS-GET-ORQM01-BAS-PROF                                 
099200       ELSE                                                               
099300          PERFORM IMS-GET-ORQM01-BAS                                      
099400       END-IF                                                             
099500     ELSE                                                                 
099600       IF ARTNR-IFYLLT                                                    
099700         IF PROFORMA                                                      
099800            PERFORM IMS-GET-ORQM01-MIN-PROF                               
099900         ELSE                                                             
100000            IF PROFORMA                                                   
100100               PERFORM IMS-GET-ORQM01-MIN-PROF                            
100200            ELSE                                                          
100300               PERFORM IMS-GET-ORQM01-MIN                                 
100400            END-IF                                                        
100500         END-IF                                                           
100600       ELSE                                                               
100700         IF KDORDBEK-IFYLLT                                               
100800           IF PROFORMA                                                    
100900              PERFORM IMS-GET-ORQM01-MAX-PROF                             
101000           ELSE                                                           
101100              PERFORM IMS-GET-ORQM01-MAX                                  
101200           END-IF                                                         
101300         END-IF                                                           
101400       END-IF                                                             
101500     END-IF                                                               
101600     IF SEGMENT-FINNS                                                     
101700       MOVE OBKR-IDARTNR        TO MOD-IDARTNR-ENTER                      
101800                                   MOD-IDARTNR-NEXT                       
101900       MOVE OBKR-IDLOPNR        TO MOD-IDLOPNR-ENTER                      
102000                                   MOD-IDLOPNR-NEXT                       
102100       MOVE OBKR-IDORDER        TO MOD-IDORDER-ENTER                      
102200                                   MOD-IDORDER-NEXT                       
102300       MOVE OBKR-IDSEKVNR       TO MOD-IDSEKVNR-ENTER                     
102400                                   MOD-IDSEKVNR-NEXT                      
102500       MOVE OBKR-IDDC           TO MOD-IDDC-ENTER                         
102600                                   MOD-IDDC-NEXT                          
102700       MOVE OBKR-KDORDBEK       TO MOD-KDORDBEK-ENTER                     
102800                                   MOD-KDORDBEK-NEXT                      
102900       IF NOT ARTNR-IFYLLT                                                
103000         MOVE OBKR-IDARTNR      TO WS-IDARTNR-NUM                         
103100       END-IF                                                             
103200                                                                          
103300       IF NOT KDORDBEK-IFYLLT                                             
103400         MOVE OBKR-KDORDBEK     TO WS-KDORDBEK-NUM                        
103500       END-IF                                                             
103600       PERFORM GA-KOLLA-VILKEN-LAESNING                                   
103700     ELSE                                                                 
103800       IF ARTNR-IFYLLT                                                    
103900         MOVE '017' TO MED-IDMFSFEL                                       
104000         CALL WMEDKONV USING MED-WMEDAREA                                 
104100         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
104200         PERFORM MFS-RENSA-FAELT-UT                                       
104300         MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                             
104400       ELSE                                                               
104500         IF KDORDBEK-IFYLLT                                               
104600           MOVE '025'              TO MED-IDMFSFEL                        
104700           CALL WMEDKONV USING MED-WMEDAREA                               
104800           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
104900           PERFORM MFS-RENSA-FAELT-UT                                     
105000           MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                           
105100         ELSE                                                             
105200           MOVE '058'              TO MED-IDMFSFEL                        
105300           CALL WMEDKONV USING MED-WMEDAREA                               
105400           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
105500           PERFORM MFS-RENSA-FAELT-UT                                     
105600           MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                           
105700         END-IF                                                           
105800       END-IF                                                             
105900     END-IF                                                               
106000     .                                                                    
106100     EJECT                                                                
106200 GA-KOLLA-VILKEN-LAESNING SECTION.                                        
106300                                                                          
106400     MOVE +1 TO INDX                                                      
106500     IF BASNR-IFYLLT                                                      
106600       PERFORM UNTIL SEGMENT-SAKNAS OR                                    
106700                     BASEN-SLUT OR                                        
106800                     INDX > MAX-INDX                                      
106900                                                                          
107000         IF W-IDGMTREF-X = OBKR-IDGMTREF                                  
107100           MOVE SPACE           TO WS-ASTERIX                             
107200           PERFORM GB-KOLLA-ORDBEK-KOD                                    
107300           ADD +1 TO INDX                                                 
107400         END-IF                                                           
107500         IF PROFORMA                                                      
107600            PERFORM IMS-GET-ORQM01-BAS-PROF                               
107700         ELSE                                                             
107800            PERFORM IMS-GET-ORQM01-BAS                                    
107900         END-IF                                                           
108000         IF SEGMENT-FINNS                                                 
108100           MOVE OBKR-KDORDBEK   TO WS-KDORDBEK-NUM                        
108200           IF OBKR-KDORDBEK = 61 AND INDX > MAX-INDX                      
108300              PERFORM GAA-BACKA-BILDEN-KOD-61                             
108400           END-IF                                                         
108500         END-IF                                                           
108600       END-PERFORM                                                        
108700     ELSE                                                                 
108800       IF ARTNR-IFYLLT                                                    
108900         PERFORM UNTIL SEGMENT-SAKNAS OR                                  
109000                       BASEN-SLUT OR                                      
109100                       INDX > MAX-INDX                                    
109200           IF W-IDGMTREF-X = OBKR-IDGMTREF                                
109300             MOVE SPACE TO WS-ASTERIX                                     
109400             PERFORM GB-KOLLA-ORDBEK-KOD                                  
109500             ADD +1 TO INDX                                               
109600           END-IF                                                         
109700           IF PROFORMA                                                    
109800              PERFORM IMS-GET-ORQM01-MIN-PROF                             
109900           ELSE                                                           
110000              PERFORM IMS-GET-ORQM01-MIN                                  
110100           END-IF                                                         
110200           IF SEGMENT-FINNS                                               
110300             MOVE OBKR-KDORDBEK TO WS-KDORDBEK-NUM                        
110400             IF OBKR-KDORDBEK = 61 AND INDX > MAX-INDX                    
110500                PERFORM GAA-BACKA-BILDEN-KOD-61                           
110600             END-IF                                                       
110700           END-IF                                                         
110800         END-PERFORM                                                      
110900       ELSE                                                               
111000         IF KDORDBEK-IFYLLT                                               
111100           PERFORM UNTIL SEGMENT-SAKNAS OR                                
111200                         BASEN-SLUT OR                                    
111300                         INDX > MAX-INDX                                  
111400             IF W-IDGMTREF-X = OBKR-IDGMTREF                              
111500               MOVE SPACE TO WS-ASTERIX                                   
111600               PERFORM GB-KOLLA-ORDBEK-KOD                                
111700               ADD +1 TO INDX                                             
111800             END-IF                                                       
111900             IF PROFORMA                                                  
112000                PERFORM IMS-GET-ORQM01-MAX-PROF                           
112100             ELSE                                                         
112200                PERFORM IMS-GET-ORQM01-MAX                                
112300             END-IF                                                       
112400             IF SEGMENT-FINNS                                             
112500               MOVE OBKR-KDORDBEK TO WS-KDORDBEK-NUM                      
112600               IF OBKR-KDORDBEK = 61 AND INDX > MAX-INDX                  
112700                  PERFORM GAA-BACKA-BILDEN-KOD-61                         
112800               END-IF                                                     
112900             END-IF                                                       
113000           END-PERFORM                                                    
113100         END-IF                                                           
113200       END-IF                                                             
113300     END-IF                                                               
113400     IF INDX > MAX-INDX AND SEGMENT-FINNS AND INDX NOT > +14              
113500       PERFORM GC-SPARA-NEXT-NYCKLAR                                      
113600     END-IF                                                               
113700     .                                                                    
113800     EJECT                                                                
113900 GAA-BACKA-BILDEN-KOD-61 SECTION.                                         
114000                                                                          
114100     IF SPAR-INDX > 1                                                     
114200        MOVE SPAR-INDX TO INDX                                            
114300        PERFORM UNTIL INDX > MAX-INDX                                     
114400           PERFORM MFS-RENSA-FAELT-PA-RAD                                 
114500           ADD +1 TO INDX                                                 
114600        END-PERFORM                                                       
114700        MOVE SPAR-IDORDER-NEXT        TO MOD-IDORDER-NEXT                 
114800        MOVE SPAR-IDARTNR-NEXT        TO MOD-IDARTNR-NEXT                 
114900        MOVE SPAR-IDLOPNR-NEXT        TO MOD-IDLOPNR-NEXT                 
115000        MOVE SPAR-IDSEKVNR-NEXT       TO MOD-IDSEKVNR-NEXT                
115100        MOVE SPAR-IDDC-NEXT           TO MOD-IDDC-NEXT                    
115200        MOVE SPAR-KDORDBEK-NEXT       TO MOD-KDORDBEK-NEXT                
115300        MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                         
115400        CALL WMEDKONV USING MED-WMEDAREA                                  
115500        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
115600        MOVE +15                      TO INDX                             
115700     END-IF                                                               
115800     .                                                                    
115900     EJECT                                                                
116000 GB-KOLLA-ORDBEK-KOD SECTION.                                             
116100                                                                          
116200     EVALUATE WS-KDORDBEK-NUM                                             
116300     WHEN 10                                                              
116400       PERFORM GBA-LAES-VISA-KOD-10                                       
116500     WHEN 15 THRU 16                                                      
116600       PERFORM GBB-LAES-VISA-KOD-15-TILL-16                               
116700     WHEN 20 THRU 22                                                      
116800       PERFORM GBQ-LAES-VISA-KOD-20-TILL-22                               
116900     WHEN 26                                                              
117000       PERFORM GBN-LAES-VISA-KOD-95-TILL-97                               
117100     WHEN 30 THRU 34                                                      
117200       PERFORM GBL-LAES-VISA-KOD-80-TILL-83                               
117300     WHEN 40                                                              
117400       PERFORM GBC-LAES-VISA-KOD-40                                       
117500     WHEN 41 THRU 42                                                      
117600       MOVE ASTERIX TO WS-ASTERIX                                         
117700       PERFORM GBD-LAES-VISA-KOD-41-TILL-42                               
117800     WHEN 43 THRU 44                                                      
117900       PERFORM GBE-LAES-VISA-KOD-43-TILL-44                               
118000     WHEN 51 THRU 56                                                      
118100     WHEN 66 THRU 68                                                      
118200       PERFORM GBF-LAES-VISA-51-56--66-68                                 
118300     WHEN 57                                                              
118400       PERFORM GBG-LAES-VISA-KOD-57                                       
118500     WHEN 58 THRU 59                                                      
118600       PERFORM GBH-LAES-VISA-KOD-58-TILL-59                               
118700     WHEN 61                                                              
118800       MOVE ASTERIX TO WS-ASTERIX                                         
118900       PERFORM GBI-LAES-VISA-KOD-61-TILL-65                               
119000     WHEN 65                                                              
119100       MOVE ASTERIX TO WS-ASTERIX                                         
119200       PERFORM GBI-LAES-VISA-KOD-61-TILL-65                               
119300     WHEN 70 THRU 71                                                      
119400       PERFORM GBJ-LAES-VISA-KOD-70-TILL-71                               
119500     WHEN 72 THRU 76                                                      
119600       PERFORM GBK-LAES-VISA-KOD-72-TILL-76                               
119700     WHEN 77                                                              
119800       PERFORM GBJ-LAES-VISA-KOD-70-TILL-71                               
119900     WHEN 80 THRU 83                                                      
120000       PERFORM GBL-LAES-VISA-KOD-80-TILL-83                               
120100     WHEN 84                                                              
120200       PERFORM GBR-LAES-VISA-KOD-84                                       
120300     WHEN 85                                                              
120400       PERFORM GBL-LAES-VISA-KOD-80-TILL-83                               
120500     WHEN 87                                                              
120600       PERFORM GBL-LAES-VISA-KOD-80-TILL-83                               
120700     WHEN 90 THRU 93                                                      
120800       PERFORM GBM-LAES-VISA-KOD-90-TILL-93                               
120900     WHEN 95 THRU 97                                                      
121000       PERFORM GBN-LAES-VISA-KOD-95-TILL-97                               
121100     WHEN 98                                                              
121200       PERFORM GBO-LAES-VISA-KOD-98                                       
121300     WHEN 99                                                              
121400       PERFORM GBP-LAES-VISA-KOD-99                                       
121500*FIX TRASIG BAS 00 I NYCKEL                                               
121600     WHEN 00                                                              
121700       PERFORM GBL-LAES-VISA-KOD-80-TILL-83                               
121800     END-EVALUATE                                                         
121900     .                                                                    
122000     EJECT                                                                
122100 GBA-LAES-VISA-KOD-10 SECTION.                                            
122200                                                                          
122300     MOVE SPACE                 TO WS-ASTERIX                             
122400     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
122500     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
122600     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
122700     MOVE OBKR-IDARTNR          TO WS-IDARTNR                             
122800     MOVE OBKR-REKSIFFR         TO WS-REKNR                               
122900     MOVE ARTIKEL-REKSIFFRA     TO MOD-IDARTNR(INDX)                      
123000     INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE            
123100     MOVE OBKR-KVBEART-Q        TO MOD-KVANTAL(INDX)                      
123200     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
123300     IF OBKR-IDKUNDRF-RO NOT = '0000000   '                               
123400       MOVE OBKR-IDKUNDRF-RO    TO WS-IDKUNDRF                            
123500     ELSE                                                                 
123600       MOVE SPACE               TO WS-IDKUNDRF                            
123700     END-IF                                                               
123800     IF BLANK-TECKEN = SPACE                                              
123900       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
124000     ELSE                                                                 
124100       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
124200     END-IF                                                               
124300     IF OBKR-TITPO > +0                                                   
124400        MOVE OBKR-TITPO         TO MOD-TITPO(INDX)                        
124500     ELSE                                                                 
124600        MOVE MFS-RENSA-FAELT    TO MOD-TITPO(INDX)                        
124700     END-IF                                                               
124800     IF BASNR-IFYLLT OR KDORDBEK-IFYLLT                                   
124900       MOVE OBKR-IDARTNR        TO W-IDARTNR-WDD301                       
125000     END-IF                                                               
125100     PERFORM S01-BEART-TEXT                                               
125200     .                                                                    
125300     EJECT                                                                
125400 GBB-LAES-VISA-KOD-15-TILL-16 SECTION.                                    
125500                                                                          
125600     MOVE SPACE                 TO WS-ASTERIX                             
125700     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
125800     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
126200     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
126300     IF OBKR-IDARTNR-TILLK > ZERO                                         
126400       MOVE OBKR-IDARTNR-TILLK TO WS-IDARTNR                              
126500       MOVE OBKR-REKSIFFR-TILLK TO WS-REKNR                               
126600       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
126700       INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE          
126800     ELSE                                                                 
126900       MOVE OBKR-IDARTNR        TO WS-IDARTNR                             
127000       MOVE OBKR-REKSIFFR       TO WS-REKNR                               
127100       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
127200       INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE          
127300     END-IF                                                               
127400     MOVE OBKR-KVBEART-Q        TO MOD-KVANTAL(INDX)                      
127500     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
127600     IF OBKR-IDKUNDRF-RO NOT = '0000000   '                               
127700       MOVE OBKR-IDKUNDRF-RO    TO WS-IDKUNDRF                            
127800     ELSE                                                                 
127900       MOVE SPACE               TO WS-IDKUNDRF                            
128000     END-IF                                                               
128100     IF BLANK-TECKEN = SPACE                                              
128200       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
128300     ELSE                                                                 
128400       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
128500     END-IF                                                               
128600     MOVE MFS-RENSA-FAELT       TO MOD-TITPO(INDX)                        
128700     IF BASNR-IFYLLT OR KDORDBEK-IFYLLT                                   
128800        IF OBKR-IDARTNR-TILLK > ZERO                                      
128900          MOVE OBKR-IDARTNR-TILLK                                         
129000                                TO W-IDARTNR-WDD301                       
129100        ELSE                                                              
129200          MOVE OBKR-IDARTNR     TO W-IDARTNR-WDD301                       
129300        END-IF                                                            
129400     END-IF                                                               
129500     PERFORM S01-BEART-TEXT                                               
129600     .                                                                    
129700     EJECT                                                                
129800 GBC-LAES-VISA-KOD-40 SECTION.                                            
129900                                                                          
130000     MOVE SPACE                 TO WS-ASTERIX                             
130100     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
130200     MOVE ASTERIX               TO WS-ASTERIX                             
130300     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
130400     IF OBKR-IDARTNR-TILLK > ZERO                                         
130800        MOVE SPACE              TO WS-KDORDBEK                            
131000        MOVE OBKR-IDARTNR-TILLK TO WS-IDARTNR                             
131100        MOVE OBKR-REKSIFFR-TILLK TO WS-REKNR                              
131200     ELSE                                                                 
131300        MOVE OBKR-IDARTNR       TO WS-IDARTNR                             
131400        MOVE OBKR-REKSIFFR      TO WS-REKNR                               
131500     END-IF                                                               
131600     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
131700     MOVE ARTIKEL-REKSIFFRA     TO MOD-IDARTNR(INDX)                      
131800     INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE            
131900     MOVE OBKR-KVBEART-TILLK    TO MOD-KVANTAL(INDX)                      
132000     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
132100     IF OBKR-IDKUNDRF-RO NOT = '0000000   '                               
132200       MOVE OBKR-IDKUNDRF-RO    TO WS-IDKUNDRF                            
132300     ELSE                                                                 
132400       MOVE SPACE               TO WS-IDKUNDRF                            
132500     END-IF                                                               
132600     IF BLANK-TECKEN = SPACE                                              
132700       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
132800     ELSE                                                                 
132900       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
133000     END-IF                                                               
133100     MOVE MFS-RENSA-FAELT       TO MOD-TITPO(INDX)                        
133200     IF BASNR-IFYLLT OR KDORDBEK-IFYLLT                                   
133300        IF OBKR-IDARTNR-TILLK > ZERO                                      
133400          MOVE OBKR-IDARTNR-TILLK                                         
133500                                TO W-IDARTNR-WDD301                       
133600        ELSE                                                              
133700          MOVE OBKR-IDARTNR     TO W-IDARTNR-WDD301                       
133800        END-IF                                                            
133900     END-IF                                                               
134000     PERFORM S01-BEART-TEXT                                               
134100     .                                                                    
134200     EJECT                                                                
134300 GBD-LAES-VISA-KOD-41-TILL-42 SECTION.                                    
134400                                                                          
134500     MOVE NEJ TO RAKNA-UPP-INDX-SW                                        
134600     IF OBKR-IDSEKVNR = 1                                                 
134700       MOVE OBKR-KDORDBEK       TO WS-KDORDBEK                            
134800       MOVE ASTERIX             TO WS-ASTERIX                             
134900       MOVE KOD                 TO MOD-KDORDBEK(INDX)                     
135000       MOVE OBKR-TIREGDAT       TO MOD-TIREGDAT(INDX)                     
135100       MOVE OBKR-IDARTNR        TO WS-IDARTNR                             
135200       MOVE OBKR-REKSIFFR       TO WS-REKNR                               
135300       MOVE ARTIKEL-REKSIFFRA   TO MOD-IDARTNR(INDX)                      
135400       INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE          
135500       MOVE OBKR-KVBEART        TO MOD-KVANTAL(INDX)                      
135600       MOVE OBKR-IDDC           TO MOD-IDDC(INDX)                         
135700       IF OBKR-IDKUNDRF-RO NOT = '0000000   '                             
135800         MOVE OBKR-IDKUNDRF-RO  TO WS-IDKUNDRF                            
135900       ELSE                                                               
136000          MOVE SPACE            TO WS-IDKUNDRF                            
136100       END-IF                                                             
136200       IF BLANK-TECKEN = SPACE                                            
136300         MOVE WS-IDORDNR5       TO MOD-IDORDNR7(INDX)                     
136400       ELSE                                                               
136500         MOVE WS-IDORDNR7       TO MOD-IDORDNR7(INDX)                     
136600       END-IF                                                             
136700       MOVE MFS-RENSA-FAELT     TO MOD-TITPO(INDX)                        
136800       IF BASNR-IFYLLT OR KDORDBEK-IFYLLT                                 
136900         MOVE OBKR-IDARTNR      TO W-IDARTNR-WDD301                       
137000       END-IF                                                             
137100       MOVE OBKR-WDQ101         TO OBKR-SEG-SPAR                          
137200       PERFORM S01-BEART-TEXT                                             
137300       MOVE JA  TO RAKNA-UPP-INDX-SW                                      
137400       MOVE OBKR-SEG-SPAR       TO OBKR-WDQ101                            
137500       MOVE '  '                TO STATUS-WS                              
137600     END-IF                                                               
137700     PERFORM GBDA-KOLLA-ERSAETTNING-FLER                                  
137800     .                                                                    
137900                                                                          
138000     EJECT                                                                
138100 GBDA-KOLLA-ERSAETTNING-FLER SECTION.                                     
138200                                                                          
138300     MOVE INDX                  TO SPAR-INDX                              
138400     MOVE OBKR-IDORDER          TO SPAR-IDORDER-NEXT                      
138500                                   W-IDORDER-41-MIN                       
138600                                   W-IDORDER-41-MAX                       
138700     MOVE OBKR-IDARTNR          TO SPAR-IDARTNR-NEXT                      
138800                                   W-IDARTNR-41-MIN                       
138900                                   W-IDARTNR-41-MAX                       
139000     MOVE OBKR-IDLOPNR          TO SPAR-IDLOPNR-NEXT                      
139100                                   W-IDLOPNR-41-MIN                       
139200                                   W-IDLOPNR-41-MAX                       
139300     MOVE OBKR-IDSEKVNR         TO SPAR-IDSEKVNR-NEXT                     
139400                                   W-IDSEKVNR-41-MIN                      
139500     MOVE OBKR-IDDC             TO SPAR-IDDC-NEXT                         
139600                                   W-IDDC-41-MIN                          
139700                                   W-IDDC-41-MAX                          
139800     MOVE OBKR-KDORDBEK         TO SPAR-KDORDBEK-NEXT                     
139900                                                                          
140000     PERFORM UNTIL SEGMENT-SAKNAS                      OR                 
140100                   BASEN-SLUT                          OR                 
140200                   INDX              > MAX-INDX        OR                 
140300                   OBKR-KDORDBEK NOT = SPAR-KDORDBEK-NEXT                 
140400       IF OBKR-IDARTNR-TILLK NOT = ZERO                                   
140500*        IF MOD-IDDC(1) NUMERIC                                           
140600           IF RAKNA-UPP-INDX                                              
140700             ADD +1 TO INDX                                               
140800           END-IF                                                         
140900*        END-IF                                                           
141000         MOVE JA TO RAKNA-UPP-INDX-SW                                     
141100         IF INDX NOT > MAX-INDX                                           
141200           MOVE    SPACE               TO WS-KDORDBEK                     
141300           MOVE    KOD                 TO MOD-KDORDBEK(INDX)              
141400           MOVE    MFS-RENSA-FAELT     TO MOD-TIREGDAT(INDX)              
141500           MOVE    OBKR-IDARTNR-TILLK  TO WS-IDARTNR                      
141600                                          W-IDARTNR-WDD301                
141700           MOVE    OBKR-REKSIFFR-TILLK TO WS-REKNR                        
141800           MOVE    ARTIKEL-REKSIFFRA   TO MOD-IDARTNR(INDX)               
141900           INSPECT MOD-IDARTNR(INDX)                                      
142000                                  REPLACING LEADING ZERO BY SPACE         
142100           MOVE    OBKR-KVBEART-TILLK  TO MOD-KVANTAL(INDX)               
142200           MOVE    OBKR-IDDC           TO MOD-IDDC(INDX)                  
142300           MOVE    OBKR-DIERS-KVOT     TO MOD-KVQPACK(INDX)               
142400           IF OBKR-IDKUNDRF-RO NOT = '0000000   '                         
142500             MOVE OBKR-IDKUNDRF-RO TO WS-IDKUNDRF                         
142600           ELSE                                                           
142700              MOVE SPACE           TO WS-IDKUNDRF                         
142800           END-IF                                                         
142900           IF BLANK-TECKEN = SPACE                                        
143000             MOVE WS-IDORDNR5   TO MOD-IDORDNR7(INDX)                     
143100           ELSE                                                           
143200             MOVE WS-IDORDNR7   TO MOD-IDORDNR7(INDX)                     
143300           END-IF                                                         
143400           MOVE MFS-RENSA-FAELT TO MOD-TITPO(INDX)                        
143500           PERFORM S01-BEART-TEXT                                         
143600         ELSE                                                             
143700           IF SPAR-INDX > 10                                              
143800             MOVE SPAR-INDX TO INDX                                       
143900             PERFORM UNTIL INDX > MAX-INDX                                
144000                PERFORM MFS-RENSA-FAELT-PA-RAD                            
144100                ADD +1 TO INDX                                            
144200             END-PERFORM                                                  
144300             MOVE SPAR-IDORDER-NEXT   TO MOD-IDORDER-NEXT                 
144400             MOVE SPAR-IDARTNR-NEXT   TO MOD-IDARTNR-NEXT                 
144500             MOVE SPAR-IDLOPNR-NEXT   TO MOD-IDLOPNR-NEXT                 
144600             MOVE SPAR-IDSEKVNR-NEXT  TO MOD-IDSEKVNR-NEXT                
144700             MOVE SPAR-IDDC-NEXT      TO MOD-IDDC-NEXT                    
144800             MOVE SPAR-KDORDBEK-NEXT  TO MOD-KDORDBEK-NEXT                
144900             MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                    
145000             CALL WMEDKONV USING MED-WMEDAREA                             
145100             MOVE MED-MFSINF TO MOD-TEMFSINF                              
145200             MOVE +15 TO INDX                                             
145300           ELSE                                                           
145400             PERFORM  GC-SPARA-NEXT-NYCKLAR                               
145500             MOVE +15 TO INDX                                             
145600           END-IF                                                         
145700         END-IF                                                           
145800       END-IF                                                             
145900       IF INDX NOT = +15                                                  
146000         IF PROFORMA                                                      
146100            PERFORM IMS-GET-ORQM01-41-PROF                                
146200         ELSE                                                             
146300            PERFORM IMS-GET-ORQM01-41                                     
146400         END-IF                                                           
146500         IF SEGMENT-FINNS                                                 
146600            IF OBKR-KDORDBEK NOT = SPAR-KDORDBEK-NEXT                     
146700               ADD +1 TO INDX                                             
146800               MOVE OBKR-KDORDBEK TO WS-KDORDBEK-NUM                      
146900               PERFORM GBDAA-KOLLA-ORDBEK-KOD                             
147000            END-IF                                                        
147100         END-IF                                                           
147200       END-IF                                                             
147300     END-PERFORM                                                          
147400     .                                                                    
147500     EJECT                                                                
147600 GBDAA-KOLLA-ORDBEK-KOD SECTION.                                          
147700                                                                          
147800     IF INDX NOT > MAX-INDX                                               
147900       EVALUATE WS-KDORDBEK-NUM                                           
148000         WHEN 10                                                          
148100           PERFORM GBA-LAES-VISA-KOD-10                                   
148200         WHEN 15 THRU 16                                                  
148300           PERFORM GBB-LAES-VISA-KOD-15-TILL-16                           
148400         WHEN 20 THRU 22                                                  
148500           PERFORM GBQ-LAES-VISA-KOD-20-TILL-22                           
148600         WHEN 26                                                          
148700           PERFORM GBN-LAES-VISA-KOD-95-TILL-97                           
148800         WHEN 30 THRU 34                                                  
148900           PERFORM GBL-LAES-VISA-KOD-80-TILL-83                           
149000         WHEN 40                                                          
149100           PERFORM GBC-LAES-VISA-KOD-40                                   
149200         WHEN 43 THRU 44                                                  
149300           PERFORM GBE-LAES-VISA-KOD-43-TILL-44                           
149400         WHEN 51 THRU 56                                                  
149500         WHEN 66 THRU 68                                                  
149600           PERFORM GBF-LAES-VISA-51-56--66-68                             
149700         WHEN 57                                                          
149800           PERFORM GBG-LAES-VISA-KOD-57                                   
149900         WHEN 58 THRU 59                                                  
150000           PERFORM GBH-LAES-VISA-KOD-58-TILL-59                           
150100         WHEN 61                                                          
150200           MOVE ASTERIX TO WS-ASTERIX                                     
150300           PERFORM GBI-LAES-VISA-KOD-61-TILL-65                           
150400         WHEN 65                                                          
150500           MOVE ASTERIX TO WS-ASTERIX                                     
150600           PERFORM GBI-LAES-VISA-KOD-61-TILL-65                           
150700         WHEN 70 THRU 71                                                  
150800           PERFORM GBJ-LAES-VISA-KOD-70-TILL-71                           
150900         WHEN 72 THRU 76                                                  
151000           PERFORM GBK-LAES-VISA-KOD-72-TILL-76                           
151100         WHEN 77                                                          
151200           PERFORM GBJ-LAES-VISA-KOD-70-TILL-71                           
151300         WHEN 80 THRU 83                                                  
151400           PERFORM GBL-LAES-VISA-KOD-80-TILL-83                           
151500         WHEN 85                                                          
151600           PERFORM GBL-LAES-VISA-KOD-80-TILL-83                           
151700         WHEN 87                                                          
151800           PERFORM GBL-LAES-VISA-KOD-80-TILL-83                           
151900         WHEN 90 THRU 93                                                  
152000           PERFORM GBM-LAES-VISA-KOD-90-TILL-93                           
152100         WHEN 95 THRU 97                                                  
152200           PERFORM GBN-LAES-VISA-KOD-95-TILL-97                           
152300         WHEN 98                                                          
152400           PERFORM GBO-LAES-VISA-KOD-98                                   
152500         WHEN 99                                                          
152600           PERFORM GBP-LAES-VISA-KOD-99                                   
152700       END-EVALUATE                                                       
152800     ELSE                                                                 
152900       MOVE OBKR-IDORDER             TO MOD-IDORDER-NEXT                  
153000       MOVE OBKR-IDARTNR             TO MOD-IDARTNR-NEXT                  
153100       MOVE OBKR-IDLOPNR             TO MOD-IDLOPNR-NEXT                  
153200       MOVE OBKR-IDSEKVNR            TO MOD-IDSEKVNR-NEXT                 
153300       MOVE OBKR-IDDC                TO MOD-IDDC-NEXT                     
153400       MOVE OBKR-KDORDBEK            TO MOD-KDORDBEK-NEXT                 
153500       MOVE INF-MORE-INFO-EXISTS     TO MED-IDMFSINF                      
153600       CALL WMEDKONV                 USING MED-WMEDAREA                   
153700       MOVE MED-MFSINF               TO MOD-TEMFSINF                      
153800       MOVE +15                      TO INDX                              
153900     END-IF                                                               
154000     .                                                                    
154100     EJECT                                                                
154200 GBE-LAES-VISA-KOD-43-TILL-44 SECTION.                                    
154300                                                                          
154400     MOVE SPACE                 TO WS-ASTERIX                             
154500     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
154600     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
154700     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
154800     IF OBKR-IDARTNR-TILLK > ZERO                                         
154900       MOVE OBKR-IDARTNR-TILLK  TO WS-IDARTNR                             
155000       MOVE OBKR-REKSIFFR-TILLK TO WS-REKNR                               
155100       MOVE ARTIKEL-REKSIFFRA   TO MOD-IDARTNR(INDX)                      
155200       INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE          
155300     ELSE                                                                 
155400       MOVE OBKR-IDARTNR        TO WS-IDARTNR                             
155500       MOVE OBKR-REKSIFFR       TO WS-REKNR                               
155600       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
155700       INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE          
155800     END-IF                                                               
155900     MOVE OBKR-KVBEART-Q        TO MOD-KVANTAL(INDX)                      
156000     MOVE OBKR-KVQPACK          TO MOD-KVQPACK(INDX)                      
156100     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
156200     IF OBKR-IDKUNDRF-RO NOT = '0000000   '                               
156300       MOVE OBKR-IDKUNDRF-RO    TO WS-IDKUNDRF                            
156400     ELSE                                                                 
156500       MOVE SPACE               TO WS-IDKUNDRF                            
156600     END-IF                                                               
156700     IF BLANK-TECKEN = SPACE                                              
156800       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
156900     ELSE                                                                 
157000       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
157100     END-IF                                                               
157200     MOVE MFS-RENSA-FAELT       TO MOD-TITPO(INDX)                        
157300     IF BASNR-IFYLLT OR KDORDBEK-IFYLLT                                   
157400        IF OBKR-IDARTNR-TILLK > ZERO                                      
157500          MOVE OBKR-IDARTNR-TILLK                                         
157600                                TO W-IDARTNR-WDD301                       
157700        ELSE                                                              
157800          MOVE OBKR-IDARTNR     TO W-IDARTNR-WDD301                       
157900        END-IF                                                            
158000     END-IF                                                               
158100     PERFORM S01-BEART-TEXT                                               
158200     .                                                                    
158300     EJECT                                                                
158400 GBF-LAES-VISA-51-56--66-68  SECTION.                                     
158500                                                                          
158600     MOVE SPACE                 TO WS-ASTERIX                             
158700     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
158800     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
158900     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
159000     IF OBKR-KDORDBEK = ZERO OR 51 OR 67                                  
159100        MOVE OBKR-IDARTNR       TO WS-IDARTNR                             
159200        MOVE OBKR-REKSIFFR      TO WS-REKNR                               
159300     ELSE                                                                 
159400        IF OBKR-IDARTNR-TILLK > ZERO                                      
159500           MOVE OBKR-IDARTNR-TILLK TO WS-IDARTNR                          
159600           MOVE OBKR-REKSIFFR-TILLK TO WS-REKNR                           
159700        ELSE                                                              
159800           MOVE OBKR-IDARTNR       TO WS-IDARTNR                          
159900           MOVE OBKR-REKSIFFR      TO WS-REKNR                            
160000        END-IF                                                            
160100     END-IF                                                               
160200     MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                          
160300     INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE            
160400     INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE            
160500     MOVE OBKR-KVBEART          TO MOD-KVANTAL(INDX)                      
160600     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
160700     IF OBKR-IDKUNDRF-RO NOT = '0000000   '                               
160800       MOVE OBKR-IDKUNDRF-RO TO WS-IDKUNDRF                               
160900     ELSE                                                                 
161000       MOVE SPACE            TO WS-IDKUNDRF                               
161100     END-IF                                                               
161200     IF BLANK-TECKEN = SPACE                                              
161300       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
161400     ELSE                                                                 
161500       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
161600     END-IF                                                               
161700     IF BASNR-IFYLLT OR KDORDBEK-IFYLLT                                   
161800        IF OBKR-IDARTNR-TILLK > ZERO                                      
161900          MOVE OBKR-IDARTNR-TILLK                                         
162000                                TO W-IDARTNR-WDD301                       
162100        ELSE                                                              
162200          MOVE OBKR-IDARTNR     TO W-IDARTNR-WDD301                       
162300        END-IF                                                            
162400     END-IF                                                               
162500     PERFORM S01-BEART-TEXT                                               
162600     .                                                                    
162700     EJECT                                                                
162800 GBG-LAES-VISA-KOD-57 SECTION.                                            
162900                                                                          
163000     MOVE    SPACE                  TO WS-ASTERIX                         
163100     MOVE    OBKR-KDORDBEK          TO WS-KDORDBEK                        
163200     MOVE    KOD                    TO MOD-KDORDBEK(INDX)                 
163300     MOVE    OBKR-TIREGDAT          TO MOD-TIREGDAT(INDX)                 
163400     MOVE    OBKR-IDARTNR           TO WS-IDARTNR                         
163500                                       W-IDARTNR                          
163600     MOVE    OBKR-REKSIFFR          TO WS-REKNR                           
163700     MOVE    ARTIKEL-REKSIFFRA      TO MOD-IDARTNR(INDX)                  
163800     INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE            
163900     MOVE    OBKR-KVBEART           TO MOD-KVANTAL(INDX)                  
164000     MOVE    OBKR-IDDC              TO MOD-IDDC(INDX)                     
164100     IF OBKR-IDKUNDRF-RO NOT = '0000000   '                               
164200       MOVE  OBKR-IDKUNDRF-RO       TO WS-IDKUNDRF                        
164300     ELSE                                                                 
164400       MOVE  SPACE                  TO WS-IDKUNDRF                        
164500     END-IF                                                               
164600     IF BLANK-TECKEN = SPACE                                              
164700       MOVE  WS-IDORDNR5            TO MOD-IDORDNR7(INDX)                 
164800     ELSE                                                                 
164900       MOVE  WS-IDORDNR7            TO MOD-IDORDNR7(INDX)                 
165000     END-IF                                                               
165100     IF BASNR-IFYLLT OR KDORDBEK-IFYLLT                                   
165200       MOVE  OBKR-IDARTNR           TO W-IDARTNR-WDD301                   
165300     END-IF                                                               
165400     MOVE    OBKR-WDQ101            TO OBKR-SEG-SPAR                      
165500     PERFORM S01-BEART-TEXT                                               
165600     PERFORM IMS-GET-GU-SATB01                                            
165700     MOVE    +1                     TO TABELL-IX                          
165800     ADD     +1                     TO INDX                               
165900     PERFORM UNTIL SEGMENT-SAKNAS  OR                                     
166000                   TABELL-IX > 5                                          
166100       MOVE DAGENS-DATUM      TO TMP1-YYMMDD                              
166200       MOVE SATB-RAD-TISTADAT TO TMP2-YYMMDD                              
166300       MOVE SATB-RAD-TISTODAT TO TMP3-YYMMDD                              
166400       PERFORM WY2000Q1                                                   
166500       IF SATB-STR-IDARTNR      < 100000000    AND                        
166600          SATB-STR-TIBORT       = 0            AND                        
166700          TMP2-YYMMDD        NOT > TMP1-YYMMDD AND                        
166800          TMP3-YYMMDD        NOT < TMP1-YYMMDD                            
166900         MOVE SATB-STR-IDARTNR TO TAB-IDARTNR(TABELL-IX)                  
167000         ADD  +1               TO TABELL-IX                               
167100       END-IF                                                             
167200       PERFORM IMS-GET-GN-SATB01                                          
167300     END-PERFORM                                                          
167400*    COMPUTE TAB-INDX = MAX-INDX - INDX                                   
167500*    IF TAB-INDX < INDX                                                   
167600     COMPUTE TAB-INDX = (INDX - 1) + (TABELL-IX - 1)                      
167700     IF TAB-INDX > MAX-INDX                                               
167800       SUBTRACT 1             FROM INDX                                   
167900       MOVE     OBKR-SEG-SPAR TO   OBKR-WDQ101                            
168000       PERFORM  GC-SPARA-NEXT-NYCKLAR                                     
168100       PERFORM  MFS-RENSA-FAELT-PA-RAD                                    
168200       MOVE     +15           TO   INDX                                   
168300     ELSE                                                                 
168400       MOVE +1 TO TABELL-IX                                               
168500       PERFORM UNTIL TAB-IDARTNR(TABELL-IX) = ZERO OR                     
168600                                 TABELL-IX  > 4                           
168700                  OR INDX > MAX-INDX                                      
168800        IF MFS-SPLIT                                                      
168900         MOVE OBKR-BERADREF             TO MOD-BEART(INDX)                
169000        ELSE                                                              
169100         MOVE    TAB-IDARTNR(TABELL-IX) TO WS-BEART-NUM                   
169200         MOVE    WS-BEART-NUM           TO MOD-BEART(INDX)                
169300         INSPECT MOD-BEART(INDX) REPLACING LEADING ZERO BY SPACE          
169400        END-IF                                                            
169500        ADD     +1                     TO TABELL-IX                       
169600        ADD     +1                     TO INDX                            
169700       END-PERFORM                                                        
169800       MOVE     ZERO TO   WS-BEART-NUM                                    
169900       SUBTRACT 1    FROM INDX                                            
170000     END-IF                                                               
170100     .                                                                    
170200     EJECT                                                                
170300 GBH-LAES-VISA-KOD-58-TILL-59 SECTION.                                    
170400                                                                          
170500     MOVE SPACE                 TO WS-ASTERIX                             
170600     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
170700     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
170800     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
170900     IF OBKR-IDARTNR-TILLK > ZERO                                         
171000        MOVE OBKR-IDARTNR-TILLK TO WS-IDARTNR                             
171100        MOVE OBKR-REKSIFFR-TILLK TO WS-REKNR                              
171200     ELSE                                                                 
171300        MOVE OBKR-IDARTNR       TO WS-IDARTNR                             
171400        MOVE OBKR-REKSIFFR      TO WS-REKNR                               
171500     END-IF                                                               
171600     MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                          
171700     INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE            
171800     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
171900     MOVE OBKR-KVBEART          TO MOD-KVANTAL(INDX)                      
172000     IF OBKR-IDKUNDRF-RO NOT = '0000000   '                               
172100       MOVE OBKR-IDKUNDRF-RO TO WS-IDKUNDRF                               
172200     ELSE                                                                 
172300       MOVE SPACE            TO WS-IDKUNDRF                               
172400     END-IF                                                               
172500     IF BLANK-TECKEN = SPACE                                              
172600       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
172700     ELSE                                                                 
172800       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
172900     END-IF                                                               
173000     PERFORM S01-BEART-TEXT                                               
173100     .                                                                    
173200                                                                          
173300     EJECT                                                                
173400 GBI-LAES-VISA-KOD-61-TILL-65 SECTION.                                    
173500                                                                          
173600     IF OBKR-IDARTNR-TILLK = ZERO AND OBKR-BEERS = SPACE                  
173700       MOVE OBKR-KDORDBEK       TO WS-KDORDBEK                            
173800                                                                          
173900       MOVE INDX                TO SPAR-INDX                              
174000       MOVE OBKR-IDORDER        TO SPAR-IDORDER-NEXT                      
174100       MOVE OBKR-IDARTNR        TO SPAR-IDARTNR-NEXT                      
174200       MOVE OBKR-IDLOPNR        TO SPAR-IDLOPNR-NEXT                      
174300       MOVE OBKR-IDSEKVNR       TO SPAR-IDSEKVNR-NEXT                     
174400       MOVE OBKR-IDDC           TO SPAR-IDDC-NEXT                         
174500       MOVE OBKR-KDORDBEK       TO SPAR-KDORDBEK-NEXT                     
174600                                                                          
174700       MOVE ASTERIX             TO WS-ASTERIX                             
174800       MOVE KOD                 TO MOD-KDORDBEK(INDX)                     
174900       MOVE OBKR-TIREGDAT       TO MOD-TIREGDAT(INDX)                     
175000       MOVE OBKR-IDARTNR        TO WS-IDARTNR                             
175100       MOVE OBKR-REKSIFFR       TO WS-REKNR                               
175200       MOVE ARTIKEL-REKSIFFRA   TO MOD-IDARTNR(INDX)                      
175300       INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE          
175400       MOVE OBKR-KVBEART        TO MOD-KVANTAL(INDX)                      
175500       MOVE OBKR-IDDC           TO MOD-IDDC(INDX)                         
175600       IF OBKR-IDKUNDRF-RO NOT = '0000000   '                             
175700         MOVE OBKR-IDKUNDRF-RO TO WS-IDKUNDRF                             
175800       ELSE                                                               
175900         MOVE SPACE            TO WS-IDKUNDRF                             
176000       END-IF                                                             
176100       IF BLANK-TECKEN = SPACE                                            
176200         MOVE WS-IDORDNR5       TO MOD-IDORDNR7(INDX)                     
176300       ELSE                                                               
176400         MOVE WS-IDORDNR7       TO MOD-IDORDNR7(INDX)                     
176500       END-IF                                                             
176600       MOVE MFS-RENSA-FAELT     TO MOD-TITPO(INDX)                        
176700       IF BASNR-IFYLLT OR KDORDBEK-IFYLLT                                 
176800         MOVE OBKR-IDARTNR      TO W-IDARTNR-WDD301                       
176900       END-IF                                                             
177000       MOVE OBKR-WDQ101         TO OBKR-SEG-SPAR                          
177100       PERFORM S01-BEART-TEXT                                             
177200       MOVE OBKR-SEG-SPAR       TO OBKR-WDQ101                            
177300       MOVE '  '                TO STATUS-WS                              
177400     ELSE                                                                 
177500       IF OBKR-BEERS NOT = SPACE                                          
177600         IF MFS-SPLIT                                                     
177700           MOVE OBKR-BERADREF   TO MOD-BEART(INDX)                        
177800         ELSE                                                             
177900           MOVE OBKR-BEERS      TO MOD-BEART(INDX)                        
178000         END-IF                                                           
178100       ELSE                                                               
178200         PERFORM GBIA-LAES-VISA-ERSAETTN-KOD-61                           
178300       END-IF                                                             
178400     END-IF                                                               
178500     .                                                                    
178600                                                                          
178700     EJECT                                                                
178800 GBIA-LAES-VISA-ERSAETTN-KOD-61 SECTION.                                  
178900                                                                          
179000     MOVE ASTERIX               TO WS-ASTERIX                             
179100     MOVE SPACE                 TO WS-KDORDBEK                            
179200     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
179300     MOVE OBKR-IDARTNR-TILLK TO WS-IDARTNR                                
179400     MOVE OBKR-REKSIFFR-TILLK TO WS-REKNR                                 
179500     IF MFS-SPLIT                                                         
179600       MOVE OBKR-BERADREF       TO MOD-BEART(INDX)                        
179700     ELSE                                                                 
179800       MOVE ARTIKEL-REKSIFFRA   TO MOD-BEART(INDX)                        
179900       INSPECT MOD-BEART(INDX) REPLACING                                  
180000                   LEADING ZERO BY SPACE                                  
180100     END-IF                                                               
180200     MOVE OBKR-IDDC TO MOD-IDDC(INDX)                                     
180300     MOVE OBKR-KVBEART-TILLK TO MOD-KVANTAL(INDX)                         
180400     MOVE OBKR-DIERS-KVOT TO MOD-KVQPACK(INDX)                            
180500     .                                                                    
180600                                                                          
180700     EJECT                                                                
180800                                                                          
180900 GBJ-LAES-VISA-KOD-70-TILL-71 SECTION.                                    
181000*    ÄVEN KOD 77 TVINGANDE TILLÄGG                                        
181100                                                                          
181200     MOVE SPACE                 TO WS-ASTERIX                             
181300     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
181400     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
181500     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
181600     IF OBKR-IDARTNR-TILLK > ZERO                                         
181700        MOVE OBKR-IDARTNR-TILLK TO WS-IDARTNR                             
181800        MOVE OBKR-REKSIFFR-TILLK TO WS-REKNR                              
181900     ELSE                                                                 
182000        MOVE OBKR-IDARTNR       TO WS-IDARTNR                             
182100        MOVE OBKR-REKSIFFR      TO WS-REKNR                               
182200     END-IF                                                               
182300     MOVE ARTIKEL-REKSIFFRA     TO MOD-IDARTNR(INDX)                      
182400     INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE            
182500     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
182600     MOVE OBKR-KVBEART-Q        TO MOD-KVANTAL(INDX)                      
182700     MOVE MFS-RENSA-FAELT       TO MOD-KVQPACK(INDX)                      
182800                                                                          
182900     IF OHUV-BEKUNDRF(1:2) = 'OC'                                         
183000*    LITE FIX FÖR ATT FÅ RÄTT ORDERREFERENS VID                           
183100*    TVINGANDE TILLÄGG FRÅN VIPS                                          
183200                                                                          
183300        MOVE OHUV-BEKUNDRF(4:7) TO WS-IDKUNDRF                            
183400     ELSE                                                                 
183500        IF OBKR-IDKUNDRF-RO NOT = '0000000   '                            
183600          MOVE OBKR-IDKUNDRF-RO TO WS-IDKUNDRF                            
183700        ELSE                                                              
183800          MOVE SPACE            TO WS-IDKUNDRF                            
183900        END-IF                                                            
184000     END-IF                                                               
184100     IF BLANK-TECKEN = SPACE                                              
184200       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
184300     ELSE                                                                 
184400       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
184500     END-IF                                                               
184600     IF BASNR-IFYLLT OR KDORDBEK-IFYLLT                                   
184700        IF OBKR-IDARTNR-TILLK > ZERO                                      
184800          MOVE OBKR-IDARTNR-TILLK                                         
184900                                TO W-IDARTNR-WDD301                       
185000        ELSE                                                              
185100          MOVE OBKR-IDARTNR     TO W-IDARTNR-WDD301                       
185200        END-IF                                                            
185300     END-IF                                                               
185400     PERFORM S01-BEART-TEXT                                               
185500     IF OBKR-TITPO > +0                                                   
185600        MOVE OBKR-TITPO         TO MOD-TITPO(INDX)                        
185700        INSPECT MOD-TITPO(INDX) REPLACING LEADING ZERO BY SPACE           
185800     ELSE                                                                 
185900        MOVE MFS-RENSA-FAELT    TO MOD-TITPO(INDX)                        
186000     END-IF                                                               
186100     .                                                                    
186200     EJECT                                                                
186300 GBK-LAES-VISA-KOD-72-TILL-76 SECTION.                                    
186400                                                                          
186500     MOVE SPACE                 TO WS-ASTERIX                             
186600     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
186700     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
186800     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
186900     IF OBKR-IDARTNR-TILLK > ZERO                                         
187000       MOVE OBKR-IDARTNR-TILLK TO WS-IDARTNR                              
187100       MOVE OBKR-REKSIFFR-TILLK TO WS-REKNR                               
187200       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
187300       INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE          
187400     ELSE                                                                 
187500       MOVE OBKR-IDARTNR        TO WS-IDARTNR                             
187600       MOVE OBKR-REKSIFFR       TO WS-REKNR                               
187700       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
187800       INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE          
187900     END-IF                                                               
188000     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
188100     IF OBKR-KDORDBEK = 74                                                
188200        MOVE OBKR-KVBEART-Q     TO MOD-KVANTAL(INDX)                      
188300     ELSE                                                                 
188400        MOVE OBKR-KVBEART       TO MOD-KVANTAL(INDX)                      
188500     END-IF                                                               
188600     IF OBKR-IDKUNDRF-RO NOT = '0000000   '                               
188700       MOVE OBKR-IDKUNDRF-RO TO WS-IDKUNDRF                               
188800     ELSE                                                                 
188900       MOVE SPACE            TO WS-IDKUNDRF                               
189000     END-IF                                                               
189100     IF BLANK-TECKEN = SPACE                                              
189200       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
189300     ELSE                                                                 
189400       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
189500     END-IF                                                               
189600*    IF OBKR-KDORDBEK = 75 OR 76                                          
189700        IF OBKR-KDTPOTYP = ZERO                                           
189800           MOVE MFS-RENSA-FAELT TO MOD-TITPO(INDX)                        
189900        ELSE                                                              
190000           MOVE OBKR-TITPO      TO MOD-TITPO(INDX)                        
190100        END-IF                                                            
190200*    END-IF                                                               
190300     IF BASNR-IFYLLT OR KDORDBEK-IFYLLT                                   
190400        IF OBKR-IDARTNR-TILLK > ZERO                                      
190500          MOVE OBKR-IDARTNR-TILLK                                         
190600                                TO W-IDARTNR-WDD301                       
190700        ELSE                                                              
190800          MOVE OBKR-IDARTNR     TO W-IDARTNR-WDD301                       
190900        END-IF                                                            
191000     END-IF                                                               
191100     PERFORM S01-BEART-TEXT                                               
191200     .                                                                    
191300                                                                          
191400     EJECT                                                                
191500 GBL-LAES-VISA-KOD-80-TILL-83 SECTION.                                    
191600                                                                          
191700     MOVE SPACE                 TO WS-ASTERIX                             
191800     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
191900     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
192000     IF WS-KDORDBEK = '00'                                                
192100       MOVE '87'                TO MOD-KDORDBEK(INDX)                     
192200     END-IF                                                               
192300     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
192400     IF OBKR-IDARTNR-TILLK > ZERO                                         
192500       MOVE OBKR-IDARTNR-TILLK TO WS-IDARTNR                              
192600       MOVE OBKR-REKSIFFR-TILLK TO WS-REKNR                               
192700       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
192800       INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE          
192900     ELSE                                                                 
193000       MOVE OBKR-IDARTNR        TO WS-IDARTNR                             
193100       MOVE OBKR-REKSIFFR       TO WS-REKNR                               
193200       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
193300       INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE          
193400     END-IF                                                               
193500     IF OBKR-KDORDBEK = 82                                                
193600       MOVE OBKR-KVBEART        TO MOD-KVANTAL(INDX)                      
193700     ELSE                                                                 
193800       MOVE OBKR-KVANNANT       TO MOD-KVANTAL(INDX)                      
193900     END-IF                                                               
194000     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
194100     IF OBKR-IDKUNDRF-RO NOT = '0000000   '                               
194200       MOVE OBKR-IDKUNDRF-RO TO WS-IDKUNDRF                               
194300     ELSE                                                                 
194400       MOVE SPACE            TO WS-IDKUNDRF                               
194500     END-IF                                                               
194600     IF BLANK-TECKEN = SPACE                                              
194700       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
194800     ELSE                                                                 
194900       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
195000     END-IF                                                               
195100     IF BASNR-IFYLLT OR KDORDBEK-IFYLLT                                   
195200        IF OBKR-IDARTNR-TILLK > ZERO                                      
195300          MOVE OBKR-IDARTNR-TILLK                                         
195400                                TO W-IDARTNR-WDD301                       
195500        ELSE                                                              
195600          MOVE OBKR-IDARTNR     TO W-IDARTNR-WDD301                       
195700        END-IF                                                            
195800     END-IF                                                               
195900     PERFORM S01-BEART-TEXT                                               
196000     .                                                                    
196100                                                                          
196200     EJECT                                                                
196300 GBM-LAES-VISA-KOD-90-TILL-93 SECTION.                                    
196400                                                                          
196500     MOVE SPACE                 TO WS-ASTERIX                             
196600     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
196700     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
196800     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
196900     IF OBKR-IDARTNR-TILLK > ZERO                                         
197000       MOVE OBKR-IDARTNR-TILLK TO WS-IDARTNR                              
197100                                  W-IDARTNR-WDK6                          
197200       MOVE OBKR-REKSIFFR-TILLK TO WS-REKNR                               
197300       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
197400       INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE          
197500     ELSE                                                                 
197600       MOVE OBKR-IDARTNR        TO WS-IDARTNR                             
197700                                   W-IDARTNR-WDK6                         
197800       MOVE OBKR-REKSIFFR       TO WS-REKNR                               
197900       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
198000       INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE          
198100     END-IF                                                               
198200     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
198300     IF OBKR-KDORDBEK = 92 OR 98                                          
198400       MOVE OBKR-KVPRERO        TO MOD-KVANTAL(INDX)                      
198500     ELSE                                                                 
198600        IF OBKR-KDORDBEK = 93                                             
198700           MOVE OBKR-KVANNANT   TO MOD-KVANTAL(INDX)                      
198800        ELSE                                                              
198900            MOVE OBKR-KVRO      TO MOD-KVANTAL(INDX)                      
199000        END-IF                                                            
199100     END-IF                                                               
199200     IF OBKR-IDKUNDRF-RO NOT = '0000000   '                               
199300       MOVE OBKR-IDKUNDRF-RO TO WS-IDKUNDRF                               
199400     ELSE                                                                 
199500       MOVE SPACE            TO WS-IDKUNDRF                               
199600     END-IF                                                               
199700     IF BLANK-TECKEN = SPACE                                              
199800       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
199900     ELSE                                                                 
200000       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
200100     END-IF                                                               
200200     IF BASNR-IFYLLT OR KDORDBEK-IFYLLT                                   
200300        IF OBKR-IDARTNR-TILLK > ZERO                                      
200400          MOVE OBKR-IDARTNR-TILLK                                         
200500                                TO W-IDARTNR-WDD301                       
200600        ELSE                                                              
200700          MOVE OBKR-IDARTNR     TO W-IDARTNR-WDD301                       
200800        END-IF                                                            
200900     END-IF                                                               
201000                                                                          
201100     IF OBKR-KDORDBEK = 90 OR 91                                          
201200        IF DCS-IDDC NOT = OBKR-IDDC                                       
201300           MOVE OBKR-IDDC            TO W-IDDC-B6                         
201400           PERFORM IMS-GU-WDB601                                          
201500        END-IF                                                            
201700        IF DCS-NDC                                                        
201800           PERFORM GBMA-HAEMTA-TIBERANK                                   
201900                                                                          
202000           IF ETA-SVAR-OK = JA                                            
202100             IF DCS-NDC-NA                                                
202200               MOVE ETA-TIAAMMDD-SVAR TO MOD-TITPO(INDX)                  
202300             ELSE                                                         
202400               IF ETA-KVAVIS-ETA > +0                                     
202500                 MOVE ETA-TIAAMMDD-SVAR TO MOD-TITPO(INDX)                
202600               ELSE                                                       
202700                 MOVE MFS-RENSA-FAELT TO MOD-TITPO(INDX)                  
202800               END-IF                                                     
202900             END-IF                                                       
203000           ELSE                                                           
203100              MOVE MFS-RENSA-FAELT   TO MOD-TITPO(INDX)                   
203200           END-IF                                                         
203300        ELSE                                                              
203400           PERFORM IMS-GET-GU-ARTC11                                      
203500                                                                          
203600           IF SEGMENT-FINNS                                               
203700              MOVE CLAG-TIDISPIN     TO MOD-TITPO(INDX)                   
203800           ELSE                                                           
203900              MOVE MFS-RENSA-FAELT   TO MOD-TITPO(INDX)                   
204000           END-IF                                                         
204100        END-IF                                                            
204200     ELSE                                                                 
204300       MOVE MFS-RENSA-FAELT          TO MOD-TITPO(INDX)                   
204400     END-IF                                                               
204500                                                                          
204600     PERFORM S01-BEART-TEXT                                               
204700     .                                                                    
204800                                                                          
204900     EJECT                                                                
205000 GBMA-HAEMTA-TIBERANK SECTION.                                            
205100                                                                          
205200     MOVE '612'                TO ETA-KDCALL                              
205300     MOVE OBKR-IDDC            TO ETA-IDDC-REC                            
205400     MOVE W-IDARTNR-WDK6       TO ETA-IDARTNR                             
205500     MOVE SPACE                TO ETA-IDLEVNR                             
205600     MOVE OBKR-KDFRAKT         TO ETA-KDFRAKT                             
205700     MOVE OBKR-TIREGDAT        TO ETA-TIAAMMDD-ANROP                      
205800                                  WS-ETA-DATUM                            
205900     IF WS-ETA-DATUM-AAR > 50                                             
206000        MOVE 19                TO ETA-TISEKEL-ANROP                       
206100     ELSE                                                                 
206200        MOVE 20                TO ETA-TISEKEL-ANROP                       
206300     END-IF                                                               
206400                                                                          
206500     CALL W218ETA  USING ETA-W218LETA                                     
206600                         ETA-ARTC-PCB ETA-WDK7-PCB                        
206700                         ETA-INLC-PCB ETA-LEVA-PCB                        
206800                         ETA-WDB6-PCB ETA-WDD9-PCB                        
206900     .                                                                    
207000                                                                          
207100     EJECT                                                                
207200 GBN-LAES-VISA-KOD-95-TILL-97 SECTION.                                    
207300                                                                          
207400     MOVE SPACE                 TO WS-ASTERIX                             
207500     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
207600     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
207700     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
207800     IF OBKR-IDARTNR-TILLK > ZERO                                         
207900       MOVE OBKR-IDARTNR-TILLK TO WS-IDARTNR                              
208000       MOVE OBKR-REKSIFFR-TILLK TO WS-REKNR                               
208100       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
208200       INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE          
208300     ELSE                                                                 
208400       MOVE OBKR-IDARTNR        TO WS-IDARTNR                             
208500       MOVE OBKR-REKSIFFR       TO WS-REKNR                               
208600       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
208700       INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE          
208800     END-IF                                                               
208900     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
209000     MOVE OBKR-KVBEART-Q        TO MOD-KVANTAL(INDX)                      
209100     IF OBKR-IDKUNDRF-RO NOT = '0000000   '                               
209200       MOVE OBKR-IDKUNDRF-RO TO WS-IDKUNDRF                               
209300     ELSE                                                                 
209400       MOVE SPACE            TO WS-IDKUNDRF                               
209500     END-IF                                                               
209600     IF BLANK-TECKEN = SPACE                                              
209700       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
209800     ELSE                                                                 
209900       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
210000     END-IF                                                               
210100     IF BASNR-IFYLLT OR KDORDBEK-IFYLLT                                   
210200        IF OBKR-IDARTNR-TILLK > ZERO                                      
210300          MOVE OBKR-IDARTNR-TILLK                                         
210400                                TO W-IDARTNR-WDD301                       
210500        ELSE                                                              
210600          MOVE OBKR-IDARTNR     TO W-IDARTNR-WDD301                       
210700        END-IF                                                            
210800     END-IF                                                               
210900     PERFORM S01-BEART-TEXT                                               
211000     IF OBKR-KDORDBEK = 96                                                
211100       MOVE OBKR-TIDISPIN       TO MOD-TITPO(INDX)                        
211200     END-IF                                                               
211300     IF OBKR-KDORDBEK = 97                                                
211400       MOVE OBKR-TIREPDAT       TO MOD-TITPO(INDX)                        
211500     END-IF                                                               
211600     .                                                                    
211700                                                                          
211800     EJECT                                                                
211900 GBO-LAES-VISA-KOD-98 SECTION.                                            
212000                                                                          
212100     MOVE SPACE                 TO WS-ASTERIX                             
212200     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
212300     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
212400     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
212500     MOVE OBKR-IDARTNR          TO WS-IDARTNR                             
212600     MOVE OBKR-REKSIFFR         TO WS-REKNR                               
212700     MOVE ARTIKEL-REKSIFFRA     TO MOD-IDARTNR(INDX)                      
212800       INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE          
212900     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
213000     MOVE OBKR-KVBEART          TO MOD-KVANTAL(INDX)                      
213100     IF OBKR-IDKUNDRF-RO NOT = '0000000   '                               
213200       MOVE OBKR-IDKUNDRF-RO TO WS-IDKUNDRF                               
213300     ELSE                                                                 
213400       MOVE SPACE            TO WS-IDKUNDRF                               
213500     END-IF                                                               
213600     IF BLANK-TECKEN = SPACE                                              
213700       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
213800     ELSE                                                                 
213900       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
214000     END-IF                                                               
214100     IF BASNR-IFYLLT OR KDORDBEK-IFYLLT                                   
214200       MOVE OBKR-IDARTNR        TO W-IDARTNR-WDD301                       
214300     END-IF                                                               
214400     PERFORM S01-BEART-TEXT                                               
214500     .                                                                    
214600                                                                          
214700     EJECT                                                                
214800 GBP-LAES-VISA-KOD-99 SECTION.                                            
214900                                                                          
215000     MOVE SPACE                 TO WS-ASTERIX                             
215100     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
215200     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
215300     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
215400     IF OBKR-IDARTNR-TILLK > ZERO                                         
215500       MOVE OBKR-IDARTNR-TILLK TO WS-IDARTNR                              
215600       MOVE OBKR-REKSIFFR-TILLK TO WS-REKNR                               
215700       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
215800       INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE          
215900     ELSE                                                                 
216000       MOVE OBKR-IDARTNR        TO WS-IDARTNR                             
216100       MOVE OBKR-REKSIFFR       TO WS-REKNR                               
216200       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
216300       INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE          
216400     END-IF                                                               
216500     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
216600     MOVE OBKR-KVPRERO          TO MOD-KVANTAL(INDX)                      
216700     IF OBKR-IDKUNDRF-RO NOT = '0000000   '                               
216800       MOVE OBKR-IDKUNDRF-RO TO WS-IDKUNDRF                               
216900     ELSE                                                                 
217000       MOVE SPACE            TO WS-IDKUNDRF                               
217100     END-IF                                                               
217200     IF BLANK-TECKEN = SPACE                                              
217300       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
217400     ELSE                                                                 
217500       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
217600     END-IF                                                               
217700     IF BASNR-IFYLLT OR KDORDBEK-IFYLLT                                   
217800        IF OBKR-IDARTNR-TILLK > ZERO                                      
217900          MOVE OBKR-IDARTNR-TILLK                                         
218000                                TO W-IDARTNR-WDD301                       
218100        ELSE                                                              
218200          MOVE OBKR-IDARTNR     TO W-IDARTNR-WDD301                       
218300        END-IF                                                            
218400     END-IF                                                               
218500     PERFORM S01-BEART-TEXT                                               
218600     .                                                                    
218700                                                                          
218800     EJECT                                                                
218900 GBQ-LAES-VISA-KOD-20-TILL-22 SECTION.                                    
219000                                                                          
219100     MOVE SPACE                 TO WS-ASTERIX                             
219200     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
219300     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
219400     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
219500     IF OBKR-IDARTNR-TILLK > ZERO                                         
219600        MOVE OBKR-IDARTNR-TILLK TO WS-IDARTNR                             
219700        MOVE OBKR-REKSIFFR-TILLK TO WS-REKNR                              
219800     ELSE                                                                 
219900        MOVE OBKR-IDARTNR       TO WS-IDARTNR                             
220000        MOVE OBKR-REKSIFFR      TO WS-REKNR                               
220100     END-IF                                                               
220200     MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                          
220300     INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE            
220400     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
220500     IF OBKR-KDORDBEK = 20                                                
220600       MOVE OBKR-KVANNANT       TO MOD-KVANTAL(INDX)                      
220700     ELSE                                                                 
220800       MOVE OBKR-KVBEART        TO MOD-KVANTAL(INDX)                      
220900     END-IF                                                               
221000     IF OBKR-IDKUNDRF-RO NOT = '0000000   '                               
221100       MOVE OBKR-IDKUNDRF-RO TO WS-IDKUNDRF                               
221200     ELSE                                                                 
221300       MOVE SPACE            TO WS-IDKUNDRF                               
221400     END-IF                                                               
221500     IF BLANK-TECKEN = SPACE                                              
221600       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
221700     ELSE                                                                 
221800       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
221900     END-IF                                                               
222000     IF BASNR-IFYLLT OR KDORDBEK-IFYLLT                                   
222100        IF OBKR-IDARTNR-TILLK > ZERO                                      
222200          MOVE OBKR-IDARTNR-TILLK                                         
222300                                TO W-IDARTNR-WDD301                       
222400        ELSE                                                              
222500          MOVE OBKR-IDARTNR     TO W-IDARTNR-WDD301                       
222600        END-IF                                                            
222700     END-IF                                                               
222800     PERFORM S01-BEART-TEXT                                               
222900     .                                                                    
223000                                                                          
223100     EJECT                                                                
223200 GBR-LAES-VISA-KOD-84 SECTION.                                            
223300                                                                          
223400     MOVE SPACE                 TO WS-ASTERIX                             
223500     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
223600     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
223700     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
223800     IF OBKR-IDARTNR-TILLK > ZERO                                         
223900       MOVE OBKR-IDARTNR-TILLK  TO WS-IDARTNR                             
224000       MOVE OBKR-REKSIFFR-TILLK TO WS-REKNR                               
224100       MOVE ARTIKEL-REKSIFFRA   TO MOD-IDARTNR(INDX)                      
224200       INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE          
224300     ELSE                                                                 
224400       MOVE OBKR-IDARTNR        TO WS-IDARTNR                             
224500       MOVE OBKR-REKSIFFR       TO WS-REKNR                               
224600       MOVE ARTIKEL-REKSIFFRA   TO MOD-IDARTNR(INDX)                      
224700       INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE          
224800     END-IF                                                               
224900     MOVE OBKR-KVBEART          TO MOD-KVANTAL(INDX)                      
225000     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
225100     IF OBKR-IDKUNDRF-RO NOT = '0000000   '                               
225200       MOVE OBKR-IDKUNDRF-RO TO WS-IDKUNDRF                               
225300     ELSE                                                                 
225400       MOVE SPACE            TO WS-IDKUNDRF                               
225500     END-IF                                                               
225600     IF BLANK-TECKEN = SPACE                                              
225700       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
225800     ELSE                                                                 
225900       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
226000     END-IF                                                               
226100     IF BASNR-IFYLLT OR KDORDBEK-IFYLLT                                   
226200        IF OBKR-IDARTNR-TILLK > ZERO                                      
226300          MOVE OBKR-IDARTNR-TILLK                                         
226400                                TO W-IDARTNR-WDD301                       
226500        ELSE                                                              
226600          MOVE OBKR-IDARTNR     TO W-IDARTNR-WDD301                       
226700        END-IF                                                            
226800     END-IF                                                               
226900     PERFORM S01-BEART-TEXT                                               
227000     .                                                                    
227100                                                                          
227200     EJECT                                                                
227300 GC-SPARA-NEXT-NYCKLAR SECTION.                                           
227400                                                                          
227500     MOVE OBKR-IDORDER            TO MOD-IDORDER-NEXT                     
227600     MOVE OBKR-IDARTNR            TO MOD-IDARTNR-NEXT                     
227700     MOVE OBKR-IDLOPNR            TO MOD-IDLOPNR-NEXT                     
227800     MOVE OBKR-IDSEKVNR           TO MOD-IDSEKVNR-NEXT                    
227900     MOVE OBKR-IDDC               TO MOD-IDDC-NEXT                        
228000     MOVE OBKR-KDORDBEK           TO MOD-KDORDBEK-NEXT                    
228100     MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                            
228200     CALL WMEDKONV USING MED-WMEDAREA                                     
228300     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
228400     .                                                                    
228500                                                                          
228600     EJECT                                                                
228700 H-LAES-HUVUD                            SECTION.                         
228800                                                                          
228900     IF PROFORMA                                                          
229000        PERFORM IMS-GET-GU-PROC01                                         
229100     ELSE                                                                 
229200        PERFORM IMS-GET-GU-ORQI01                                         
229300     END-IF                                                               
229400                                                                          
229500     IF SEGMENT-FINNS                                                     
229600       MOVE JA                           TO FL-HUVUD-FINNS                
229700                                                                          
229800       IF PROFORMA                                                        
229900         IF PHUV-BEVARREF = 'W480      '                                  
230000           MOVE NEJ                      TO FL-HUVUD-FINNS                
230100         END-IF                                                           
230200       ELSE                                                               
230300         IF OHUV-BEVARREF = 'W480      '                                  
230400           MOVE NEJ                      TO FL-HUVUD-FINNS                
230500         END-IF                                                           
230600       END-IF                                                             
230700     ELSE                                                                 
230800       MOVE NEJ                          TO FL-HUVUD-FINNS                
230900     END-IF                                                               
231000     .                                                                    
231100     EJECT                                                                
231200 S01-BEART-TEXT                          SECTION.                         
231300     IF MSG-SIGNON-USERID(1:5) = 'PC361'                                  
231400     OR MSG-SIGNON-USERID = 'PC06879'                                     
231500     OR                     'PC42337'                                     
231600     OR                     'PC56243'                                     
231700     OR                     'PC42727'                                     
231800     OR                     'PC33574'                                     
231900     OR                     'PC51848'                                     
232000     OR                     'PC65926'                                     
232100     OR                     'PCCQ305'                                     
232200     OR                     'PCCS032'                                     
232300     OR                     'PCCG915'                                     
232300     OR                     'PCCG975'                                     
232400     OR                     'PCCV363'                                     
232410     OR                     'PCCW991'                                     
232420     OR                     'PCCQ089'                                     
232500       IF MFS-SPLIT                                                       
232600         MOVE OBKR-BERADREF     TO MOD-BEART(INDX)                        
232700       ELSE                                                               
232800         MOVE OBKR-IDPGM        TO MOD-BEART(INDX)                        
232900       END-IF                                                             
233000     ELSE                                                                 
233100       IF MFS-SPLIT                                                       
233200         MOVE OBKR-BERADREF     TO MOD-BEART(INDX)                        
233300       ELSE                                                               
233400         PERFORM IMS-GU-BENA-BENA11                                       
233500         IF SEGMENT-FINNS                                                 
233600           MOVE TEXT-BEART      TO MOD-BEART(INDX)                        
233700         ELSE                                                             
233800           MOVE MFS-RENSA-FAELT TO MOD-BEART(INDX)                        
233900         END-IF                                                           
234000       END-IF                                                             
234100     END-IF                                                               
234200     .                                                                    
234300     EJECT                                                                
234400 MFS-RENSA-FAELT-UT SECTION.                                              
234500                                                                          
234600*    --- ALLA UTDATA-FÄLT                                                 
234700     MOVE +1 TO INDX                                                      
234800     PERFORM UNTIL INDX > MAX-INDX                                        
234900       MOVE MFS-RENSA-FAELT TO MOD-KDORDBEK(INDX)                         
235000                               MOD-TIREGDAT(INDX)                         
235100                               MOD-IDARTNR(INDX)                          
235200                               MOD-BEART(INDX)                            
235300                               MOD-KVANTAL(INDX)                          
235400                               MOD-KVQPACK(INDX)                          
235500                               MOD-IDDC(INDX)                             
235600                               MOD-IDORDNR7(INDX)                         
235700                               MOD-TITPO(INDX)                            
235800       ADD +1 TO INDX                                                     
235900     END-PERFORM                                                          
236000     .                                                                    
236100     SKIP2                                                                
236200 MFS-RENSA-FAELT-PA-RAD SECTION.                                          
236300                                                                          
236400*    --- UTDATA-FÄLT PÅ SKÄRMEN                                           
236500     MOVE MFS-RENSA-FAELT TO MOD-KDORDBEK(INDX)                           
236600                             MOD-TIREGDAT(INDX)                           
236700                             MOD-IDARTNR(INDX)                            
236800                             MOD-BEART(INDX)                              
236900                             MOD-KVANTAL(INDX)                            
237000                             MOD-KVQPACK(INDX)                            
237100                             MOD-IDDC(INDX)                               
237200                             MOD-IDORDNR7(INDX)                           
237300                             MOD-TITPO(INDX)                              
237400     .                                                                    
237500     EJECT                                                                
237600 IMS-GET-MSG SECTION.                                                     
237700                                                                          
237800     MOVE '  QC' TO GODK-STATUSKODER                                      
237900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
238000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
238100     PERFORM IMS-STATUSKONTROLL                                           
238200     .                                                                    
238300     SKIP3                                                                
238400 IMS-INSERT-MSG SECTION.                                                  
238500                                                                          
238600     IF ENGLISH-TEXT                                                      
238700       IF MFS-SPLIT                                                       
238800          MOVE 'LINE-REF.'   TO MOD-VARHEAD                               
238900       ELSE                                                               
239000          MOVE 'DESCRIPTION' TO MOD-VARHEAD                               
239100       END-IF                                                             
239200       MOVE 'N' TO MFS-KDHUVOMR                                           
239300     ELSE                                                                 
239400       IF MFS-SPLIT                                                       
239500          MOVE 'RAD-REF. '   TO MOD-VARHEAD                               
239600       ELSE                                                               
239700          MOVE 'BENÄMNING'   TO MOD-VARHEAD                               
239800       END-IF                                                             
239900     END-IF                                                               
240000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
240100     MOVE SPACE TO GODK-STATUSKODER                                       
240200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
240300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
240400     PERFORM IMS-STATUSKONTROLL                                           
240500     .                                                                    
240600     EJECT                                                                
240700 IMS-GET-GU-ORQI01 SECTION.                                               
240800                                                                          
240900     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
241000          DELIMITED BY SIZE INTO SSA1                                     
241100     MOVE '  GE' TO GODK-STATUSKODER                                      
241200     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-ORQI01 SSA1                    
241300     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
241400     PERFORM IMS-STATUSKONTROLL                                           
241500     .                                                                    
241600                                                                          
241700 IMS-GET-GU-PROC01 SECTION.                                               
241800                                                                          
241900     STRING 'WLPROC01(WDE801KY =' W-IDGMTREF-X ')'                        
242000          DELIMITED BY SIZE INTO SSA1                                     
242100     MOVE '  GE' TO GODK-STATUSKODER                                      
242200     CALL CBLTDLI USING GU PROC-PCB DLI-IO-PROC01 SSA1                    
242300     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
242400     PERFORM IMS-STATUSKONTROLL                                           
242500     .                                                                    
242600                                                                          
242700 IMS-GET-ORQM01-BAS SECTION.                                              
242800                                                                          
242900     IF WS1-IDDC = ZERO                                                   
243000        STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-BAS-MIN-X                 
243100                       '&WDQ101KY<=' W-WDQ101KY-BAS-MAX-X                 
243200                       '&IDSYSTEMNE' W-IDSYSTEM-X ')'                     
243300          DELIMITED BY SIZE INTO SSA1                                     
243400     ELSE                                                                 
243500        STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-BAS-MIN-X                 
243600                       '&WDQ101KY<=' W-WDQ101KY-BAS-MAX-X                 
243700                       '&IDDC    >=' W-IDDC-MIN-MIN-X                     
243800                       '&IDDC    <=' W-IDDC-MIN-MAX-X                     
243900                       '&IDSYSTEMNE' W-IDSYSTEM-X ')'                     
244000          DELIMITED BY SIZE INTO SSA1                                     
244100     END-IF                                                               
244200     MOVE '  GBGE' TO GODK-STATUSKODER                                    
244300     CALL CBLTDLI USING GN ORQM-PCB DLI-IO-ORQM01 SSA1                    
244400     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
244500     PERFORM IMS-STATUSKONTROLL                                           
244600     .                                                                    
244700                                                                          
244800 IMS-GET-ORQM01-BAS-PROF SECTION.                                         
244900                                                                          
245000     IF WS1-IDDC = ZERO                                                   
245100        STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-BAS-MIN-X                 
245200                       '&WDQ101KY<=' W-WDQ101KY-BAS-MAX-X                 
245300                       '&IDSYSTEM =' W-IDSYSTEM-X ')'                     
245400          DELIMITED BY SIZE INTO SSA1                                     
245500     ELSE                                                                 
245600        STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-BAS-MIN-X                 
245700                       '&WDQ101KY<=' W-WDQ101KY-BAS-MAX-X                 
245800                       '&IDDC    >=' W-IDDC-MIN-MIN-X                     
245900                       '&IDDC    <=' W-IDDC-MIN-MAX-X                     
246000                       '&IDSYSTEM =' W-IDSYSTEM-X ')'                     
246100          DELIMITED BY SIZE INTO SSA1                                     
246200     END-IF                                                               
246300     MOVE '  GBGE' TO GODK-STATUSKODER                                    
246400     CALL CBLTDLI USING GN ORQM-PCB DLI-IO-ORQM01 SSA1                    
246500     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
246600     PERFORM IMS-STATUSKONTROLL                                           
246700     .                                                                    
246800                                                                          
246900 IMS-GET-ORQM01-41  SECTION.                                              
247000                                                                          
247100     IF WS1-IDDC = ZERO                                                   
247200        STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-41-MIN-X                  
247300                       '&WDQ101KY<=' W-WDQ101KY-41-MAX-X                  
247400                       '&IDSYSTEMNE' W-IDSYSTEM-X ')'                     
247500             DELIMITED BY SIZE INTO SSA1                                  
247600     ELSE                                                                 
247700        STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-41-MIN-X                  
247800                       '&WDQ101KY<=' W-WDQ101KY-41-MAX-X                  
247900                       '&IDDC     =' W-IDDC-41-MIN-X                      
248000                       '&IDDC     =' W-IDDC-41-MAX-X                      
248100                       '&IDSYSTEMNE' W-IDSYSTEM-X ')'                     
248200             DELIMITED BY SIZE INTO SSA1                                  
248300     END-IF                                                               
248400     MOVE '  GBGE' TO GODK-STATUSKODER                                    
248500     CALL CBLTDLI USING GN ORQM-PCB DLI-IO-ORQM01 SSA1                    
248600     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
248700     PERFORM IMS-STATUSKONTROLL                                           
248800     .                                                                    
248900                                                                          
249000 IMS-GET-ORQM01-41-PROF  SECTION.                                         
249100                                                                          
249200     IF WS1-IDDC = ZERO                                                   
249300        STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-41-MIN-X                  
249400                       '&WDQ101KY<=' W-WDQ101KY-41-MAX-X                  
249500                       '&IDSYSTEM =' W-IDSYSTEM-X ')'                     
249600             DELIMITED BY SIZE INTO SSA1                                  
249700     ELSE                                                                 
249800        STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-41-MIN-X                  
249900                       '&WDQ101KY<=' W-WDQ101KY-41-MAX-X                  
250000                       '&IDDC     =' W-IDDC-41-MIN-X                      
250100                       '&IDDC     =' W-IDDC-41-MAX-X                      
250200                       '&IDSYSTEM =' W-IDSYSTEM-X ')'                     
250300             DELIMITED BY SIZE INTO SSA1                                  
250400     END-IF                                                               
250500     MOVE '  GBGE' TO GODK-STATUSKODER                                    
250600     CALL CBLTDLI USING GN ORQM-PCB DLI-IO-ORQM01 SSA1                    
250700     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
250800     PERFORM IMS-STATUSKONTROLL                                           
250900     .                                                                    
251000                                                                          
251100 IMS-GET-ORQM01-MAX SECTION.                                              
251200                                                                          
251300     IF WS1-IDDC = ZERO                                                   
251400        STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MAX-MIN-X                 
251500                       '&WDQ101KY<=' W-WDQ101KY-MAX-MAX-X                 
251600                       '&KDORDBEK =' W-KDORDBEK-MAX-MIN-X                 
251700                       '&KDORDBEK =' W-KDORDBEK-MAX-MAX-X                 
251800                       '&IDSYSTEMNE' W-IDSYSTEM-X ')'                     
251900             DELIMITED BY SIZE INTO SSA1                                  
252000     ELSE                                                                 
252100        STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MAX-MIN-X                 
252200                       '&WDQ101KY<=' W-WDQ101KY-MAX-MAX-X                 
252300                       '&IDDC     =' W-IDDC-MAX-MIN-X                     
252400                       '&KDORDBEK =' W-KDORDBEK-MAX-MIN-X                 
252500                       '&IDDC     =' W-IDDC-MAX-MAX-X                     
252600                       '&KDORDBEK =' W-KDORDBEK-MAX-MAX-X                 
252700                       '&IDSYSTEMNE' W-IDSYSTEM-X ')'                     
252800             DELIMITED BY SIZE INTO SSA1                                  
252900     END-IF                                                               
253000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
253100     CALL CBLTDLI USING GN ORQM-PCB DLI-IO-ORQM01 SSA1                    
253200     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
253300     PERFORM IMS-STATUSKONTROLL                                           
253400     .                                                                    
253500     EJECT                                                                
253600 IMS-GET-ORQM01-MAX-PROF SECTION.                                         
253700                                                                          
253800     IF WS1-IDDC = ZERO                                                   
253900        STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MAX-MIN-X                 
254000                       '&WDQ101KY<=' W-WDQ101KY-MAX-MAX-X                 
254100                       '&KDORDBEK =' W-KDORDBEK-MAX-MIN-X                 
254200                       '&KDORDBEK =' W-KDORDBEK-MAX-MAX-X                 
254300                       '&IDSYSTEM =' W-IDSYSTEM-X ')'                     
254400             DELIMITED BY SIZE INTO SSA1                                  
254500     ELSE                                                                 
254600        STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MAX-MIN-X                 
254700                       '&WDQ101KY<=' W-WDQ101KY-MAX-MAX-X                 
254800                       '&IDDC     =' W-IDDC-MAX-MIN-X                     
254900                       '&KDORDBEK =' W-KDORDBEK-MAX-MIN-X                 
255000                       '&IDDC     =' W-IDDC-MAX-MAX-X                     
255100                       '&KDORDBEK =' W-KDORDBEK-MAX-MAX-X                 
255200                       '&IDSYSTEM =' W-IDSYSTEM-X ')'                     
255300             DELIMITED BY SIZE INTO SSA1                                  
255400     END-IF                                                               
255500     MOVE '  GBGE' TO GODK-STATUSKODER                                    
255600     CALL CBLTDLI USING GN ORQM-PCB DLI-IO-ORQM01 SSA1                    
255700     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
255800     PERFORM IMS-STATUSKONTROLL                                           
255900     .                                                                    
256000     EJECT                                                                
256100 IMS-GET-ORQM01-MIN SECTION.                                              
256200                                                                          
256300     IF WS1-IDDC = ZERO                                                   
256400        STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-MIN-X                 
256500                       '&WDQ101KY<=' W-WDQ101KY-MIN-MAX-X                 
256600                       '&IDSYSTEMNE' W-IDSYSTEM-X ')'                     
256700             DELIMITED BY SIZE INTO SSA1                                  
256800     ELSE                                                                 
256900        STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-MIN-X                 
257000                       '&WDQ101KY<=' W-WDQ101KY-MIN-MAX-X                 
257100                       '&IDDC     =' W-IDDC-MIN-MIN-X                     
257200                       '&IDDC     =' W-IDDC-MIN-MAX-X                     
257300                       '&IDSYSTEMNE' W-IDSYSTEM-X ')'                     
257400             DELIMITED BY SIZE INTO SSA1                                  
257500     END-IF                                                               
257600     MOVE '  GBGE' TO GODK-STATUSKODER                                    
257700     CALL CBLTDLI USING GN ORQM-PCB DLI-IO-ORQM01 SSA1                    
257800     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
257900     PERFORM IMS-STATUSKONTROLL                                           
258000     .                                                                    
258100     EJECT                                                                
258200                                                                          
258300 IMS-GET-ORQM01-MIN-PROF SECTION.                                         
258400                                                                          
258500     IF WS1-IDDC = ZERO                                                   
258600        STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-MIN-X                 
258700                       '&WDQ101KY<=' W-WDQ101KY-MIN-MAX-X                 
258800                       '&IDSYSTEM =' W-IDSYSTEM-X ')'                     
258900             DELIMITED BY SIZE INTO SSA1                                  
259000     ELSE                                                                 
259100        STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-MIN-X                 
259200                       '&WDQ101KY<=' W-WDQ101KY-MIN-MAX-X                 
259300                       '&IDDC     =' W-IDDC-MIN-MIN-X                     
259400                       '&IDDC     =' W-IDDC-MIN-MAX-X                     
259500                       '&IDSYSTEM =' W-IDSYSTEM-X ')'                     
259600             DELIMITED BY SIZE INTO SSA1                                  
259700     END-IF                                                               
259800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
259900     CALL CBLTDLI USING GN ORQM-PCB DLI-IO-ORQM01 SSA1                    
260000     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
260100     PERFORM IMS-STATUSKONTROLL                                           
260200     .                                                                    
260300     EJECT                                                                
260400                                                                          
260500 IMS-GU-BENA-BENA11 SECTION.                                              
260600                                                                          
260700     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
260800          DELIMITED BY SIZE INTO SSA1                                     
260900     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
261000          DELIMITED BY SIZE INTO SSA2                                     
261100     MOVE '  GE' TO GODK-STATUSKODER                                      
261200     CALL CBLTDLI USING GU BENA-PCB DLI-IO-BENA11 SSA1 SSA2               
261300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
261400     PERFORM IMS-STATUSKONTROLL                                           
261500     .                                                                    
261600     EJECT                                                                
261700 IMS-GET-GU-SATB01 SECTION.                                               
261800                                                                          
261900     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
262000          DELIMITED BY SIZE INTO SSA1                                     
262100     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
262200          DELIMITED BY SIZE INTO SSA2                                     
262300     MOVE '  GE' TO GODK-STATUSKODER                                      
262400     CALL CBLTDLI USING GU SATB-PCB DLI-IO-SATB11 SSA1 SSA2               
262500     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
262600     PERFORM IMS-STATUSKONTROLL                                           
262700     .                                                                    
262800                                                                          
262900 IMS-GET-GN-SATB01 SECTION.                                               
263000                                                                          
263100     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
263200          DELIMITED BY SIZE INTO SSA1                                     
263300     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
263400          DELIMITED BY SIZE INTO SSA2                                     
263500     MOVE '  GE' TO GODK-STATUSKODER                                      
263600     CALL CBLTDLI USING GN SATB-PCB DLI-IO-SATB11 SSA1 SSA2               
263700     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
263800     PERFORM IMS-STATUSKONTROLL                                           
263900     .                                                                    
264000     EJECT                                                                
264100 IMS-GET-GU-ARTC11 SECTION.                                               
264200                                                                          
264300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-WDK6-X ')'                    
264400          DELIMITED BY SIZE INTO SSA1                                     
264500     MOVE 'WLARTC11 ' TO SSA2                                             
264600     MOVE '  GE' TO GODK-STATUSKODER                                      
264700     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-ARTC11 SSA1 SSA2               
264800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
264900     PERFORM IMS-STATUSKONTROLL                                           
265000     .                                                                    
265100                                                                          
265200 IMS-GU-WDB601    SECTION.                                                
265300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
265400          DELIMITED BY SIZE INTO SSA1                                     
265500     MOVE '  GE' TO GODK-STATUSKODER                                      
265600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
265700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
265800     PERFORM IMS-STATUSKONTROLL                                           
265900     IF SEGMENT-SAKNAS                                                    
266000         MOVE SPACE TO DCS-KDDC                                           
266100     END-IF                                                               
266200     .                                                                    
266300 IMS-STATUSKONTROLL SECTION.                                              
266400                                                                          
266500     SET STATUS-IX TO 1                                                   
266600     SEARCH GODK-STATUS                                                   
266700       AT END CALL FELLOG                                                 
266800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
266900     END-SEARCH                                                           
267000     .                                                                    
267100*    -COPY WY2000Q1                                                       
267200     EJECT                                                                
