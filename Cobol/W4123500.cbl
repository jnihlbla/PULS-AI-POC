000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4123500.                                                
000400 AUTHOR.         GERRY CARMICHAEL                                         
000500 DATE-WRITTEN.   91/07/03.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        LÄSER EN FIL INNEHÅLLANDE POSTER FRÅN WDQ1                       
001100*        VILKA SKA ORDERBEKRÄFTAS OCH SKRIVER UT DEM                      
001200*        PÅ LASER PRINTER CENTRALT OCH PÅ FICHE.                          
001300*                                                                         
001400*        PROGRAMMET LÄSER   WLORQI  (WDQ2)                                
001500*        PROGRAMMET LÄSER   WLPROC  (WDE8)                                
001600*        PROGRAMMET LÄSER   WLBENA  (WDD3)                                
001700*        PROGRAMMET LÄSER   WL4732  (WDG6)                                
001800*        PROGRAMMET LÄSER   WLSATB  (WDJ1)                                
001900*                                                                         
002000                                                                          
002100 ENVIRONMENT DIVISION.                                                    
002200 INPUT-OUTPUT SECTION.                                                    
002300*                                                                         
002400 FILE-CONTROL.                                                            
002500*                                                                         
002600*          --- INFIL PRINTFIL ORDERBEKRÄFTELSER                           
002700     SELECT W41234               ASSIGN TO W41235D1.                      
002800*                                                                         
002900*          --- UTFIL LASER PRINTER PV                                     
003000     SELECT W4123501             ASSIGN TO W41235D2.                      
003100     SELECT W4123502             ASSIGN TO W41235D3.                      
003200     SELECT W4123503             ASSIGN TO W41235D4.                      
003210     SELECT W4123504             ASSIGN TO W41235D5.                      
003300*                                                                         
003400 DATA DIVISION.                                                           
003500 SKIP2                                                                    
003600 FILE SECTION.                                                            
003700 SKIP2                                                                    
003800 FD  W41234                                                               
003900     LABEL RECORD STANDARD                                                
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  IN34-POST  -COPY W412034  -L                                         
004400 SKIP2                                                                    
004500 FD  W4123501                                                             
004600     LABEL RECORD STANDARD                                                
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000 01  W4123501-RAD.                                                        
005100   03   RAD-501-SKIP             PIC X(1).                                
005200   03   RAD-W4123501             PIC X(80).                               
005300 SKIP2                                                                    
005400 FD  W4123502                                                             
005500     LABEL RECORD STANDARD                                                
005600     RECORDING       F                                                    
005700     BLOCK CONTAINS  0.                                                   
005800                                                                          
005900 01  W4123502-RAD.                                                        
006000   03   RAD-502-SKIP             PIC X(1).                                
006100   03   RAD-W4123502             PIC X(80).                               
006200 SKIP2                                                                    
006300 FD  W4123503                                                             
006400     LABEL RECORD STANDARD                                                
006500     RECORDING       F                                                    
006600     BLOCK CONTAINS  0.                                                   
006700                                                                          
006800 01  W4123503-RAD.                                                        
006900   03   RAD-503-SKIP             PIC X(1).                                
007000   03   RAD-W4123503             PIC X(80).                               
007100 EJECT                                                                    
007101                                                                          
007110 FD  W4123504                                                             
007120     LABEL RECORD STANDARD                                                
007130     RECORDING       F                                                    
007140     BLOCK CONTAINS  0.                                                   
007150                                                                          
007160 01  W4123504-RAD.                                                        
007170   03   RAD-504-SKIP             PIC X(1).                                
007180   03   RAD-W4123504             PIC X(80).                               
007190 EJECT                                                                    
007200 WORKING-STORAGE SECTION.                                                 
007300*    -COPY WY2000W1                                                       
007400     SKIP3                                                                
007500 77  STYR                        PIC 9(1)    VALUE ZERO.                  
007600 77  IDPGM                       PIC X(08)   VALUE 'W4123500'.            
007700 77  W-KDFRAKT                   PIC S9(3)   COMP-3 VALUE ZERO.           
007800                                                                          
007900 77  JA                          PIC X       VALUE 'J'.                   
008000 77  NEJ                         PIC X       VALUE 'N'.                   
008100                                                                          
008200 77  RADNR                       PIC S9(9)  VALUE +0    COMP SYNC.        
008300 77  RADNR-MAX                   PIC S9(9)  VALUE +68   COMP SYNC.        
008400 77  SIDNR                       PIC S9(9)  VALUE +0    COMP SYNC.        
008500                                                                          
008600 77  W41234-EOF-SW               PIC X       VALUE 'N'.                   
008700     88  END-OF-W41234                       VALUE 'J'.                   
008800                                                                          
008900 77  ARB-SW                      PIC X       VALUE 'N'.                   
009000     88  ARBTABELL-FUNNEN                    VALUE 'J'.                   
009100                                                                          
009200 77  ORDER-RENSAD-SW             PIC X       VALUE 'N'.                   
009300     88  ORDER-RENSAD                        VALUE 'J'.                   
009400                                                                          
009500 77  WS-INDX-SATS                PIC S9(9)   VALUE +0  COMP SYNC.         
009600 77  WS-INDX-SATS-MAX            PIC S9(9)   VALUE +5  COMP SYNC.         
009700                                                                          
009800                                                                          
009900 01  WS-SATS-TABELL.                                                      
010000     03  WS-IDARTNR-SATS         PIC S9(9)   COMP-3 OCCURS 5.             
010100                                                                          
010200 01  WS-IDKUNDRF                 PIC X(10).                               
010300 01  FILLER REDEFINES WS-IDKUNDRF.                                        
010400     03  WS-IDORDNR5             PIC 9(5).                                
010500     03  WS-IDKUNDRF-6--10       PIC X(5).                                
010600 01  FILLER REDEFINES WS-IDKUNDRF.                                        
010700     03  WS-IDORDNR7             PIC 9(7).                                
010800     03  FILLER                  PIC X(3).                                
010900     EJECT                                                                
011000                                                                          
011100 01  WS-PV-FTG-NAMN              PIC X(18)                                
011200     VALUE 'CAR PARTS         '.                                          
011300                                                                          
011400*            *** FRANSK BENÄMNING PÅ ORDERBEKRÄFTELSE ***                 
011500 01  FRANSK-BENAEMNING.                                                   
011600     05  FILLER          PIC X(15)   VALUE ' CONFIRMATION D'.             
011700     05  FILLER          PIC X       VALUE QUOTE.                         
011800     05  FILLER          PIC X(7)    VALUE 'ORDRE'.                       
011900     SKIP2                                                                
012000*            *** ITALIENSK BENÄMNING PÅ ORDERBEKRÄFTELSE ***              
012100 01  ITALIENSK-BENAEMNING.                                                
012200     05  FILLER          PIC X(15)   VALUE ' CONFIRMATION D'.             
012300     05  FILLER          PIC X       VALUE QUOTE.                         
012400     05  FILLER          PIC X(7)    VALUE 'ORDRE'.                       
012500     EJECT                                                                
012600 01  IN-ORDER-ID                 PIC X(11).                               
012700 01 FILLER REDEFINES IN-ORDER-ID.                                         
012800     03  IN-IDDISTR              PIC S9(5)  COMP-3.                       
012900     03  IN-IDKUNDNR             PIC S9(7)  COMP-3.                       
013000     03  IN-IDORDER              PIC S9(7)  COMP-3.                       
013100                                                                          
013200 01  GAMMAL-ORDER-ID             PIC X(11).                               
013300 01 FILLER REDEFINES GAMMAL-ORDER-ID.                                     
013400     03  GAMMAL-IDDISTR          PIC S9(5)  COMP-3.                       
013500     03  GAMMAL-IDKUNDNR         PIC S9(7)  COMP-3.                       
013600     03  GAMMAL-IDORDER          PIC S9(7)  COMP-3.                       
013700                                                                          
013800 01  CURRENT-DATE.                                                        
013900     03  CURRENT-YY              PIC 9(2).                                
014000     03  CURRENT-MM              PIC 9(2).                                
014100     03  CURRENT-DD              PIC 9(2).                                
014200 01  DAGENS-DATUM REDEFINES CURRENT-DATE   PIC 9(6).                      
014300                                                                          
014400 01  SPRAK-IX                    PIC 9.                                   
014500                                                                          
014600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
014700 01  GENERELLA-SUBPROGRAM.                                                
014800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
015100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
015200 SKIP2                                                                    
015300*    --- PARAMETRAR TILL POSTSUM                                          
015400*                                                                         
015500*01  -COPY W0005    -PRE  POSTSUM-                                        
015600*                                                                         
015700     EJECT                                                                
015800                                                                          
015900 01  IN34-AREA-START             PIC X(24)   VALUE                        
016000                                 'IN34-AREA-START '.                      
016100     SKIP2                                                                
016200*                                                                         
016300*01  AREA -COPY W412034     -PRE IN34-                                    
016400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016500*                                                                         
016600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016700                                                                          
016800 01  NYCKLAR-TILL-DLI.                                                    
016900                                                                          
017000                                                                          
017100*----> DIREKTNYCKEL TILL ORDERHUVUDET  WDQ2                               
017200                                                                          
017300     03  W-IDORDER-X.                                                     
017400         05  W-IDORDER           PIC S9(7) COMP-3.                        
017500                                                                          
017600     03  W-IDDC-X.                                                        
017700         05  W-IDDC              PIC X(2).                                
017800                                                                          
017900*----> DIREKTNYCKEL TILL PROFORMAHUVUDET  WDE8                            
018000                                                                          
018100     03  W-IDGMTREF-X.                                                    
018200         05  W-IDDISTR           PIC S9(5) COMP-3.                        
018300         05  W-IDKUNDNR          PIC S9(7) COMP-3.                        
018400         05  W-IDKUNDRF          PIC X(10) VALUE SPACE.                   
018500                                                                          
018600*----> DIREKTNYCKEL TILL WDD301                                           
018700                                                                          
018800     03  W-IDARTNR-X.                                                     
018900         05  W-IDARTNR           PIC S9(9) COMP-3.                        
019000                                                                          
019100*----> DIREKTNYCKEL TILL WDD311                                           
019200                                                                          
019300     03  W-IDSKYLT-X.                                                     
019400         05  W-IDSKYLT           PIC  X(3).                               
019500                                                                          
019600*----> FRAKTTEXT 4732                                                     
019700                                                                          
019800     03  W-4732-IDHTYP-X.                                                 
019900         05  W-4732-IDHTYP       PIC  X(4)  VALUE '4732'.                 
020000         05  W-4732-KDFRAKT      PIC S9(3)  COMP-3.                       
020100         05  W-4732-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
020200                                                                          
020300*----> SATSREGISTRET                                                      
020400                                                                          
020500     03  W-WDJ1CSEQ-X.                                                    
020600         05  W-IDLEVNR-J1        PIC  X(5)  VALUE SPACE.                  
020700         05  W-BELEVART-J1       PIC X(30)  VALUE SPACE.                  
020800         05  W-IDARTNR-J1        PIC S9(9)  VALUE +0 COMP-3.              
020900                                                                          
021000     03  W-IDLEVNR-X             PIC  X(5)  VALUE SPACE.                  
021100                                                                          
021200*    ___TEST AV DISTRIKT FÖR VAL AV MOTTAGARE AV OBEKR.                   
021300                                                                          
021400 01  TEST-IDDISTR                PIC S9(5)  COMP-3.                       
021500*01  FILLER -COPY  WWDIST66  -RED  TEST-IDDISTR                           
021600     SKIP2                                                                
021700                                                                          
021800*    --- STATUS-KOD FRÅN IMS                                              
021900                                                                          
022000 01  STATUS-WS                   PIC  X(02).                              
022100     88  SEGMENT-FINNS                       VALUE '  '.                  
022200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
022300     88  BASEN-SLUT                          VALUE 'GB'.                  
022400     SKIP2                                                                
022500 01  GODK-STATUSKODER.                                                    
022600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022700                                                                          
022800 01  SSA1                        PIC X(96).                               
022900 01  SSA2                        PIC X(96).                               
023000 01  SSA3                        PIC X(96).                               
023100     EJECT                                                                
023200                                                                          
023300***********************************                                       
023400*  PRINTRADER FÖR ORDERBEKRÄFTELSE*                                       
023500***********************************                                       
023600 01  W001-RAD.                                                            
023700*                                                                         
023800     03  FILLER                  PIC X(80)  VALUE SPACE.                  
023900                                                                          
024000 01  HRAD1.                                                               
024100     03   FILLER                  PIC X(5)  VALUE SPACE.                  
024200     03   HRAD1-LISTID            PIC X(12).                              
024300     03   HRAD1-FTG-NAMN          PIC X(18).                              
024400     03   HRAD1-RUBRIK            PIC X(23).                              
024500     03   FILLER                  PIC X(18) VALUE SPACE.                  
024600     03   HRAD1-IDSID             PIC ZZ9.                                
024700     03   FILLER                  PIC X(1)  VALUE SPACE.                  
024800                                                                          
024900 01  HRAD7.                                                               
025000     03   FILLER                  PIC X(7)  VALUE SPACE.                  
025100     03   HRAD7-IDDISTR           PIC Z(3)9.                              
025200     03   FILLER                  PIC X(45) VALUE SPACE.                  
025300     03   HRAD7-BEKUNDRF          PIC X(15).                              
025400     03   FILLER                  PIC X(9)  VALUE SPACE.                  
025500                                                                          
025600 01  HRAD8.                                                               
025700     03   FILLER                  PIC X(21) VALUE SPACE.                  
025800     03   HRAD8-BEGMT-RAD1        PIC X(27).                              
025900     03   FILLER                  PIC X(32) VALUE SPACE.                  
026000                                                                          
026100 01  HRAD9.                                                               
026200     03   FILLER                  PIC X(7)  VALUE SPACE.                  
026300     03   HRAD9-IDKUNDNR          PIC Z(5)9.                              
026400     03   FILLER                  PIC X(8)  VALUE SPACE.                  
026500     03   HRAD9-BEGMT-RAD2        PIC X(27).                              
026600     03   FILLER                  PIC X(8)  VALUE SPACE.                  
026700     03   HRAD9-TIREGDAT          PIC 9(6).                               
026800     03   FILLER                  PIC X(7)  VALUE SPACE.                  
026900     03   HRAD9-KDFRAKT           PIC Z(2).                               
027000     03   FILLER                  PIC X(9)  VALUE SPACE.                  
027100                                                                          
027200 01  HRAD10.                                                              
027300     03   FILLER                  PIC X(21) VALUE SPACE.                  
027400     03   HRAD10-ADGMT-GATA       PIC X(27).                              
027500     03   FILLER                  PIC X(32) VALUE SPACE.                  
027600                                                                          
027700 01  HRAD11.                                                              
027800     03   FILLER                  PIC X(7)  VALUE SPACE.                  
027900     03   HRAD11-IDORDNR7         PIC Z(6)9.                              
028000     03   FILLER                  PIC X(7)  VALUE SPACE.                  
028100     03   HRAD11-ADGMT-PADR       PIC X(27).                              
028200     03   FILLER                  PIC X(8)  VALUE SPACE.                  
028300     03   HRAD11-BEFRAKT          PIC X(20).                              
028400     03   FILLER                  PIC X(4)  VALUE SPACE.                  
028500                                                                          
028600 01  HRAD12.                                                              
028700     03   FILLER                  PIC X(21) VALUE SPACE.                  
028800     03   HRAD12-ADGMT-LAND       PIC X(27).                              
028900     03   FILLER                  PIC X(32) VALUE SPACE.                  
029000                                                                          
029100 01  HRAD13.                                                              
029200     03   FILLER                  PIC X(12) VALUE SPACE.                  
029300     03   HRAD13-KDORDKL          PIC X(1).                               
029400     03   FILLER                  PIC X(43) VALUE SPACE.                  
029500     03   HRAD13-TIAAMMDD         PIC 9(6).                               
029600     03   FILLER                  PIC X(18) VALUE SPACE.                  
029700                                                                          
029800 01  RAD.                                                                 
029900     03   FILLER                  PIC X(6)  VALUE SPACE.                  
030000     03   RAD-KDORDBEK            PIC Z(2).                               
030100     03   RAD-ASTERISK            PIC X(1)  VALUE SPACE.                  
030200     03   FILLER                  PIC X(2)  VALUE SPACE.                  
030300     03   RAD-TIORDREG            PIC 9(6).                               
030400     03   FILLER                  PIC X(2)  VALUE SPACE.                  
030500     03   RAD-IDARTNR             PIC Z(7)9.                              
030600     03   RAD-STRECK              PIC X(1).                               
030700     03   RAD-REKSIFFR            PIC 9(1).                               
030800     03   FILLER                  PIC X(2)  VALUE SPACE.                  
030900     03   RAD-BEART               PIC X(15).                              
031000     03   FILLER                  PIC X(2)  VALUE SPACE.                  
031100     03   RAD-KVBEART             PIC Z(5)9.                              
031200     03   FILLER                  PIC X(1)  VALUE SPACE.                  
031300     03   RAD-KVQPACK-1           PIC Z(4)9.                              
031400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
031500     03   RAD-IDDC                PIC X(2).                               
031600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
031700     03   RAD-IDORDNR             PIC Z(7).                               
031800     03   FILLER                  PIC X(2)  VALUE SPACE.                  
031900     03   RAD-TITPO               PIC 9(6).                               
032000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
032100                                                                          
032200     EJECT                                                                
032300*    --- IMS FUNKTIONSKODER                                               
032400*01  -COPY W0003                                                          
032500     EJECT                                                                
032600*    ---  DLI INPUT-OUTPUT AREA                                           
032700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
032800                                                                          
032900 01  DLI-IO-AREA2.                                                        
033000*        05 -COPY WDQ201                                                  
033100     EJECT                                                                
033200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
033300                                                                          
033400 01  DLI-IO-AREA1.                                                        
033500                                                                          
033600     03  IO-AREA1                PIC X(4000)  VALUE SPACE.                
033700                                                                          
033800     03  WLORQI12 REDEFINES IO-AREA1.                                     
033900*        05 -COPY WDQ212                                                  
034000     EJECT                                                                
034100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
034200                                                                          
034300 01  DLI-IO-AREA3.                                                        
034400     03  WLBENA11.                                                        
034500*        05 -COPY WDD311                                                  
034600     EJECT                                                                
034700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA4'.        
034800                                                                          
034900 01  DLI-IO-AREA4.                                                        
035000                                                                          
035100     03  IO-AREA4                PIC X(200)  VALUE SPACE.                 
035200                                                                          
035300     03  WL473201 REDEFINES IO-AREA4.                                     
035400*        05 -COPY WDGX473B                                                
035500     03  WL473201 REDEFINES IO-AREA4.                                     
035600*        05 -COPY WDGX4732                                                
035700     EJECT                                                                
035800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA5'.        
035900                                                                          
036000 01  DLI-IO-AREA5.                                                        
036100                                                                          
036200     03  IO-AREA5                PIC X(400)  VALUE SPACE.                 
036300                                                                          
036400     03  WL473201 REDEFINES IO-AREA5.                                     
036500*        05 -COPY WDJ111     -PRE SATB-                                   
036600*        05 -COPY WDJ101     -PRE SATB-                                   
036700     EJECT                                                                
036800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA6'.        
036900                                                                          
037000 01  DLI-IO-AREA6.                                                        
037100                                                                          
037200     03  IO-AREA6                PIC X(2000) VALUE SPACE.                 
037300                                                                          
037400     03  WLPROC01 REDEFINES IO-AREA6.                                     
037500*        05 -COPY WDE801                                                  
037600     EJECT                                                                
037700 LINKAGE SECTION.                                                         
037800                                                                          
037900*01  -COPY W0008      -PRE ORQI-                                          
038000     05  FILLER                  PIC X.                                   
038100     EJECT                                                                
038200*01  -COPY W0008      -PRE BENA-                                          
038300     05  FILLER                  PIC X.                                   
038400     EJECT                                                                
038500*01  -COPY W0008      -PRE 4732-                                          
038600     05  FILLER                  PIC X.                                   
038700     EJECT                                                                
038800*01  -COPY W0008      -PRE SATB-                                          
038900     05  FILLER                  PIC X.                                   
039000     EJECT                                                                
039100*01  -COPY W0008      -PRE PROC-                                          
039200     05  FILLER                  PIC X.                                   
039300     EJECT                                                                
039400 PROCEDURE DIVISION  USING ORQI-PCB                                       
039500                           BENA-PCB                                       
039600                           4732-PCB                                       
039700                           SATB-PCB                                       
039800                           PROC-PCB.                                      
039900                                                                          
040000     ENTRY 'DLITCBL' USING ORQI-PCB                                       
040100                           BENA-PCB                                       
040200                           4732-PCB                                       
040300                           SATB-PCB                                       
040400                           PROC-PCB.                                      
040500                                                                          
040600     PERFORM A-INIT                                                       
040700                                                                          
040800     PERFORM S01-READ-W41234                                              
040900                                                                          
041000     IF NOT END-OF-W41234                                                 
041100       MOVE IN34-IDDISTR     TO IN-IDDISTR                                
041200       MOVE IN34-IDKUNDNR    TO IN-IDKUNDNR                               
041300       MOVE IN34-IDORDER     TO IN-IDORDER                                
041400     END-IF                                                               
041500                                                                          
041600     PERFORM UNTIL END-OF-W41234                                          
041700                                                                          
041800       PERFORM B-NY-ORDER                                                 
041900                                                                          
042000       IF NOT ORDER-RENSAD                                                
042100                                                                          
042200         MOVE IN-ORDER-ID    TO GAMMAL-ORDER-ID                           
042300                                                                          
042400         PERFORM UNTIL END-OF-W41234 OR                                   
042500                       IN-ORDER-ID NOT = GAMMAL-ORDER-ID                  
042600                                                                          
042700           PERFORM C-SKRIV-RAD                                            
042800                                                                          
042900           PERFORM S01-READ-W41234                                        
043000                                                                          
043100           MOVE IN34-IDDISTR TO IN-IDDISTR                                
043200           MOVE IN34-IDKUNDNR TO IN-IDKUNDNR                              
043300           MOVE IN34-IDORDER TO IN-IDORDER                                
043400                                                                          
043500         END-PERFORM                                                      
043600                                                                          
043700       ELSE                                                               
043800                                                                          
043900         PERFORM S01-READ-W41234                                          
044000                                                                          
044100       END-IF                                                             
044200                                                                          
044300     END-PERFORM                                                          
044400                                                                          
044500     PERFORM Z-FINIT                                                      
044600                                                                          
044700     MOVE ZERO TO RETURN-CODE                                             
044800     GOBACK                                                               
044900     .                                                                    
045000     EJECT                                                                
045100 A-INIT SECTION.                                                          
045200     OPEN INPUT  W41234                                                   
045300     OPEN OUTPUT W4123501                                                 
045400                 W4123502                                                 
045500                 W4123503                                                 
045510                 W4123504                                                 
045600                                                                          
045700                                                                          
045800     MOVE NEJ TO W41234-EOF-SW                                            
045900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
046000     ACCEPT CURRENT-DATE  FROM DATE                                       
046100     MOVE ZERO              TO GAMMAL-IDDISTR                             
046200                               GAMMAL-IDKUNDNR                            
046300                               GAMMAL-IDORDER                             
046400     .                                                                    
046500     EJECT                                                                
046600                                                                          
046700 B-NY-ORDER SECTION.                                                      
046800                                                                          
046900     PERFORM BA-LAES-OHUV                                                 
047000                                                                          
047100     IF NOT ORDER-RENSAD                                                  
047200                                                                          
047300       PERFORM BC-REDIGERA-HUVUD                                          
047400                                                                          
047500     END-IF                                                               
047600     .                                                                    
047700     EJECT                                                                
047800                                                                          
047900 BA-LAES-OHUV       SECTION.                                              
048000     MOVE NEJ                      TO ARB-SW                              
048100     MOVE NEJ                      TO ORDER-RENSAD-SW                     
048200     IF IN34-IDPTYP = '002'                                               
048300        MOVE IN34-IDDISTR          TO W-IDDISTR                           
048400        MOVE IN34-IDKUNDNR         TO W-IDKUNDNR                          
048500        MOVE IN34-IDKUNDRF         TO W-IDKUNDRF                          
048600        PERFORM IMS-GU-WDE8                                               
048700        IF SEGMENT-SAKNAS                                                 
048800          MOVE JA                  TO ORDER-RENSAD-SW                     
048900        ELSE                                                              
049000          CONTINUE                                                        
049100        END-IF                                                            
049200     ELSE                                                                 
049300        MOVE IN34-IDORDER          TO W-IDORDER                           
049400        PERFORM IMS-GU-WDQ201                                             
049500        IF SEGMENT-SAKNAS                                                 
049600          MOVE JA                  TO ORDER-RENSAD-SW                     
049700        ELSE                                                              
049800          IF OHUV-IDDC-TVS > ZERO                                         
049900            MOVE OHUV-IDDC-TVS TO W-IDDC                                  
050000          ELSE                                                            
050100            MOVE OHUV-IDDC-PRIM     TO W-IDDC                             
050200          END-IF                                                          
050300          PERFORM IMS-GNP-WDQ212                                          
050400          IF SEGMENT-FINNS                                                
050500            MOVE JA                TO ARB-SW                              
050600          END-IF                                                          
050700        END-IF                                                            
050800     END-IF                                                               
050900     .                                                                    
051000     EJECT                                                                
051100                                                                          
051200 BC-REDIGERA-HUVUD SECTION.                                               
051300     MOVE 100                TO RADNR                                     
051400     MOVE ZERO               TO SIDNR                                     
051500     IF IN34-IDPTYP = '002'                                               
051600        IF PHUV-IDSKYLT = 'S  '                                           
051700             MOVE '  PROFORMABEKRÄFTELSE  ' TO HRAD1-RUBRIK               
051800             MOVE 1                         TO SPRAK-IX                   
051900        ELSE                                                              
052000             MOVE 'PROFORMA CONFIRMATION  ' TO HRAD1-RUBRIK               
052100             MOVE 2                         TO SPRAK-IX                   
052200        END-IF                                                            
052300     ELSE                                                                 
052400        IF OHUV-IDSKYLT = 'S  '                                           
052500          MOVE '  ORDERBEKRÄFTELSE     ' TO HRAD1-RUBRIK                  
052600          MOVE 1                         TO SPRAK-IX                      
052700        ELSE                                                              
052800          IF OHUV-IDSKYLT = 'D  '                                         
052900            MOVE '  AUFTRAGSBESTÄTIGUNG  ' TO HRAD1-RUBRIK                
053000            MOVE 5                         TO SPRAK-IX                    
053100          ELSE                                                            
053200            IF OHUV-IDSKYLT = 'E  '                                       
053300              MOVE 'CONFIRMACION DE PEDIDO ' TO HRAD1-RUBRIK              
053400              MOVE 4                         TO SPRAK-IX                  
053500            ELSE                                                          
053600              IF OHUV-IDSKYLT = 'F  '                                     
053700                MOVE FRANSK-BENAEMNING         TO HRAD1-RUBRIK            
053800                MOVE 3                         TO SPRAK-IX                
053900              ELSE                                                        
054000                IF OHUV-IDSKYLT = 'I  '                                   
054100                  MOVE ITALIENSK-BENAEMNING      TO HRAD1-RUBRIK          
054200                  MOVE 6                         TO SPRAK-IX              
054300                ELSE                                                      
054400                  MOVE '  ORDER CONFIRMATION   ' TO HRAD1-RUBRIK          
054500                  MOVE 2                         TO SPRAK-IX              
054600                END-IF                                                    
054700              END-IF                                                      
054800            END-IF                                                        
054900          END-IF                                                          
055000        END-IF                                                            
055100     END-IF                                                               
055200                                                                          
055300     MOVE IN34-IDDISTR              TO HRAD7-IDDISTR                      
055400     MOVE IN34-BEKUNDRF             TO HRAD7-BEKUNDRF                     
055500                                                                          
055600     IF IN34-IDPTYP = '002'                                               
055700        MOVE PHUV-BEBETRAD-1        TO HRAD8-BEGMT-RAD1                   
055800     ELSE                                                                 
055900        MOVE OHUV-BEGMT-RAD1        TO HRAD8-BEGMT-RAD1                   
056000     END-IF                                                               
056100                                                                          
056200     MOVE IN34-IDKUNDNR             TO HRAD9-IDKUNDNR                     
056300                                                                          
056400     IF IN34-IDPTYP = '002'                                               
056500        MOVE PHUV-BEBETRAD-2        TO HRAD9-BEGMT-RAD2                   
056600     ELSE                                                                 
056700        MOVE OHUV-BEGMT-RAD2        TO HRAD9-BEGMT-RAD2                   
056800     END-IF                                                               
056900                                                                          
057000     MOVE IN34-TIREGDAT             TO HRAD9-TIREGDAT                     
057100                                                                          
057200     IF IN34-IDPTYP = '001'                                               
057300       IF ARBTABELL-FUNNEN                                                
057400         IF ARB-KDFRAKT > ZERO                                            
057500           MOVE ARB-KDFRAKT         TO HRAD9-KDFRAKT                      
057600                                       W-KDFRAKT                          
057700           PERFORM S04-BEFRAKT                                            
057800         END-IF                                                           
057900       END-IF                                                             
058000     ELSE                                                                 
058100       IF PHUV-KDFRAKT > ZERO                                             
058200         MOVE PHUV-KDFRAKT          TO HRAD9-KDFRAKT                      
058300                                       W-KDFRAKT                          
058400         PERFORM S04-BEFRAKT                                              
058500       END-IF                                                             
058600     END-IF                                                               
058700                                                                          
058800     IF IN34-IDPTYP = '002'                                               
058900        MOVE PHUV-ADBETRAD-1        TO HRAD10-ADGMT-GATA                  
059000     ELSE                                                                 
059100        MOVE OHUV-ADGMT-GATA        TO HRAD10-ADGMT-GATA                  
059200     END-IF                                                               
059300                                                                          
059400     MOVE IN34-IDKUNDRF (1:7)       TO HRAD11-IDORDNR7                    
059500     IF IN34-IDPTYP = '002'                                               
059600        MOVE PHUV-ADBETRAD-2        TO HRAD11-ADGMT-PADR                  
059700     ELSE                                                                 
059800        MOVE OHUV-ADGMT-PADR        TO HRAD11-ADGMT-PADR                  
059900     END-IF                                                               
060000                                                                          
060100     IF IN34-IDPTYP = '002'                                               
060200        MOVE PHUV-ADBETRAD-3        TO HRAD12-ADGMT-LAND                  
060300     ELSE                                                                 
060400        MOVE OHUV-ADGMT-LAND        TO HRAD12-ADGMT-LAND                  
060500     END-IF                                                               
060600                                                                          
060700     MOVE IN34-KDORDKL              TO HRAD13-KDORDKL                     
060800                                                                          
060900     IF IN34-IDPTYP = '001'                                               
061000       IF ARBTABELL-FUNNEN                                                
061100         MOVE ARB-DATRPAVD (3:6)    TO HRAD13-TIAAMMDD                    
061200         IF ARB-DATRPAVD (3:6) = ZERO                                     
061300          INSPECT HRAD13-TIAAMMDD REPLACING LEADING ZERO BY SPACE         
061400         END-IF                                                           
061500       END-IF                                                             
061600     ELSE                                                                 
061700       MOVE ZERO                    TO HRAD13-TIAAMMDD                    
061800       INSPECT HRAD13-TIAAMMDD REPLACING LEADING ZERO BY SPACE            
061900     END-IF                                                               
062000     .                                                                    
062100     EJECT                                                                
062200                                                                          
062300 C-SKRIV-RAD SECTION.                                                     
062400                                                                          
062500     MOVE SPACE                    TO RAD                                 
062600     MOVE '-'                      TO RAD-STRECK                          
062700     MOVE IN34-KDORDBEK            TO RAD-KDORDBEK                        
062800     IF IN34-TIORDREG > ZERO                                              
062900       MOVE IN34-TIORDREG          TO RAD-TIORDREG                        
063000     END-IF                                                               
063100     IF IN34-IDPTYP = '002'                                               
063200        MOVE PHUV-IDSKYLT          TO W-IDSKYLT-X                         
063300     ELSE                                                                 
063400        MOVE OHUV-IDSKYLT          TO W-IDSKYLT-X                         
063500     END-IF                                                               
063600                                                                          
063700     PERFORM CA-REDIGERA-PER-OBKR-KOD                                     
063800     .                                                                    
063900     EJECT                                                                
064000                                                                          
064100 CA-REDIGERA-PER-OBKR-KOD SECTION.                                        
064200                                                                          
064300                                                                          
064400                                                                          
064500     EVALUATE IN34-KDORDBEK                                               
064600     WHEN 10                                                              
064700        PERFORM CAA-REDIGERA-KOD-10                                       
064800     WHEN 15 THRU 16                                                      
064900        PERFORM CAB-REDIGERA-KOD-15-TILL-16                               
065000     WHEN 20 THRU 22                                                      
065100        PERFORM CAR-REDIGERA-KOD-20-TILL-22                               
065200     WHEN 30 THRU 34                                                      
065300        PERFORM CAL-REDIG-KOD-80--83-85-87                                
065400     WHEN 40                                                              
065500        PERFORM CAC-REDIGERA-KOD-40                                       
065600     WHEN 41 THRU 42                                                      
065700        PERFORM CAD-REDIGERA-KOD-41-TILL-42                               
065800     WHEN 43 THRU 44                                                      
065900        PERFORM CAE-REDIGERA-KOD-43-TILL-44                               
066000     WHEN 51 THRU 56                                                      
066100        PERFORM CAF-REDIGERA-KOD-52-56-66-67                              
066200     WHEN 66 THRU 67                                                      
066300        PERFORM CAF-REDIGERA-KOD-52-56-66-67                              
066400     WHEN 57                                                              
066500        PERFORM CAG-REDIGERA-KOD-57                                       
066600     WHEN 58 THRU 59                                                      
066700        PERFORM CAH-REDIGERA-KOD-58-TILL-59                               
066800     WHEN 61                                                              
066900        PERFORM CAI-REDIGERA-KOD-61-65                                    
067000     WHEN 65                                                              
067100        PERFORM CAI-REDIGERA-KOD-61-65                                    
067200     WHEN 70 THRU 71                                                      
067300        PERFORM CAJ-REDIGERA-KOD-70-TILL-71                               
067400     WHEN 72 THRU 76                                                      
067500        PERFORM CAK-REDIGERA-KOD-72-TILL-76                               
067600     WHEN 77                                                              
067700        PERFORM CAJ-REDIGERA-KOD-70-TILL-71                               
067800     WHEN 80 THRU 83                                                      
067900        PERFORM CAL-REDIG-KOD-80--83-85-87                                
068000     WHEN 85                                                              
068100        PERFORM CAL-REDIG-KOD-80--83-85-87                                
068200     WHEN 87                                                              
068300        PERFORM CAL-REDIG-KOD-80--83-85-87                                
068400     WHEN 90 THRU 93                                                      
068500        PERFORM CAM-REDIGERA-KOD-90-TILL-93                               
068600     WHEN 95 THRU 96                                                      
068700        PERFORM CAN-REDIGERA-KOD-95-TILL-96                               
068800     WHEN 98                                                              
068900        PERFORM CAO-REDIGERA-KOD-98                                       
069000     WHEN 99                                                              
069100        PERFORM CAP-REDIGERA-KOD-99                                       
069200     WHEN ZERO                                                            
069300        PERFORM CAQ-REDIGERA-KOD-00                                       
069400     END-EVALUATE                                                         
069500     .                                                                    
069600     EJECT                                                                
069700                                                                          
069800 CAA-REDIGERA-KOD-10 SECTION.                                             
069900                                                                          
070000     IF RADNR > RADNR-MAX                                                 
070100       PERFORM S02-SKRIV-HUVUD                                            
070200     END-IF                                                               
070300                                                                          
070400     MOVE IN34-IDARTNR         TO RAD-IDARTNR                             
070500                                  W-IDARTNR                               
070600     MOVE IN34-REKSIFFR        TO RAD-REKSIFFR                            
070700     PERFORM S03-HAMTA-BENAMNING                                          
070800     MOVE IN34-KVBEART-Q       TO RAD-KVBEART                             
070900     MOVE IN34-IDDC            TO RAD-IDDC                                
071000     MOVE IN34-IDKUNDRF-RO     TO WS-IDKUNDRF                             
071100     IF WS-IDKUNDRF-6--10 = SPACE                                         
071200        MOVE WS-IDORDNR5       TO RAD-IDORDNR                             
071300     ELSE                                                                 
071400        MOVE WS-IDORDNR7       TO RAD-IDORDNR                             
071500     END-IF                                                               
071600     IF IN34-TITPO IS NUMERIC AND IN34-TITPO > ZERO                       
071700        MOVE IN34-TITPO        TO RAD-TITPO                               
071800     END-IF                                                               
071900                                                                          
072000     MOVE RAD                  TO W001-RAD                                
072100     PERFORM S05-SKRIV-RAD                                                
072200     .                                                                    
072300     EJECT                                                                
072400                                                                          
072500 CAB-REDIGERA-KOD-15-TILL-16 SECTION.                                     
072600                                                                          
072700                                                                          
072800     IF RADNR > RADNR-MAX                                                 
072900       PERFORM S02-SKRIV-HUVUD                                            
073000     END-IF                                                               
073100                                                                          
073200     IF IN34-IDARTNR-TILLK > ZERO                                         
073300        MOVE IN34-IDARTNR-TILLK    TO RAD-IDARTNR                         
073400                                      W-IDARTNR                           
073500        MOVE IN34-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
073600     ELSE                                                                 
073700        MOVE IN34-IDARTNR          TO RAD-IDARTNR                         
073800                                      W-IDARTNR                           
073900        MOVE IN34-REKSIFFR         TO RAD-REKSIFFR                        
074000     END-IF                                                               
074100     PERFORM S03-HAMTA-BENAMNING                                          
074200     MOVE IN34-KVBEART-Q           TO RAD-KVBEART                         
074300     MOVE IN34-IDDC                TO RAD-IDDC                            
074400     IF IN34-IDKUNDRF-RO = SPACE OR '0000000   '                          
074500        MOVE IN34-IDKUNDRF         TO WS-IDKUNDRF                         
074600     ELSE                                                                 
074700        MOVE IN34-IDKUNDRF-RO      TO WS-IDKUNDRF                         
074800     END-IF                                                               
074900     IF WS-IDKUNDRF-6--10 = SPACE                                         
075000        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
075100     ELSE                                                                 
075200        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
075300     END-IF                                                               
075400                                                                          
075500     MOVE RAD                  TO W001-RAD                                
075600     PERFORM S05-SKRIV-RAD                                                
075700     .                                                                    
075800     EJECT                                                                
075900                                                                          
076000 CAC-REDIGERA-KOD-40 SECTION.                                             
076100                                                                          
076200                                                                          
076300     IF RADNR > RADNR-MAX                                                 
076400       PERFORM S02-SKRIV-HUVUD                                            
076500     END-IF                                                               
076600                                                                          
076700     MOVE '*'                      TO RAD-ASTERISK                        
076800     IF IN34-IDARTNR-TILLK > ZERO                                         
076900       MOVE ZERO                   TO RAD-KDORDBEK                        
077000       MOVE IN34-IDARTNR-TILLK     TO RAD-IDARTNR                         
077100                                      W-IDARTNR                           
077200       MOVE IN34-REKSIFFR-TILLK    TO RAD-REKSIFFR                        
077300     ELSE                                                                 
077400       MOVE IN34-IDARTNR           TO RAD-IDARTNR                         
077500                                      W-IDARTNR                           
077600       MOVE IN34-REKSIFFR          TO RAD-REKSIFFR                        
077700     END-IF                                                               
077800                                                                          
077900     PERFORM S03-HAMTA-BENAMNING                                          
078000     MOVE IN34-KVBEART-Q           TO RAD-KVBEART                         
078100     MOVE IN34-IDDC                TO RAD-IDDC                            
078200     IF IN34-IDKUNDRF-RO = SPACE OR '0000000   '                          
078300        MOVE IN34-IDKUNDRF         TO WS-IDKUNDRF                         
078400     ELSE                                                                 
078500        MOVE IN34-IDKUNDRF-RO      TO WS-IDKUNDRF                         
078600     END-IF                                                               
078700     IF WS-IDKUNDRF-6--10 = SPACE                                         
078800        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
078900     ELSE                                                                 
079000        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
079100     END-IF                                                               
079200                                                                          
079300     MOVE RAD                  TO W001-RAD                                
079400     PERFORM S05-SKRIV-RAD                                                
079500     .                                                                    
079600     EJECT                                                                
079700                                                                          
079800 CAD-REDIGERA-KOD-41-TILL-42 SECTION.                                     
079900                                                                          
080000                                                                          
080100     IF RADNR > RADNR-MAX                                                 
080200       PERFORM S02-SKRIV-HUVUD                                            
080300     END-IF                                                               
080400                                                                          
080500     IF IN34-IDARTNR-TILLK = +0                                           
080600        IF RADNR + 2 > RADNR-MAX                                          
080700           PERFORM S02-SKRIV-HUVUD                                        
080800        END-IF                                                            
080900                                                                          
081000        PERFORM CADA-RED-ERSATT-ARTIKEL                                   
081100     ELSE                                                                 
081200        PERFORM CADB-RED-ERSETTANDE-ARTIKEL                               
081300     END-IF                                                               
081400     .                                                                    
081500     EJECT                                                                
081600                                                                          
081700 CADA-RED-ERSATT-ARTIKEL SECTION.                                         
081800                                                                          
081900     MOVE '*'                      TO RAD-ASTERISK                        
082000     MOVE IN34-IDARTNR             TO RAD-IDARTNR                         
082100     MOVE IN34-REKSIFFR            TO RAD-REKSIFFR                        
082200     MOVE IN34-IDARTNR             TO W-IDARTNR                           
082300     PERFORM S03-HAMTA-BENAMNING                                          
082400     MOVE IN34-KVBEART             TO RAD-KVBEART                         
082500     MOVE IN34-IDDC                TO RAD-IDDC                            
082600     IF IN34-IDKUNDRF-RO = SPACE OR '0000000   '                          
082700        MOVE IN34-IDKUNDRF         TO WS-IDKUNDRF                         
082800     ELSE                                                                 
082900        MOVE IN34-IDKUNDRF-RO      TO WS-IDKUNDRF                         
083000     END-IF                                                               
083100     IF WS-IDKUNDRF-6--10 = SPACE                                         
083200        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
083300     ELSE                                                                 
083400        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
083500     END-IF                                                               
083600                                                                          
083700     MOVE RAD                  TO W001-RAD                                
083800     PERFORM S05-SKRIV-RAD                                                
083900     .                                                                    
084000     EJECT                                                                
084100                                                                          
084200 CADB-RED-ERSETTANDE-ARTIKEL SECTION.                                     
084300                                                                          
084400     MOVE ZERO                     TO RAD-KDORDBEK                        
084500     MOVE '*'                      TO RAD-ASTERISK                        
084600     MOVE ZERO                     TO RAD-TIORDREG                        
084700     INSPECT RAD-TIORDREG REPLACING LEADING ZERO BY SPACE                 
084800     MOVE IN34-IDARTNR-TILLK       TO RAD-IDARTNR                         
084900                                      W-IDARTNR                           
085000     MOVE IN34-REKSIFFR-TILLK      TO RAD-REKSIFFR                        
085100     PERFORM S03-HAMTA-BENAMNING                                          
085200     MOVE IN34-KVBEART-TILLK       TO RAD-KVBEART                         
085300     MOVE IN34-IDDC                TO RAD-IDDC                            
085400     MOVE IN34-DIERS-KVOT          TO RAD-KVQPACK-1                       
085500                                                                          
085600     MOVE RAD                  TO W001-RAD                                
085700     PERFORM S05-SKRIV-RAD                                                
085800     .                                                                    
085900     EJECT                                                                
086000                                                                          
086100 CAE-REDIGERA-KOD-43-TILL-44 SECTION.                                     
086200                                                                          
086300                                                                          
086400     IF RADNR > RADNR-MAX                                                 
086500       PERFORM S02-SKRIV-HUVUD                                            
086600     END-IF                                                               
086700                                                                          
086800     IF IN34-IDARTNR-TILLK > ZERO                                         
086900        MOVE IN34-IDARTNR-TILLK    TO RAD-IDARTNR                         
087000                                      W-IDARTNR                           
087100        MOVE IN34-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
087200     ELSE                                                                 
087300        MOVE IN34-IDARTNR          TO RAD-IDARTNR                         
087400                                      W-IDARTNR                           
087500        MOVE IN34-REKSIFFR         TO RAD-REKSIFFR                        
087600     END-IF                                                               
087700     PERFORM S03-HAMTA-BENAMNING                                          
087800     MOVE IN34-KVBEART-Q           TO RAD-KVBEART                         
087900     MOVE IN34-IDDC                TO RAD-IDDC                            
088000     MOVE IN34-KVQPACK-1           TO RAD-KVQPACK-1                       
088100     IF IN34-IDKUNDRF-RO = SPACE OR '0000000   '                          
088200        MOVE IN34-IDKUNDRF         TO WS-IDKUNDRF                         
088300     ELSE                                                                 
088400        MOVE IN34-IDKUNDRF-RO      TO WS-IDKUNDRF                         
088500     END-IF                                                               
088600     IF WS-IDKUNDRF-6--10 = SPACE                                         
088700        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
088800     ELSE                                                                 
088900        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
089000     END-IF                                                               
089100                                                                          
089200     MOVE RAD                  TO W001-RAD                                
089300     PERFORM S05-SKRIV-RAD                                                
089400     .                                                                    
089500     EJECT                                                                
089600                                                                          
089700 CAF-REDIGERA-KOD-52-56-66-67 SECTION.                                    
089800                                                                          
089900                                                                          
090000     IF RADNR > RADNR-MAX                                                 
090100       PERFORM S02-SKRIV-HUVUD                                            
090200     END-IF                                                               
090300                                                                          
090400     IF IN34-IDARTNR-TILLK > ZERO                                         
090500        MOVE IN34-IDARTNR-TILLK    TO RAD-IDARTNR                         
090600                                      W-IDARTNR                           
090700        MOVE IN34-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
090800     ELSE                                                                 
090900        MOVE IN34-IDARTNR          TO RAD-IDARTNR                         
091000                                      W-IDARTNR                           
091100        MOVE IN34-REKSIFFR         TO RAD-REKSIFFR                        
091200     END-IF                                                               
091300     PERFORM S03-HAMTA-BENAMNING                                          
091400     MOVE IN34-KVBEART             TO RAD-KVBEART                         
091500     MOVE IN34-IDDC                TO RAD-IDDC                            
091600     MOVE IN34-IDKUNDRF            TO WS-IDKUNDRF                         
091700     IF WS-IDKUNDRF-6--10 = SPACE                                         
091800        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
091900     ELSE                                                                 
092000        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
092100     END-IF                                                               
092200                                                                          
092300     MOVE RAD                  TO W001-RAD                                
092400     PERFORM S05-SKRIV-RAD                                                
092500     .                                                                    
092600     EJECT                                                                
092700                                                                          
092800 CAG-REDIGERA-KOD-57 SECTION.                                             
092900                                                                          
093000     PERFORM CAGA-HAMTA-KOLLA-SATSART                                     
093100                                                                          
093200     IF IN34-IDARTNR-TILLK > ZERO                                         
093300        MOVE IN34-IDARTNR-TILLK    TO RAD-IDARTNR                         
093400                                      W-IDARTNR                           
093500        MOVE IN34-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
093600     ELSE                                                                 
093700        MOVE IN34-IDARTNR          TO RAD-IDARTNR                         
093800                                      W-IDARTNR                           
093900        MOVE IN34-REKSIFFR         TO RAD-REKSIFFR                        
094000     END-IF                                                               
094100     PERFORM S03-HAMTA-BENAMNING                                          
094200     MOVE IN34-KVBEART             TO RAD-KVBEART                         
094300     MOVE IN34-IDDC                TO RAD-IDDC                            
094400     MOVE IN34-IDKUNDRF            TO WS-IDKUNDRF                         
094500     IF WS-IDKUNDRF-6--10 = SPACE                                         
094600        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
094700     ELSE                                                                 
094800        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
094900     END-IF                                                               
095000                                                                          
095100     IF WS-INDX-SATS > +1                                                 
095200        MOVE SPACE                 TO W001-RAD                            
095300        PERFORM S05-SKRIV-RAD                                             
095400        MOVE RAD                   TO W001-RAD                            
095500        PERFORM S05-SKRIV-RAD                                             
095600                                                                          
095700        MOVE SPACE                 TO RAD                                 
095800        MOVE +1                    TO WS-INDX-SATS                        
095900        PERFORM UNTIL WS-INDX-SATS > WS-INDX-SATS-MAX OR                  
096000                      WS-IDARTNR-SATS(WS-INDX-SATS) = +0                  
096100                                                                          
096200           MOVE WS-IDARTNR-SATS(WS-INDX-SATS)                             
096300                                   TO RAD-BEART                           
096400           INSPECT RAD-BEART REPLACING LEADING ZERO BY SPACE              
096500           ADD +2 TO RADNR                                                
096600           MOVE RAD                TO W001-RAD                            
096700           PERFORM S05-SKRIV-RAD                                          
096800           ADD +1 TO WS-INDX-SATS                                         
096900        END-PERFORM                                                       
097000     END-IF                                                               
097100     .                                                                    
097200     EJECT                                                                
097300                                                                          
097400 CAGA-HAMTA-KOLLA-SATSART SECTION.                                        
097500                                                                          
097600     MOVE +1 TO WS-INDX-SATS                                              
097700     PERFORM UNTIL WS-INDX-SATS > WS-INDX-SATS-MAX                        
097800        MOVE +0                TO WS-IDARTNR-SATS(WS-INDX-SATS)           
097900        ADD +1                 TO WS-INDX-SATS                            
098000     END-PERFORM                                                          
098100                                                                          
098200     MOVE +1 TO WS-INDX-SATS                                              
098300     IF IN34-IDARTNR-TILLK > ZERO                                         
098400       MOVE IN34-IDARTNR-TILLK   TO W-IDARTNR-J1                          
098500     ELSE                                                                 
098600       MOVE IN34-IDARTNR         TO W-IDARTNR-J1                          
098700     END-IF                                                               
098800                                                                          
098900     PERFORM IMS-GU-SATB-WDJ111-01                                        
099000     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
099100                   WS-INDX-SATS > WS-INDX-SATS-MAX                        
099200        IF SATB-STR-IDARTNR < 100000000 AND                               
099300           SATB-STR-TIBORT = 0                                            
099400           MOVE SATB-RAD-TISTADAT   TO TMP1-YYMMDD                        
099500           MOVE DAGENS-DATUM        TO TMP2-YYMMDD                        
099600           PERFORM WY2000P1                                               
099700           IF TMP1-YYMMDD       NOT > TMP2-YYMMDD                         
099800              MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD                     
099900              MOVE DAGENS-DATUM        TO TMP2-YYMMDD                     
100000              PERFORM WY2000P1                                            
100100              IF TMP1-YYMMDD    NOT < TMP2-YYMMDD                         
100200                                                                          
100300                 MOVE SATB-STR-IDARTNR                                    
100400                               TO WS-IDARTNR-SATS(WS-INDX-SATS)           
100500                 ADD +1        TO WS-INDX-SATS                            
100600              END-IF                                                      
100700           END-IF                                                         
100800        END-IF                                                            
100900        PERFORM IMS-GN-SATB-WDJ111-01                                     
101000     END-PERFORM                                                          
101100                                                                          
101200     IF RADNR + WS-INDX-SATS > RADNR-MAX                                  
101300        PERFORM S02-SKRIV-HUVUD                                           
101400     END-IF                                                               
101500     .                                                                    
101600     EJECT                                                                
101700                                                                          
101800 CAH-REDIGERA-KOD-58-TILL-59 SECTION.                                     
101900                                                                          
102000                                                                          
102100     IF RADNR > RADNR-MAX                                                 
102200       PERFORM S02-SKRIV-HUVUD                                            
102300     END-IF                                                               
102400                                                                          
102500     IF IN34-IDARTNR-TILLK > ZERO                                         
102600        MOVE IN34-IDARTNR-TILLK    TO RAD-IDARTNR                         
102700                                      W-IDARTNR                           
102800        MOVE IN34-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
102900     ELSE                                                                 
103000        MOVE IN34-IDARTNR          TO RAD-IDARTNR                         
103100                                      W-IDARTNR                           
103200        MOVE IN34-REKSIFFR         TO RAD-REKSIFFR                        
103300     END-IF                                                               
103400     PERFORM S03-HAMTA-BENAMNING                                          
103500     MOVE IN34-KVBEART             TO RAD-KVBEART                         
103600     MOVE IN34-IDDC                TO RAD-IDDC                            
103700     MOVE IN34-IDKUNDRF            TO WS-IDKUNDRF                         
103800     IF WS-IDKUNDRF-6--10 = SPACE                                         
103900        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
104000     ELSE                                                                 
104100        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
104200     END-IF                                                               
104300                                                                          
104400     MOVE RAD                  TO W001-RAD                                
104500     PERFORM S05-SKRIV-RAD                                                
104600     .                                                                    
104700     EJECT                                                                
104800                                                                          
104900 CAI-REDIGERA-KOD-61-65 SECTION.                                          
105000                                                                          
105100     IF RADNR + 2 > RADNR-MAX                                             
105200        PERFORM S02-SKRIV-HUVUD                                           
105300     END-IF                                                               
105400                                                                          
105500     IF IN34-IDARTNR-TILLK = +0 AND                                       
105600        IN34-BEERS = SPACE                                                
105700        PERFORM CAIA-RED-ERSATT-ARTIKEL                                   
105800     ELSE                                                                 
105900        PERFORM CAIB-RED-ERSETTANDE-ARTIKEL                               
106000     END-IF                                                               
106100     .                                                                    
106200     EJECT                                                                
106300                                                                          
106400 CAIA-RED-ERSATT-ARTIKEL SECTION.                                         
106500                                                                          
106600     MOVE '*'                      TO RAD-ASTERISK                        
106700     MOVE IN34-IDARTNR             TO RAD-IDARTNR                         
106800                                      W-IDARTNR                           
106900     MOVE IN34-REKSIFFR            TO RAD-REKSIFFR                        
107000     PERFORM S03-HAMTA-BENAMNING                                          
107100     MOVE IN34-KVBEART             TO RAD-KVBEART                         
107200     MOVE IN34-IDDC                TO RAD-IDDC                            
107300     IF IN34-IDKUNDRF-RO = SPACE OR '0000000   '                          
107400        MOVE IN34-IDKUNDRF         TO WS-IDKUNDRF                         
107500     ELSE                                                                 
107600        MOVE IN34-IDKUNDRF-RO      TO WS-IDKUNDRF                         
107700     END-IF                                                               
107800     IF WS-IDKUNDRF-6--10 = SPACE                                         
107900        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
108000     ELSE                                                                 
108100        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
108200     END-IF                                                               
108300                                                                          
108400     MOVE RAD                  TO W001-RAD                                
108500     PERFORM S05-SKRIV-RAD                                                
108600     .                                                                    
108700     EJECT                                                                
108800                                                                          
108900 CAIB-RED-ERSETTANDE-ARTIKEL SECTION.                                     
109000                                                                          
109100     MOVE ZERO                     TO RAD-KDORDBEK                        
109200     MOVE ZERO                     TO RAD-TIORDREG                        
109300     INSPECT RAD-TIORDREG REPLACING LEADING ZERO BY SPACE                 
109400     IF IN34-BEERS NOT = SPACE                                            
109500        MOVE IN34-BEERS            TO RAD-BEART                           
109600        MOVE SPACE                 TO RAD-STRECK                          
109700     ELSE                                                                 
109800        MOVE '*'                   TO RAD-ASTERISK                        
109900        MOVE IN34-IDARTNR-TILLK    TO RAD-IDARTNR                         
110000                                      W-IDARTNR                           
110100        MOVE IN34-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
110200        PERFORM S03-HAMTA-BENAMNING                                       
110300        MOVE IN34-KVBEART-TILLK    TO RAD-KVBEART                         
110400        MOVE IN34-IDDC             TO RAD-IDDC                            
110500        MOVE IN34-DIERS-KVOT       TO RAD-KVQPACK-1                       
110600     END-IF                                                               
110700                                                                          
110800     MOVE RAD                  TO W001-RAD                                
110900     PERFORM S05-SKRIV-RAD                                                
111000     .                                                                    
111100     EJECT                                                                
111200                                                                          
111300 CAJ-REDIGERA-KOD-70-TILL-71 SECTION.                                     
111400                                                                          
111500     IF RADNR > RADNR-MAX                                                 
111600       PERFORM S02-SKRIV-HUVUD                                            
111700     END-IF                                                               
111800                                                                          
111900     IF IN34-IDARTNR-TILLK > ZERO                                         
112000        MOVE IN34-IDARTNR-TILLK    TO RAD-IDARTNR                         
112100                                      W-IDARTNR                           
112200        MOVE IN34-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
112300     ELSE                                                                 
112400        MOVE IN34-IDARTNR          TO RAD-IDARTNR                         
112500                                      W-IDARTNR                           
112600        MOVE IN34-REKSIFFR         TO RAD-REKSIFFR                        
112700     END-IF                                                               
112800     PERFORM S03-HAMTA-BENAMNING                                          
112900     MOVE IN34-KVBEART-Q           TO RAD-KVBEART                         
113000     MOVE IN34-IDDC                TO RAD-IDDC                            
113100     IF IN34-IDKUNDRF-RO = SPACE OR '0000000   '                          
113200        MOVE IN34-IDKUNDRF         TO WS-IDKUNDRF                         
113300     ELSE                                                                 
113400        MOVE IN34-IDKUNDRF-RO      TO WS-IDKUNDRF                         
113500     END-IF                                                               
113600     IF WS-IDKUNDRF-6--10 = SPACE                                         
113700        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
113800     ELSE                                                                 
113900        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
114000     END-IF                                                               
114100     IF IN34-TITPO > ZERO                                                 
114200       MOVE IN34-TITPO             TO RAD-TITPO                           
114300     END-IF                                                               
114400                                                                          
114500     MOVE RAD                  TO W001-RAD                                
114600     PERFORM S05-SKRIV-RAD                                                
114700     .                                                                    
114800     EJECT                                                                
114900                                                                          
115000 CAK-REDIGERA-KOD-72-TILL-76 SECTION.                                     
115100                                                                          
115200     IF RADNR > RADNR-MAX                                                 
115300       PERFORM S02-SKRIV-HUVUD                                            
115400     END-IF                                                               
115500                                                                          
115600     IF IN34-IDARTNR-TILLK > ZERO                                         
115700        MOVE IN34-IDARTNR-TILLK    TO RAD-IDARTNR                         
115800                                      W-IDARTNR                           
115900        MOVE IN34-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
116000     ELSE                                                                 
116100        MOVE IN34-IDARTNR          TO RAD-IDARTNR                         
116200                                      W-IDARTNR                           
116300        MOVE IN34-REKSIFFR         TO RAD-REKSIFFR                        
116400     END-IF                                                               
116500     PERFORM S03-HAMTA-BENAMNING                                          
116600     IF IN34-KDORDBEK = 74                                                
116700       MOVE IN34-KVBEART-Q         TO RAD-KVBEART                         
116800     ELSE                                                                 
116900       MOVE IN34-KVBEART           TO RAD-KVBEART                         
117000     END-IF                                                               
117100     MOVE IN34-IDDC                TO RAD-IDDC                            
117200     IF IN34-IDKUNDRF-RO = SPACE OR '0000000   '                          
117300        MOVE IN34-IDKUNDRF         TO WS-IDKUNDRF                         
117400     ELSE                                                                 
117500        MOVE IN34-IDKUNDRF-RO      TO WS-IDKUNDRF                         
117600     END-IF                                                               
117700     IF WS-IDKUNDRF-6--10 = SPACE                                         
117800        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
117900     ELSE                                                                 
118000        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
118100     END-IF                                                               
118200     IF IN34-KDORDBEK = 75 OR 76                                          
118300        IF IN34-KDTPOTYP NOT = ZERO                                       
118400          IF IN34-TITPO > ZERO                                            
118500            MOVE IN34-TITPO        TO RAD-TITPO                           
118600          END-IF                                                          
118700        END-IF                                                            
118800     END-IF                                                               
118900                                                                          
119000     MOVE RAD                  TO W001-RAD                                
119100     PERFORM S05-SKRIV-RAD                                                
119200     .                                                                    
119300     EJECT                                                                
119400                                                                          
119500 CAL-REDIG-KOD-80--83-85-87 SECTION.                                      
119600                                                                          
119700     IF RADNR > RADNR-MAX                                                 
119800       PERFORM S02-SKRIV-HUVUD                                            
119900     END-IF                                                               
120000                                                                          
120100     IF IN34-IDARTNR-TILLK > ZERO                                         
120200        MOVE IN34-IDARTNR-TILLK    TO RAD-IDARTNR                         
120300                                      W-IDARTNR                           
120400        MOVE IN34-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
120500     ELSE                                                                 
120600        MOVE IN34-IDARTNR          TO RAD-IDARTNR                         
120700                                      W-IDARTNR                           
120800        MOVE IN34-REKSIFFR         TO RAD-REKSIFFR                        
120900     END-IF                                                               
121000     PERFORM S03-HAMTA-BENAMNING                                          
121100     MOVE IN34-IDDC                TO RAD-IDDC                            
121200     IF IN34-KDORDBEK = 82                                                
121300        MOVE IN34-KVBEART          TO RAD-KVBEART                         
121400     ELSE                                                                 
121500        MOVE IN34-KVANNANT         TO RAD-KVBEART                         
121600     END-IF                                                               
121700     IF IN34-IDKUNDRF-RO = SPACE OR '0000000   '                          
121800        MOVE IN34-IDKUNDRF         TO WS-IDKUNDRF                         
121900     ELSE                                                                 
122000        MOVE IN34-IDKUNDRF-RO      TO WS-IDKUNDRF                         
122100     END-IF                                                               
122200     IF WS-IDKUNDRF-6--10 = SPACE                                         
122300        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
122400     ELSE                                                                 
122500        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
122600     END-IF                                                               
122700                                                                          
122800     MOVE RAD                  TO W001-RAD                                
122900     PERFORM S05-SKRIV-RAD                                                
123000     .                                                                    
123100     EJECT                                                                
123200                                                                          
123300 CAM-REDIGERA-KOD-90-TILL-93 SECTION.                                     
123400                                                                          
123500     IF RADNR > RADNR-MAX                                                 
123600       PERFORM S02-SKRIV-HUVUD                                            
123700     END-IF                                                               
123800                                                                          
123900     IF IN34-IDARTNR-TILLK > ZERO                                         
124000        MOVE IN34-IDARTNR-TILLK    TO RAD-IDARTNR                         
124100                                      W-IDARTNR                           
124200        MOVE IN34-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
124300     ELSE                                                                 
124400        MOVE IN34-IDARTNR          TO RAD-IDARTNR                         
124500                                      W-IDARTNR                           
124600        MOVE IN34-REKSIFFR         TO RAD-REKSIFFR                        
124700     END-IF                                                               
124800     MOVE IN34-IDDC                TO RAD-IDDC                            
124900     PERFORM S03-HAMTA-BENAMNING                                          
125000     IF IN34-KDORDBEK = 92                                                
125100        MOVE IN34-KVPRERO          TO RAD-KVBEART                         
125200     ELSE                                                                 
125300        IF IN34-KDORDBEK = 93                                             
125400           MOVE IN34-KVANNANT      TO RAD-KVBEART                         
125500        ELSE                                                              
125600           MOVE IN34-KVRO          TO RAD-KVBEART                         
125700        END-IF                                                            
125800     END-IF                                                               
125900     MOVE IN34-IDKUNDRF            TO WS-IDKUNDRF                         
126000     IF WS-IDKUNDRF-6--10 = SPACE                                         
126100        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
126200     ELSE                                                                 
126300        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
126400     END-IF                                                               
126500                                                                          
126600     MOVE RAD                  TO W001-RAD                                
126700     PERFORM S05-SKRIV-RAD                                                
126800     .                                                                    
126900     EJECT                                                                
127000                                                                          
127100 CAN-REDIGERA-KOD-95-TILL-96 SECTION.                                     
127200                                                                          
127300     IF RADNR > RADNR-MAX                                                 
127400       PERFORM S02-SKRIV-HUVUD                                            
127500     END-IF                                                               
127600                                                                          
127700     IF IN34-IDARTNR-TILLK > ZERO                                         
127800        MOVE IN34-IDARTNR-TILLK    TO RAD-IDARTNR                         
127900                                      W-IDARTNR                           
128000        MOVE IN34-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
128100     ELSE                                                                 
128200        MOVE IN34-IDARTNR          TO RAD-IDARTNR                         
128300                                      W-IDARTNR                           
128400        MOVE IN34-REKSIFFR         TO RAD-REKSIFFR                        
128500     END-IF                                                               
128600     PERFORM S03-HAMTA-BENAMNING                                          
128700     MOVE IN34-KVBEART-Q           TO RAD-KVBEART                         
128800     MOVE IN34-IDDC                TO RAD-IDDC                            
128900     IF IN34-IDKUNDRF-RO = SPACE OR '0000000   '                          
129000        MOVE IN34-IDKUNDRF         TO WS-IDKUNDRF                         
129100     ELSE                                                                 
129200        MOVE IN34-IDKUNDRF-RO      TO WS-IDKUNDRF                         
129300     END-IF                                                               
129400     IF WS-IDKUNDRF-6--10 = SPACE                                         
129500        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
129600     ELSE                                                                 
129700        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
129800     END-IF                                                               
129900     IF IN34-KDORDBEK = 96                                                
130000       IF IN34-TIDISPIN > ZERO                                            
130100         MOVE IN34-TIDISPIN        TO RAD-TITPO                           
130200       END-IF                                                             
130300     END-IF                                                               
130400                                                                          
130500     MOVE RAD                  TO W001-RAD                                
130600     PERFORM S05-SKRIV-RAD                                                
130700     .                                                                    
130800     EJECT                                                                
130900                                                                          
131000 CAO-REDIGERA-KOD-98 SECTION.                                             
131100                                                                          
131200                                                                          
131300     IF RADNR > RADNR-MAX                                                 
131400       PERFORM S02-SKRIV-HUVUD                                            
131500     END-IF                                                               
131600                                                                          
131700     MOVE IN34-IDARTNR             TO RAD-IDARTNR                         
131800                                      W-IDARTNR                           
131900     MOVE IN34-REKSIFFR            TO RAD-REKSIFFR                        
132000     PERFORM S03-HAMTA-BENAMNING                                          
132100     MOVE IN34-KVBEART             TO RAD-KVBEART                         
132200     MOVE IN34-IDDC                TO RAD-IDDC                            
132300     IF IN34-IDKUNDRF-RO = SPACE OR '0000000   '                          
132400        MOVE IN34-IDKUNDRF         TO WS-IDKUNDRF                         
132500     ELSE                                                                 
132600        MOVE IN34-IDKUNDRF-RO      TO WS-IDKUNDRF                         
132700     END-IF                                                               
132800     IF WS-IDKUNDRF-6--10 = SPACE                                         
132900        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
133000     ELSE                                                                 
133100        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
133200     END-IF                                                               
133300                                                                          
133400     MOVE RAD                  TO W001-RAD                                
133500     PERFORM S05-SKRIV-RAD                                                
133600     .                                                                    
133700     EJECT                                                                
133800                                                                          
133900 CAP-REDIGERA-KOD-99 SECTION.                                             
134000                                                                          
134100     IF RADNR > RADNR-MAX                                                 
134200       PERFORM S02-SKRIV-HUVUD                                            
134300     END-IF                                                               
134400                                                                          
134500     IF IN34-IDARTNR-TILLK > ZERO                                         
134600        MOVE IN34-IDARTNR-TILLK    TO RAD-IDARTNR                         
134700                                      W-IDARTNR                           
134800        MOVE IN34-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
134900     ELSE                                                                 
135000        MOVE IN34-IDARTNR          TO RAD-IDARTNR                         
135100                                      W-IDARTNR                           
135200        MOVE IN34-REKSIFFR         TO RAD-REKSIFFR                        
135300     END-IF                                                               
135400     MOVE IN34-IDDC                TO RAD-IDDC                            
135500     PERFORM S03-HAMTA-BENAMNING                                          
135600     MOVE IN34-KVPRERO             TO RAD-KVBEART                         
135700     MOVE IN34-IDKUNDRF            TO WS-IDKUNDRF                         
135800     IF WS-IDKUNDRF-6--10 = SPACE                                         
135900        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
136000     ELSE                                                                 
136100        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
136200     END-IF                                                               
136300                                                                          
136400     MOVE RAD                  TO W001-RAD                                
136500     PERFORM S05-SKRIV-RAD                                                
136600                                                                          
136700     .                                                                    
136800     EJECT                                                                
136900                                                                          
137000 CAQ-REDIGERA-KOD-00 SECTION.                                             
137100                                                                          
137200     IF RADNR > RADNR-MAX                                                 
137300       PERFORM S02-SKRIV-HUVUD                                            
137400     END-IF                                                               
137500     .                                                                    
137600     EJECT                                                                
137700                                                                          
137800 CAR-REDIGERA-KOD-20-TILL-22 SECTION.                                     
137900                                                                          
138000                                                                          
138100     IF RADNR > RADNR-MAX                                                 
138200       PERFORM S02-SKRIV-HUVUD                                            
138300     END-IF                                                               
138400                                                                          
138500     IF IN34-IDARTNR-TILLK > ZERO                                         
138600        MOVE IN34-IDARTNR-TILLK    TO RAD-IDARTNR                         
138700                                      W-IDARTNR                           
138800        MOVE IN34-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
138900     ELSE                                                                 
139000        MOVE IN34-IDARTNR          TO RAD-IDARTNR                         
139100                                      W-IDARTNR                           
139200        MOVE IN34-REKSIFFR         TO RAD-REKSIFFR                        
139300     END-IF                                                               
139400     PERFORM S03-HAMTA-BENAMNING                                          
139500     IF IN34-KDORDBEK = 20                                                
139600       MOVE IN34-KVANNANT          TO RAD-KVBEART                         
139700     ELSE                                                                 
139800       MOVE IN34-KVBEART-Q         TO RAD-KVBEART                         
139900     END-IF                                                               
140000     MOVE IN34-IDDC                TO RAD-IDDC                            
140100     IF IN34-IDKUNDRF-RO = SPACE OR '0000000   '                          
140200        MOVE IN34-IDKUNDRF         TO WS-IDKUNDRF                         
140300     ELSE                                                                 
140400        MOVE IN34-IDKUNDRF-RO      TO WS-IDKUNDRF                         
140500     END-IF                                                               
140600     IF WS-IDKUNDRF-6--10 = SPACE                                         
140700        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
140800     ELSE                                                                 
140900        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
141000     END-IF                                                               
141100                                                                          
141200     MOVE RAD                  TO W001-RAD                                
141300     PERFORM S05-SKRIV-RAD                                                
141400     .                                                                    
141500     EJECT                                                                
141600                                                                          
141700 Z-FINIT      SECTION.                                                    
141900                                                                          
142000     CLOSE W41234                                                         
142100           W4123501                                                       
142200           W4123502                                                       
142300           W4123503                                                       
142310           W4123504                                                       
142400     SKIP2                                                                
142500     MOVE 'S' TO POSTSUM-OPKOD                                            
142600     CALL POSTSUM USING POSTSUM-PARM                                      
142700     .                                                                    
142800     EJECT                                                                
142900 S01-READ-W41234    SECTION.                                              
143000     READ W41234 RECORD INTO IN34-AREA                                    
143100     AT END                                                               
143200       MOVE JA               TO W41234-EOF-SW                             
143300                                                                          
143400     NOT AT END                                                           
143500        MOVE 'W41234' TO POSTSUM-FDNAMN                                   
143600        MOVE 'W41235D1' TO POSTSUM-DDNAMN2                                
143700        MOVE IN34-IDPTYP TO POSTSUM-TRANSTYP                              
143800        CALL POSTSUM USING POSTSUM-PARM                                   
143900                                                                          
144000     END-READ                                                             
144100     .                                                                    
144200     EJECT                                                                
144300 S02-SKRIV-HUVUD SECTION.                                                 
144400     ADD        1    TO  SIDNR                                            
144500     MOVE       1    TO  STYR                                             
144600     MOVE    SIDNR   TO HRAD1-IDSID                                       
144700     MOVE    HRAD1   TO  W001-RAD                                         
144800     PERFORM S06-SKRIV-RUBRIK                                             
144900     MOVE       5    TO  STYR                                             
145000     MOVE    HRAD7   TO  W001-RAD                                         
145100     PERFORM S05-SKRIV-RAD                                                
145200     MOVE       1    TO  STYR                                             
145300     MOVE    HRAD8   TO  W001-RAD                                         
145400     PERFORM S05-SKRIV-RAD                                                
145500     MOVE    HRAD9   TO  W001-RAD                                         
145600     PERFORM S05-SKRIV-RAD                                                
145700     MOVE    HRAD10  TO  W001-RAD                                         
145800     PERFORM S05-SKRIV-RAD                                                
145900     MOVE    HRAD11  TO  W001-RAD                                         
146000     PERFORM S05-SKRIV-RAD                                                
146100     MOVE    HRAD12  TO  W001-RAD                                         
146200     PERFORM S05-SKRIV-RAD                                                
146300     MOVE    HRAD13  TO  W001-RAD                                         
146400     PERFORM S05-SKRIV-RAD                                                
146500     MOVE       2    TO  STYR                                             
146600     MOVE    SPACE   TO  W001-RAD                                         
146700     PERFORM S05-SKRIV-RAD                                                
146800     MOVE      20    TO  RADNR                                            
146900     .                                                                    
147000     EJECT                                                                
147100 S03-HAMTA-BENAMNING SECTION.                                             
147200                                                                          
147300     PERFORM IMS-GU-BENA                                                  
147400                                                                          
147500     IF SEGMENT-FINNS                                                     
147600        MOVE TEXT-BEART        TO RAD-BEART                               
147700     END-IF                                                               
147800     .                                                                    
147900     EJECT                                                                
148000                                                                          
148100 S04-BEFRAKT        SECTION.                                              
148200     MOVE W-KDFRAKT                 TO W-4732-KDFRAKT                     
148300     PERFORM IMS-GU-4732                                                  
148400                                                                          
148500     IF SEGMENT-FINNS                                                     
148600         MOVE FRAKT-BEFRAKT (SPRAK-IX) TO HRAD11-BEFRAKT                  
148700     END-IF                                                               
148800     .                                                                    
148900     EJECT                                                                
149000 S05-SKRIV-RAD SECTION.                                                   
149100     MOVE IN-IDDISTR  TO  TEST-IDDISTR                                    
149200                                                                          
149300     IF DIST66-PRINTER-EUROPA2                                            
149400       MOVE STYR TO RAD-501-SKIP                                          
149500       MOVE W001-RAD TO RAD-W4123501                                      
149600       WRITE W4123501-RAD AFTER STYR                                      
149700     ELSE                                                                 
149800       IF DIST66-PRINTER-NORDEN                                           
149900         MOVE STYR TO RAD-502-SKIP                                        
150000         MOVE W001-RAD TO RAD-W4123502                                    
150100         WRITE W4123502-RAD AFTER STYR                                    
150200       ELSE                                                               
150300         IF DIST66-PRINTER-OVERSEAS                                       
150400           MOVE STYR TO RAD-503-SKIP                                      
150500           MOVE W001-RAD TO RAD-W4123503                                  
150600           WRITE W4123503-RAD AFTER STYR                                  
150610         ELSE                                                             
150700           IF DIST66-PRINTER-EUROPA3                                      
150710             MOVE STYR TO RAD-504-SKIP                                    
150720             MOVE W001-RAD TO RAD-W4123504                                
150730             WRITE W4123504-RAD AFTER STYR                                
150800           ELSE                                                           
150900             MOVE STYR TO RAD-503-SKIP                                    
151000             MOVE W001-RAD TO RAD-W4123503                                
151100             WRITE W4123503-RAD AFTER STYR                                
151200           END-IF                                                         
151210         END-IF                                                           
151300       END-IF                                                             
151400     END-IF                                                               
151500                                                                          
151600     ADD 2 TO    RADNR                                                    
151700     .                                                                    
151800     EJECT                                                                
151900 S06-SKRIV-RUBRIK SECTION.                                                
152000     MOVE IN-IDDISTR  TO  TEST-IDDISTR                                    
152100                                                                          
152200     IF DIST66-PRINTER-EUROPA2                                            
152300       MOVE SPACE            TO HRAD1-LISTID                              
152400       MOVE SPACE            TO HRAD1-FTG-NAMN                            
152500       MOVE HRAD1 TO W001-RAD                                             
152600       MOVE STYR TO RAD-501-SKIP                                          
152700       MOVE W001-RAD TO RAD-W4123501                                      
152800       WRITE W4123501-RAD AFTER PAGE                                      
152900       MOVE 'W41235' TO POSTSUM-FDNAMN                                    
153000       MOVE 'W41235D2' TO POSTSUM-DDNAMN2                                 
153100       MOVE 'PVL'     TO POSTSUM-TRANSTYP                                 
153200       CALL POSTSUM USING POSTSUM-PARM                                    
153300     ELSE                                                                 
153400       IF DIST66-PRINTER-NORDEN                                           
153500         MOVE SPACE            TO HRAD1-LISTID                            
153600         MOVE SPACE            TO HRAD1-FTG-NAMN                          
153700         MOVE HRAD1 TO W001-RAD                                           
153800         MOVE STYR TO RAD-502-SKIP                                        
153900         MOVE W001-RAD TO RAD-W4123502                                    
154000         WRITE W4123502-RAD AFTER PAGE                                    
154100         MOVE 'W41235' TO POSTSUM-FDNAMN                                  
154200         MOVE 'W41235D3' TO POSTSUM-DDNAMN2                               
154300         MOVE 'PVL'     TO POSTSUM-TRANSTYP                               
154400         CALL POSTSUM USING POSTSUM-PARM                                  
154500       ELSE                                                               
154600         IF DIST66-PRINTER-OVERSEAS                                       
154700           MOVE SPACE            TO HRAD1-LISTID                          
154800           MOVE SPACE            TO HRAD1-FTG-NAMN                        
154900           MOVE HRAD1 TO W001-RAD                                         
155000           MOVE STYR TO RAD-503-SKIP                                      
155100           MOVE W001-RAD TO RAD-W4123503                                  
155200           WRITE W4123503-RAD AFTER PAGE                                  
155300           MOVE 'W41235' TO POSTSUM-FDNAMN                                
155400           MOVE 'W41235D4' TO POSTSUM-DDNAMN2                             
155500           MOVE 'PVL'     TO POSTSUM-TRANSTYP                             
155600           CALL POSTSUM USING POSTSUM-PARM                                
155601         ELSE                                                             
155610           IF DIST66-PRINTER-EUROPA3                                      
155620             MOVE SPACE          TO HRAD1-LISTID                          
155630             MOVE SPACE          TO HRAD1-FTG-NAMN                        
155640             MOVE HRAD1 TO W001-RAD                                       
155650             MOVE STYR TO RAD-504-SKIP                                    
155660             MOVE W001-RAD TO RAD-W4123504                                
155670             WRITE W4123504-RAD AFTER PAGE                                
155680             MOVE 'W41235' TO POSTSUM-FDNAMN                              
155690             MOVE 'W41235D5' TO POSTSUM-DDNAMN2                           
155691             MOVE 'PVL'   TO POSTSUM-TRANSTYP                             
155692             CALL POSTSUM USING POSTSUM-PARM                              
155700           ELSE                                                           
155800             MOVE SPACE          TO HRAD1-LISTID                          
155900             MOVE SPACE          TO HRAD1-FTG-NAMN                        
156000             MOVE HRAD1 TO W001-RAD                                       
156100             MOVE STYR TO RAD-503-SKIP                                    
156200             MOVE W001-RAD TO RAD-W4123503                                
156300             WRITE W4123503-RAD AFTER PAGE                                
156400             MOVE 'W41235' TO POSTSUM-FDNAMN                              
156500             MOVE 'W41235D4' TO POSTSUM-DDNAMN2                           
156600             MOVE 'PVL'   TO POSTSUM-TRANSTYP                             
156700             CALL POSTSUM USING POSTSUM-PARM                              
156800           END-IF                                                         
156810         END-IF                                                           
156900       END-IF                                                             
157000     END-IF                                                               
157100                                                                          
157200     ADD 6   TO  RADNR                                                    
157300     .                                                                    
157400     EJECT                                                                
157500* --- IMS SEKTIONER ---                                                   
157600                                                                          
157700 IMS-GU-WDQ201        SECTION.                                            
157800                                                                          
157900     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
158000          DELIMITED BY SIZE INTO SSA1                                     
158100     MOVE '  GE' TO GODK-STATUSKODER                                      
158200     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA2 SSA1                     
158300     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
158400     PERFORM IMS-STATUSKONTROLL                                           
158500     .                                                                    
158600     EJECT                                                                
158700 IMS-GNP-WDQ212        SECTION.                                           
158800                                                                          
158900     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
159000          DELIMITED BY SIZE INTO SSA1                                     
159100     MOVE '  GE' TO GODK-STATUSKODER                                      
159200     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA1 SSA1                    
159300     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
159400     PERFORM IMS-STATUSKONTROLL                                           
159500     .                                                                    
159600     EJECT                                                                
159700                                                                          
159800                                                                          
159900 IMS-GU-WDE8          SECTION.                                            
160000                                                                          
160100     STRING 'WLPROC01(WDE801KY =' W-IDGMTREF-X ')'                        
160200          DELIMITED BY SIZE INTO SSA1                                     
160300     MOVE '  GE' TO GODK-STATUSKODER                                      
160400     CALL CBLTDLI USING GU PROC-PCB DLI-IO-AREA6 SSA1                     
160500     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
160600     PERFORM IMS-STATUSKONTROLL                                           
160700     .                                                                    
160800     EJECT                                                                
160900                                                                          
161000 IMS-GU-BENA          SECTION.                                            
161100                                                                          
161200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
161300          DELIMITED BY SIZE INTO SSA1                                     
161400     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
161500          DELIMITED BY SIZE INTO SSA2                                     
161600     MOVE '  GE' TO GODK-STATUSKODER                                      
161700     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA3 SSA1 SSA2                
161800     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
161900     PERFORM IMS-STATUSKONTROLL                                           
162000     .                                                                    
162100     EJECT                                                                
162200                                                                          
162300                                                                          
162400 IMS-GU-4732          SECTION.                                            
162500*                             FRAKTKOD => TRANSPORT-SÄTT                  
162600                                                                          
162700     STRING 'WL473201(WDGXKEY  =' W-4732-IDHTYP-X ')'                     
162800          DELIMITED BY SIZE INTO SSA1                                     
162900     MOVE 'WL473211 '        TO SSA2                                      
163000     MOVE '  GE' TO GODK-STATUSKODER                                      
163100     CALL CBLTDLI USING GU 4732-PCB DLI-IO-AREA4 SSA1 SSA2                
163200     MOVE 4732-STATUS-CODE TO STATUS-WS                                   
163300     PERFORM IMS-STATUSKONTROLL                                           
163400     .                                                                    
163500     EJECT                                                                
163600 IMS-GU-SATB-WDJ111-01 SECTION.                                           
163700     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
163800          DELIMITED BY SIZE INTO SSA1                                     
163900     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
164000          DELIMITED BY SIZE INTO SSA2                                     
164100     MOVE '  GE'              TO GODK-STATUSKODER                         
164200     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA5 SSA1 SSA2                
164300     MOVE SATB-STATUS-CODE    TO STATUS-WS                                
164400     PERFORM IMS-STATUSKONTROLL                                           
164500     .                                                                    
164600                                                                          
164700                                                                          
164800                                                                          
164900 IMS-GN-SATB-WDJ111-01 SECTION.                                           
165000     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
165100          DELIMITED BY SIZE INTO SSA1                                     
165200     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
165300          DELIMITED BY SIZE INTO SSA2                                     
165400     MOVE '  GEGB'            TO GODK-STATUSKODER                         
165500     CALL CBLTDLI USING GN SATB-PCB DLI-IO-AREA5 SSA1 SSA2                
165600     MOVE SATB-STATUS-CODE    TO STATUS-WS                                
165700     PERFORM IMS-STATUSKONTROLL                                           
165800     .                                                                    
165900 IMS-STATUSKONTROLL SECTION.                                              
166000                                                                          
166100     SET STATUS-IX TO 1                                                   
166200     SEARCH GODK-STATUS                                                   
166300       AT END CALL FELLOG                                                 
166400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
166500     END-SEARCH                                                           
166600     .                                                                    
166700     EJECT                                                                
166800*    -COPY WY2000P1                                                       
