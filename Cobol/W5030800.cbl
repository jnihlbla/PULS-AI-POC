000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5030800.                                                
000300 AUTHOR.         MARKUS ASPFJÄLL.                                         
000400 DATE-WRITTEN.   99/06/29.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
000900*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0178               
001000*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
001100*        AUTOMATJUSTERING, INVENTERINGSBILD                               
001200*        PROGRAMET GÖR ETT AV FÖLJANDE ALT.                               
001300*        1. OM HITTAT ANTAL KLARAR INVENTERINGSREGLERNA                   
001400*           UPPDATERAS INVENTERINGSHIST OCH POST TAS BORT IFRÅN           
001500*           INVENTERINGSKÖN. ARTIKELREGISTRER UPPDATERAS MED              
001600*           MED NYTT LAGERSALDO. EKONOMISKA BASER UPPDATERAS              
001700*                                                                         
001800*        2. OM HITTAT ANTAL INTE KLARAR REGLERNA UPPDATERAS BARA          
001900*           INVENTERINGSKÖN SÅ ATT ARTIKEL KAN OM INVENTERAS.             
002000*           PÅ LISTNR NOLLAS ALLT UTOM FÖRSTA SIFFRAN                     
002100*           1 = INVENTERAT EN GÅNG                                        
002200*           2 = INVENTERAT TVÅ GÅNGER                                     
002300*           3 = KAN INTE INVENTERAS FLER GÅNGER                           
002400*                                                                         
002500*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
002600*        PROGRAMMET UPPDATERAR WLARTS (WDK7)                              
002700*        PROGRAMMET UPPDATERAR WLINVA (WDH1)                              
002800*        PROGRAMMET UPPDATERAR WLINVC (WDH7)                              
002900*        PROGRAMMET UPPDATERAR        (WDR8)                              
003000*        PROGRAMMET UPPDATERAR WLSAPA (WDR9)                              
003100*        PROGRAMMET UPPDATERAR WLLOGA (WDL9)                              
003200*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
003300*                                                                         
003400*    INDATA.                                                              
003500*        TRANSAKTION: W5T308                                              
003600*                     W5T308U                                             
003700*        MID:         W5I30801                                            
003800*                                                                         
003900*    UTDATA.                                                              
004000*        MOD:         W5O30801                                            
004100*                                                                         
004200*                                                                         
004300***************************************************************           
004400*    ÄNDRINGAR:                                                           
004500*                                                                         
004600* 2014-04-08  E'TRACKER 10228775                                          
004700*             - RÄTTA UPPLÄGG AV ARTIKLAR PÅ WDL711 MED DC 11.            
004800*                                                                         
004900                                                                          
005000     SKIP3                                                                
005100 ENVIRONMENT DIVISION.                                                    
005200     EJECT                                                                
005300 DATA DIVISION.                                                           
005400 WORKING-STORAGE SECTION.                                                 
005500                                                                          
005600*    -- CHECKED BY WY2000                                                 
005700 77  IDPGM                       PIC X(08)   VALUE 'W5030800'.            
005800                                                                          
005900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
006000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
006100                                                                          
006200 77  WS-SECTION                  PIC X(32).                               
006300 77  WS-IMS                      PIC X(32).                               
006400 77  JA                          PIC X       VALUE 'J'.                   
006500 77  NEJ                         PIC X       VALUE 'N'.                   
006600 77  INDX                        PIC S9(9)   VALUE +0  COMP SYNC.         
006700 77  IX                          PIC S9(9)   VALUE +0  COMP SYNC.         
006800 77  MAX-INDX                    PIC S9(9)   VALUE +10 COMP SYNC.         
006900 77  TRANS-TID                   PIC 9(9).                                
007000 01  WS-IDLISTNR                 PIC 9(6)    VALUE ZERO.                  
007100 01  WS-IDPRTOMG                 PIC S9      VALUE ZERO  COMP-3.          
007200 01  WS-IDLOPNR                  PIC S9(5)   VALUE ZERO  COMP-3.          
007300 01  WS-IDUSER                   PIC X(8)    VALUE SPACE.                 
007400 01  WS-MSGI-IDUSER              PIC X(8)    VALUE SPACE.                 
007500 01  WS-ANTAL                    PIC S9(7)   VALUE ZERO  COMP-3.          
007600 01  WS-ANTAL-N                  PIC 9(7)    VALUE ZERO  COMP-3.          
007700 01  WS-PRARTSTD             PIC S9(7)V9(2)  VALUE ZERO  COMP-3.          
007800 01  WS-BELOPP               PIC S9(7)V9(2)  VALUE ZERO  COMP-3.          
007900 01  WS-TISEGKEY                 PIC 9(9)    VALUE ZERO.                  
008000 01  WS-TECKEN                   PIC X       VALUE SPACE.                 
008100 01  WS-KVLS-COMP                PIC S9(7)   VALUE ZERO  COMP-3.          
008200 01  WS-KVLS                     PIC S9(7)   VALUE ZERO.                  
008300 01  WS-CLAG-KVLS                PIC S9(7)   VALUE ZERO.                  
008400 01  WS-XLAG-KVLS                PIC S9(7)   VALUE ZERO.                  
008500 01  WS-KVLS-NEW                 PIC S9(7)   VALUE ZERO  COMP-3.          
008600 01  WS-LOGG-UREF1               PIC X(17)   VALUE ZERO.                  
008700 01  WS-KDSORT                   PIC X(2).                                
008800 01  WS-KDINVKAT                 PIC S9(3)   VALUE ZERO  COMP-3.          
008900 01  WS-KDJUSTYP                 PIC S9      VALUE ZERO  COMP-3.          
009000 01  WS-FYSANTAL                 PIC 9(6)    VALUE ZERO.                  
009100 01  WS-FYSANTAL-X               PIC X(6)    VALUE SPACE.                 
009200 01  WS-DIFFANTAL                PIC 9(6)    VALUE ZERO.                  
009300 01  WS-DIFFANTAL-X              PIC X(6)    VALUE SPACE.                 
009400 01  W-EKH-IDARTNR               PIC X(9)    VALUE SPACE.                 
009500 01  WS-ANTAL-EFR                PIC 9       VALUE ZERO.                  
009600 01  WS-TIINVDAT                 PIC 9(5)    VALUE ZERO COMP-3.           
009700 01 WS-SORT-ADLAGOMR             PIC 9(3)    VALUE ZERO.                  
009800 01 WS-SORT-ADGANG               PIC 9(3)    VALUE ZERO.                  
009900 01 WS-SORT-ADPLATS              PIC 9(5)    VALUE ZERO.                  
010000 01 WS-SORT-PRIO                 PIC 9       VALUE ZERO.                  
010100 01 WS-SORT-VVKL                 PIC 9       VALUE ZERO.                  
010200 01 WS-IDRT-KEY                  PIC X(2)    VALUE SPACE.                 
010300 01 WS-KVANTAL                   PIC S9(7)   VALUE ZERO  COMP-3.          
010400 01 W-PRAVCOST               PIC S9(7)V9(2) VALUE ZERO COMP-3.            
010500                                                                          
010600*    --- DIVERSE FAELT                                                    
010700 01  WS-DAGENS-DATUM            PIC 9(8).                                 
010800 01  WS-DAGENS-DATUM-GRP.                                                 
010900     03 DAGENS-AA               PIC 9(2).                                 
011000     03 DAGENS-AAMMDD           PIC 9(6).                                 
011100 01  WS-TISEGKEYAREA.                                                     
011200     03 WS-TIAAAAMMDDL          PIC 9(9)     VALUE ZERO.                  
011300     03 FILLER REDEFINES WS-TIAAAAMMDDL.                                  
011400        05 WS-TISEGKEY-DAT      PIC 9(8).                                 
011500        05 WS-TISEGKEY-LOPNR    PIC 9.                                    
011600 01  WS-HAENDELSE-TYPER.                                                  
011700     03 WS-EKH-KDEKSHT.                                                   
011800        05  WS-KDEKSHT-1        PIC 9(2).                                 
011900        05  WS-KDEKSHT-2        PIC 9.                                    
012000                                                                          
012100 01  WS-SPAR-HIST.                                                        
012200     03 WS-DAREGDAT-CRE         PIC 9(8).                                 
012300     03 WS-DAREGDAT-PR1         PIC 9(8).                                 
012400     03 WS-DAREGDAT-PR2         PIC 9(8).                                 
012500     03 WS-DAREGDAT-PR3         PIC 9(8).                                 
012600     03 WS-IDUSER-PR1           PIC X(8).                                 
012700     03 WS-IDUSER-PR2           PIC X(8).                                 
012800     03 WS-IDUSER-PR3           PIC X(8).                                 
012900     03 WS-IDUSER-CRE           PIC X(8).                                 
013000                                                                          
013100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
013200                                                                          
013300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
013400     88  INDATA-OK                           VALUE 'J'.                   
013500     88  INDATA-FEL                          VALUE 'N'.                   
013600                                                                          
013700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
013800     88  NYCKLAR-OK                          VALUE 'J'.                   
013900     88  NYCKLAR-FEL                         VALUE 'N'.                   
014000                                                                          
014100 77  TABELL-IFYLLD-SW            PIC X       VALUE 'N'.                   
014200     88  TABELL-IFYLLD                       VALUE 'J'.                   
014300     88  TABELL-TOM                          VALUE 'N'.                   
014400                                                                          
014500 77  TABELL-SLUT-SW              PIC X       VALUE 'N'.                   
014600     88  TABELL-SLUT                         VALUE 'J'.                   
014700     88  TABELL-FINNS                        VALUE 'N'.                   
014800                                                                          
014900 77  ALLT-OK-SW                  PIC X       VALUE 'J'.                   
015000     88  ALLT-OK                             VALUE 'J'.                   
015100     88  ALLT-FEL                            VALUE 'N'.                   
015200                                                                          
015300 77  POST-SW                     PIC X       VALUE 'N'.                   
015400     88  POST-FINNS                          VALUE 'J'.                   
015500     88  POST-SAKNAS                         VALUE 'N'.                   
015600                                                                          
015700 77  UPDATE-SW                   PIC X       VALUE 'N'.                   
015800     88  UPDATE-DONE                         VALUE 'J'.                   
015900     88  NO-UPDATE                           VALUE 'N'.                   
016000                                                                          
016100 77  FEL-SW                      PIC X       VALUE 'N'.                   
016200     88  FEL-FINNS                           VALUE 'J'.                   
016300     88  FEL-FINNS-EJ                        VALUE 'N'.                   
016400                                                                          
016500 77  FORTSATT-SW                 PIC X       VALUE 'J'.                   
016600     88  FORTSATT                            VALUE 'J'.                   
016700     88  FORTSATT-INTE                       VALUE 'N'.                   
016800                                                                          
016900 77  INFO-SW                     PIC X       VALUE 'J'.                   
017000     88  INFO-FINNS                          VALUE 'J'.                   
017100     88  INFO-SAKNAS                         VALUE 'N'.                   
017200                                                                          
017300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
017400     88  EGEN-MID                            VALUE '5308'.                
017500     88  GODK-MID                            VALUE '5301' '5302'          
017600                                                   '5303' '5304'          
017700                                                   '5305' '5306'          
017800                                                   '5307' '5308'          
017900                                                   '5309'.                
018000     88  HELP-MID                            VALUE '0551'.                
018100     EJECT                                                                
018200*    ÖVRIGT                                                               
018300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
018400 01  GENERELLA-SUBPROGRAM.                                                
018500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
018600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
018700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
018800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
018900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
019000     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
019100     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
019200     EJECT                                                                
019300*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
019400*01 -COPY WDATAREA                                                        
019500                                                                          
019600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
019700*01 -COPY WMEDAREA                                                        
019800                                                                          
019900*    ---- VALID IDDC CODES                                                
020000*01 -COPY WWDC99                                                          
020100                                                                          
020200*01 -COPY WWDCKONS                                                        
020300                                                                          
020400*    --- PARAMETRAR TILL SUBPROGRAM W009CIA                               
020500*01 -COPY W009CIA                                                         
020600     EJECT                                                                
020700                                                                          
020800     EJECT                                                                
020900 01  MESSAGE-CODES.                                                       
021000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
021100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
021200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
021300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
021400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
021500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
021600     03  KEYS-ARE-MISSING        PIC X(3)    VALUE '005'.                 
021700     03  NO-UPDATE-DONE          PIC X(3)    VALUE '034'.                 
021800     03  UPDATE-NOT-ALLOWED      PIC X(3)    VALUE '007'.                 
021810     03  USE-SCREEN-5106-INSTEAD PIC X(3)    VALUE '607'.                 
021900     EJECT                                                                
022000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
022100*                                                                         
022200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
022300     SKIP3                                                                
022400*01 -COPY WMSGINIT                                                        
022500     EJECT                                                                
022600                                                                          
022700*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
022800*                                                                         
022900 01  FILLER                      PIC X(16) VALUE 'SPAR-AREA '.            
023000 01  SPAR-AREA.                                                           
023100   03  SPAR-IDTRANS              PIC X(8)   VALUE '5308'.                 
023200   03  SPAR-IDUSER               PIC X(8)   VALUE  ZERO.                  
023300   03  SPAR-IDLISTNR             PIC 9(6)   VALUE  ZERO.                  
023400   03  SPAR-TABELL.                                                       
023500       05 TABELL OCCURS 10.                                               
023600         07 ART-TABELL.                                                   
023700           09 TAB-ARTIKEL         PIC S9(9) COMP-3 VALUE ZERO.            
023800           09 TAB-BENAMNING       PIC X(14)        VALUE SPACE.           
023900           09 TAB-KVAKS           PIC S9(7) COMP-3 VALUE ZERO.            
024000           09 TAB-KVLS            PIC S9(7) COMP-3 VALUE ZERO.            
024100           09 TAB-KVEFRS          PIC S9(7) COMP-3 VALUE ZERO.            
024200   03  SPAR-TABELL2.                                                      
024300       05 TABELL2 OCCURS 10.                                              
024400         07 ANTALS-TABELL.                                                
024500           09 TAB-ANTAL           PIC 9(7)         VALUE ZERO.            
024600           09 TAB-TECKEN          PIC X            VALUE SPACE.           
024700           09 TAB-DIFFERANS       PIC 9(7)         VALUE ZERO.            
024800           09 TAB-FLOMINV         PIC X            VALUE SPACE.           
024900   03  SPAR-FEL-TABELL.                                                   
025000       05 TABELL3 OCCURS 10.                                              
025100           09 FEL-ARTIKEL         PIC S9(9) COMP-3 VALUE ZERO.            
025200           09 FEL-BENAMNING       PIC X(14)        VALUE SPACE.           
025300           09 FEL-KVAKS           PIC S9(7) COMP-3 VALUE ZERO.            
025400           09 FEL-KVLS            PIC S9(7) COMP-3 VALUE ZERO.            
025500           09 FEL-KVEFRS          PIC S9(7) COMP-3 VALUE ZERO.            
025600           09 FEL-ANTAL           PIC 9(7)         VALUE ZERO.            
025700           09 FEL-TECKEN          PIC X            VALUE SPACE.           
025800           09 FEL-DIFFERANS       PIC 9(7)         VALUE ZERO.            
025900           09 FEL-FLOMINV         PIC X            VALUE SPACE.           
026000           09 FEL-RAD             PIC X            VALUE SPACE.           
026100     EJECT                                                                
026200 01  SORT-TAB-HJALP-AREA.                                                 
026300     03  TAB-SORT-MAX             PIC S9(9)  COMP    VALUE +10.           
026400     03  TAB-SORT-MIN             PIC S9(9)  COMP    VALUE +1.            
026500     03  TAB-SORT-STEG-LANGD      PIC S9(9)  COMP    VALUE +57.           
026600     03  TAB-SORT-VERKLIGT-ANTAL  PIC S9(9)  COMP    VALUE +0.            
026700     03  TAB-SORT-ANTAL           PIC S9(9)  COMP   VALUE +10.            
026800     03  TAB-SORT-LANGD           PIC S9(9)  COMP   VALUE +22.            
026900                                                                          
027000 01  FILLER                       PIC X(16) VALUE 'SORT-TABELL'.          
027100 01  SORT-TABELL.                                                         
027200     03  SORT-TABELL-AREA OCCURS 10 INDEXED BY SORT-IX.                   
027300        05 TAB-SORT-FAELT.                                                
027400          07 TAB-SORT-ADLAGOMR    PIC 9(3).                               
027500          07 TAB-SORT-ADGANG      PIC 9(3).                               
027600          07 TAB-SORT-ADPLATS     PIC 9(5).                               
027700          07 TAB-SORT-PRIO        PIC 9.                                  
027800          07 TAB-SORT-VVKL        PIC 9.                                  
027900          07 TAB-SORT-IDARTNR     PIC 9(9).                               
028000        05 TAB-SORT-BENAMNING     PIC X(14).                              
028100        05 TAB-SORT-KVAKS         PIC S9(7).                              
028200        05 TAB-SORT-KVLS          PIC S9(7).                              
028300        05 TAB-SORT-KVEFRS        PIC S9(7).                              
028400                                                                          
028500     EJECT                                                                
028600                                                                          
028700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
028800*                                                                         
028900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
029000     SKIP3                                                                
029100*01  MID -COPY W5I30801                                                   
029200     EJECT                                                                
029300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
029400     SKIP3                                                                
029500*01  -COPY WMSGAREA                                                       
029600     EJECT                                                                
029700     03  MOD REDEFINES MSG-AREA.                                          
029800*      05  -COPY W5O30801                                                 
029900     EJECT                                                                
030000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
030100     SKIP3                                                                
030200*01  -COPY WMFSAREA                                                       
030300     EJECT                                                                
030400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
030500*                                                                         
030600     EJECT                                                                
030700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
030800     SKIP3                                                                
030900 01  NYCKLAR-TILL-DLI.                                                    
031000     03  W-IDDC-X.                                                        
031100         05  W-IDDC              PIC X(2)     VALUE SPACE.                
031200                                                                          
031300     03  W-TISEGKEY-X.                                                    
031400         05  W-TISEGKEY          PIC S9(9)    VALUE ZERO COMP-3.          
031500                                                                          
031600     03  W-IDARTNR-X.                                                     
031700         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
031800                                                                          
031900     03  W-IDARTNR-MIN-X.                                                 
032000         05  W-IDARTNR-MIN       PIC S9(9)    VALUE ZERO COMP-3.          
032100                                                                          
032200     03  W-IDARTNR-MAX-X.                                                 
032300         05  W-IDARTNR-MAX       PIC S9(9)    VALUE ZERO COMP-3.          
032400                                                                          
032500     03  W-KDSEGKEY-X.                                                    
032600         05  W-KDSEGKEY          PIC X(1)          VALUE '1'.             
032700                                                                          
032800     03  W-KDRADSTA-X.                                                    
032900         05  W-KDRADSTA          PIC S9       VALUE +3 COMP-3.            
033000                                                                          
033100     03  W-WDH111KY-X.                                                    
033200         05  W-IDDC-WDH1         PIC X(2)          VALUE SPACE.           
033300         05  W-KDINVKAT-WDH1     PIC S9(3) COMP-3  VALUE ZERO.            
033400         05  W-TISEGKEY-WDH1     PIC S9(9) COMP-3  VALUE ZERO.            
033500         05  W-DAREGDAT-SORT     PIC  9(8)         VALUE ZERO.            
033600                                                                          
033700     03  W-WDH111KY-MIN-X.                                                
033800         05  W-IDDC-WDH1-MIN     PIC X(2)          VALUE SPACE.           
033900         05  W-KDINVKAT-WDH1-MIN PIC S9(3) COMP-3  VALUE ZERO.            
034000         05  W-TISEGKEY-WDH1-MIN PIC S9(9) COMP-3  VALUE ZERO.            
034100         05  W-DAREGDAT-SORT-MIN     PIC  9(8)     VALUE ZERO.            
034200                                                                          
034300     03  W-WDH111KY-MAX-X.                                                
034400        05  W-IDDC-WDH1-MAX     PIC X(2)          VALUE SPACE.            
034500        05  W-KDINVKAT-WDH1-MAX PIC S9(3) COMP-3  VALUE 999.              
034600        05  W-TISEGKEY-WDH1-MAX PIC S9(9) COMP-3 VALUE +999999999.        
034700        05  W-DAREGDAT-SORT-MAX     PIC  9(8)     VALUE 99999999.         
034800                                                                          
034900     03  W-IDARTNR-WDD3-X.                                                
035000         05  W-IDARTNR-WDD3      PIC S9(9)  VALUE ZERO COMP-3.            
035100                                                                          
035200     03  W-IDSKYLT-X.                                                     
035300         05  W-IDSKYLT           PIC X(3)   VALUE SPACE.                  
035400     03    W-WDH1BSEQ.                                                    
035500       05   W-SEQB-IDDC          PIC X(2)  VALUE SPACE.                   
035600       05   W-SEQB-DAREGDAT      PIC S9(9) VALUE ZERO      COMP-3.        
035700       05   W-SEQB-ADLAGOMR      PIC S9(3) VALUE ZERO      COMP-3.        
035800       05   W-SEQB-ADGANG        PIC S9(3) VALUE ZERO      COMP-3.        
035900       05   W-SEQB-ADPLATS       PIC S9(5) VALUE ZERO      COMP-3.        
036000       05   W-SEQB-IDPRTOMG      PIC S9    VALUE ZERO      COMP-3.        
036100       05   W-SEQB-IDLOPNR       PIC S9(5) VALUE ZERO      COMP-3.        
036200                                                                          
036300     03    W-WDH1BSEQ-MIN-X.                                              
036400       05   W-SEQB-IDDC-MIN      PIC X(2)  VALUE SPACE.                   
036500       05   W-SEQB-DAREGDAT-MIN  PIC S9(9) VALUE ZERO      COMP-3.        
036600       05   W-SEQB-ADLAGOMR-MIN  PIC S9(3) VALUE ZERO      COMP-3.        
036700       05   W-SEQB-ADGANG-MIN    PIC S9(3) VALUE ZERO      COMP-3.        
036800       05   W-SEQB-ADPLATS-MIN   PIC S9(5) VALUE ZERO      COMP-3.        
036900       05   W-SEQB-IDPRTOMG-MIN  PIC S9    VALUE ZERO      COMP-3.        
037000       05   W-SEQB-IDLOPNR-MIN   PIC S9(5) VALUE ZERO      COMP-3.        
037100                                                                          
037200     03    W-WDH1BSEQ-MAX-X.                                              
037300       05   W-SEQB-IDDC-MAX      PIC X(2)  VALUE SPACE.                   
037400       05   W-SEQB-DAREGDAT-MAX PIC S9(9) VALUE +999999999 COMP-3.        
037500       05   W-SEQB-ADLAGOMR-MAX  PIC S9(3) VALUE +999      COMP-3.        
037600       05   W-SEQB-ADGANG-MAX    PIC S9(3) VALUE +999      COMP-3.        
037700       05   W-SEQB-ADPLATS-MAX   PIC S9(5) VALUE +99999    COMP-3.        
037800       05   W-SEQB-IDPRTOMG-MAX  PIC S9    VALUE +9        COMP-3.        
037900       05   W-SEQB-IDLOPNR-MAX   PIC S9(5) VALUE +99999    COMP-3.        
038000                                                                          
038100     03 W-IDPRTINV-X.                                                     
038200       05   W-IDPRTOMG           PIC S9    COMP-3.                        
038300       05   W-IDLOPNR            PIC S9(5) COMP-3.                        
038400                                                                          
038500     03  W-IDDC-B6-X.                                                     
038600         05 W-IDDC-B6                  PIC X(2).                          
038700                                                                          
038800 01  W-WDGXKEY-ROT-X.                                                     
038900     03  FILLER              PIC X(4)  VALUE '5115'.                      
039000     03  FILLER              PIC X(26) VALUE LOW-VALUE.                   
039100                                                                          
039200 01  W-IDARTNR-UTR-X.                                                     
039300     03  W-IDDC-UTR          PIC X(2)   VALUE SPACE.                      
039400     03  W-IDARTNR-UTR       PIC S9(9)  VALUE ZERO COMP-3.                
039500     03  FILLER              PIC  X(8)  VALUE LOW-VALUE.                  
039600 01  W-WDGX-4505-KEY-X.                                                   
039700     03  W-IDHTYP-4505       PIC X(4)    VALUE '4505'.                    
039800     03  W-IDDC-4505         PIC X(2)    VALUE '11'.                      
039900     03  FILLER              PIC X(24) VALUE LOW-VALUE.                   
040000                                                                          
040100 01  FILLER                  PIC X(24) VALUE 'WDH111-SPARA'.              
040200 01  WDH111-SPARA.                                                        
040300     03  -COPY WDH111 -PRE  SPARA-                                        
040400                                                                          
040500     SKIP2                                                                
040600*    --- STATUS-KOD FRÅN IMS                                              
040700 01  STATUS-WS                   PIC XX.                                  
040800     88  SEGMENT-FINNS                       VALUE '  '.                  
040900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
041000     88  INDEX-FINNS-REDAN                   VALUE 'NI'.                  
041100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
041200     88  BAS-SLUT                            VALUE 'GB'.                  
041300     SKIP2                                                                
041400 01  GODK-STATUSKODER.                                                    
041500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
041600     SKIP3                                                                
041700 01  SSA1                        PIC X(132).                              
041800 01  SSA2                        PIC X(132).                              
041900 01  SSA3                        PIC X(132).                              
042000     EJECT                                                                
042100*    --- IMS FUNKTIONSKODER                                               
042200*01  -COPY W0003                                                          
042300     EJECT                                                                
042400*    ---  DLI INPUT-OUTPUT AREA                                           
042500                                                                          
042600 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK601'.           
042700 01  DLI-IO-WDK601.                                                       
042800*  03  -COPY WDK601.                                                      
042900                                                                          
043000 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK611'.           
043100 01  DLI-IO-WDK611.                                                       
043200*  03  -COPY WDK611.                                                      
043300                                                                          
043400 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK629'.           
043500 01  DLI-IO-WDK629.                                                       
043600*    03  -COPY WDK629                                                     
043700                                                                          
043800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTS01'.                    
043900 01  DLI-IO-WLARTS01.                                                     
044000*    03  -COPY WDK701                                                     
044100     EJECT                                                                
044200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTS11'.                    
044300 01  DLI-IO-WLARTS11.                                                     
044400*    03  -COPY WDK711                                                     
044500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH101'.                      
044600 01  DLI-IO-WDH101.                                                       
044700*    03  -COPY WDH101 -PRE INVA-                                          
044800     EJECT                                                                
044900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH111'.                      
045000 01  DLI-IO-WDH111.                                                       
045100*    03  -COPY WDH111 -PRE  INVA-                                         
045200     EJECT                                                                
045300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH121'.                      
045400 01  DLI-IO-WDH121.                                                       
045500*    03  -COPY WDH121 -PRE  INVA-                                         
045600     EJECT                                                                
045700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLINVB01'.                    
045800 01  DLI-IO-WLINVB01.                                                     
045900*    03  -COPY WDH1A1                                                     
046000     EJECT                                                                
046100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLINVC01'.                    
046200 01  DLI-IO-WLINVC01.                                                     
046300*    03  -COPY WDH701                                                     
046400     EJECT                                                                
046500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLINVC11'.                    
046600 01  DLI-IO-WLINVC11.                                                     
046700*    03  -COPY WDH711                                                     
046800     EJECT                                                                
046900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBENA01'.                    
047000 01  DLI-IO-WLBENA01.                                                     
047100*    03  -COPY WDD311                                                     
047200     EJECT                                                                
047300 01  FILLER                      PIC X(16) VALUE 'WLLOGA01'.              
047400*01  WLLOGA01    -COPY WDL901                                             
047500     EJECT                                                                
047600 01  FILLER                      PIC X(16) VALUE 'WLSAPA01'.              
047700*01  WLSAPA01    -COPY WDR901                                             
047800*    05 -COPY W510EKHA -RED FIL-WDR901-DATA                               
047900     EJECT                                                                
048000 01  FILLER                      PIC X(16) VALUE 'WFILBO1'.               
048100*01  WFILB01     -COPY WDR801 -PRE A08-                                   
048200*    05 -COPY W510A08  -RED A08-FIL-WDR801-DATA -PRE A08-                 
048300     EJECT                                                                
048400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDH1B1'.         
048500 01  DLI-IO-WDH1B1.                                                       
048600*    03 -COPY WDH111 -PRE SEQB-                                           
048700     EJECT                                                                
048800 01  FILLER              PIC X(16)   VALUE 'WDGX5116'.                    
048900*                                                                         
049000*01  WLXXEF11     -COPY WDGX5116.                                         
049100     EJECT                                                                
049200 01  FILLER              PIC X(16)   VALUE 'WDGX4506'.                    
049300*                                                                         
049400*01  WL450511     -COPY WDGX4506.                                         
049500                                                                          
049600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
049700 01   DLI-IO-AREA-B601.                                                   
049800*     03  -COPY WDB601                                                    
049900                                                                          
050000     EJECT                                                                
050100 LINKAGE SECTION.                                                         
050200*01  -COPY W0009   -PRE MSG-                                              
050300*01  -COPY W0008   -PRE USEA-                                             
050400     05  FILLER                  PIC X.                                   
050500                                                                          
050600*01  -COPY W0008  -PRE WDK6-                                              
050700     05  FILLER                  PIC X.                                   
050800                                                                          
050900*01  -COPY W0008  -PRE INVA-                                              
051000     05  FILLER                  PIC X.                                   
051100                                                                          
051200*01  -COPY W0008  -PRE INVC-                                              
051300     05  FILLER                  PIC X.                                   
051400                                                                          
051500*01  -COPY W0008  -PRE BENA-                                              
051600     05  FILLER                  PIC X.                                   
051700                                                                          
051800*01  -COPY W0008  -PRE LOGA-                                              
051900     05  FILLER                  PIC X.                                   
052000                                                                          
052100*01  -COPY W0008  -PRE SAPA-                                              
052200     05  FILLER                  PIC X.                                   
052300                                                                          
052400*01  -COPY W0008  -PRE WFILB-                                             
052500     05  FILLER                  PIC X.                                   
052600                                                                          
052700*01  -COPY W0008  -PRE WDH1B-                                             
052800     05  FILLER                  PIC X.                                   
052900                                                                          
053000*01  -COPY W0008  -PRE XXEF-                                              
053100     05  FILLER                  PIC X.                                   
053200                                                                          
053300*01  -COPY W0008  -PRE 4505-                                              
053400     05  FILLER                  PIC X.                                   
053500                                                                          
053600*01  -COPY W0008  -PRE ARTS-                                              
053700     05  FILLER                  PIC X.                                   
053800                                                                          
053900*01  -COPY W0008  -PRE WDB6-                                              
054000     05  FILLER                  PIC X.                                   
054100                                                                          
054200     EJECT                                                                
054300 PROCEDURE DIVISION  USING  MSG-PCB  USEA-PCB WDK6-PCB INVA-PCB           
054400                            INVC-PCB BENA-PCB LOGA-PCB SAPA-PCB           
054500                            WFILB-PCB WDH1B-PCB XXEF-PCB 4505-PCB         
054600                            ARTS-PCB WDB6-PCB.                            
054700 MAIN SECTION.                                                            
054800     ENTRY 'DLITCBL' USING  MSG-PCB  USEA-PCB WDK6-PCB INVA-PCB           
054900                            INVC-PCB BENA-PCB LOGA-PCB SAPA-PCB           
055000                            WFILB-PCB WDH1B-PCB XXEF-PCB 4505-PCB         
055100                            ARTS-PCB WDB6-PCB.                            
055200     PERFORM IMS-GET-MSG                                                  
055300     IF SEGMENT-FINNS                                                     
055400       PERFORM A-INIT                                                     
055500       PERFORM B-KOLLA-NYCKLAR                                            
055600       IF NYCKLAR-OK                                                      
055700         IF MFS-UPDATE                                                    
055800           PERFORM G-KOLLA-INPUT                                          
055900           IF INDATA-OK                                                   
056000             PERFORM H-UPPDATERA                                          
056100           ELSE                                                           
056200             PERFORM HFA-FYLL-FELTABELL                                   
056300             MOVE JA TO FEL-SW                                            
056400           END-IF                                                         
056500         ELSE                                                             
056600           IF MFS-FIRST                                                   
056700             PERFORM C-FOERSTA-SIDA                                       
056800           ELSE                                                           
056900             PERFORM D-SAMMA-SIDA                                         
057000           END-IF                                                         
057100         END-IF                                                           
057200         IF FEL-FINNS                                                     
057300           PERFORM I-LAES-VISA-FELTABELL                                  
057400         ELSE                                                             
057500           IF ALLT-OK                                                     
057600             PERFORM F-LAES-VISA-INFO                                     
057700           END-IF                                                         
057800         END-IF                                                           
057900       END-IF                                                             
058000       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O30801 + 4                      
058100       PERFORM IMS-INSERT-MSG                                             
058200     END-IF                                                               
058300                                                                          
058400     MOVE ZERO TO RETURN-CODE                                             
058500     GOBACK                                                               
058600     .                                                                    
058700     EJECT                                                                
058800 A-INIT SECTION.                                                          
058900                                                                          
059000     IF MSG-DUBBLA-TRANSKODER                                             
059100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I30801                 
059200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
059300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
059400     ELSE                                                                 
059500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I30801                  
059600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
059700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
059800     END-IF                                                               
059900                                                                          
060000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
060100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
060200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
060300     MOVE MSGI-IDRT-KEY TO WS-IDRT-KEY                                    
060400                                                                          
060500     MOVE LOW-VALUE TO MSG-AREA                                           
060600     MOVE 'W5O30801' TO MFS-IDMOD                                         
060700     MOVE '5308' TO MOD-IDTRANS                                           
060800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
060900                                                                          
061000     IF EGEN-MID OR HELP-MID                                              
061100       CONTINUE                                                           
061200     ELSE                                                                 
061300       MOVE SPACE TO MFS-KDTRTYP                                          
061400       MOVE '7' TO MFS-IDPFK                                              
061500     END-IF                                                               
061600     PERFORM S01-DATUM                                                    
061700     MOVE NEJ               TO FEL-SW                                     
061800     .                                                                    
061900     EJECT                                                                
062000 B-KOLLA-NYCKLAR SECTION.                                                 
062100                                                                          
062200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
062300     MOVE '001'             TO MSGI-KDCALL                                
062400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
062500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
062600     MOVE '5308'            TO MSGI-IDTRANS                               
062700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
062800     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
062900     IF GODK-MID                                                          
063000       MOVE MSGI-SPAR-AREA       TO SPAR-AREA                             
063100     END-IF                                                               
063200                                                                          
063300     MOVE JA                TO NYCKLAR-SW                                 
063400     MOVE MSGI-IDUSER TO WS-MSGI-IDUSER                                   
063500*----KONTROLLERA DC------------------------                               
063600     MOVE MSGI-IDDC         TO WS-IDDC                                    
063700                               W-IDDC-B6                                  
063800     PERFORM IMS-GU-WDB601                                                
063900                                                                          
064000                                                                          
064100     MOVE WS-IDDC           TO W-SEQB-IDDC-MIN                            
064200                               W-SEQB-IDDC-MAX                            
064300                               W-IDDC-WDH1                                
064400                               W-IDDC-WDH1-MIN                            
064500                               W-IDDC-WDH1-MAX                            
064600                               W-IDDC-UTR                                 
064700                               W-IDDC                                     
064800                                                                          
064900*----KONTROLLERA OK-----------------------                                
065000     IF MID-OK-IN NOT = ALL '+'                                           
065100       IF MID-OK-IN = 'J' OR 'Y'                                          
065200         MOVE JA TO MID-OK-IN                                             
065300       ELSE                                                               
065400         MOVE NEJ TO NYCKLAR-SW                                           
065500         MOVE MFS-RENSA-FAELT TO MOD-OK                                   
065600       END-IF                                                             
065700     ELSE                                                                 
065800       MOVE MFS-RENSA-FAELT TO MOD-OK                                     
065900     END-IF                                                               
066000*-----------------------------------------                                
066100                                                                          
066200                                                                          
066300*----KONTROLLERA LISTNR-------------------                                
066400     MOVE MFS-RENSA-FAELT             TO MOD-IDLISTNR-IN                  
066500       IF MID-IDLISTNR-IN  = ALL '+'                                      
066600         MOVE SPAR-IDLISTNR             TO WS-IDLISTNR                    
066700         IF WS-IDLISTNR NUMERIC                                           
066800*         MOVE SPAR-IDLISTNR(1:1)      TO W-SEQB-IDPRTOMG-MIN             
066900*                                          W-SEQB-IDPRTOMG-MAX            
067000          MOVE SPAR-IDLISTNR(1:1)      TO  WS-IDPRTOMG                    
067100                                           W-IDPRTOMG                     
067200*          MOVE SPAR-IDLISTNR(2:5)      TO W-SEQB-IDLOPNR-MIN             
067300*                                          W-SEQB-IDLOPNR-MAX             
067400          MOVE SPAR-IDLISTNR(2:5)       TO WS-IDLOPNR                     
067500                                           W-IDLOPNR                      
067600*         MOVE SPAR-IDLISTNR            TO W-IDPRTINV                     
067700         ELSE                                                             
067800           MOVE NEJ                     TO NYCKLAR-SW                     
067900         END-IF                                                           
068000       ELSE                                                               
068100         IF MID-IDLISTNR-IN NOT NUMERIC                                   
068200           MOVE NEJ                   TO NYCKLAR-SW                       
068300         ELSE                                                             
068400           MOVE MID-IDLISTNR-IN       TO WS-IDLISTNR                      
068500                                         SPAR-IDLISTNR                    
068600                                                                          
068700*          MOVE MID-IDLISTNR-IN(1:1)  TO W-SEQB-IDPRTOMG-MIN              
068800*                                        W-SEQB-IDPRTOMG-MAX              
068900           MOVE MID-IDLISTNR-IN(1:1)  TO WS-IDPRTOMG                      
069000                                         W-IDPRTOMG                       
069100*          MOVE MID-IDLISTNR-IN(2:5)  TO W-SEQB-IDLOPNR-MIN               
069200*                                        W-SEQB-IDLOPNR-MAX               
069300           MOVE MID-IDLISTNR-IN(2:5)  TO WS-IDLOPNR                       
069400                                         W-IDLOPNR                        
069500*          MOVE MID-IDLISTNR-IN       TO W-IDPRTINV                       
069600           MOVE '7'              TO MFS-IDPFK                             
069700           MOVE SPACE            TO MFS-KDTRTYP                           
069800         END-IF                                                           
069900       END-IF                                                             
070000*----KONTROLLERA IDUSER-------------------                                
070100     MOVE MFS-RENSA-FAELT     TO MOD-IDUSER-IN                            
070200                                                                          
070300      IF GODK-MID                                                         
070400       IF MID-IDUSER-IN = ALL '+'                                         
070500         IF SPAR-IDUSER NOT = ALL '+' AND EGEN-MID                        
070600           MOVE SPAR-IDUSER      TO WS-IDUSER                             
070700         ELSE                                                             
070800           MOVE NEJ              TO NYCKLAR-SW                            
070900         END-IF                                                           
071000       ELSE                                                               
071100         MOVE MID-IDUSER-IN      TO WS-IDUSER                             
071200                                    SPAR-IDUSER                           
071300       END-IF                                                             
071400      ELSE                                                                
071500        MOVE NEJ TO NYCKLAR-SW                                            
071600      END-IF                                                              
071700                                                                          
071800     IF EGEN-MID AND NYCKLAR-OK                                           
071900       MOVE WS-IDLISTNR          TO MOD-IDLISTNR-UT                       
072000       MOVE WS-IDUSER            TO MOD-IDUSER-UT                         
072100       MOVE WS-IDDC              TO MOD-IDDC-UT                           
072200     END-IF                                                               
072300                                                                          
072400     IF NYCKLAR-FEL                                                       
072500       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
072600       CALL WMEDKONV USING MED-WMEDAREA                                   
072700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
072800       PERFORM MFS-RENSA-FAELT-IN                                         
072900       PERFORM MFS-RENSA-FAELT-UT                                         
073000     END-IF                                                               
073100     IF NOT GODK-MID                                                      
073200       MOVE '002'         TO MSGI-KDCALL                                  
073300       MOVE '5302'        TO MSGI-IDTRANS                                 
073400       MOVE ALL '+' TO SPAR-AREA                                          
073500       MOVE SPAR-AREA     TO MSGI-SPAR-AREA                               
073600       CALL W005INIT USING MSGI-WMSGINIT                                  
073700            USEA-PCB                                                      
073800     END-IF                                                               
073900     .                                                                    
074000     EJECT                                                                
074100 C-FOERSTA-SIDA SECTION.                                                  
074200     PERFORM MFS-RENSA-FAELT-IN                                           
074300     MOVE +1 TO INDX                                                      
074400     PERFORM UNTIL INDX > MAX-INDX                                        
074500       MOVE MFS-RENSA-FAELT TO MID-FELRAD-IN(INDX)                        
074600       ADD +1 TO INDX                                                     
074700     END-PERFORM                                                          
074800     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
074900     CALL WMEDKONV USING MED-WMEDAREA                                     
075000     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
075100     .                                                                    
075200     EJECT                                                                
075300 D-SAMMA-SIDA SECTION.                                                    
075400     MOVE 'D-SAMMA' TO WS-SECTION                                         
075500     MOVE +1 TO INDX                                                      
075600     MOVE NEJ TO INFO-SW                                                  
075700     PERFORM UNTIL INDX > MAX-INDX OR INFO-FINNS                          
075800       IF MID-ANTAL-IN(INDX) NOT = ALL '+'                                
075900         MOVE JA                 TO INFO-SW                               
076000       END-IF                                                             
076100       IF MID-DIFFERANS-IN(INDX) NOT = ALL '+'                            
076200         MOVE JA                 TO INFO-SW                               
076300       END-IF                                                             
076400     ADD +1 TO INDX                                                       
076500     END-PERFORM                                                          
076600     IF INFO-FINNS                                                        
076700       MOVE +1 TO INDX                                                    
076800       PERFORM UNTIL INDX > MAX-INDX OR INFO-SAKNAS                       
076900         IF MID-FELRAD-IN(INDX) = 'J'                                     
077000           MOVE NEJ TO INFO-SW                                            
077100         END-IF                                                           
077200       ADD +1 TO INDX                                                     
077300       END-PERFORM                                                        
077400       IF INFO-FINNS                                                      
077500         MOVE +1 TO INDX                                                  
077600         PERFORM UNTIL INDX > MAX-INDX                                    
077700         IF TAB-ARTIKEL(INDX) NUMERIC                                     
077800           MOVE TAB-ARTIKEL  (INDX) TO MOD-IDARTNR  (INDX)                
077900           MOVE TAB-BENAMNING(INDX) TO MOD-BEART-SVE(INDX)                
078000           MOVE TAB-KVAKS    (INDX) TO MOD-KVAKS    (INDX)                
078100           MOVE TAB-KVLS     (INDX) TO MOD-KVLS     (INDX)                
078200           MOVE TAB-KVEFRS   (INDX) TO MOD-KVEFRS   (INDX)                
078300         END-IF                                                           
078400         ADD +1 TO INDX                                                   
078500         END-PERFORM                                                      
078600         PERFORM MFS-ROER-EJ-FAELT-IN                                     
078700         PERFORM MFS-LAES-IN-IGEN                                         
078800         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
078900         CALL WMEDKONV USING MED-WMEDAREA                                 
079000         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
079100         MOVE NEJ TO ALLT-OK-SW                                           
079200       ELSE                                                               
079300         MOVE JA TO ALLT-OK-SW                                            
079400         PERFORM MFS-RENSA-FAELT-IN                                       
079500       END-IF                                                             
079600     ELSE                                                                 
079700       MOVE JA TO ALLT-OK-SW                                              
079800       PERFORM MFS-RENSA-FAELT-IN                                         
079900     END-IF                                                               
080000     .                                                                    
080100     EJECT                                                                
080200 F-LAES-VISA-INFO SECTION.                                                
080300     MOVE 'F-VISA ' TO WS-SECTION                                         
080400     MOVE NEJ TO POST-SW                                                  
080500     MOVE ALL '+' TO SPAR-TABELL                                          
080600                     SPAR-TABELL2                                         
080700     MOVE SPACE   TO SPAR-FEL-TABELL                                      
080800     PERFORM FA-INITIERA-SORT-TABELL                                      
080900     PERFORM IMS-GU-WDH111-BSEQ                                           
081000     IF SEGMENT-FINNS                                                     
081100     MOVE +1 TO INDX                                                      
081200     SET SORT-IX TO +1                                                    
081300       PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS OR                 
081400                                        BAS-SLUT                          
081500         IF SEQB-INV-IDPRTOMG = WS-IDPRTOMG AND                           
081600            (SEQB-INV-IDLOPNR = WS-IDLOPNR) AND                           
081700            (SEQB-INV-FLINVSKR = 'J') AND                                 
081800            (SEQB-INV-FLINVBEH = 'N') AND                                 
081900            (SEQB-INV-IDDC     = W-IDDC)                                  
082000           MOVE JA                  TO POST-SW                            
082100           PERFORM IMS-GNP-WDH111-BSEQ                                    
082200                                                                          
082300           MOVE INVA-ART-IDARTNR    TO W-IDARTNR-WDD3                     
082400           IF DCS-SWEDEN                                                  
082500             MOVE 'S  '             TO W-IDSKYLT                          
082600           ELSE                                                           
082700             MOVE 'GB '             TO W-IDSKYLT                          
082800           END-IF                                                         
082900           PERFORM IMS-GET-BENA                                           
083000           PERFORM FB-FYLL-SORT-AREA                                      
083100           ADD +1                   TO INDX                               
083200           SET SORT-IX UP BY +1                                           
083300         END-IF                                                           
083400       PERFORM IMS-GN-WDH111-BSEQ                                         
083500       END-PERFORM                                                        
083600       IF TAB-SORT-BENAMNING(1) NOT = ALL '9'                             
083700         PERFORM FC-CALL-SORT                                             
083800         PERFORM FD-FYLL-SIDAN                                            
083900       END-IF                                                             
084000                                                                          
084100*      CALL FELLOG                                                        
084200                                                                          
084300       MOVE '002'              TO MSGI-KDCALL                             
084400       MOVE '5308'             TO SPAR-IDTRANS                            
084500       MOVE SPAR-AREA          TO MSGI-SPAR-AREA                          
084600       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
084700       IF POST-SAKNAS AND UPDATE-DONE                                     
084800         MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                            
084900         MOVE INF-UPDATE-DONE  TO MED-IDMFSINF                            
085000         CALL WMEDKONV USING MED-WMEDAREA                                 
085100         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
085200         MOVE MED-MFSINF       TO MOD-TEMFSINF                            
085300       ELSE                                                               
085400         IF POST-SAKNAS                                                   
085500           MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                          
085600           CALL WMEDKONV USING MED-WMEDAREA                               
085700           MOVE MED-MFSFEL       TO MOD-TEMFSFEL                          
085800           MOVE MFS-RENSA-FAELT  TO MOD-TEMFSINF                          
085900         END-IF                                                           
086000       END-IF                                                             
086100     ELSE                                                                 
086200       IF SEGMENT-SAKNAS AND UPDATE-DONE                                  
086300          MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                           
086400          MOVE INF-UPDATE-DONE  TO MED-IDMFSINF                           
086500          CALL WMEDKONV USING MED-WMEDAREA                                
086600          MOVE MED-MFSFEL       TO MOD-TEMFSFEL                           
086700          MOVE MED-MFSINF       TO MOD-TEMFSINF                           
086800          PERFORM MFS-RENSA-FAELT-UT                                      
086900          PERFORM MFS-RENSA-RAD-UT                                        
087000       ELSE                                                               
087100         IF SEGMENT-SAKNAS                                                
087200            MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                         
087300            CALL WMEDKONV USING MED-WMEDAREA                              
087400            MOVE MED-MFSFEL       TO MOD-TEMFSFEL                         
087500            MOVE MFS-RENSA-FAELT  TO MOD-TEMFSINF                         
087600            PERFORM MFS-RENSA-FAELT-UT                                    
087700            PERFORM MFS-RENSA-RAD-UT                                      
087800         END-IF                                                           
087900       END-IF                                                             
088000     END-IF                                                               
088100                                                                          
088200     .                                                                    
088300 FA-INITIERA-SORT-TABELL SECTION.                                         
088400     MOVE 'FA-INIT' TO WS-SECTION                                         
088500     SET SORT-IX TO +1                                                    
088600     PERFORM UNTIL (SORT-IX) > TAB-SORT-MAX                               
088700     MOVE ALL '9'       TO   TAB-SORT-ADLAGOMR   (SORT-IX)                
088800                             TAB-SORT-ADGANG     (SORT-IX)                
088900                             TAB-SORT-ADPLATS    (SORT-IX)                
089000                             TAB-SORT-PRIO       (SORT-IX)                
089100                             TAB-SORT-VVKL       (SORT-IX)                
089200                             TAB-SORT-IDARTNR    (SORT-IX)                
089300                             TAB-SORT-BENAMNING  (SORT-IX)                
089400                             TAB-SORT-KVAKS      (SORT-IX)                
089500                             TAB-SORT-KVLS       (SORT-IX)                
089600                             TAB-SORT-KVEFRS     (SORT-IX)                
089700                                                                          
089800     SET SORT-IX UP BY +1                                                 
089900     END-PERFORM                                                          
090000     .                                                                    
090100     EJECT                                                                
090200 FB-FYLL-SORT-AREA SECTION.                                               
090300      MOVE 'FB-FYLL' TO WS-SECTION                                        
090400      MOVE SEQB-INV-ADLAGOMR   TO WS-SORT-ADLAGOMR                        
090500      MOVE SEQB-INV-ADGANG     TO WS-SORT-ADGANG                          
090600      MOVE SEQB-INV-ADPLATS    TO WS-SORT-ADPLATS                         
090700      MOVE SEQB-INV-KDINVPRIO  TO WS-SORT-PRIO                            
090800      MOVE SEQB-INV-KDVVKL     TO WS-SORT-VVKL                            
090900                                                                          
091000      MOVE WS-SORT-ADLAGOMR    TO TAB-SORT-ADLAGOMR  (SORT-IX)            
091100      MOVE WS-SORT-ADGANG      TO TAB-SORT-ADGANG    (SORT-IX)            
091200      MOVE WS-SORT-ADPLATS     TO TAB-SORT-ADPLATS   (SORT-IX)            
091300      MOVE WS-SORT-PRIO        TO TAB-SORT-PRIO      (SORT-IX)            
091400      MOVE WS-SORT-VVKL        TO TAB-SORT-VVKL      (SORT-IX)            
091500      MOVE SEQB-INV-KVAKS-OLD  TO TAB-SORT-KVAKS     (SORT-IX)            
091600      MOVE SEQB-INV-KVLS-OLD   TO TAB-SORT-KVLS      (SORT-IX)            
091700      MOVE SEQB-INV-KVEFRS-OLD TO TAB-SORT-KVEFRS    (SORT-IX)            
091800      MOVE INVA-ART-IDARTNR    TO TAB-SORT-IDARTNR   (SORT-IX)            
091900      MOVE TEXT-BEART          TO TAB-SORT-BENAMNING (SORT-IX)            
092000                                                                          
092100     .                                                                    
092200     EJECT                                                                
092300 FC-CALL-SORT SECTION.                                                    
092400     MOVE 'FC-CALL' TO WS-SECTION                                         
092500     CALL WINTSOR USING SORT-TABELL                                       
092600                  TAB-SORT-STEG-LANGD                                     
092700                  TAB-SORT-ANTAL                                          
092800                  TAB-SORT-FAELT(1)                                       
092900                  TAB-SORT-LANGD                                          
093000                                                                          
093100     .                                                                    
093200     EJECT                                                                
093300 FD-FYLL-SIDAN SECTION.                                                   
093400     MOVE 'FD-FYLL' TO WS-SECTION                                         
093500     MOVE +1 TO INDX                                                      
093600     SET SORT-IX TO +1                                                    
093700     PERFORM UNTIL  SORT-IX > MAX-INDX OR                                 
093800             TAB-SORT-BENAMNING(SORT-IX) = ALL '9'                        
093900       MOVE TAB-SORT-KVAKS(SORT-IX)     TO MOD-KVAKS     (INDX)           
094000                                           TAB-KVAKS     (INDX)           
094100       MOVE TAB-SORT-KVLS (SORT-IX)     TO MOD-KVLS      (INDX)           
094200                                           TAB-KVLS      (INDX)           
094300       MOVE TAB-SORT-KVEFRS (SORT-IX)   TO MOD-KVEFRS    (INDX)           
094400                                           TAB-KVEFRS    (INDX)           
094500       MOVE TAB-SORT-IDARTNR(SORT-IX)   TO MOD-IDARTNR   (INDX)           
094600                                           TAB-ARTIKEL   (INDX)           
094700       MOVE TAB-SORT-BENAMNING(SORT-IX) TO MOD-BEART-SVE (INDX)           
094800                                           TAB-BENAMNING (INDX)           
094900       MOVE 'N'                         TO MOD-FELRAD-UT (INDX)           
095000       MOVE MFS-RENSA-FAELT             TO MOD-ANTAL     (INDX)           
095100                                           MOD-TECKEN    (INDX)           
095200                                           MOD-DIFFERANS (INDX)           
095300       ADD +1 TO INDX                                                     
095400       SET SORT-IX UP BY +1                                               
095500     END-PERFORM                                                          
095600     IF  (SORT-IX) <  MAX-INDX                                            
095700       PERFORM UNTIL (SORT-IX) > MAX-INDX                                 
095800         PERFORM MFS-RENSA-RADEN-UT                                       
095900         MOVE 'N'               TO MOD-FELRAD-UT (INDX)                   
096000         ADD +1 TO INDX                                                   
096100         SET SORT-IX UP BY +1                                             
096200       END-PERFORM                                                        
096300     END-IF                                                               
096400     .                                                                    
096500     EJECT                                                                
096600 G-KOLLA-INPUT SECTION.                                                   
096700     MOVE 'G-KOLLA' TO WS-SECTION                                         
096800     MOVE JA  TO INDATA-SW                                                
096900     MOVE NEJ TO FEL-SW                                                   
097000     MOVE +1 TO INDX                                                      
097100                                                                          
097200     IF SPAR-IDLISTNR(1:1) > 2                                            
097300       MOVE NEJ          TO INDATA-SW                                     
097400       MOVE USE-SCREEN-5106-INSTEAD TO MED-IDMFSINF                       
097500       CALL WMEDKONV USING MED-WMEDAREA                                   
097600       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
097700       MOVE +1 TO INDX                                                    
097800       PERFORM UNTIL INDX > MAX-INDX                                      
097900       IF MID-ANTAL-IN(INDX) NOT = ALL '+'                                
098000         MOVE MFS-NUM-FAELT-FEL TO MOD-ANTAL-ATTR (INDX)                  
098100       ELSE                                                               
098200         IF MID-DIFFERANS-IN (INDX) NOT = ALL '+'                         
098300           MOVE MFS-NUM-FAELT-FEL TO MOD-DIFFERANS-ATTR(INDX)             
098400           MOVE MFS-ALFA-FAELT-FEL TO MOD-TECKEN-ATTR(INDX)               
098500         END-IF                                                           
098600       END-IF                                                             
098700       IF MID-FLOMINV-IN(INDX) NOT = ALL'+'                               
098800         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLOMINV-ATTR(INDX)                
098900       END-IF                                                             
099000       ADD +1 TO INDX                                                     
099100       END-PERFORM                                                        
099200       PERFORM MFS-ROER-EJ-FAELT-UT                                       
099300       PERFORM MFS-ROER-EJ-FAELT-IN                                       
099400     ELSE                                                                 
099500*-- KONTROLLERAR OM TABELLEN ÄR IFYLLD                                    
099600                                                                          
099700       IF INDATA-OK                                                       
099800       MOVE NEJ TO TABELL-IFYLLD-SW                                       
099900         PERFORM UNTIL INDX > MAX-INDX                                    
100000           IF MID-ANTAL-IN(INDX) NOT = ALL '+'                            
100100             MOVE JA  TO TABELL-IFYLLD-SW                                 
100200                                                                          
100300           END-IF                                                         
100400           IF MID-DIFFERANS-IN(INDX) NOT = ALL '+'                        
100500             MOVE JA  TO TABELL-IFYLLD-SW                                 
100600                                                                          
100700           END-IF                                                         
100800           IF MID-FLOMINV-IN(INDX) NOT = ALL '+'                          
100900             MOVE JA  TO TABELL-IFYLLD-SW                                 
101000                                                                          
101100           END-IF                                                         
101200           IF TABELL-IFYLLD                                               
101300             MOVE 10 TO INDX                                              
101400           END-IF                                                         
101500         ADD +1 TO INDX                                                   
101600         END-PERFORM                                                      
101700       END-IF                                                             
101800                                                                          
101900***-------------------------------------------                            
102000***----KONTROLLERA SÅ ATT TABELL INTE ÄR IFYLLD SAMTIDIGT SOM             
102100***----OK FALTET                                                          
102200       IF TABELL-IFYLLD AND INDATA-OK AND MID-OK-IN = JA                  
102300         MOVE NEJ TO INDATA-SW                                            
102400         MOVE MFS-ALFA-FAELT-FEL TO MOD-OK-ATTR                           
102500       END-IF                                                             
102600       IF INDATA-OK AND TABELL-TOM                                        
102700         IF MID-OK-IN NOT = ALL '+'                                       
102800           IF MID-OK-IN = JA                                              
102900             MOVE MFS-ALFA-FAELT-RAETT TO MOD-OK-ATTR                     
103000           ELSE                                                           
103100             MOVE NEJ TO INDATA-SW                                        
103200             MOVE MFS-ALFA-FAELT-FEL TO MOD-OK-ATTR                       
103300           END-IF                                                         
103400         END-IF                                                           
103500       END-IF                                                             
103600                                                                          
103700       IF MID-OK-IN = ALL '+' AND TABELL-TOM AND INDATA-OK                
103800         MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                        
103900         CALL WMEDKONV USING MED-WMEDAREA                                 
104000         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
104100         PERFORM MFS-ROER-EJ-FAELT-IN                                     
104200         PERFORM MFS-ROER-EJ-FAELT-UT                                     
104300         MOVE NEJ TO INDATA-SW                                            
104400       ELSE                                                               
104500                                                                          
104600***----KONTROLLERA TABELL RADER-------------                              
104700***----OM VÄRDEN ÄR NUMERISKA   ------------                              
104800***----OM FLOMINV ÄR IFYLLD     ------------                              
104900                                                                          
105000         IF INDATA-OK                                                     
105100           MOVE +1                   TO INDX                              
105200           PERFORM UNTIL INDX > MAX-INDX                                  
105300             IF MID-ANTAL-IN (INDX) NOT = ALL '+'                         
105400                MOVE MID-ANTAL-IN (INDX) TO WS-FYSANTAL-X                 
105500                                                                          
105600                INSPECT WS-FYSANTAL-X REPLACING                           
105700                        LEADING SPACE BY ZERO                             
105800              IF WS-FYSANTAL-X NOT NUMERIC                                
105900                MOVE NEJ              TO INDATA-SW                        
106000                MOVE 'J' TO FEL-RAD(INDX)                                 
106100                MOVE MFS-NUM-FAELT-FEL TO MOD-ANTAL-ATTR (INDX)           
106200              ELSE                                                        
106300               MOVE WS-FYSANTAL-X       TO TAB-ANTAL (INDX)               
106400               IF MID-FLOMINV-IN(INDX) = ALL '+'                          
106500                MOVE MFS-NUM-FAELT-RAETT TO MOD-ANTAL-ATTR (INDX)         
106600               ELSE                                                       
106700                MOVE MFS-ALFA-FAELT-FEL TO MOD-FLOMINV-ATTR (INDX)        
106800                MOVE NEJ           TO INDATA-SW                           
106900                MOVE 'J' TO FEL-RAD(INDX)                                 
107000               END-IF                                                     
107100              END-IF                                                      
107200             END-IF                                                       
107300                                                                          
107400             IF MID-DIFFERANS-IN (INDX) NOT = ALL '+'                     
107500               MOVE MID-DIFFERANS-IN(INDX) TO WS-DIFFANTAL-X              
107600                INSPECT WS-DIFFANTAL-X REPLACING                          
107700                        LEADING SPACE BY ZERO                             
107800              IF WS-DIFFANTAL-X NOT NUMERIC                               
107900                MOVE NEJ              TO INDATA-SW                        
108000                MOVE 'J' TO FEL-RAD(INDX)                                 
108100                MOVE MFS-NUM-FAELT-FEL TO                                 
108200                     MOD-DIFFERANS-ATTR (INDX)                            
108300              ELSE                                                        
108400                MOVE WS-DIFFANTAL-X TO TAB-DIFFERANS (INDX)               
108500                IF MID-ANTAL-IN (INDX) NOT = ALL '+' AND                  
108600                   (MID-DIFFERANS-IN (INDX) NOT = ALL '+')                
108700                  MOVE MFS-NUM-FAELT-FEL TO MOD-ANTAL-ATTR (INDX)         
108800                                         MOD-DIFFERANS-ATTR (INDX)        
108900                  MOVE NEJ               TO INDATA-SW                     
109000                  MOVE 'J' TO FEL-RAD(INDX)                               
109100                ELSE                                                      
109200                 MOVE WS-DIFFANTAL-X TO TAB-DIFFERANS (INDX)              
109300                 IF MID-FLOMINV-IN(INDX) = ALL '+'                        
109400                   MOVE MFS-NUM-FAELT-RAETT TO                            
109500                                    MOD-DIFFERANS-ATTR(INDX)              
109600                 ELSE                                                     
109700                   MOVE MFS-ALFA-FAELT-FEL TO                             
109800                        MOD-FLOMINV-ATTR (INDX)                           
109900                   MOVE NEJ           TO INDATA-SW                        
110000                   MOVE 'J' TO FEL-RAD(INDX)                              
110100                 END-IF                                                   
110200                END-IF                                                    
110300              END-IF                                                      
110400             END-IF                                                       
110500                                                                          
110600             IF MID-TECKEN-IN (INDX) NOT = ' '                            
110700               IF MID-TECKEN-IN (INDX)  = '-' OR '+'                      
110800                IF MID-DIFFERANS-IN(INDX) NOT = ALL '+' AND               
110900                   (MID-ANTAL-IN (INDX) = ALL '+')                        
111000                 MOVE MFS-ALFA-FAELT-RAETT TO                             
111100                      MOD-TECKEN-ATTR(INDX)                               
111200                 MOVE MID-TECKEN-IN (INDX) TO TAB-TECKEN (INDX)           
111300                ELSE                                                      
111400                 MOVE NEJ              TO INDATA-SW                       
111500                 MOVE 'J' TO FEL-RAD(INDX)                                
111600                 MOVE MFS-ALFA-FAELT-FEL TO MOD-TECKEN-ATTR (INDX)        
111700                END-IF                                                    
111800               ELSE                                                       
111900                 MOVE NEJ              TO INDATA-SW                       
112000                 MOVE 'J' TO FEL-RAD(INDX)                                
112100                 MOVE MFS-ALFA-FAELT-FEL TO MOD-TECKEN-ATTR (INDX)        
112200               END-IF                                                     
112300             END-IF                                                       
112400             IF MID-FLOMINV-IN(INDX) NOT = ALL '+'                        
112500               IF MID-FLOMINV-IN(INDX) = 'J' OR 'Y'                       
112600                 MOVE MID-FLOMINV-IN(INDX) TO TAB-FLOMINV (INDX)          
112700                 MOVE MFS-ALFA-FAELT-RAETT TO                             
112800                      MOD-FLOMINV-ATTR (INDX)                             
112900               ELSE                                                       
113000                 MOVE MFS-ALFA-FAELT-FEL TO                               
113100                      MOD-FLOMINV-ATTR (INDX)                             
113200                 MOVE NEJ              TO INDATA-SW                       
113300                 MOVE 'J' TO FEL-RAD(INDX)                                
113400               END-IF                                                     
113500             END-IF                                                       
113600           ADD +1                 TO INDX                                 
113700           END-PERFORM                                                    
113800         END-IF                                                           
113900                                                                          
114000         IF INDATA-FEL                                                    
114100*          MOVE JA TO FEL-SW                                              
114200           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
114300           CALL WMEDKONV USING MED-WMEDAREA                               
114400           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
114500*          MOVE 'FYLL I FYS ANT' TO MOD-TEMFSINF                          
114600           PERFORM MFS-ROER-EJ-FAELT-UT                                   
114700           PERFORM MFS-ROER-EJ-FAELT-IN                                   
114800         END-IF                                                           
114900                                                                          
115000      END-IF                                                              
115100     END-IF                                                               
115200     .                                                                    
115300     EJECT                                                                
115400 H-UPPDATERA SECTION.                                                     
115500*------KONTROLLER FÖR INVENTERINGNEN                                      
115600     MOVE 'H-UPPDA' TO WS-SECTION                                         
115700     MOVE +1 TO INDX                                                      
115800     MOVE +1 TO IX                                                        
115900     MOVE NEJ TO UPDATE-SW                                                
116000     MOVE NEJ TO FEL-SW                                                   
116100     PERFORM UNTIL INDX > MAX-INDX                                        
116200     IF TAB-ARTIKEL(INDX) NUMERIC                                         
116300       MOVE TAB-ARTIKEL(INDX) TO W-IDARTNR                                
116400       IF MID-OK-IN = JA                                                  
116500         IF W-IDARTNR NUMERIC                                             
116600           PERFORM IMS-GET-ARTC-ART                                       
116700           PERFORM IMS-GET-ARTC-CLAG                                      
116800           MOVE CLAG-PRARTSTD   TO WS-PRARTSTD                            
116900           IF CDC-SE                                                      
117000             MOVE +0            TO CLAG-KVUTRS                            
117100             MOVE +0            TO CLAG-KVINVS                            
117200             MOVE WS-TIINVDAT   TO CLAG-TIINVDAT                          
117300             PERFORM IMS-REPL-ARTC-CLAG                                   
117400             MOVE ZERO          TO WS-ANTAL                               
117500             MOVE '+'           TO WS-TECKEN                              
117600             MOVE JA            TO ALLT-OK-SW                             
117700           ELSE                                                           
117800             PERFORM IMS-GHU-WDK711                                       
117900             MOVE +0          TO SLAG-KVUTRS                              
118000             MOVE +0          TO SLAG-KVINVS                              
118100             MOVE WS-TIINVDAT TO SLAG-TIINVDAT                            
118200             PERFORM IMS-REPL-SLAGER-SEGM                                 
118300             MOVE ZERO        TO WS-ANTAL                                 
118400             MOVE '+'         TO WS-TECKEN                                
118500             MOVE JA          TO ALLT-OK-SW                               
118600           END-IF                                                         
118700         END-IF                                                           
118800       ELSE                                                               
118900         IF MID-ANTAL-IN (INDX) NOT = ALL '+' OR                          
119000            MID-DIFFERANS-IN (INDX) NOT = ALL '+' OR                      
119100            MID-FLOMINV-IN(INDX) NOT = ALL '+'                            
119200           MOVE TAB-ARTIKEL (INDX)    TO  W-IDARTNR                       
119300           PERFORM HA-LAES-WDK6                                           
119400           PERFORM HAA-KOLLA-DIFF                                         
119500           PERFORM HAB-KOLLA-MED-REGLER                                   
119600           IF ALLT-FEL OR MID-FLOMINV-IN(INDX) NOT = ALL '+'              
119700             MOVE 'J' TO FEL-RAD(IX)                                      
119800             PERFORM HF-FYLL-FELTABELL                                    
119900             PERFORM HG-NOLLA-LISTNR                                      
120000             MOVE JA  TO FEL-SW                                           
120100           END-IF                                                         
120200         ELSE                                                             
120300           IF TAB-ARTIKEL (INDX) NUMERIC                                  
120400             MOVE 'N' TO FEL-RAD(IX)                                      
120500             PERFORM HF-FYLL-FELTABELL                                    
120600             MOVE NEJ TO FORTSATT-SW                                      
120700           END-IF                                                         
120800         END-IF                                                           
120900       END-IF                                                             
121000       IF ALLT-OK AND FORTSATT                                            
121100         PERFORM HAC-KOLLA-MED-WDH1                                       
121200         IF ALLT-OK AND FORTSATT                                          
121300           IF CDC-SE                                                      
121400*            PERFORM IMS-GET-ARTC-ART                                     
121500*            PERFORM IMS-GET-ARTC-CLAG                                    
121600             MOVE +0 TO CLAG-KVUTRS                                       
121700             PERFORM IMS-REPL-ARTC-CLAG                                   
121800**** OM DET ÄR EN REFILLARTIKEL SÅ SKALL FLAGGA SÄTTAS TILL N             
121900             PERFORM IMS-GHU-WDK629                                       
122000             IF SEGMENT-FINNS                                             
122100               IF CREF-FLREFNYO = JA                                      
122200                 MOVE NEJ         TO CREF-FLREFNYO                        
122300                 PERFORM IMS-REPL-WDK629                                  
122400               END-IF                                                     
122500             END-IF                                                       
122600             COMPUTE WS-KVANTAL =                                         
122700              CLAG-KVLS + CLAG-KVEFRS + CLAG-KVAKS-CDC                    
122800           ELSE                                                           
122900             IF DCS-SDC OR DCS-NDC-PF OR DCS-NDC-NA                       
123000               MOVE +0 TO SLAG-KVUTRS                                     
123100               PERFORM IMS-REPL-SLAGER-SEGM                               
123200             END-IF                                                       
123300           END-IF                                                         
123400           PERFORM HB-UPPDATERA-WDH1                                      
123500           IF ALLT-OK                                                     
123600             PERFORM HC-UPPDATERA-WDH7                                    
123700             IF ALLT-OK                                                   
123800               IF DCS-NDC-NA                                              
123900                 IF WS-ANTAL NOT = 0                                      
124000                   PERFORM HK-FYLL-WDR8-AREA                              
124100                   PERFORM HKA-UPPDATERA-WDR8                             
124200                 END-IF                                                   
124300               ELSE                                                       
124400                 IF WS-ANTAL NOT = 0                                      
124500                   PERFORM HD-FYLL-WDR9-AREA                              
124600                   PERFORM HDA-UPPDATERA-WDR9                             
124700                 END-IF                                                   
124800               END-IF                                                     
124900               IF ALLT-OK                                                 
125000                 PERFORM HE-FYLL-WDL9-AREA                                
125100                 PERFORM HEA-UPPDATERA-WDL9                               
125200                 IF ALLT-OK                                               
125300                   IF CDC-SE                                              
125400                     PERFORM HH-TAECKNING-CDC                             
125500                   END-IF                                                 
125600                   PERFORM HI-RADERA-UTREDSALDO                           
125700                   PERFORM HJ-UPPDATERA-WDK7                              
125800                   MOVE JA TO UPDATE-SW                                   
125900                 END-IF                                                   
126000               END-IF                                                     
126100             END-IF                                                       
126200           END-IF                                                         
126300         END-IF                                                           
126400       END-IF                                                             
126500     END-IF                                                               
126600     ADD +1 TO INDX                                                       
126700     END-PERFORM                                                          
126800     IF  UPDATE-DONE AND FEL-FINNS-EJ                                     
126900       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
127000       CALL WMEDKONV USING MED-WMEDAREA                                   
127100       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
127200       PERFORM MFS-FORM-ATTR                                              
127300       PERFORM MFS-RENSA-FAELT-IN                                         
127400     END-IF                                                               
127500     IF FEL-FINNS                                                         
127600      IF DCS-SWEDEN                                                       
127700       MOVE 'UPPLYSTA FÄLT GÅR TILL OMINVENTERING' TO MOD-TEMFSINF        
127800      ELSE                                                                
127900       MOVE 'HIGHLIT FIELD GOES TO REINVENT.'  TO MOD-TEMFSINF            
128000      END-IF                                                              
128100       MOVE NEJ TO ALLT-OK-SW                                             
128200     END-IF                                                               
128300                                                                          
128400     .                                                                    
128500     EJECT                                                                
128600 HA-LAES-WDK6 SECTION.                                                    
128700     MOVE 'HA-LAES' TO WS-SECTION                                         
128800     MOVE JA                  TO ALLT-OK-SW                               
128900     MOVE JA                  TO FORTSATT-SW                              
129000     PERFORM IMS-GET-ARTC-ART                                             
129100     IF SEGMENT-FINNS                                                     
129200       MOVE ART-KDSORT          TO WS-KDSORT                              
129300       PERFORM IMS-GET-ARTC-CLAG                                          
129400       IF SEGMENT-FINNS                                                   
129500         MOVE CLAG-PRARTSTD     TO WS-PRARTSTD                            
129600         MOVE CLAG-KVLS         TO WS-XLAG-KVLS                           
129700       ELSE                                                               
129800         MOVE NEJ               TO ALLT-OK-SW                             
129900       END-IF                                                             
130000     ELSE                                                                 
130100       MOVE NEJ                 TO ALLT-OK-SW                             
130200     END-IF                                                               
130300     IF DCS-SDC OR DCS-NDC-PF OR DCS-NDC-NA                               
130400       PERFORM IMS-GHU-WDK711                                             
130500       MOVE SLAG-KVLS           TO WS-XLAG-KVLS                           
130600       MOVE SLAG-PRAVCOST       TO W-PRAVCOST                             
130700     END-IF                                                               
130800                                                                          
130900     .                                                                    
131000     EJECT                                                                
131100 HAA-KOLLA-DIFF SECTION.                                                  
131200     MOVE 'HAA-KOL' TO WS-SECTION                                         
131300     IF ALLT-OK                                                           
131400       COMPUTE WS-KVLS = TAB-KVEFRS(INDX) + TAB-KVLS(INDX)                
131500*--- POSITIVT LAGERSALDO                                                  
131600*--- FYSISKT ANTAL IFRÅN BILDEN                                           
131700       IF WS-KVLS >= 0                                                    
131800         IF TAB-ANTAL (INDX) NOT = ALL '+'                                
131900           IF TAB-ANTAL (INDX) >= WS-KVLS                                 
132000                                                                          
132100             COMPUTE WS-ANTAL = TAB-ANTAL(INDX) - WS-KVLS                 
132200             COMPUTE WS-KVLS-NEW = WS-XLAG-KVLS + WS-ANTAL                
132300           ELSE                                                           
132400                                                                          
132500             COMPUTE WS-ANTAL = TAB-ANTAL (INDX) - WS-KVLS                
132600             COMPUTE WS-KVLS-NEW = WS-ANTAL + WS-XLAG-KVLS                
132700                                                                          
132800           END-IF                                                         
132900           IF WS-ANTAL >= 0                                               
133000             MOVE '+' TO WS-TECKEN                                        
133100           ELSE                                                           
133200             MOVE '-' TO WS-TECKEN                                        
133300           END-IF                                                         
133400         ELSE                                                             
133500*--- DIFFERANS MELLAN KVLS OCH HITTAT ANTAL                               
133600           IF TAB-DIFFERANS (INDX) NOT = ALL '+'                          
133700             IF TAB-TECKEN (INDX) = '+'                                   
133800              COMPUTE WS-ANTAL    = TAB-DIFFERANS (INDX)                  
133900                                                                          
134000              COMPUTE WS-KVLS-NEW = WS-ANTAL + WS-XLAG-KVLS               
134100             ELSE                                                         
134200              COMPUTE WS-ANTAL = TAB-DIFFERANS(INDX)                      
134300                                                                          
134400              COMPUTE WS-KVLS-NEW = WS-XLAG-KVLS - WS-ANTAL               
134500             END-IF                                                       
134600             IF TAB-TECKEN (INDX) = '+'                                   
134700               MOVE '+'                      TO WS-TECKEN                 
134800             ELSE                                                         
134900               MOVE '-'                      TO WS-TECKEN                 
135000               COMPUTE WS-ANTAL =  WS-ANTAL * -1                          
135100             END-IF                                                       
135200           END-IF                                                         
135300         END-IF                                                           
135400       ELSE                                                               
135500*--- NEGATIVT LAGERSALDO                                                  
135600         IF TAB-ANTAL (INDX) NOT = ALL '+'                                
135700           IF TAB-ANTAL (INDX) > WS-KVLS                                  
135800             MOVE '+'                      TO WS-TECKEN                   
135900           ELSE                                                           
136000             MOVE '-'                      TO WS-TECKEN                   
136100           END-IF                                                         
136200           COMPUTE WS-ANTAL    = TAB-ANTAL(INDX) - WS-KVLS                
136300           COMPUTE WS-KVLS-NEW = WS-XLAG-KVLS + WS-ANTAL                  
136400         ELSE                                                             
136500           IF TAB-DIFFERANS (INDX) NOT = ALL '+'                          
136600             COMPUTE WS-KVLS-NEW = WS-XLAG-KVLS +                         
136700                                   TAB-DIFFERANS(INDX)                    
136800             MOVE TAB-TECKEN(INDX)         TO WS-TECKEN                   
136900             MOVE TAB-DIFFERANS(INDX)      TO WS-ANTAL                    
137000             IF TAB-TECKEN(INDX) = '-'                                    
137100               COMPUTE WS-ANTAL = WS-ANTAL * -1                           
137200             END-IF                                                       
137300           END-IF                                                         
137400         END-IF                                                           
137500       END-IF                                                             
137600     END-IF                                                               
137700     .                                                                    
137800     EJECT                                                                
137900 HAB-KOLLA-MED-REGLER SECTION.                                            
138000     MOVE 'HAB-KOL' TO WS-SECTION                                         
138100     IF ALLT-OK                                                           
138200       IF WS-ANTAL NOT = 0                                                
138300         MOVE WS-ANTAL           TO WS-ANTAL-N                            
138400         COMPUTE WS-BELOPP = WS-ANTAL-N * WS-PRARTSTD                     
138500         IF WS-IDPRTOMG =  1                                              
138700           IF WS-BELOPP > 5000                                            
138800             MOVE NEJ            TO ALLT-OK-SW                            
138900           ELSE                                                           
139000             MOVE JA             TO ALLT-OK-SW                            
139100           END-IF                                                         
139900                                                                          
140000         ELSE                                                             
140100           IF WS-BELOPP > 10000                                           
140200             MOVE NEJ            TO ALLT-OK-SW                            
140300           ELSE                                                           
140310             IF CDC AND WS-BELOPP > 5000                                  
140320               MOVE NEJ            TO ALLT-OK-SW                          
140330             ELSE                                                         
140340               MOVE JA             TO ALLT-OK-SW                          
140350             END-IF                                                       
140500           END-IF                                                         
140600         END-IF                                                           
140700       ELSE                                                               
140800        MOVE ZERO                TO WS-ANTAL                              
140900        MOVE WS-XLAG-KVLS        TO WS-KVLS-NEW                           
141000        MOVE JA                  TO ALLT-OK-SW                            
141100       END-IF                                                             
141200       IF CDC-SE                                                          
141300*------ NYA VARDEN TILL WDK611 SEGMENT                                    
141400         MOVE WS-KVLS-NEW        TO CLAG-KVLS                             
141500         MOVE WS-ANTAL           TO CLAG-KVINVS                           
141600         MOVE WS-TIINVDAT        TO CLAG-TIINVDAT                         
141700       ELSE                                                               
141800*------ NYA VARDEN TILL WDK711 SEGMENT                                    
141900         MOVE WS-KVLS-NEW        TO SLAG-KVLS                             
142000         MOVE WS-ANTAL           TO SLAG-KVINVS                           
142100         MOVE WS-TIINVDAT        TO SLAG-TIINVDAT                         
142200       END-IF                                                             
142300     END-IF                                                               
142400                                                                          
142500     .                                                                    
142600     EJECT                                                                
142700 HAC-KOLLA-MED-WDH1 SECTION.                                              
142800     MOVE 'HAC-KOL' TO WS-SECTION                                         
142900     MOVE NEJ TO POST-SW                                                  
143000     MOVE NEJ TO ALLT-OK-SW                                               
143100     MOVE NEJ TO FORTSATT-SW                                              
143200     PERFORM IMS-GET-INVA-ART                                             
143300     IF SEGMENT-FINNS                                                     
143400       PERFORM IMS-GET-INVA-INV                                           
143500       PERFORM UNTIL SEGMENT-SAKNAS OR POST-FINNS                         
143600         IF INVA-INV-IDPRTOMG = WS-IDPRTOMG AND                           
143700           (INVA-INV-IDLOPNR  = WS-IDLOPNR)                               
143800           MOVE JA                    TO POST-SW                          
143900           MOVE JA                    TO ALLT-OK-SW                       
144000           MOVE JA                    TO FORTSATT-SW                      
144100           MOVE INVA-INV-KDINVKAT     TO WS-KDINVKAT                      
144200           MOVE INVA-INV-DAREGDAT-CRE TO WS-DAREGDAT-CRE                  
144300           MOVE INVA-INV-DAREGDAT-PR1 TO WS-DAREGDAT-PR1                  
144400           MOVE INVA-INV-DAREGDAT-PR2 TO WS-DAREGDAT-PR2                  
144500           MOVE INVA-INV-DAREGDAT-PR3 TO WS-DAREGDAT-PR3                  
144600           PERFORM IMS-GNP-WDH121                                         
144700           PERFORM UNTIL SEGMENT-SAKNAS                                   
144800             IF INVA-INVL-KDSEGKEY = '0'                                  
144900               MOVE INVA-INVL-IDUSER   TO WS-IDUSER-CRE                   
145000             END-IF                                                       
145100             IF INVA-INVL-KDSEGKEY = '1'                                  
145200               MOVE INVA-INVL-IDUSER   TO WS-IDUSER-PR1                   
145300             END-IF                                                       
145400             IF INVA-INVL-KDSEGKEY = '2'                                  
145500               MOVE INVA-INVL-IDUSER   TO WS-IDUSER-PR2                   
145600             END-IF                                                       
145700             IF INVA-INVL-KDSEGKEY = '3'                                  
145800               MOVE INVA-INVL-IDUSER   TO WS-IDUSER-PR3                   
145900             END-IF                                                       
146000           PERFORM IMS-GNP-WDH121                                         
146100           END-PERFORM                                                    
146200         ELSE                                                             
146300           PERFORM IMS-GET-INVA-INV                                       
146400         END-IF                                                           
146500       END-PERFORM                                                        
146600     END-IF                                                               
146700     .                                                                    
146800     EJECT                                                                
146900 HB-UPPDATERA-WDH1 SECTION.                                               
147000     MOVE 'HB-UPPD' TO WS-SECTION                                         
147100     MOVE NEJ TO POST-SW                                                  
147200     PERFORM IMS-GET-INVA-ART                                             
147300     IF SEGMENT-FINNS                                                     
147400       PERFORM IMS-GET-INVA-INV                                           
147500       PERFORM UNTIL SEGMENT-SAKNAS OR POST-FINNS                         
147600         IF INVA-INV-IDPRTOMG = WS-IDPRTOMG AND                           
147700           (INVA-INV-IDLOPNR  = WS-IDLOPNR)                               
147800           MOVE JA                    TO POST-SW                          
147900           MOVE INVA-INV-KDINVKAT     TO WS-KDINVKAT                      
148000           MOVE INVA-INV-DAREGDAT-CRE TO WS-DAREGDAT-CRE                  
148100           MOVE INVA-INV-DAREGDAT-PR1 TO WS-DAREGDAT-PR1                  
148200           MOVE INVA-INV-DAREGDAT-PR2 TO WS-DAREGDAT-PR2                  
148300           MOVE INVA-INV-DAREGDAT-PR3 TO WS-DAREGDAT-PR3                  
148400                                                                          
148500           PERFORM IMS-GNP-WDH121                                         
148600           PERFORM UNTIL SEGMENT-SAKNAS                                   
148700             IF INVA-INVL-KDSEGKEY = '0'                                  
148800               MOVE INVA-INVL-IDUSER   TO WS-IDUSER-CRE                   
148900             END-IF                                                       
149000             IF INVA-INVL-KDSEGKEY = '1'                                  
149100               MOVE INVA-INVL-IDUSER   TO WS-IDUSER-PR1                   
149200             END-IF                                                       
149300             IF INVA-INVL-KDSEGKEY = '2'                                  
149400               MOVE INVA-INVL-IDUSER   TO WS-IDUSER-PR2                   
149500             END-IF                                                       
149600             IF INVA-INVL-KDSEGKEY = '3'                                  
149700               MOVE INVA-INVL-IDUSER   TO WS-IDUSER-PR3                   
149800             END-IF                                                       
149900           PERFORM IMS-GNP-WDH121                                         
150000           END-PERFORM                                                    
150100*          MOVE INVA-INV-IDUSER-PR1   TO WS-IDUSER-PR1                    
150200*          MOVE INVA-INV-IDUSER-PR2   TO WS-IDUSER-PR2                    
150300*          MOVE INVA-INV-IDUSER-PR3   TO WS-IDUSER-PR3                    
150400         ELSE                                                             
150500           PERFORM IMS-GET-INVA-INV                                       
150600         END-IF                                                           
150700       END-PERFORM                                                        
150800     END-IF                                                               
150900                                                                          
151000                                                                          
151100     PERFORM HBC-RENSA-WDH1                                               
151200                                                                          
151300     PERFORM IMS-GET-INVA-ART                                             
151400     IF SEGMENT-SAKNAS                                                    
151500       MOVE W-IDARTNR   TO INVA-ART-IDARTNR                               
151600       PERFORM IMS-ISRT-INVA-ART                                          
151700     END-IF                                                               
151800     PERFORM HBA-FYLL-INVA-AREA                                           
151900     PERFORM IMS-ISRT-INVA-INV                                            
152000     PERFORM UNTIL SEGMENT-FINNS                                          
152100                                                                          
152200       IF SEGMENT-FINNS-REDAN OR INDEX-FINNS-REDAN                        
152300         ADD 1                TO INVA-INV-TISEGKEY                        
152400         PERFORM IMS-ISRT-INVA-INV                                        
152500       END-IF                                                             
152600     END-PERFORM                                                          
152700     PERFORM HBAA-FYLL-WDH121                                             
152800     .                                                                    
152900     EJECT                                                                
153000 HBA-FYLL-INVA-AREA SECTION.                                              
153100     MOVE 'HBA-FYL' TO WS-SECTION                                         
153200     MOVE SPACE TO INVA-INV-WDH111                                        
153300     MOVE NEJ             TO INVA-INV-FLINVSKR                            
153400                             INVA-INV-FLINV2B                             
153500                             INVA-INV-FLINV3E                             
153600                             INVA-INV-FLINV4N                             
153700     IF NDC-NA                                                            
153800       MOVE NEJ           TO INVA-INV-FLINV2C                             
153900                             INVA-INV-FLINV2D                             
154000       MOVE JA            TO INVA-INV-FLINV4R                             
154100                             INVA-INV-FLINV4P                             
154200     ELSE                                                                 
154300       MOVE JA            TO INVA-INV-FLINV2C                             
154400                             INVA-INV-FLINV2D                             
154500       MOVE NEJ           TO INVA-INV-FLINV4R                             
154600                             INVA-INV-FLINV4P                             
154700     END-IF                                                               
154800     MOVE NEJ             TO INVA-INV-FLINV85                             
154900     MOVE SPACE           TO INVA-INV-FILLER1                             
155000                             INVA-INV-FILLER2                             
155100     MOVE WS-IDDC         TO INVA-INV-IDDC                                
155200     MOVE WS-ANTAL        TO INVA-INV-KVJUSTKV                            
155300     MOVE SPACE           TO INVA-INV-TEINVANM                            
155400     MOVE +12             TO INVA-INV-KDINVKAT                            
155500     MOVE WS-KDINVKAT     TO INVA-INV-KDINVKAT-OLD                        
155600     MOVE JA              TO INVA-INV-FLINVBEH                            
155700     MOVE ART-IDFKNGRP    TO INVA-INV-IDFKNGRP                            
155800     MOVE ZERO            TO INVA-INV-KDINVPRIO                           
155900     MOVE ART-KDPRODSL    TO INVA-INV-KDPRODSL                            
156000     MOVE CLAG-KDVVKL     TO INVA-INV-KDVVKL                              
156100     MOVE CLAG-KDPSLLOC   TO INVA-INV-KDPSLLOC                            
156200                                                                          
156300     MOVE ZERO                TO INVA-INV-ADLAGOMR                        
156400                                 INVA-INV-ADGANG                          
156500                                 INVA-INV-ADPLATS                         
156600     IF CDC-SE                                                            
156700       MOVE CLAG-ADLAGOMR     TO INVA-INV-ADLAGOMR                        
156800       MOVE CLAG-ADGANG       TO INVA-INV-ADGANG                          
156900       MOVE CLAG-ADPLATS      TO INVA-INV-ADPLATS                         
157000     ELSE                                                                 
157100       PERFORM IMS-GU-WDK711                                              
157200       IF SEGMENT-FINNS                                                   
157300         MOVE SLAG-ADLAGOMR   TO INVA-INV-ADLAGOMR                        
157400         MOVE SLAG-ADGANG     TO INVA-INV-ADGANG                          
157500         MOVE SLAG-ADPLATS    TO INVA-INV-ADPLATS                         
157600       END-IF                                                             
157700     END-IF                                                               
157800                                                                          
157900     IF CDC-SE                                                            
158000       IF CLAG-KDERS = 11 OR 14 OR 17 OR 18 OR 19                         
158100         MOVE JA              TO INVA-INV-FLINV85                         
158200       END-IF                                                             
158300     END-IF                                                               
158400     MOVE 0               TO WS-TISEGKEY-LOPNR                            
158500     MOVE WS-DAGENS-DATUM TO WS-TISEGKEY-DAT                              
158600                             INVA-INV-DAREGDAT                            
158700                             INVA-INV-DAREGDAT-CRE                        
158800     MOVE WS-DAGENS-DATUM TO INVA-INV-DAREGDAT-SORT                       
158900                                                                          
159000                                                                          
159100     MOVE ZERO            TO INVA-INV-DAREGDAT-PR1                        
159200                             INVA-INV-DAREGDAT-PR2                        
159300                             INVA-INV-DAREGDAT-PR3                        
159400*    MOVE SPACE           TO INVA-INV-IDUSER-PR1                          
159500*                            INVA-INV-IDUSER-PR2                          
159600*                            INVA-INV-IDUSER-PR3                          
159700     MOVE WS-TIAAAAMMDDL  TO INVA-INV-TISEGKEY                            
159800     MOVE ZERO            TO INVA-INV-IDPRTOMG                            
159900                             INVA-INV-IDLOPNR                             
160000                             INVA-INV-KVAKS-OLD                           
160100                             INVA-INV-KVEFRS-OLD                          
160200                             INVA-INV-KVLS-OLD                            
160300     .                                                                    
160400     EJECT                                                                
160500 HBAA-FYLL-WDH121 SECTION.                                                
160600                                                                          
160700     MOVE '0'            TO INVA-INVL-KDSEGKEY                            
160800     MOVE MSGI-IDUSER    TO INVA-INVL-IDUSER                              
160900     PERFORM IMS-INSERT-WDH121                                            
161000                                                                          
161100     MOVE '1'            TO INVA-INVL-KDSEGKEY                            
161200     MOVE SPACE          TO INVA-INVL-IDUSER                              
161300     PERFORM IMS-INSERT-WDH121                                            
161400     MOVE '2'            TO INVA-INVL-KDSEGKEY                            
161500     MOVE SPACE          TO INVA-INVL-IDUSER                              
161600     PERFORM IMS-INSERT-WDH121                                            
161700     MOVE '3'            TO INVA-INVL-KDSEGKEY                            
161800     MOVE SPACE          TO INVA-INVL-IDUSER                              
161900     PERFORM IMS-INSERT-WDH121                                            
162000                                                                          
162100     .                                                                    
162200     EJECT                                                                
162300 HBC-RENSA-WDH1 SECTION.                                                  
162400*----MÅSTE BÖRJA OM IFRÅN BÖRJAN PÅ ARTIKEL FÖR ATT KUNNA                 
162500*----RENSA ALLA POSTER                                                    
162600     MOVE 'HBC-REN' TO WS-SECTION                                         
162700     MOVE 001        TO W-KDINVKAT-WDH1-MIN                               
162800     MOVE 099        TO W-KDINVKAT-WDH1-MAX                               
162900                                                                          
163000     PERFORM IMS-GET-INVA-ART                                             
163100     IF SEGMENT-FINNS                                                     
163200       PERFORM IMS-GET-INVA-INV                                           
163300       PERFORM UNTIL SEGMENT-SAKNAS                                       
163400         IF INVA-INV-KDINVKAT = +1 OR +2 OR +3 OR +4 OR +5 OR +9          
163500           MOVE JA    TO INVA-INV-FLINVBEH                                
163600           MOVE ZERO  TO INVA-INV-IDPRTOMG                                
163700           MOVE ZERO  TO INVA-INV-IDLOPNR                                 
163800           IF INVA-INV-KDINVKAT = +2                                      
163900              MOVE WS-ANTAL        TO INVA-INV-KVJUSTKV                   
164000           END-IF                                                         
164100           PERFORM IMS-REPL-INVA-INV                                      
164200         END-IF                                                           
164300         PERFORM IMS-GET-INVA-INV                                         
164400       END-PERFORM                                                        
164500     END-IF                                                               
164600     .                                                                    
164700     EJECT                                                                
164800 HC-UPPDATERA-WDH7 SECTION.                                               
164900     MOVE 'HC-UPPD' TO WS-SECTION                                         
165000     PERFORM IMS-GHU-INVHIST-ROT                                          
165100     IF SEGMENT-FINNS                                                     
165200       MOVE JA              TO ALLT-OK-SW                                 
165300     ELSE                                                                 
165400       MOVE W-IDARTNR       TO INVA-IDARTNR                               
165500       PERFORM IMS-ISRT-INVHIST-ROT                                       
165600     END-IF                                                               
165700     PERFORM HCA-SKAPA-TISEGKEY                                           
165800     PERFORM HCB-KOLLA-KDJUSTYP                                           
165900     MOVE WS-TISEGKEY       TO INVH-TISEGKEY                              
166000     MOVE WS-IDDC           TO INVH-IDDC                                  
166100     MOVE WS-DAGENS-DATUM   TO INVH-DAREGDAT-CLO                          
166200     MOVE JA                TO INVH-FLAUTLSJ                              
166300     MOVE WS-MSGI-IDUSER    TO INVH-IDUSER-CLO                            
166400     MOVE SPACE             TO INVH-IDPW                                  
166500     MOVE WS-ANTAL          TO INVH-KVJUSTKV                              
166600     MOVE INVH-KDJUSTYP     TO WS-KDJUSTYP                                
166700     MOVE WS-PRARTSTD       TO INVH-PRARTSTD                              
166800     MOVE WS-DAREGDAT-CRE   TO INVH-DAREGDAT-CRE                          
166900     MOVE WS-DAREGDAT-PR1   TO INVH-DAREGDAT-PR1                          
167000     MOVE WS-DAREGDAT-PR2   TO INVH-DAREGDAT-PR2                          
167100     MOVE WS-DAREGDAT-PR3   TO INVH-DAREGDAT-PR3                          
167200     MOVE WS-IDUSER-PR1     TO INVH-IDUSER-PR1                            
167300     MOVE WS-IDUSER-PR2     TO INVH-IDUSER-PR2                            
167400     MOVE WS-IDUSER-PR3     TO INVH-IDUSER-PR3                            
167500     MOVE WS-IDUSER-CRE     TO INVH-IDUSER-CRE                            
167600     MOVE JA                TO ALLT-OK-SW                                 
167700     IF CDC-SE                                                            
167800       MOVE WS-KVANTAL      TO INVH-KVANTAL                               
167900     ELSE                                                                 
168000       MOVE +0              TO INVH-KVANTAL                               
168100     END-IF                                                               
168200                                                                          
168300     PERFORM IMS-ISRT-INVHIST-SEGM                                        
168400     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
168500       IF SEGMENT-FINNS-REDAN                                             
168600         SUBTRACT 1 FROM INVH-TISEGKEY                                    
168700         PERFORM IMS-ISRT-INVHIST-SEGM                                    
168800       END-IF                                                             
168900     END-PERFORM                                                          
169000     MOVE ZERO              TO WS-DAREGDAT-CRE                            
169100                               WS-DAREGDAT-PR1                            
169200                               WS-DAREGDAT-PR2                            
169300                               WS-DAREGDAT-PR3                            
169400     MOVE SPACE             TO WS-IDUSER-PR1                              
169500                               WS-IDUSER-PR2                              
169600                               WS-IDUSER-PR3                              
169700                               WS-IDUSER-CRE                              
169800     .                                                                    
169900     EJECT                                                                
170000 HCA-SKAPA-TISEGKEY SECTION.                                              
170100     MOVE 'HCA-SKA' TO WS-SECTION                                         
170200     MOVE 9 TO WS-TISEGKEY-LOPNR                                          
170300     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAGENS-DATUM                   
170400     MOVE WS-DAGENS-DATUM TO WS-TISEGKEY-DAT                              
170500     COMPUTE WS-TISEGKEY = 999999999 - WS-TIAAAAMMDDL                     
170600     .                                                                    
170700     EJECT                                                                
170800 HCB-KOLLA-KDJUSTYP SECTION.                                              
170900     MOVE 'HCB-KOL' TO WS-SECTION                                         
171000     EVALUATE TRUE                                                        
171100       WHEN WS-KDINVKAT = +1                                              
171200         MOVE +1  TO INVH-KDJUSTYP                                        
171300         MOVE '1' TO WS-LOGG-UREF1                                        
171400       WHEN WS-KDINVKAT = +2                                              
171500         MOVE +2  TO INVH-KDJUSTYP                                        
171600         MOVE '2' TO WS-LOGG-UREF1                                        
171700       WHEN WS-KDINVKAT = +3                                              
171800         MOVE +3  TO INVH-KDJUSTYP                                        
171900         MOVE '3' TO WS-LOGG-UREF1                                        
172000       WHEN WS-KDINVKAT = +4                                              
172100         MOVE +4  TO INVH-KDJUSTYP                                        
172200         MOVE '4' TO WS-LOGG-UREF1                                        
172300       WHEN WS-KDINVKAT = +5                                              
172400         MOVE +5  TO INVH-KDJUSTYP                                        
172500         MOVE '5' TO WS-LOGG-UREF1                                        
172600       WHEN WS-KDINVKAT = +8                                              
172700         MOVE +8  TO INVH-KDJUSTYP                                        
172800         MOVE '8' TO WS-LOGG-UREF1                                        
172900       WHEN WS-KDINVKAT = +9                                              
173000         MOVE +9  TO INVH-KDJUSTYP                                        
173100         MOVE '9' TO WS-LOGG-UREF1                                        
173200       WHEN OTHER                                                         
173300         MOVE +0  TO INVH-KDJUSTYP                                        
173400         MOVE +0  TO WS-LOGG-UREF1                                        
173500     END-EVALUATE                                                         
173600     .                                                                    
173700     EJECT                                                                
173800 HD-FYLL-WDR9-AREA  SECTION.                                              
173900     MOVE 'HD-FYLL' TO WS-SECTION                                         
174000     MOVE 'W5030800'       TO FIL-IDPGM                                   
174100     MOVE WS-DAGENS-DATUM  TO FIL-DAREGDAT                                
174200     ACCEPT FIL-TIKLOCK    FROM TIME                                      
174300     MOVE 1                TO FIL-IDSEKVNR                                
174400     MOVE 'W510EKHA'       TO FIL-IDCPYTXT                                
174500     MOVE WS-IDUSER        TO FIL-IDUSER                                  
174600     MOVE W-IDARTNR        TO EKH-IDARTNR                                 
174700     MOVE '403'            TO EKH-KDEKHHT                                 
174800     MOVE '40'             TO WS-KDEKSHT-1                                
174900     MOVE WS-KDJUSTYP      TO WS-KDEKSHT-2                                
175000     MOVE WS-EKH-KDEKSHT   TO EKH-KDEKSHT                                 
175100     MOVE 'DET'            TO EKH-KDEKNIVA                                
175200     MOVE WS-IDDC          TO EKH-IDDC-SEND                               
175300                              EKH-IDDC-REC                                
175400     MOVE ZERO             TO EKH-IDDISTR                                 
175500                              EKH-IDKUNDNR                                
175600     MOVE W-IDARTNR        TO W-EKH-IDARTNR                               
175700                                                                          
175800     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
175900     MOVE W-EKH-IDARTNR    TO CIA-IDARTBET-IN                             
176000     CALL W009CIA USING       CIA-W009CIA                                 
176100     MOVE CIA-IDARTBET-UT TO EKH-IDVERGL                                  
176200                                                                          
176300                                                                          
176400     MOVE WS-DAGENS-DATUM  TO EKH-DAVERDAT                                
176500     MOVE ART-KDPRODSL     TO EKH-KDPRODSL                                
176600     MOVE ZERO             TO EKH-KDPSLLOC                                
176700     MOVE SPACE            TO EKH-FLLSBOK                                 
176800     MOVE 'SEK'            TO EKH-KDVALISO                                
176900     MOVE 1.00             TO EKH-PRKURS                                  
177000     MOVE ZERO             TO EKH-PRARTNTO                                
177100     MOVE ZERO             TO EKH-PRARTSJK                                
177200     MOVE ZERO             TO EKH-PRHEMTAG                                
177300     MOVE WS-PRARTSTD      TO EKH-PRARTSTD                                
177400     MOVE ZERO             TO EKH-PRLANDCO                                
177500                              EKH-PRINK                                   
177600                              EKH-PRDIRLON                                
177700                              EKH-PRDMTRL                                 
177800                              EKH-PROVRPAL                                
177900                              EKH-SUBEL                                   
178000     MOVE WS-ANTAL         TO EKH-KVANTAL                                 
178100     MOVE '5308'           TO EKH-IDTRANS                                 
178200     MOVE ZERO             TO EKH-BEVAT                                   
178300                              EKH-IDANALYS                                
178400                              EKH-IDKONTO                                 
178500                              EKH-KDANMORS                                
178600                              EKH-KDFRAKT                                 
178700                              EKH-SUVAT                                   
178800     MOVE ZERO             TO EKH-DAAVIDAT                                
178900                              EKH-IDAVINR                                 
179000                              EKH-KDAVVTYP                                
179100                              EKH-KDRT                                    
179200                              EKH-KVANTMOT                                
179300                              EKH-KVAVIS                                  
179400     MOVE WS-KDSORT        TO EKH-KDSORT                                  
179500     IF WS-IDRT-KEY = 'ET'                                                
179600       MOVE JA             TO EKH-FLDCET                                  
179700     ELSE                                                                 
179800       MOVE NEJ            TO EKH-FLDCET                                  
179900     END-IF                                                               
180000     MOVE SPACE            TO EKH-KDTRADP                                 
180100                              EKH-IDKST                                   
180200                              EKH-IDLEVNR                                 
180300                              EKH-IDKUNDRF                                
180310                              EKH-IDFAKT-EXP                              
180400     .                                                                    
180500     EJECT                                                                
180600 HDA-UPPDATERA-WDR9 SECTION.                                              
180700                                                                          
180800     PERFORM IMS-ISRT-WDR901                                              
180900     PERFORM UNTIL SEGMENT-FINNS                                          
181000      ADD +1 TO FIL-IDSEKVNR                                              
181100      PERFORM IMS-ISRT-WDR901                                             
181200     END-PERFORM                                                          
181300     .                                                                    
181400     EJECT                                                                
181500 HE-FYLL-WDL9-AREA SECTION.                                               
181600* LÄGGER UPP SALDOLOGG I WDL9                                             
181700     MOVE W-IDARTNR             TO LOGG-IDARTNR                           
181800     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - WS-DAGENS-DATUM            
181900     ACCEPT TRANS-TID FROM TIME                                           
182000     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TID                 
182100     MOVE 9                       TO LOGG-IDSEKVNR                        
182200     MOVE WS-IDDC                 TO LOGG-IDDC                            
182300     MOVE 'MISC'                  TO LOGG-IDHUVTYP                        
182400     MOVE 'ADJ'                   TO LOGG-IDSUBTYP                        
182500     MOVE IDPGM                   TO LOGG-IDPGM                           
182600     MOVE '5308'                  TO LOGG-IDTRANS                         
182700     MOVE WS-MSGI-IDUSER          TO LOGG-IDUSER                          
182800     MOVE SPACE                   TO LOGG-REF                             
182900     MOVE WS-LOGG-UREF1           TO LOGG-UREF1                           
183000     MOVE WS-IDUSER               TO LOGG-UREF2                           
183100     MOVE WS-TECKEN               TO LOGG-IDTECKEN-KVLS                   
183200     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS-PAV              
183300                                     LOGG-IDTECKEN-KVEFRS                 
183400                                     LOGG-IDTECKEN-KVAKS                  
183500     IF WS-ANTAL < 0                                                      
183600       COMPUTE LOGG-KVART-SALDO = WS-ANTAL * -1                           
183700     ELSE                                                                 
183800       MOVE WS-ANTAL              TO LOGG-KVART-SALDO                     
183900     END-IF                                                               
184000     IF CDC-SE                                                            
184100       MOVE CLAG-KVAKS-PAV        TO LOGG-KVAKS-PAV                       
184200       MOVE CLAG-KVEFRS           TO LOGG-KVEFRS                          
184300       MOVE CLAG-KVLS             TO LOGG-KVLS                            
184400       COMPUTE LOGG-KVAKS = CLAG-KVAKS-CDC +                              
184500                            CLAG-KVAKS-T                                  
184600     ELSE                                                                 
184700       MOVE SLAG-KVAKS-PAV        TO LOGG-KVAKS-PAV                       
184800       MOVE SLAG-KVEFRS           TO LOGG-KVEFRS                          
184900       MOVE SLAG-KVLS             TO LOGG-KVLS                            
185000       MOVE SLAG-KVAKS-SDC        TO LOGG-KVAKS                           
185100     END-IF                                                               
185200     MOVE ZERO                    TO LOGG-DAREGDAT-LADD                   
185300     .                                                                    
185400     EJECT                                                                
185500 HEA-UPPDATERA-WDL9 SECTION.                                              
185600     PERFORM IMS-ISRT-WDL901                                              
185700     IF SEGMENT-FINNS-REDAN                                               
185800        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
185900          ADD -1 TO LOGG-IDSEKVNR                                         
186000          PERFORM IMS-ISRT-WDL901                                         
186100        END-PERFORM                                                       
186200     END-IF                                                               
186300     .                                                                    
186400     EJECT                                                                
186500 HF-FYLL-FELTABELL SECTION.                                               
186600     MOVE TAB-ARTIKEL     (INDX)   TO FEL-ARTIKEL   (IX)                  
186700     MOVE TAB-BENAMNING   (INDX)   TO FEL-BENAMNING (IX)                  
186800     MOVE TAB-KVAKS       (INDX)   TO FEL-KVAKS     (IX)                  
186900     MOVE TAB-KVLS        (INDX)   TO FEL-KVLS      (IX)                  
187000     MOVE TAB-KVEFRS      (INDX)   TO FEL-KVEFRS    (IX)                  
187100     MOVE TAB-ANTAL       (INDX)   TO FEL-ANTAL     (IX)                  
187200     IF FEL-RAD (IX) = 'N'                                                
187300       MOVE SPACE                  TO FEL-TECKEN    (IX)                  
187400     ELSE                                                                 
187500       MOVE TAB-TECKEN    (INDX)   TO FEL-TECKEN    (IX)                  
187600     END-IF                                                               
187700     MOVE TAB-DIFFERANS   (INDX)   TO FEL-DIFFERANS (IX)                  
187800     MOVE TAB-FLOMINV     (INDX)   TO FEL-FLOMINV   (IX)                  
187900     ADD +1 TO IX                                                         
188000     .                                                                    
188100     EJECT                                                                
188200 HFA-FYLL-FELTABELL SECTION.                                              
188300     MOVE +1 TO INDX                                                      
188400     PERFORM UNTIL INDX > MAX-INDX                                        
188500     IF TAB-ARTIKEL     (INDX) NUMERIC                                    
188600       MOVE TAB-ARTIKEL     (INDX)   TO FEL-ARTIKEL   (INDX)              
188700       MOVE TAB-BENAMNING   (INDX)   TO FEL-BENAMNING (INDX)              
188800       MOVE TAB-KVAKS       (INDX)   TO FEL-KVAKS     (INDX)              
188900       MOVE TAB-KVLS        (INDX)   TO FEL-KVLS      (INDX)              
189000       MOVE TAB-KVEFRS      (INDX)   TO FEL-KVEFRS    (INDX)              
189100     END-IF                                                               
189200                                                                          
189300     IF   TAB-ANTAL       (INDX)   NUMERIC                                
189400     MOVE TAB-ANTAL       (INDX)   TO FEL-ANTAL     (INDX)                
189500     END-IF                                                               
189600                                                                          
189700     MOVE TAB-TECKEN    (INDX)     TO FEL-TECKEN    (INDX)                
189800                                                                          
189900     IF   TAB-DIFFERANS   (INDX)   NUMERIC                                
190000     MOVE TAB-DIFFERANS   (INDX)   TO FEL-DIFFERANS (INDX)                
190100     END-IF                                                               
190200                                                                          
190300     IF   TAB-FLOMINV     (INDX)   NOT = ALL '+'                          
190400     MOVE TAB-FLOMINV     (INDX)   TO FEL-FLOMINV   (INDX)                
190500     END-IF                                                               
190600                                                                          
190700     ADD +1 TO INDX                                                       
190800     END-PERFORM                                                          
190900     .                                                                    
191000     EJECT                                                                
191100 HG-NOLLA-LISTNR SECTION.                                                 
191200     MOVE 'HG-NOLL' TO WS-SECTION                                         
191300     MOVE NEJ TO POST-SW                                                  
191400     PERFORM IMS-GET-INVA-ART                                             
191500     IF SEGMENT-FINNS                                                     
191600       PERFORM IMS-GET-INVA-INV                                           
191700       IF SEGMENT-FINNS                                                   
191800         PERFORM UNTIL SEGMENT-SAKNAS OR POST-FINNS                       
191900           IF INVA-INV-IDLOPNR = WS-IDLOPNR AND                           
192000              (INVA-INV-IDPRTOMG = WS-IDPRTOMG)                           
192100             MOVE JA                 TO POST-SW                           
192200             MOVE ZERO               TO INVA-INV-IDLOPNR                  
192300*--INGEN ADD,ADD +1                  TO INVA-INV-IDPRTOMG                 
192400*--GÖRS I PGM W5030100                                                    
192500             MOVE 'N'                TO INVA-INV-FLINVSKR                 
192600             MOVE WS-DAGENS-DATUM    TO INVA-INV-DAREGDAT                 
192700* NYCKEL KAN COMPUTE INVA-INV-DAREGDAT-SORT =                             
192800* INTE ÄNDRAS  99999999 - WS-DAGENS-DATUM                                 
192900             PERFORM IMS-REPL-INVA-INV                                    
193000           ELSE                                                           
193100             PERFORM IMS-GET-INVA-INV                                     
193200             IF SEGMENT-SAKNAS                                            
193300               MOVE NEJ TO ALLT-OK-SW                                     
193400             END-IF                                                       
193500           END-IF                                                         
193600         END-PERFORM                                                      
193700       ELSE                                                               
193800         MOVE NEJ TO ALLT-OK-SW                                           
193900       END-IF                                                             
194000     ELSE                                                                 
194100       MOVE NEJ TO ALLT-OK-SW                                             
194200     END-IF                                                               
194300     .                                                                    
194400     EJECT                                                                
194500 HH-TAECKNING-CDC SECTION.                                                
194600     MOVE 'HH-TAEC' TO WS-SECTION                                         
194700     IF WS-TECKEN =  '+'                                                  
194800       MOVE W-IDARTNR    TO 4506-IDARTNR                                  
194900       MOVE 11           TO 4506-KDTAKORS                                 
195000       MOVE ZERO         TO 4506-KVANTMOT                                 
195100       MOVE WS-IDDC      TO W-IDDC-4505                                   
195200       PERFORM IMS-ISRT-450511                                            
195300     END-IF                                                               
195400     .                                                                    
195500     EJECT                                                                
195600 HI-RADERA-UTREDSALDO SECTION.                                            
195700     MOVE 'HI-RADE' TO WS-SECTION                                         
195800     MOVE W-IDARTNR TO W-IDARTNR-UTR                                      
195900     PERFORM IMS-GHU-ART-UTREDNSALDO                                      
196000                                                                          
196100     IF SEGMENT-FINNS                                                     
196200       PERFORM IMS-DELETE-ART-UTREDNSALDO                                 
196300     END-IF                                                               
196400     .                                                                    
196500     EJECT                                                                
196600 HJ-UPPDATERA-WDK7 SECTION.                                               
196700     MOVE 'HJ-UPPD' TO WS-SECTION                                         
196800                                                                          
196900     IF W-IDDC NOT = WC-CDC-SE                                            
197000       PERFORM IMS-GHU-WDK711                                             
197100       IF SEGMENT-FINNS                                                   
197200         IF SLAG-FLREFNYO = JA                                            
197300           MOVE NEJ TO SLAG-FLREFNYO                                      
197400           PERFORM IMS-REPL-SLAGER-SEGM                                   
197500         END-IF                                                           
197600       END-IF                                                             
197700     END-IF                                                               
197800     .                                                                    
197900     EJECT                                                                
198000 HK-FYLL-WDR8-AREA  SECTION.                                              
198100     MOVE 'HK      ' TO WS-SECTION                                        
198200     MOVE 'W5030800'       TO A08-FIL-IDPGM                               
198300     MOVE WS-DAGENS-DATUM(2:7) TO A08-FIL-TIREGDAT                        
198400     ACCEPT A08-FIL-TIKLOCK    FROM TIME                                  
198500     MOVE 1                TO A08-FIL-IDSEKVNR                            
198600     MOVE 'W510A08 '       TO A08-FIL-IDCPYTXT                            
198700     MOVE 'A08'            TO A08-IDPTYP                                  
198800     MOVE 'M10'            TO A08-KDEKOHT                                 
198900     IF DCS-NDC-NA AND DCS-CANADA                                         
199000       MOVE 54             TO A08-IDFTG                                   
199100     ELSE                                                                 
199200       MOVE 53             TO A08-IDFTG                                   
199300     END-IF                                                               
199400     MOVE W-IDDC           TO A08-IDDC-SEND                               
199500                              A08-IDDC-REC                                
199600     MOVE WS-DAGENS-DATUM  TO A08-DAJUSTDA                                
199700     MOVE W-IDARTNR        TO A08-IDARTNR                                 
199800     MOVE ART-KDPRODSL     TO A08-KDPRODSL                                
199900     MOVE CLAG-KDPSLLOC    TO A08-KDPSLLOC                                
200000     MOVE WS-ANTAL         TO A08-KVJUSTKV                                
200100     MOVE W-PRAVCOST       TO A08-PRAVCOST                                
200200     MOVE INVA-INV-KDINVKAT TO A08-KDINVKAT                               
200300     MOVE SPACE             TO A08-TEINVANM                               
200400     .                                                                    
200500     EJECT                                                                
200600 HKA-UPPDATERA-WDR8 SECTION.                                              
200700                                                                          
200800     PERFORM IMS-ISRT-WDR801                                              
200900     PERFORM UNTIL SEGMENT-FINNS                                          
201000      ADD +1 TO A08-FIL-IDSEKVNR                                          
201100      PERFORM IMS-ISRT-WDR801                                             
201200     END-PERFORM                                                          
201300     .                                                                    
201400     EJECT                                                                
201500 I-LAES-VISA-FELTABELL SECTION.                                           
201600     MOVE 'I-LAES-' TO WS-SECTION                                         
201700     PERFORM MFS-RENSA-RAD-UT                                             
201800     PERFORM MFS-RENSA-FAELT-IN                                           
201900     PERFORM MFS-RENSA-FELRAD-IN                                          
202000     MOVE SPAR-IDLISTNR         TO MOD-IDLISTNR-UT                        
202100     MOVE SPAR-IDUSER           TO MOD-IDUSER-UT                          
202200*    MOVE MFS-RENSA-FAELT       TO MOD-TEMFSFEL                           
202300     MOVE +1 TO INDX                                                      
202400     PERFORM UNTIL INDX > MAX-INDX                                        
202500     IF FEL-ARTIKEL (INDX) NUMERIC                                        
202600       MOVE FEL-ARTIKEL  (INDX)   TO MOD-IDARTNR  (INDX)                  
202700       MOVE FEL-BENAMNING(INDX)   TO MOD-BEART-SVE(INDX)                  
202800       MOVE FEL-KVAKS    (INDX)   TO MOD-KVAKS    (INDX)                  
202900       MOVE FEL-KVLS     (INDX)   TO MOD-KVLS     (INDX)                  
203000       MOVE FEL-KVEFRS   (INDX)   TO MOD-KVEFRS   (INDX)                  
203100       IF FEL-ANTAL (INDX) NUMERIC                                        
203200         MOVE FEL-ANTAL  (INDX)   TO MOD-ANTAL    (INDX)                  
203300         IF FEL-RAD (INDX) = 'J'                                          
203400           MOVE MFS-NUM-FAELT-FEL TO MOD-ANTAL-ATTR (INDX)                
203500         END-IF                                                           
203600       ELSE                                                               
203700         MOVE MFS-RENSA-FAELT     TO MOD-ANTAL(INDX)                      
203800       END-IF                                                             
203900                                                                          
204000       MOVE FEL-TECKEN   (INDX)   TO MOD-TECKEN   (INDX)                  
204100                                                                          
204200       IF FEL-DIFFERANS  (INDX) NUMERIC                                   
204300         MOVE FEL-DIFFERANS(INDX) TO MOD-DIFFERANS(INDX)                  
204400         IF FEL-RAD (INDX) = 'J'                                          
204500           MOVE MFS-NUM-FAELT-FEL TO MOD-DIFFERANS-ATTR (INDX)            
204600           MOVE MFS-ALFA-FAELT-FEL TO MOD-TECKEN-ATTR (INDX)              
204700         END-IF                                                           
204800       ELSE                                                               
204900         MOVE MFS-RENSA-FAELT     TO MOD-DIFFERANS(INDX)                  
205000         MOVE MFS-RENSA-FAELT     TO MOD-TECKEN(INDX)                     
205100                                                                          
205200       END-IF                                                             
205300                                                                          
205400       IF FEL-FLOMINV (INDX) = 'J' OR 'Y'                                 
205500         MOVE FEL-FLOMINV (INDX)  TO MOD-FLOMINV-UT (INDX)                
205600         MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLOMINV-ATTR (INDX)              
205700       ELSE                                                               
205800         MOVE MFS-RENSA-FAELT     TO MOD-FLOMINV-UT (INDX)                
205900       END-IF                                                             
206000                                                                          
206100       IF FEL-RAD (INDX) = 'J'                                            
206200         MOVE 'J'               TO MOD-FELRAD-UT  (INDX)                  
206300       ELSE                                                               
206400         MOVE 'N'               TO MOD-FELRAD-UT (INDX)                   
206500       END-IF                                                             
206600     END-IF                                                               
206700                                                                          
206800     ADD +1 TO INDX                                                       
206900     END-PERFORM                                                          
207000     .                                                                    
207100     EJECT                                                                
207200 S01-DATUM SECTION.                                                       
207300                                                                          
207400     MOVE 'IDAG  '            TO DAT-KDDATFORM                            
207500     CALL  WDATKONV  USING       DAT-KDDATFORM                            
207600                                 DAT-I-TIDATUM                            
207700                                 DAT-O-TIDATUM                            
207800                                 DAT-KDSVAR                               
207900                                                                          
208000     IF DAT-KDSVAR-OK                                                     
208100         CONTINUE                                                         
208200     ELSE                                                                 
208300         CALL  FELLOG                                                     
208400     END-IF                                                               
208500                                                                          
208600     MOVE DAT-TIAAVVD         TO WS-TIINVDAT                              
208700     MOVE DAT-TIAAMMDD        TO DAGENS-AAMMDD                            
208800                                                                          
208900     MOVE DAT-TISEKEL         TO DAGENS-AA                                
209000     MOVE WS-DAGENS-DATUM-GRP TO WS-DAGENS-DATUM                          
209100     .                                                                    
209200     EJECT                                                                
209300 MFS-RENSA-FAELT-UT SECTION.                                              
209400                                                                          
209500     MOVE MFS-RENSA-FAELT TO MOD-OK                                       
209600                             MOD-IDDC-UT                                  
209700                             MOD-IDLISTNR-UT                              
209800                             MOD-IDUSER-UT                                
209900                                                                          
210000     PERFORM MFS-RENSA-RAD-UT                                             
210100     .                                                                    
210200     SKIP3                                                                
210300 MFS-RENSA-RAD-UT SECTION.                                                
210400     MOVE MFS-RENSA-FAELT TO MOD-OK                                       
210500     MOVE +1 TO INDX                                                      
210600     PERFORM UNTIL INDX > MAX-INDX                                        
210700                                                                          
210800     MOVE MFS-RENSA-FAELT TO                                              
210900                             MOD-IDARTNR   (INDX)                         
211000                             MOD-BEART-SVE (INDX)                         
211100                             MOD-KVAKS     (INDX)                         
211200                             MOD-KVLS      (INDX)                         
211300                             MOD-KVEFRS    (INDX)                         
211400                             MOD-ANTAL     (INDX)                         
211500                             MOD-TECKEN    (INDX)                         
211600                             MOD-DIFFERANS (INDX)                         
211700                             MOD-FLOMINV-UT(INDX)                         
211800                                                                          
211900     ADD +1 TO INDX                                                       
212000     END-PERFORM                                                          
212100     .                                                                    
212200     SKIP3                                                                
212300 MFS-RENSA-RADEN-UT SECTION.                                              
212400                                                                          
212500     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR   (INDX)                         
212600                             MOD-BEART-SVE (INDX)                         
212700                             MOD-KVAKS     (INDX)                         
212800                             MOD-KVLS      (INDX)                         
212900                             MOD-KVEFRS    (INDX)                         
213000                             MOD-ANTAL     (INDX)                         
213100                             MOD-TECKEN    (INDX)                         
213200                             MOD-DIFFERANS (INDX)                         
213300                             MOD-FLOMINV-UT(INDX)                         
213400                                                                          
213500     .                                                                    
213600     SKIP3                                                                
213700 MFS-RENSA-RADEN-IN SECTION.                                              
213800                                                                          
213900     MOVE MFS-RENSA-FAELT TO MOD-ANTAL     (INDX)                         
214000                             MOD-TECKEN    (INDX)                         
214100                             MOD-DIFFERANS    (INDX)                      
214200                             MOD-FLOMINV-UT(INDX)                         
214300                                                                          
214400     .                                                                    
214500     SKIP3                                                                
214600 MFS-RENSA-FAELT-IN SECTION.                                              
214700                                                                          
214800*    --- ALLA INDATA-FÄLT                                                 
214900     MOVE MFS-RENSA-FAELT TO MOD-IDUSER-IN                                
215000                             MOD-IDLISTNR-IN                              
215100                             MOD-OK                                       
215200     MOVE +1 TO INDX                                                      
215300     PERFORM UNTIL INDX > MAX-INDX                                        
215400       PERFORM MFS-RENSA-RADEN-IN                                         
215500       ADD +1 TO INDX                                                     
215600     END-PERFORM                                                          
215700     .                                                                    
215800     EJECT                                                                
215900 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
216000                                                                          
216100*    --- ALLA UTDATA-FÄLT                                                 
216200     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLISTNR-UT                            
216300                               MOD-IDUSER-UT                              
216400     .                                                                    
216500     SKIP3                                                                
216600 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
216700                                                                          
216800*    --- ALLA INDATA-FÄLT                                                 
216900     MOVE MFS-ROER-EJ-FAELT TO MOD-OK                                     
217000     MOVE +1 TO INDX                                                      
217100     PERFORM UNTIL INDX > MAX-INDX                                        
217200     MOVE MFS-ROER-EJ-FAELT TO MOD-ANTAL        (INDX)                    
217300                               MOD-TECKEN       (INDX)                    
217400                               MOD-DIFFERANS    (INDX)                    
217500                               MOD-FLOMINV-UT   (INDX)                    
217600                               MOD-FELRAD-UT    (INDX)                    
217700     ADD +1 TO INDX                                                       
217800     END-PERFORM                                                          
217900     .                                                                    
218000     EJECT                                                                
218100 MFS-FORM-ATTR SECTION.                                                   
218200                                                                          
218300*    --- ALLA INDATA-FÄLT                                                 
218400     MOVE MFS-FORMATETS-ATTR TO MOD-OK-ATTR                               
218500     MOVE +1 TO INDX                                                      
218600     PERFORM UNTIL INDX > MAX-INDX                                        
218700     MOVE MFS-FORMATETS-ATTR TO MOD-ANTAL-ATTR     (INDX)                 
218800                                MOD-TECKEN-ATTR    (INDX)                 
218900                                MOD-DIFFERANS-ATTR (INDX)                 
219000                                MOD-FLOMINV-ATTR   (INDX)                 
219100     ADD +1 TO INDX                                                       
219200     END-PERFORM                                                          
219300     .                                                                    
219400     SKIP2                                                                
219500 MFS-LAES-IN-IGEN SECTION.                                                
219600                                                                          
219700*    --- ALLA INDATA-FÄLT                                                 
219800     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-OK-ATTR                            
219900     MOVE +1 TO INDX                                                      
220000     PERFORM UNTIL INDX > MAX-INDX                                        
220100     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ANTAL-ATTR     (INDX)              
220200                                   MOD-TECKEN-ATTR    (INDX)              
220300                                   MOD-DIFFERANS-ATTR (INDX)              
220400                                   MOD-FLOMINV-ATTR   (INDX)              
220500     ADD +1 TO INDX                                                       
220600     END-PERFORM                                                          
220700     .                                                                    
220800     EJECT                                                                
220900 MFS-RENSA-FELRAD-IN SECTION.                                             
221000     MOVE +1 TO INDX                                                      
221100     PERFORM UNTIL INDX > MAX-INDX                                        
221200     MOVE MFS-RENSA-FAELT       TO MID-FELRAD-IN(INDX)                    
221300     ADD +1 TO INDX                                                       
221400     END-PERFORM                                                          
221500     .                                                                    
221600* --- IMS SEKTIONER ---                                                   
221700     SKIP3                                                                
221800                                                                          
221900 IMS-GET-MSG SECTION.                                                     
222000                                                                          
222100     MOVE '  QC' TO GODK-STATUSKODER                                      
222200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
222300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
222400     PERFORM IMS-STATUSKONTROLL                                           
222500     .                                                                    
222600     SKIP3                                                                
222700 IMS-INSERT-MSG SECTION.                                                  
222800                                                                          
222900     IF MSGI-IDLAND-SPR = 'SE'                                            
223000       MOVE '0' TO MFS-KDHUVOMR                                           
223100     ELSE                                                                 
223200       MOVE 'N' TO MFS-KDHUVOMR                                           
223300     END-IF                                                               
223400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
223500     MOVE SPACE TO GODK-STATUSKODER                                       
223600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
223700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
223800     PERFORM IMS-STATUSKONTROLL                                           
223900     .                                                                    
224000     EJECT                                                                
224100 IMS-GET-ARTC-ART SECTION.                                                
224200     MOVE 'GET-ARTC-ART        ' TO WS-IMS                                
224300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
224400          DELIMITED BY SIZE INTO SSA1                                     
224500     MOVE '    ' TO GODK-STATUSKODER                                      
224600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
224700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
224800     PERFORM IMS-STATUSKONTROLL                                           
224900     .                                                                    
225000     EJECT                                                                
225100 IMS-GET-ARTC-CLAG SECTION.                                               
225200     MOVE 'GET-ARTC-CLAG       ' TO WS-IMS                                
225300     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
225400          DELIMITED BY SIZE INTO SSA1                                     
225500     MOVE '    ' TO GODK-STATUSKODER                                      
225600     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                  
225700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
225800     PERFORM IMS-STATUSKONTROLL                                           
225900     .                                                                    
226000     SKIP3                                                                
226100 IMS-REPL-ARTC-CLAG SECTION.                                              
226200     MOVE 'REPL-CLAG           ' TO WS-IMS                                
226300     MOVE '  ' TO GODK-STATUSKODER                                        
226400     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
226500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
226600     PERFORM IMS-STATUSKONTROLL                                           
226700     .                                                                    
226800     EJECT                                                                
226900                                                                          
227000 IMS-GHU-WDK629  SECTION.                                                 
227100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
227200          DELIMITED BY SIZE INTO SSA1                                     
227300     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
227400          DELIMITED BY SIZE INTO SSA2                                     
227500     MOVE   'WDK629  '        TO SSA3                                     
227600     MOVE '  GE' TO GODK-STATUSKODER                                      
227700     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK629 SSA1 SSA2 SSA3         
227800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
227900     PERFORM IMS-STATUSKONTROLL                                           
228000     .                                                                    
228100     SKIP3                                                                
228200 IMS-REPL-WDK629 SECTION.                                                 
228300     MOVE '  ' TO GODK-STATUSKODER                                        
228400     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK629                       
228500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
228600     PERFORM IMS-STATUSKONTROLL                                           
228700     .                                                                    
228800     EJECT                                                                
228900************ WDK7 ARTIKELREGISTER SDC  ***************************        
229000                                                                          
229100 IMS-GHU-WDK711 SECTION.                                                  
229200     MOVE 'GHU-WDK711          ' TO WS-IMS                                
229300     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
229400            DELIMITED BY SIZE INTO SSA1                                   
229500     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
229600            DELIMITED BY SIZE INTO SSA2                                   
229700     MOVE '  GE' TO GODK-STATUSKODER                                      
229800     CALL  CBLTDLI  USING GHU ARTS-PCB DLI-IO-WLARTS11 SSA1 SSA2          
229900     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
230000     PERFORM IMS-STATUSKONTROLL                                           
230100     .                                                                    
230200     SKIP2                                                                
230300                                                                          
230400 IMS-GU-WDK711 SECTION.                                                   
230500     MOVE 'GU-WDK711           ' TO WS-IMS                                
230600     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
230700            DELIMITED BY SIZE INTO SSA1                                   
230800     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
230900            DELIMITED BY SIZE INTO SSA2                                   
231000     MOVE '    ' TO GODK-STATUSKODER                                      
231100     CALL  CBLTDLI  USING GU ARTS-PCB DLI-IO-WLARTS11 SSA1 SSA2           
231200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
231300     PERFORM IMS-STATUSKONTROLL                                           
231400     .                                                                    
231500     SKIP2                                                                
231600 IMS-REPL-SLAGER-SEGM SECTION.                                            
231700     MOVE 'REPL-SLAGER-SEG     ' TO WS-IMS                                
231800     MOVE '  ' TO GODK-STATUSKODER                                        
231900     CALL  CBLTDLI  USING REPL ARTS-PCB DLI-IO-WLARTS11                   
232000     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
232100     PERFORM IMS-STATUSKONTROLL                                           
232200     .                                                                    
232300     EJECT                                                                
232400**** WDH1 BASEN INVENTERINGSKÖN ***************************               
232500                                                                          
232600 IMS-GET-INVA-ART SECTION.                                                
232700     MOVE 'GET-INVA-ART        ' TO WS-IMS                                
232800     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
232900          DELIMITED BY SIZE INTO SSA1                                     
233000     MOVE '  GE' TO GODK-STATUSKODER                                      
233100     CALL CBLTDLI USING GHU INVA-PCB DLI-IO-WDH101 SSA1                   
233200     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
233300     PERFORM IMS-STATUSKONTROLL                                           
233400     .                                                                    
233500     EJECT                                                                
233600 IMS-GET-INVA-INV SECTION.                                                
233700     MOVE 'GET-INVA-INV        ' TO WS-IMS                                
233800     STRING 'WDH111  (WDH111KY=>' W-WDH111KY-MIN-X                        
233900                    '&WDH111KY=<' W-WDH111KY-MAX-X                        
234000                    '&FLINVBEH =' NEJ ')'                                 
234100            DELIMITED BY SIZE INTO SSA1                                   
234200     MOVE '  GE' TO GODK-STATUSKODER                                      
234300     CALL CBLTDLI USING GHNP INVA-PCB DLI-IO-WDH111 SSA1                  
234400     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
234500     PERFORM IMS-STATUSKONTROLL                                           
234600     .                                                                    
234700     SKIP3                                                                
234800 IMS-GNP-WDH121   SECTION.                                                
234900     MOVE 'GNP-WDH121          ' TO WS-IMS                                
235000     MOVE 'WDH121  ' TO SSA1                                              
235100     MOVE '  GE' TO GODK-STATUSKODER                                      
235200     CALL CBLTDLI USING GNP INVA-PCB DLI-IO-WDH121   SSA1                 
235300     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
235400     PERFORM IMS-STATUSKONTROLL                                           
235500     .                                                                    
235600     SKIP3                                                                
235700 IMS-REPL-INVA-INV SECTION.                                               
235800     MOVE 'REPL-INVA-INV       ' TO WS-IMS                                
235900     MOVE '  ' TO GODK-STATUSKODER                                        
236000     CALL CBLTDLI USING REPL INVA-PCB DLI-IO-WDH111                       
236100     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
236200     PERFORM IMS-STATUSKONTROLL                                           
236300     .                                                                    
236400     EJECT                                                                
236500 IMS-ISRT-INVA-ART    SECTION.                                            
236600     MOVE 'ISRT-INVA-ART       ' TO WS-IMS                                
236700     MOVE 'WDH101  ' TO SSA1                                              
236800     MOVE '  ' TO GODK-STATUSKODER                                        
236900     CALL  CBLTDLI  USING ISRT INVA-PCB DLI-IO-WDH101 SSA1                
237000     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
237100     PERFORM IMS-STATUSKONTROLL                                           
237200     .                                                                    
237300     EJECT                                                                
237400 IMS-ISRT-INVA-INV    SECTION.                                            
237500     MOVE 'ISRT-INA-INV        ' TO WS-IMS                                
237600     MOVE 'WDH111  ' TO SSA1                                              
237700     MOVE '  IINI' TO GODK-STATUSKODER                                    
237800     CALL  CBLTDLI  USING ISRT INVA-PCB DLI-IO-WDH111 SSA1                
237900     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
238000     PERFORM IMS-STATUSKONTROLL                                           
238100     .                                                                    
238200     EJECT                                                                
238300 IMS-INSERT-WDH121    SECTION.                                            
238400     MOVE 'ISRT-WDH121         ' TO WS-IMS                                
238500     MOVE 'WDH121  ' TO SSA1                                              
238600     MOVE '  II' TO GODK-STATUSKODER                                      
238700     CALL  CBLTDLI  USING ISRT INVA-PCB DLI-IO-WDH121 SSA1                
238800     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
238900     PERFORM IMS-STATUSKONTROLL                                           
239000     .                                                                    
239100     EJECT                                                                
239200 IMS-GU-WDH111-BSEQ   SECTION.                                            
239300     MOVE 'GU-WDH111-BSEQ      ' TO WS-IMS                                
239400     STRING 'WDH111  (WDH1BSEQ=>' W-WDH1BSEQ-MIN-X                        
239500                    '&WDH1BSEQ=<' W-WDH1BSEQ-MAX-X                        
239600                    '&IDPRTINV =' W-IDPRTINV-X ')'                        
239700            DELIMITED BY SIZE INTO SSA1                                   
239800     MOVE '  GE' TO GODK-STATUSKODER                                      
239900     CALL CBLTDLI USING GHU WDH1B-PCB DLI-IO-WDH1B1 SSA1                  
240000     MOVE WDH1B-STATUS-CODE TO STATUS-WS                                  
240100     PERFORM IMS-STATUSKONTROLL                                           
240200     .                                                                    
240300                                                                          
240400 IMS-GN-WDH111-BSEQ   SECTION.                                            
240500     MOVE 'GN-WDH111-BSEQ      ' TO WS-IMS                                
240600     STRING 'WDH111  (WDH1BSEQ=>' W-WDH1BSEQ-MIN-X                        
240700                    '&WDH1BSEQ=<' W-WDH1BSEQ-MAX-X                        
240800                    '&IDPRTINV =' W-IDPRTINV-X ')'                        
240900            DELIMITED BY SIZE INTO SSA1                                   
241000                                                                          
241100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
241200     CALL CBLTDLI USING GHN WDH1B-PCB DLI-IO-WDH1B1 SSA1                  
241300     MOVE WDH1B-STATUS-CODE TO STATUS-WS                                  
241400     PERFORM IMS-STATUSKONTROLL                                           
241500     .                                                                    
241600                                                                          
241700 IMS-GNP-WDH111-BSEQ   SECTION.                                           
241800     MOVE 'GNP-WDH111-BSEQ     ' TO WS-IMS                                
241900     MOVE   'WDH101' TO SSA1                                              
242000     MOVE '  ' TO GODK-STATUSKODER                                        
242100     CALL CBLTDLI USING GNP WDH1B-PCB DLI-IO-WDH101 SSA1                  
242200     MOVE WDH1B-STATUS-CODE TO STATUS-WS                                  
242300     PERFORM IMS-STATUSKONTROLL                                           
242400     .                                                                    
242500                                                                          
242600************ WDH7 INVENTERINGSHISTORIK  **************************        
242700                                                                          
242800 IMS-GHU-INVHIST-ROT SECTION.                                             
242900     MOVE 'GHU-INVHIST-ROT     ' TO WS-IMS                                
243000     STRING 'WLINVC01(IDARTNR  =' W-IDARTNR-X ')'                         
243100            DELIMITED BY SIZE INTO SSA1                                   
243200     MOVE '  GE' TO GODK-STATUSKODER                                      
243300     CALL  CBLTDLI  USING GHU INVC-PCB DLI-IO-WLINVC01 SSA1               
243400     MOVE INVC-STATUS-CODE TO STATUS-WS                                   
243500     PERFORM IMS-STATUSKONTROLL                                           
243600     .                                                                    
243700     SKIP2                                                                
243800 IMS-ISRT-INVHIST-ROT SECTION.                                            
243900     MOVE 'ISRT-INVHIST-ROT    ' TO WS-IMS                                
244000     MOVE 'WLINVC01 ' TO SSA1                                             
244100     MOVE '  ' TO GODK-STATUSKODER                                        
244200     CALL  CBLTDLI  USING ISRT INVC-PCB DLI-IO-WLINVC01 SSA1              
244300     MOVE INVC-STATUS-CODE TO STATUS-WS                                   
244400     PERFORM IMS-STATUSKONTROLL                                           
244500     .                                                                    
244600     EJECT                                                                
244700 IMS-ISRT-INVHIST-SEGM SECTION.                                           
244800     MOVE 'ISRT-INVHIST-SEGM   ' TO WS-IMS                                
244900     MOVE 'WLINVC11 ' TO SSA1                                             
245000     MOVE '  II' TO GODK-STATUSKODER                                      
245100     CALL  CBLTDLI  USING ISRT INVC-PCB DLI-IO-WLINVC11 SSA1              
245200     MOVE INVC-STATUS-CODE TO STATUS-WS                                   
245300     PERFORM IMS-STATUSKONTROLL                                           
245400     .                                                                    
245500     EJECT                                                                
245600 IMS-GET-BENA SECTION.                                                    
245700                                                                          
245800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-WDD3-X ')'                    
245900          DELIMITED BY SIZE INTO SSA1                                     
246000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
246100          DELIMITED BY SIZE INTO SSA2                                     
246200     MOVE '  GE' TO GODK-STATUSKODER                                      
246300     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA01 SSA1 SSA2             
246400     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
246500     PERFORM IMS-STATUSKONTROLL                                           
246600     .                                                                    
246700     EJECT                                                                
246800********** WDL9 SALDOLOGG*****************************************        
246900 IMS-ISRT-WDL901 SECTION.                                                 
247000     SKIP2                                                                
247100     MOVE 'WLLOGA01 ' TO SSA1                                             
247200     MOVE '  II' TO GODK-STATUSKODER                                      
247300     CALL CBLTDLI USING ISRT LOGA-PCB WLLOGA01 SSA1                       
247400     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
247500     PERFORM IMS-STATUSKONTROLL                                           
247600     .                                                                    
247700     EJECT                                                                
247800********** WDR8 PEDAL*********************************************        
247900 IMS-ISRT-WDR801   SECTION.                                               
248000                                                                          
248100     MOVE 'WLFILB01 '   TO SSA1                                           
248200     MOVE '  II' TO GODK-STATUSKODER                                      
248300     CALL CBLTDLI USING ISRT WFILB-PCB A08-WFILB01 SSA1                   
248400     MOVE WFILB-STATUS-CODE TO STATUS-WS                                  
248500     PERFORM IMS-STATUSKONTROLL                                           
248600     .                                                                    
248700     EJECT                                                                
248800********** WDR9 PEDAL*********************************************        
248900 IMS-ISRT-WDR901 SECTION.                                                 
249000     SKIP2                                                                
249100     MOVE 'WLSAPA01 ' TO SSA1                                             
249200     MOVE '  II' TO GODK-STATUSKODER                                      
249300     CALL CBLTDLI USING ISRT SAPA-PCB WLSAPA01 SSA1                       
249400     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
249500     PERFORM IMS-STATUSKONTROLL                                           
249600     .                                                                    
249700     EJECT                                                                
249800************* WDR4 4505 HÄNDELSEBAS ( TÄCKNING ORDERADSREG.) ****         
249900                                                                          
250000 IMS-ISRT-450511 SECTION.                                                 
250100                                                                          
250200     STRING 'WL450501(WDGXKEY  =' W-WDGX-4505-KEY-X ')'                   
250300            DELIMITED BY SIZE INTO SSA1                                   
250400     MOVE 'WL450511 '                   TO SSA2                           
250500     MOVE '  ' TO GODK-STATUSKODER                                        
250600     CALL  CBLTDLI  USING ISRT 4505-PCB WL450511 SSA1 SSA2                
250700     MOVE 4505-STATUS-CODE TO STATUS-WS                                   
250800     PERFORM IMS-STATUSKONTROLL                                           
250900     .                                                                    
251000     EJECT                                                                
251100                                                                          
251200************ WDG2 XXEF HÄNDELSEBAS (UTREDNINGSSALDO) *************        
251300                                                                          
251400 IMS-GHU-ART-UTREDNSALDO SECTION.                                         
251500                                                                          
251600     STRING 'WLXXEF01(WDGXKEY  =' W-WDGXKEY-ROT-X ')'                     
251700           DELIMITED BY SIZE INTO SSA1                                    
251800     STRING 'WLXXEF11(WDGXKEY  =' W-IDARTNR-UTR-X ')'                     
251900           DELIMITED BY SIZE INTO SSA2                                    
252000     MOVE '  GE' TO GODK-STATUSKODER                                      
252100     CALL  CBLTDLI  USING GHU XXEF-PCB WLXXEF11 SSA1 SSA2                 
252200     MOVE XXEF-STATUS-CODE TO STATUS-WS                                   
252300     PERFORM IMS-STATUSKONTROLL                                           
252400     .                                                                    
252500     SKIP2                                                                
252600 IMS-DELETE-ART-UTREDNSALDO SECTION.                                      
252700                                                                          
252800     MOVE '  ' TO GODK-STATUSKODER                                        
252900     CALL  CBLTDLI  USING DLET XXEF-PCB WLXXEF11                          
253000     MOVE XXEF-STATUS-CODE TO STATUS-WS                                   
253100     PERFORM IMS-STATUSKONTROLL                                           
253200     .                                                                    
253300     EJECT                                                                
253400 IMS-GU-WDB601    SECTION.                                                
253500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
253600          DELIMITED BY SIZE INTO SSA1                                     
253700     MOVE '  ' TO GODK-STATUSKODER                                        
253800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
253900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
254000     PERFORM IMS-STATUSKONTROLL                                           
254100     .                                                                    
254200     EJECT                                                                
254300 IMS-STATUSKONTROLL SECTION.                                              
254400                                                                          
254500     SET STATUS-IX TO 1                                                   
254600     SEARCH GODK-STATUS                                                   
254700       AT END                                                             
254800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
254900         DELIMITED BY SIZE INTO FELTEXT                                   
255000         CALL FELLOG                                                      
255100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
255200         CONTINUE                                                         
255300     END-SEARCH                                                           
255400     .                                                                    
