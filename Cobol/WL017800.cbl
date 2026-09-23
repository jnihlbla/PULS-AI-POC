000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL017800.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   04/10/25.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.LDC.AUTOMATICADJ                                
000800*    WEB-LDC: WL017800 PROGRAM IS A REPLICA OF W5030800 PROGRAM           
000900*             AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                    
001000*                                                                         
001100*    FUNCTION:                                                            
001200*        *                                                                
001300*        AUTOMATJUSTERING, INVENTERINGSBILD                               
001400*        PROGRAMET GÖR ETT AV FÖLJANDE ALT.                               
001500*        1. OM HITTAT ANTAL KLARAR INVENTERINGSREGLERNA                   
001600*           UPPDATERAS INVENTERINGSHIST OCH POST TAS BORT IFRÅN           
001700*           INVENTERINGSKÖN. ARTIKELREGISTRER UPPDATERAS MED              
001800*           MED NYTT LAGERSALDO. EKONOMISKA BASER UPPDATERAS              
001900*                                                                         
002000*        2. OM HITTAT ANTAL INTE KLARAR REGLERNA UPPDATERAS BARA          
002100*           INVENTERINGSKÖN SÅ ATT ARTIKEL KAN OM INVENTERAS.             
002200*           PÅ LISTNR NOLLAS ALLT UTOM FÖRSTA SIFFRAN                     
002300*           1 = INVENTERAT EN GÅNG                                        
002400*           2 = INVENTERAT TVÅ GÅNGER                                     
002500*           3 = KAN INTE INVENTERAS FLER GÅNGER                           
002600*                                                                         
002700*                                                                         
002800*        UPDATES WDK7 (FLREFNYO = NOO) WHEN NOT DC11.                     
002900*                                                                         
003000*    2017-08     ETRACKER 10306981                                        
003100*                                                                         
003200*    INDATA.                                                              
003300*        TRANSACTION: WL0178T                                             
003400*        REQUEST:     WL0178I1                                            
003500*                                                                         
003600*    OUTDATA.                                                             
003700*        RESPONSE:    WL0178O1                                            
003800                                                                          
003900     SKIP3                                                                
004000 ENVIRONMENT DIVISION.                                                    
004100     SKIP2                                                                
004200 INPUT-OUTPUT SECTION.                                                    
004300                                                                          
004400 FILE-CONTROL.                                                            
004500     EJECT                                                                
004600 DATA DIVISION.                                                           
004700     SKIP3                                                                
004800 FILE SECTION.                                                            
004900     EJECT                                                                
005000 WORKING-STORAGE SECTION.                                                 
005100 77  IDPGM                       PIC X(08)   VALUE 'WL017800'.            
005200                                                                          
005300*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
005400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005500 77  KDRC-DISPLAY                PIC Z(5).                                
005510 77  WS-KVANTAL                  PIC S9(7)   VALUE ZERO  COMP-3.          
005600 77  CURR-DISPLAY                PIC X(16) VALUE 'MAIN'.                  
005700 77  CURR-SECTION                PIC X(16) VALUE 'MAIN'.                  
005800 77  CURR-IMS-SECTION            PIC X(16) VALUE SPACE.                   
005900 77  WS-IDSKYLT-CN               PIC X(3)   VALUE 'RCN'.                  
006000 77  WS-IDSKYLT-GB               PIC X(3)   VALUE 'GB '.                  
006100 77  WS-CP-UTF8                  PIC X(4)   VALUE 'UTF8'.                 
006200 77  WS-CP-EBCDIC                PIC X(3)   VALUE '278'.                  
006300 77  WS-CONV-BEART               PIC X(25)  VALUE SPACE.                  
006400                                                                          
006500 77  WS-FLTRACK                  PIC X(1)    VALUE 'N'.                   
006510 77  WS-FLLOCAL                  PIC X(1)    VALUE 'N'.                   
006520 77  WS-USE-TEMP                 PIC X(1)    VALUE 'N'.                   
006530 77  WS-VALD-KVAVIS              PIC 9(6)    VALUE ZERO.                  
006540 77  WS-TRCK-KVANTMOT            PIC 9(6)    VALUE ZERO.                  
006550 77  WS-TEMP-KVAVIS              PIC 9(6)    VALUE ZERO.                  
006560 77  WS-TEMP-KVAVIS-SHOW         PIC Z(4)9.                               
006570 77  WS-LOGT-SIGN                PIC X(1).                                
006580 77  WS-SLAG-KVLS                PIC S9(7)   COMP-3.                      
006590 77  WS-TRCK-IDTRACK             PIC X(25).                               
006600 77  IDTRACK-QTY-SW              PIC X       VALUE 'N'.                   
006700     88  IDTRACK-QTY-DONE                    VALUE 'J'.                   
006800     88  IDTRACK-QTY-NOT-DONE                VALUE 'N'.                   
006900                                                                          
007000 77  WS-DIFF                     PIC X       VALUE 'N'.                   
007100     88  NOT-DIFF-OK                         VALUE 'N'.                   
007200     88  DIFF-OK                             VALUE 'J'.                   
007300                                                                          
007400                                                                          
007500 77  JA                          PIC X       VALUE 'J'.                   
007600 77  NEJ                         PIC X       VALUE 'N'.                   
007700 77  IX                          PIC 9(9)    VALUE 0.                     
007800 77  INDX                        PIC 9(9)    VALUE 0.                     
007900 77  INDX2                       PIC 9(9)    VALUE 0.                     
008000 77  MAX-INDX                    PIC 9(9)    VALUE 10.                    
008100 77  MSG-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
008200 77  TRANS-TID                   PIC 9(9).                                
008300 77  WS-KVRADER                  PIC 9(9)    VALUE 0.                     
008400                                                                          
008500 77  WS-IDELMT-ERROR             PIC X(16)  VALUE SPACE.                  
008600 77  WS-IDMSG-ERROR              PIC X(03)  VALUE SPACE.                  
008700 77  WS-IDMSG-INFO               PIC X(03)   VALUE SPACE.                 
008800                                                                          
008900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009000     88  NYCKLAR-OK                          VALUE 'J'.                   
009100     88  NYCKLAR-FEL                         VALUE 'N'.                   
009200                                                                          
009300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009400     88  INDATA-OK                           VALUE 'J'.                   
009500     88  INDATA-FEL                          VALUE 'N'.                   
009600                                                                          
009700 77  TABELL-IFYLLD-SW            PIC X       VALUE 'N'.                   
009800     88  TABELL-IFYLLD                       VALUE 'J'.                   
009900     88  TABELL-TOM                          VALUE 'N'.                   
010000                                                                          
010100 77  POST-SW                     PIC X       VALUE 'N'.                   
010200     88  POST-FINNS                          VALUE 'J'.                   
010300     88  POST-SAKNAS                         VALUE 'N'.                   
010400                                                                          
010500 77  ALLT-OK-SW                  PIC X       VALUE 'J'.                   
010600     88  ALLT-OK                             VALUE 'J'.                   
010700     88  ALLT-FEL                            VALUE 'N'.                   
010800                                                                          
010900 77  FEL-US-SW                   PIC X       VALUE 'N'.                   
011000     88  FEL-US                              VALUE 'J'.                   
011100                                                                          
011200 77  FORTSATT-SW                 PIC X       VALUE 'J'.                   
011300     88  FORTSATT                            VALUE 'J'.                   
011400     88  FORTSATT-INTE                       VALUE 'N'.                   
011500                                                                          
011600 77  UPDATE-SW                   PIC X       VALUE 'N'.                   
011700     88  UPDATE-DONE                         VALUE 'J'.                   
011800     88  NO-UPDATE                           VALUE 'N'.                   
011900                                                                          
012000 77  FEL-SW                      PIC X       VALUE 'N'.                   
012100     88  FEL-FINNS                           VALUE 'J'.                   
012200     88  FEL-FINNS-EJ                        VALUE 'N'.                   
012300                                                                          
012400 77  OMINVENTERING-SW            PIC X       VALUE 'N'.                   
012500     88  OMINVENTERING                       VALUE 'J'.                   
012600     88  INGEN-OMINVENTERING                 VALUE 'N'.                   
012700 77  FIRST-REC-TRANS-SW          PIC X       VALUE 'N'.                   
012800     88  NOT-FIRST-REC-TRANS                 VALUE 'N'.                   
012900     88  FIRST-REC-TRANS                     VALUE 'J'.                   
013000 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
013100 77  W-IDARTNR-EDIT-X            PIC Z(9).                                
013200 77  WS-ADDRESS-WHSTOCKA         PIC X(50)                                
013300       VALUE 'CARPARTS.PULS.WHSTOCKADJ'.                                  
013400 77  WS-ADDRESS-MQASYNC          PIC X(50)                                
013401       VALUE 'CARPARTS.PULS.MQASYNC'.                                     
013402                                                                          
013403 01  WS-IDPRTOMG                 PIC S9      VALUE ZERO  COMP-3.          
013404 01  WS-IDLOPNR                  PIC S9(5)   VALUE ZERO  COMP-3.          
013405 01  WS-IDUSER                   PIC X(8)    VALUE SPACE.                 
013406 01  WS-MSGI-IDUSER              PIC X(8)    VALUE SPACE.                 
013407 01  WS-PRARTSTD             PIC S9(7)V9(2)  VALUE ZERO  COMP-3.          
013408 01  WS-PRAVCOST             PIC S9(7)V9(2)  VALUE ZERO  COMP-3.          
013409 01  WS-BELOPP               PIC S9(7)V9(2)  VALUE ZERO  COMP-3.          
013500 01  WS-TISEGKEY                 PIC 9(9)    VALUE ZERO.                  
013600 01  WS-TIINVDAT                 PIC 9(5)    VALUE ZERO  COMP-3.          
013700 01  WS-KDINVKAT                 PIC S9(3)   VALUE ZERO  COMP-3.          
013800 01  WS-KDJUSTYP                 PIC S9      VALUE ZERO  COMP-3.          
013900 01  WS-ANTAL                    PIC S9(9)   VALUE ZERO.                  
014000 01  WS-ANTAL-N                  PIC 9(8)    VALUE ZERO.                  
014100 01  WS-TECKEN                   PIC X       VALUE SPACE.                 
014200 01  WS-KDSORT                   PIC X(2).                                
014300 01  WS-XLAG-KVLS                PIC S9(7)   VALUE ZERO.                  
014400 01  WS-KVLS                     PIC S9(7)   VALUE ZERO.                  
014500 01  WS-TAB-KVLS                 PIC S9(7)   VALUE ZERO.                  
014600 01  WS-KVLS-NEW                 PIC S9(7)   VALUE ZERO.                  
014700 01  WS-KVLS-UPD                 PIC S9(7)   VALUE ZERO.                  
014800* 01  WS-KVLS-NEW                 PIC S9(7)   VALUE ZERO  COMP-3.         
014900 01  WS-LOGG-UREF1               PIC X(17)   VALUE ZERO.                  
015000 01  WS-SORT-ADLAGOMR            PIC 9(3)    VALUE ZERO.                  
015100 01  WS-SORT-ADGANG              PIC 9(3)    VALUE ZERO.                  
015200 01  WS-SORT-ADPLATS             PIC 9(5)    VALUE ZERO.                  
015300 01  WS-SORT-PRIO                PIC 9       VALUE ZERO.                  
015400 01  WS-SORT-VVKL                PIC 9       VALUE ZERO.                  
015500 01  W-EKH-IDARTNR               PIC X(9)    VALUE SPACE.                 
015600 01  WS-DAGENS-TID               PIC 9(8).                                
015700 01  AKTUELL-TID-X.                                                       
015800     03  AKTUELL-TTMM-LOC    PIC 9(4).                                    
015900     03  FILLER              PIC 9(4).                                    
016000 01  W-DATUM-Y.                                                           
016001     03  W-DATUM-LOCAL       PIC 9(6).                                    
016002                                                                          
016003 01  WS-DAGENS-DATUM            PIC 9(8).                                 
016004 01  WS-DAGENS-DATUM-GRP.                                                 
016005     03 DAGENS-AA               PIC 9(2).                                 
016100     03 DAGENS-AAMMDD           PIC 9(6).                                 
016200                                                                          
016300 01  WS-TISEGKEYAREA.                                                     
016400     03 WS-TIAAAAMMDDL          PIC 9(9)     VALUE ZERO.                  
016500     03 FILLER REDEFINES WS-TIAAAAMMDDL.                                  
016600        05 WS-TISEGKEY-DAT      PIC 9(8).                                 
016700        05 WS-TISEGKEY-LOPNR    PIC 9.                                    
016800                                                                          
016900 01  WS-HAENDELSE-TYPER.                                                  
017000     03 WS-EKH-KDEKSHT.                                                   
017100        05  WS-KDEKSHT-1        PIC 9(2).                                 
017200        05  WS-KDEKSHT-2        PIC 9.                                    
017300                                                                          
017400 01  WS-SPAR-HIST.                                                        
017500     03 WS-DAREGDAT-CRE         PIC 9(8).                                 
017600     03 WS-DAREGDAT-PR1         PIC 9(8).                                 
017700     03 WS-DAREGDAT-PR2         PIC 9(8).                                 
017800     03 WS-DAREGDAT-PR3         PIC 9(8).                                 
017900     03 WS-IDUSER-PR1           PIC X(8).                                 
018000     03 WS-IDUSER-PR2           PIC X(8).                                 
018100     03 WS-IDUSER-PR3           PIC X(8).                                 
018200     03 WS-IDUSER-CRE           PIC X(8).                                 
018300                                                                          
018400                                                                          
018500 01  SPAR-AREA.                                                           
018600   03  SPAR-IDTRANS              PIC X(8)   VALUE 'L178'.                 
018700   03  SPAR-IDUSER               PIC X(8)   VALUE  ZERO.                  
018800   03  SPAR-IDLISTNR             PIC 9(6)   VALUE  ZERO.                  
018900   03  SPAR-TABELL.                                                       
019000       05 TABELL OCCURS 10.                                               
019100         07 ART-TABELL.                                                   
019200           09 TAB-ARTIKEL         PIC S9(9)        VALUE ZERO.            
019300           09 TAB-BENAMNING       PIC X(25)        VALUE SPACE.           
019400           09 TAB-KVAKS           PIC S9(7)        VALUE ZERO.            
019500           09 TAB-KVLS            PIC S9(7)        VALUE ZERO.            
019600           09 TAB-KVEFRS          PIC S9(7)        VALUE ZERO.            
019700           09 TAB-MESSAGE         PIC X(3)         VALUE SPACE.           
019800   03  SPAR-TABELL2.                                                      
019900       05 TABELL2 OCCURS 10.                                              
020000         07 ANTALS-TABELL.                                                
020100           09 TAB-ANTAL           PIC 9(7)         VALUE ZERO.            
020200           09 TAB-TECKEN          PIC X            VALUE SPACE.           
020300           09 TAB-DIFFERANS       PIC 9(7)         VALUE ZERO.            
020400           09 TAB-FLOMINV         PIC X            VALUE SPACE.           
020500                                                                          
020600   03  SPAR-FEL-TABELL.                                                   
020700       05 TABELL3 OCCURS 10.                                              
020800           09 FEL-ARTIKEL         PIC S9(9)        VALUE ZERO.            
020900           09 FEL-BENAMNING       PIC X(25)        VALUE SPACE.           
021000           09 FEL-KVAKS           PIC S9(7)        VALUE ZERO.            
021100           09 FEL-KVLS            PIC S9(7)        VALUE ZERO.            
021200           09 FEL-KVEFRS          PIC S9(7)        VALUE ZERO.            
021300           09 FEL-ANTAL           PIC 9(7)         VALUE ZERO.            
021400           09 FEL-TECKEN          PIC X            VALUE SPACE.           
021500           09 FEL-DIFFERANS       PIC 9(7)         VALUE ZERO.            
021600           09 FEL-FLOMINV         PIC X            VALUE SPACE.           
021700           09 FEL-RAD             PIC X            VALUE SPACE.           
021800           09 FEL-MESSAGE         PIC X(3)         VALUE SPACE.           
021900     EJECT                                                                
022000 01  SORT-TAB-HJALP-AREA.                                                 
022100     03  TAB-SORT-MAX             PIC S9(9)  COMP    VALUE +10.           
022200     03  TAB-SORT-STEG-LANGD      PIC S9(9)  COMP    VALUE +68.           
022300*    03  TAB-SORT-STEG-LANGD      PIC S9(9)  COMP    VALUE +57.           
022400     03  TAB-SORT-ANTAL           PIC S9(9)  COMP   VALUE +10.            
022500     03  TAB-SORT-LANGD           PIC S9(9)  COMP   VALUE +22.            
022600                                                                          
022700 01  FILLER                       PIC X(16) VALUE 'SORT-TABELL'.          
022800 01  SORT-TABELL.                                                         
022900     03  SORT-TABELL-AREA OCCURS 10 INDEXED BY SORT-IX.                   
023000        05 TAB-SORT-FAELT.                                                
023100          07 TAB-SORT-ADLAGOMR    PIC 9(3).                               
023200          07 TAB-SORT-ADGANG      PIC 9(3).                               
023300          07 TAB-SORT-ADPLATS     PIC 9(5).                               
023400          07 TAB-SORT-PRIO        PIC 9.                                  
023500          07 TAB-SORT-VVKL        PIC 9.                                  
023600          07 TAB-SORT-IDARTNR     PIC 9(9).                               
023700        05 TAB-SORT-BENAMNING     PIC X(25).                              
023800        05 TAB-SORT-KVAKS         PIC S9(7).                              
023900        05 TAB-SORT-KVLS          PIC S9(7).                              
024000        05 TAB-SORT-KVEFRS        PIC S9(7).                              
024100*       05 TAB-SORT-MESSAGE       PIC X(3).                               
024200     EJECT                                                                
024300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
024400 01  GENERAL-SUBPROGRAMS.                                                 
024500     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
024600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
024700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
024800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
024900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
025000     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
025010     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
025100     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
025200     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
025300     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
025400     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
025500     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
025510     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
025600     SKIP3                                                                
025700*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
025800*01 -COPY WDATAREA                                                        
025900 01  FILLER              PIC X(16) VALUE 'WL01TIDZ-AREA'.                 
025901*01  -COPY WL01TIDZ                                                       
025902                                                                          
026000*    --- PARAMETRAR TILL SUBPROGRAM W009CIA                               
026100*01 -COPY W009CIA                                                         
026200                                                                          
026300*    NOTAFISCAL                                                           
026400 01  NOTF-AREA.                                                           
026500*    03  -COPY W611NOTF                                                   
026600     EJECT                                                                
026700 01  FILLER                      PIC X(16)   VALUE 'MSG PROP'.            
026800*01  -COPY WZ04PROP                                                       
026801     EJECT                                                                
026802*    --- PARAMETERS TO ABEND                                              
026803                                                                          
026804 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
026805 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
026806 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
026807     SKIP3                                                                
026900 01  FILLER                    PIC X(8)    VALUE 'WDECAREA'.              
027000     SKIP3                                                                
027100*01       -COPY WDECAREA                                                  
027200     EJECT                                                                
027300 01  MESSAGE-CODES.                                                       
027400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
027500     03  ERR-UNAUTHORIZED        PIC X(3)    VALUE '00A'.                 
027600     03  ERR-INVALID             PIC X(3)    VALUE '023'.                 
027700     EJECT                                                                
027800 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
027900*01  -COPY WZ01SEND                                                       
027901     EJECT                                                                
027902*                                                                         
027903 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
028000     SKIP3                                                                
028100*01  -COPY WZ01SUB                                                        
028200     EJECT                                                                
028300                                                                          
028400 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
028500*01  -COPY WMSGCONV                                                       
028600                                                                          
028700 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH'.            
028800*01  -COPY WZ01AUTH                                                       
028900                                                                          
029000 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
029100     SKIP3                                                                
029200 01  REQU-AREA.                                                           
029300*    03  -COPY WZ01REQ2                                                   
029400*    03  -COPY WL0178I1                                                   
029500     EJECT                                                                
029600 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
029700     SKIP3                                                                
029800 01  RESP-AREA.                                                           
029900*    03  -COPY WZ01RES2                                                   
030000*    03  -COPY WL0178O1                                                   
030100     EJECT                                                                
030200*                                                                         
030300 01  FILLER                      PIC X(16)   VALUE 'DC-CODES '.           
030400                                                                          
030500*01  -COPY WWDC99                                                         
030600*                                                                         
030700*01  -COPY WWDCKONS                                                       
030800*                                                                         
030900*01  -COPY WTRAUTF8                                                       
031000*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
031100*                                                                         
031200 01    IMS-WS.                                                            
031300   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
031400     SKIP3                                                                
031500*                        **** STATUS-KOD FRÅN IMS                         
031600   03    STATUS-WS       PIC XX.                                          
031700         88  SEGMENT-FINNS       VALUE '  '.                              
031800         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
031900         88  SEGMENT-FINNS-REDAN VALUE 'II'.                              
032000         88  INDEX-FINNS-REDAN   VALUE 'NI'.                              
032100         88  SEGMENT-SLUT        VALUE 'GB'.                              
032200                                                                          
032300   03    GODK-STATUSKODER.                                                
032400     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
032500                                                                          
032600 01    SSA1                      PIC X(132).                              
032700 01    SSA2                      PIC X(132).                              
032800 01    SSA3                      PIC X(132).                              
032900                                                                          
033000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
033100     SKIP3                                                                
033200 01  W-WDGX-4505-KEY-X.                                                   
033300     03  W-IDHTYP-4505       PIC X(4)    VALUE '4505'.                    
033400     03  W-IDDC-4505         PIC X(2)    VALUE '11'.                      
033500     03  FILLER              PIC X(24) VALUE LOW-VALUE.                   
033600 01  NYCKLAR-TILL-DLI.                                                    
033700     03  W-IDDC-X.                                                        
033800         05  W-IDDC              PIC X(2)     VALUE SPACE.                
033900                                                                          
034000     03  W-IDARTNR-X.                                                     
034100         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
034200                                                                          
034300     03  W-IDARTNR-UTR-X.                                                 
034400         05  W-IDDC-UTR          PIC X(2)     VALUE SPACE.                
034500         05  W-IDARTNR-UTR       PIC S9(9)    VALUE ZERO COMP-3.          
034600         05  FILLER              PIC X(8)     VALUE LOW-VALUE.            
034700                                                                          
034800     03  W-KDSEGKEY-X.                                                    
034900         05  W-KDSEGKEY          PIC X(1)     VALUE '1'.                  
035000                                                                          
035100     03  W-IDSKYLT-X.                                                     
035200         05  W-IDSKYLT           PIC X(3)     VALUE SPACE.                
035300                                                                          
035400     03 W-IDPRTINV-X.                                                     
035500         05  W-IDPRTOMG          PIC S9       COMP-3.                     
035600         05  W-IDLOPNR           PIC S9(5)    COMP-3.                     
035700                                                                          
035800     03  W-WDGXKEY-ROT-X.                                                 
035900         05  FILLER              PIC X(4)     VALUE '5115'.               
036000         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
036100                                                                          
036200     03  W-WDH1BSEQ-MIN-X.                                                
036300         05  W-SEQB-IDDC-MIN     PIC X(2)     VALUE SPACE.                
036400         05  W-SEQB-DAREGDAT-MIN PIC S9(9)    VALUE ZERO COMP-3.          
036500         05  W-SEQB-ADLAGOMR-MIN PIC S9(3)    VALUE ZERO COMP-3.          
036600         05  W-SEQB-ADGANG-MIN   PIC S9(3)    VALUE ZERO COMP-3.          
036700         05  W-SEQB-ADPLATS-MIN  PIC S9(5)    VALUE ZERO COMP-3.          
036800         05  W-SEQB-IDPRTOMG-MIN PIC S9       VALUE ZERO COMP-3.          
036900         05  W-SEQB-IDLOPNR-MIN  PIC S9(5)    VALUE ZERO COMP-3.          
037000                                                                          
037100     03  W-WDH1BSEQ-MAX-X.                                                
037200         05  W-SEQB-IDDC-MAX     PIC X(2)     VALUE SPACE.                
037300         05  W-SEQB-DAREGDAT-MAX PIC S9(9)                                
037400                                 VALUE +999999999 COMP-3.                 
037500         05  W-SEQB-ADLAGOMR-MAX PIC S9(3) VALUE +999    COMP-3.          
037600         05  W-SEQB-ADGANG-MAX   PIC S9(3) VALUE +999    COMP-3.          
037700         05  W-SEQB-ADPLATS-MAX  PIC S9(5) VALUE +99999  COMP-3.          
037800         05  W-SEQB-IDPRTOMG-MAX PIC S9    VALUE +9      COMP-3.          
037900         05  W-SEQB-IDLOPNR-MAX  PIC S9(5) VALUE +99999  COMP-3.          
038000                                                                          
038100     03  W-WDH111KY-X.                                                    
038200         05  W-IDDC-WDH1         PIC X(2)          VALUE SPACE.           
038300         05  W-KDINVKAT-WDH1     PIC S9(3) COMP-3  VALUE ZERO.            
038400         05  W-TISEGKEY-WDH1     PIC S9(9) COMP-3  VALUE ZERO.            
038500         05  W-DAREGDAT-SORT-WDH1                                         
038600                                 PIC  9(8)         VALUE ZERO.            
038700                                                                          
038800     03  W-WDH111KY-MIN-X.                                                
038900         05  W-IDDC-WDH1-MIN     PIC X(2)          VALUE SPACE.           
039000         05  W-KDINVKAT-WDH1-MIN PIC S9(3) COMP-3  VALUE ZERO.            
039100         05  W-TISEGKEY-WDH1-MIN PIC S9(9) COMP-3  VALUE ZERO.            
039200         05  W-DAREGDAT-SORT-MIN     PIC  9(8)     VALUE ZERO.            
039300                                                                          
039400     03  W-WDH111KY-MAX-X.                                                
039500         05  W-IDDC-WDH1-MAX     PIC X(2)          VALUE SPACE.           
039600         05  W-KDINVKAT-WDH1-MAX PIC S9(3) COMP-3  VALUE 999.             
039700         05  W-TISEGKEY-WDH1-MAX PIC S9(9) COMP-3                         
039800                                           VALUE +999999999.              
039900         05  W-DAREGDAT-SORT-MAX     PIC  9(8)     VALUE 99999999.        
040000                                                                          
040100     03  W-IDARTNR-WDD3-X.                                                
040200         05  W-IDARTNR-WDD3      PIC S9(9) VALUE ZERO COMP-3.             
040300                                                                          
040400     03  W-IDDC-B6-X.                                                     
040500         05 W-IDDC-B6                  PIC X(2).                          
040600                                                                          
040700     03  W-DAINLEV-X.                                                     
040800         05  W-DAINLEV           PIC 9(16).                               
040900                                                                          
041000     EJECT                                                                
041100*                            IMS FUNKTIONSKODER                           
041200*01    -COPY W0003                                                        
041300                                                                          
041400  01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH101'.                     
041500  01  DLI-IO-WDH101.                                                      
041600*     03  -COPY WDH101 -PRE INVA-                                         
041700                                                                          
041800  01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH111'.                     
041900  01  DLI-IO-WDH111.                                                      
042000*     03  -COPY WDH111 -PRE  INVA-                                        
042100                                                                          
042200  01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH121'.                     
042300  01  DLI-IO-WDH121.                                                      
042400*     03  -COPY WDH121 -PRE  INVA-                                        
042500                                                                          
042600                                                                          
042700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH1B1'.                      
042800 01  DLI-IO-WDH1B1.                                                       
042900*    03 -COPY WDH111 -PRE SEQB-                                           
043000                                                                          
043100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBENA01'.                    
043200 01  DLI-IO-WLBENA01.                                                     
043300*    03  -COPY WDD311                                                     
043400                                                                          
043500 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK601'.           
043600 01  DLI-IO-WDK601.                                                       
043700*  03  -COPY WDK601.                                                      
043800                                                                          
043900 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK611'.           
044000 01  DLI-IO-WDK611.                                                       
044100*  03  -COPY WDK611.                                                      
044200                                                                          
044300 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK629'.           
044400 01  DLI-IO-WDK629.                                                       
044500*    03  -COPY WDK629                                                     
044600                                                                          
044700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
044800 01  DLI-IO-WDK711.                                                       
044900*    03  -COPY WDK711                                                     
045000                                                                          
045100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK728'.                      
045200 01  DLI-IO-WDK728.                                                       
045300*   03   -COPY WDK728                                                     
045400                                                                          
045500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL301'.                      
045600 01  DLI-IO-WDL301.                                                       
045700*03  -COPY WDL301                                                         
045800     EJECT                                                                
045900                                                                          
046000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLINVC01'.                    
046100 01  DLI-IO-WLINVC01.                                                     
046200*    03  -COPY WDH701                                                     
046300                                                                          
046400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLINVC11'.                    
046500 01  DLI-IO-WLINVC11.                                                     
046600*    03  -COPY WDH711                                                     
046700                                                                          
046800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLSAPA01'.                    
046900*01  WLSAPA01    -COPY WDR901                                             
047000*    05 -COPY W510EKHA -RED FIL-WDR901-DATA                               
047100                                                                          
047200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLOGA01'.                    
047300*01  WLLOGA01    -COPY WDL901                                             
047400                                                                          
047500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5116'.                    
047600*01  WLXXEF11     -COPY WDGX5116.                                         
047700                                                                          
047800 01  FILLER              PIC X(16)   VALUE 'WDGX4506'.                    
047900*                                                                         
048000*01  WL450511     -COPY WDGX4506.                                         
048100                                                                          
048200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
048300 01   DLI-IO-AREA-B601.                                                   
048400*     03  -COPY WDB601                                                    
048500                                                                          
048600 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDR801'.              
048700*01  WDR801      -COPY WDR801 -PRE WDR8-                                  
048800*    05 -COPY W510EKHA -RED WDR8-FIL-WDR801-DATA -PRE WDR8-               
048900*    05 -COPY W510A08  -RED WDR8-FIL-WDR801-DATA -PRE A08-                
049000                                                                          
049100     EJECT                                                                
049200 LINKAGE SECTION.                                                         
049300*01  -COPY W0009   -PRE MSG-                                              
049310 01  MQASYNC-PCB                 PIC X.                                   
049400 01  ATAB-PCB                    PIC X.                                   
049410     EJECT                                                                
049500*01  -COPY W0008  -PRE WDK6-                                              
049600     05  FILLER                  PIC X.                                   
049700                                                                          
049800*01  -COPY W0008  -PRE INVA-                                              
049900     05  FILLER                  PIC X.                                   
050000                                                                          
050100*01  -COPY W0008  -PRE INVC-                                              
050200     05  FILLER                  PIC X.                                   
050300                                                                          
050400*01  -COPY W0008  -PRE BENA-                                              
050500     05  FILLER                  PIC X.                                   
050600                                                                          
050700*01  -COPY W0008  -PRE LOGA-                                              
050800     05  FILLER                  PIC X.                                   
050900                                                                          
051000*01  -COPY W0008  -PRE SAPA-                                              
051100     05  FILLER                  PIC X.                                   
051200                                                                          
051300*01  -COPY W0008  -PRE WDH1B-                                             
051400     05  FILLER                  PIC X.                                   
051500                                                                          
051600*01  -COPY W0008  -PRE XXEF-                                              
051700     05  FILLER                  PIC X.                                   
051800                                                                          
051900*01  -COPY W0008  -PRE 4505-                                              
052000     05  FILLER                  PIC X.                                   
052100                                                                          
052200*01  -COPY W0008  -PRE WDK7-                                              
052300     05  FILLER                  PIC X.                                   
052400                                                                          
052500*01  -COPY W0008  -PRE WDB6-                                              
052600     05  FILLER                  PIC X.                                   
052700                                                                          
052800*01  -COPY W0008  -PRE WDR8-                                              
052900     05  FILLER                  PIC X.                                   
053000*01  -COPY W0008     -PRE WDL3-                                           
053100         05  FILLER           PIC X.                                      
053200                                                                          
053300     EJECT                                                                
053400 PROCEDURE DIVISION  USING  MSG-PCB MQASYNC-PCB                           
053410                            ATAB-PCB WDK6-PCB INVA-PCB                    
053500                            INVC-PCB  BENA-PCB LOGA-PCB SAPA-PCB          
053600                            WDH1B-PCB XXEF-PCB 4505-PCB                   
053700                            WDK7-PCB WDB6-PCB WDR8-PCB                    
053800                            WDL3-PCB.                                     
053900 MAIN SECTION.                                                            
054000     ENTRY 'DLITCBL' USING  MSG-PCB MQASYNC-PCB                           
054010                            ATAB-PCB WDK6-PCB INVA-PCB                    
054100                            INVC-PCB  BENA-PCB LOGA-PCB SAPA-PCB          
054200                            WDH1B-PCB XXEF-PCB 4505-PCB                   
054300                            WDK7-PCB WDB6-PCB WDR8-PCB                    
054400                            WDL3-PCB.                                     
054500                                                                          
054600     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
054700     IF SUB-KDRC = 0                                                      
054800       PERFORM A-INIT                                                     
054900       IF NYCKLAR-OK                                                      
055000         PERFORM B-KOLLA-NYCKLAR                                          
055100         IF NYCKLAR-OK                                                    
055200           IF REQU-UPDATE                                                 
055300             PERFORM G-KOLLA-INPUT                                        
055400             IF INDATA-OK                                                 
055500               PERFORM H-UPPDATERA                                        
055600             END-IF                                                       
055700           END-IF                                                         
055800           IF FEL-FINNS                                                   
055900             IF WS-IDPRTOMG > 2                                           
056000               PERFORM F-LAES-VISA-INFO                                   
056100             ELSE                                                         
056200               PERFORM I-LAES-VISA-FELTABELL                              
056300             END-IF                                                       
056400           ELSE                                                           
056500             IF ALLT-OK                                                   
056600               IF SUB-KDTRANS = 'WLA178' AND REQU-UPDATE                  
056700*                When called from API, there is no need to read           
056800*                again to check for remaining lines.                      
056900                 CONTINUE                                                 
057000               ELSE                                                       
057100                 PERFORM F-LAES-VISA-INFO                                 
057200               END-IF                                                     
057300             END-IF                                                       
057400           END-IF                                                         
057500         END-IF                                                           
057600       END-IF                                                             
057700       IF SUB-KDTRANS = 'WLA178'                                          
057800         PERFORM S11-MSG-CONV                                             
057900       END-IF                                                             
058000       PERFORM S02-RETURN-RESPONSE                                        
058100     END-IF                                                               
058200                                                                          
058300                                                                          
058400     MOVE ZERO TO RETURN-CODE                                             
058500     GOBACK                                                               
058600     .                                                                    
058700                                                                          
058800 A-INIT SECTION.                                                          
058900     MOVE 'A-INIT'  TO CURR-SECTION                                       
059000                                                                          
059100     MOVE JA                     TO NYCKLAR-SW                            
059200                                                                          
059300     MOVE ALL '+'   TO RESP-AREA                                          
059400     MOVE +1 TO INDX                                                      
059500*    -- FILL DESCRIPTIONS WITH UNICODE PLUS CHARACTERS                    
059600     PERFORM UNTIL INDX > MAX-INDX                                        
059700       MOVE ALL X'2B'   TO RESP-BEART-SVE (INDX)                          
059800       ADD 1 TO INDX                                                      
059900     END-PERFORM                                                          
060000                                                                          
060100     MOVE SPACE     TO RESP-IDMSG-ERROR                                   
060200                       RESP-IDMSG-INFO                                    
060300                       RESP-IDELMT-ERROR                                  
060400     MOVE 001       TO RESP-IDRESVER                                      
060500     MOVE ZERO      TO RESP-KVRADER                                       
060600     IF SUB-KDTRANS(1:7) = 'WL0178T' OR 'WL0178U'                         
060700       CONTINUE                                                           
060800     ELSE                                                                 
060900                                                                          
061000       MOVE 001                  TO AUTH-KDCALL                           
061100       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
061200                                    REQU-WZ01REQ2                         
061300       IF AUTH-KDRC > 0                                                   
061400         MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                      
061500         MOVE NEJ                TO NYCKLAR-SW                            
061600       END-IF                                                             
061700                                                                          
061800       MOVE FUNCTION UPPER-CASE (REQU-KDPGMACT)     TO                    
061900                                 REQU-KDPGMACT                            
062000       MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY)     TO                    
062100                                 REQU-IDDC-KEY                            
062200       MOVE FUNCTION UPPER-CASE (REQU-IDUSER-KEY)   TO                    
062300                                 REQU-IDUSER-KEY                          
062400       MOVE FUNCTION UPPER-CASE (REQU-FLDIFF)       TO                    
062500                                 REQU-FLDIFF                              
062600     END-IF                                                               
062700     PERFORM AA-DATUM                                                     
062800                                                                          
062900     .                                                                    
063000 AA-DATUM SECTION.                                                        
063100     MOVE 'AA-DATUM'  TO CURR-SECTION                                     
063200                                                                          
063300     MOVE 'IDAG  '            TO DAT-KDDATFORM                            
063400     CALL  WDATKONV  USING       DAT-KDDATFORM                            
063500                                 DAT-I-TIDATUM                            
063600                                 DAT-O-TIDATUM                            
063700                                 DAT-KDSVAR                               
063800                                                                          
063900     IF DAT-KDSVAR-OK                                                     
064000         CONTINUE                                                         
064100     ELSE                                                                 
064200         CALL  FELLOG                                                     
064300     END-IF                                                               
064400                                                                          
064500     MOVE DAT-TIAAVVD         TO WS-TIINVDAT                              
064600     MOVE DAT-TIAAMMDD        TO DAGENS-AAMMDD                            
064700                                                                          
064800     MOVE DAT-TISEKEL         TO DAGENS-AA                                
064900     MOVE WS-DAGENS-DATUM-GRP TO WS-DAGENS-DATUM                          
065000     ACCEPT WS-DAGENS-TID FROM TIME                                       
065010     MOVE   WS-DAGENS-TID TO  AKTUELL-TID-X                               
065100     .                                                                    
065200                                                                          
065300 B-KOLLA-NYCKLAR SECTION.                                                 
065400     MOVE 'B-KOLLA-NYCKLAR '  TO CURR-SECTION                             
065500                                                                          
065600     IF REQU-QUERY OR REQU-UPDATE                                         
065700        CONTINUE                                                          
065800     ELSE                                                                 
065900        MOVE '023'            TO RESP-IDMSG-ERROR                         
066000*       WRONG ACTION KEY ***                                              
066100        MOVE 'KDPGMACT'       TO RESP-IDELMT-ERROR                        
066200        MOVE NEJ              TO NYCKLAR-SW                               
066300     END-IF                                                               
066400                                                                          
066500     IF NYCKLAR-OK                                                        
066600        MOVE REQU-IDUSER-KEY  TO WS-MSGI-IDUSER                           
066700*----KONTROLLERA DC------------------------                               
066800        MOVE REQU-IDDC-KEY    TO W-IDDC-B6                                
066900                                                                          
067000        PERFORM IMS-GU-WDB601                                             
067100        MOVE DCS-IDDC      TO W-SEQB-IDDC-MIN                             
067200                              W-SEQB-IDDC-MAX                             
067300                              W-IDDC-WDH1                                 
067400                              W-IDDC-WDH1-MIN                             
067500                              W-IDDC-WDH1-MAX                             
067600                              W-IDDC-UTR                                  
067700                              W-IDDC                                      
067800        IF DCS-FLTRACK = 'J'                                              
067900           MOVE 'J'  TO WS-FLTRACK                                        
068000        ELSE                                                              
068100           MOVE 'N'  TO WS-FLTRACK                                        
068200        END-IF                                                            
068300     END-IF                                                               
068400                                                                          
068500*----KONTROLLERA LISTNR-------------------                                
068600     IF NYCKLAR-OK                                                        
068700        IF REQU-IDLISTNR-KEY = ALL '+'                                    
068800           MOVE '026'           TO RESP-IDMSG-ERROR                       
068900*          NO INPUT DATA IS ENTERED ***                                   
069000           MOVE 'IDLISTNR'      TO RESP-IDELMT-ERROR                      
069100           MOVE NEJ             TO NYCKLAR-SW                             
069200        ELSE                                                              
069300           IF REQU-IDLISTNR-KEY NOT NUMERIC                               
069400              MOVE '024'        TO RESP-IDMSG-ERROR                       
069500*             NOT NUMERIC ***                                             
069600              MOVE 'IDLISTNR'   TO RESP-IDELMT-ERROR                      
069700              MOVE NEJ          TO NYCKLAR-SW                             
069800           ELSE                                                           
069900              MOVE REQU-IDLISTNR-KEY(1:1) TO WS-IDPRTOMG                  
070000                                             W-IDPRTOMG                   
070100              MOVE REQU-IDLISTNR-KEY(2:5) TO WS-IDLOPNR                   
070200                                             W-IDLOPNR                    
070300           END-IF                                                         
070400        END-IF                                                            
070500     END-IF                                                               
070600                                                                          
070700*----KONTROLLERA IDUSER-------------------                                
070800     IF NYCKLAR-OK                                                        
070900        IF REQU-IDUSER-KEY = ALL '+'                                      
071000           MOVE '026'                   TO RESP-IDMSG-ERROR               
071100*          NO INPUT DATA IS ENTERED ***                                   
071200           MOVE 'IDUSER'                TO RESP-IDELMT-ERROR              
071300           MOVE NEJ                     TO NYCKLAR-SW                     
071400        ELSE                                                              
071500           MOVE REQU-IDUSER-KEY         TO WS-IDUSER                      
071600        END-IF                                                            
071700     END-IF                                                               
071800                                                                          
071900     IF REQU-UPDATE                                                       
072000        IF NYCKLAR-OK                                                     
072100           IF REQU-FLDIFF = JA  OR NEJ                                    
072200              CONTINUE                                                    
072300           ELSE                                                           
072400              MOVE '283'                TO RESP-IDMSG-ERROR               
072500*             J/N   ***                                                   
072600              MOVE NEJ                  TO NYCKLAR-SW                     
072700           END-IF                                                         
072800        END-IF                                                            
072900                                                                          
073000        IF NYCKLAR-OK                                                     
073100           IF REQU-KVRADER NUMERIC                                        
073200              IF REQU-KVRADER = ZERO                                      
073300                 MOVE '023'             TO RESP-IDMSG-ERROR               
073400*                ZERO NOT ALLOWED ***                                     
073500                 MOVE 'KVRADER'         TO RESP-IDELMT-ERROR              
073600                 MOVE NEJ               TO NYCKLAR-SW                     
073700              END-IF                                                      
073800           ELSE                                                           
073900              MOVE '024'                TO RESP-IDMSG-ERROR               
074000*             NOT NUMERIC ***                                             
074100              MOVE 'KVRADER'            TO RESP-IDELMT-ERROR              
074200              MOVE NEJ                  TO NYCKLAR-SW                     
074300           END-IF                                                         
074400        END-IF                                                            
074500     END-IF                                                               
074600                                                                          
074700     IF NYCKLAR-OK                                                        
074800        MOVE REQU-IDDC-KEY     TO RESP-IDDC-KEY                           
074900        MOVE REQU-IDLISTNR-KEY TO RESP-IDLISTNR-KEY                       
075000        MOVE REQU-IDUSER-KEY   TO RESP-IDUSER-KEY                         
075100        MOVE REQU-FLDIFF       TO RESP-FLDIFF                             
075200        MOVE REQU-IDDC-KEY     TO W-SEQB-IDDC-MIN                         
075300                                  W-SEQB-IDDC-MAX                         
075400                                  W-IDDC-WDH1-MIN                         
075500                                  W-IDDC-WDH1-MAX                         
075600                                  W-IDDC-UTR                              
075700                                  W-IDDC                                  
075800     END-IF                                                               
075900                                                                          
076000     .                                                                    
076100     EJECT                                                                
076200                                                                          
076300 F-LAES-VISA-INFO SECTION.                                                
076400     MOVE 'F-LAES-VISA-INFO'  TO CURR-SECTION                             
076500                                                                          
076600     MOVE NEJ TO POST-SW                                                  
076700     MOVE ALL '+' TO SPAR-TABELL                                          
076800                     SPAR-TABELL2                                         
076900     MOVE SPACE   TO SPAR-FEL-TABELL                                      
077000     PERFORM FA-INITIERA-SORT-TABELL                                      
077100     PERFORM IMS-GU-WDH111-BSEQ                                           
077200     IF SEGMENT-FINNS                                                     
077300        MOVE +1 TO INDX                                                   
077400        SET SORT-IX TO +1                                                 
077500        PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS OR                
077600                                        SEGMENT-SLUT                      
077700           IF SEQB-INV-IDPRTOMG = WS-IDPRTOMG AND                         
077800             (SEQB-INV-IDLOPNR = WS-IDLOPNR) AND                          
077900             (SEQB-INV-FLINVSKR = 'J') AND                                
078000             (SEQB-INV-FLINVBEH = 'N') AND                                
078100             (SEQB-INV-IDDC     = W-IDDC)                                 
078200              MOVE JA                  TO POST-SW                         
078300              PERFORM IMS-GNP-WDH101-BSEQ                                 
078400                                                                          
078500              MOVE INVA-ART-IDARTNR    TO W-IDARTNR-WDD3                  
078600              MOVE REQU-IDDC-KEY       TO WS-IDDC                         
078700              MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                     
078800              IF DCS-UNICODE-IDSKYLT                                      
078900                 MOVE 'UTF8'             TO TRAUTF8-KDCP                  
079000              ELSE                                                        
079100                 MOVE '278 '             TO TRAUTF8-KDCP                  
079200              END-IF                                                      
079300              PERFORM IMS-GET-BENA                                        
079400              IF SEGMENT-FINNS                                            
079500                MOVE TEXT-BEART      TO TRAUTF8-TECONV-FROM               
079600              ELSE                                                        
079700                MOVE WS-CP-EBCDIC    TO TRAUTF8-KDCP                      
079800                MOVE SPACE           TO TRAUTF8-TECONV-FROM               
079900              END-IF                                                      
080000              IF TRAUTF8-TECONV-FROM = SPACES                             
080100               MOVE 'GB'  TO W-IDSKYLT                                    
080200               MOVE '278' TO TRAUTF8-KDCP                                 
080300               PERFORM IMS-get-bena                                       
080400               MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                  
080500              END-IF                                                      
080600*------       STRIP SPACE OR CONVERT TO UNICODE                           
080700              CALL WTRAUTF8 USING TRAUTF8-AREA                            
080800*------       MOVE CONVERTED DESCRIPTION TO THE RESPONSE                  
080900              MOVE TRAUTF8-TECONV-TO TO WS-CONV-BEART                     
081000              PERFORM FB-FYLL-SORT-AREA                                   
081100              ADD +1                   TO INDX                            
081200              SET SORT-IX UP BY +1                                        
081300           END-IF                                                         
081400           PERFORM IMS-GN-WDH111-BSEQ                                     
081500        END-PERFORM                                                       
081600        IF POST-SAKNAS                                                    
081700          MOVE '027'           TO RESP-IDMSG-ERROR                        
081800*         NO  DATA FOUND ***                                              
081900          MOVE 'IDLISTNR'      TO RESP-IDELMT-ERROR                       
082000        END-IF                                                            
082100        IF TAB-SORT-BENAMNING(1) NOT = ALL '9'                            
082200           PERFORM FC-CALL-SORT                                           
082300           PERFORM FD-FYLL-SIDAN                                          
082400        END-IF                                                            
082500                                                                          
082600     ELSE                                                                 
082700        IF SEGMENT-SAKNAS                                                 
082800          MOVE '027'           TO RESP-IDMSG-ERROR                        
082900*         NO  DATA FOUND ***                                              
083000          MOVE 'IDLISTNR'      TO RESP-IDELMT-ERROR                       
083100                                                                          
083200        END-IF                                                            
083300     END-IF                                                               
083400                                                                          
083500     .                                                                    
083600 FA-INITIERA-SORT-TABELL SECTION.                                         
083700     MOVE 'FA-INITIERA-SORT'  TO CURR-SECTION                             
083800                                                                          
083900     SET SORT-IX TO +1                                                    
084000     PERFORM UNTIL (SORT-IX) > TAB-SORT-MAX                               
084100     MOVE ALL '9'       TO   TAB-SORT-ADLAGOMR   (SORT-IX)                
084200                             TAB-SORT-ADGANG     (SORT-IX)                
084300                             TAB-SORT-ADPLATS    (SORT-IX)                
084400                             TAB-SORT-PRIO       (SORT-IX)                
084500                             TAB-SORT-VVKL       (SORT-IX)                
084600                             TAB-SORT-IDARTNR    (SORT-IX)                
084700                             TAB-SORT-BENAMNING  (SORT-IX)                
084800                             TAB-SORT-KVAKS      (SORT-IX)                
084900                             TAB-SORT-KVLS       (SORT-IX)                
085000                             TAB-SORT-KVEFRS     (SORT-IX)                
085100*                            TAB-SORT-MESSAGE    (SORT-IX)                
085200                                                                          
085300     SET SORT-IX UP BY +1                                                 
085400     END-PERFORM                                                          
085500     .                                                                    
085600     EJECT                                                                
085700 FB-FYLL-SORT-AREA SECTION.                                               
085800     MOVE 'FB-FYLL-SORT-ARE'  TO CURR-SECTION                             
085900                                                                          
086000      MOVE SEQB-INV-ADLAGOMR   TO WS-SORT-ADLAGOMR                        
086100      MOVE SEQB-INV-ADGANG     TO WS-SORT-ADGANG                          
086200      MOVE SEQB-INV-ADPLATS    TO WS-SORT-ADPLATS                         
086300      MOVE SEQB-INV-KDINVPRIO  TO WS-SORT-PRIO                            
086400      MOVE SEQB-INV-KDVVKL     TO WS-SORT-VVKL                            
086500                                                                          
086600      MOVE WS-SORT-ADLAGOMR    TO TAB-SORT-ADLAGOMR  (SORT-IX)            
086700      MOVE WS-SORT-ADGANG      TO TAB-SORT-ADGANG    (SORT-IX)            
086800      MOVE WS-SORT-ADPLATS     TO TAB-SORT-ADPLATS   (SORT-IX)            
086900      MOVE WS-SORT-PRIO        TO TAB-SORT-PRIO      (SORT-IX)            
087000      MOVE WS-SORT-VVKL        TO TAB-SORT-VVKL      (SORT-IX)            
087100      MOVE SEQB-INV-KVAKS-OLD  TO TAB-SORT-KVAKS     (SORT-IX)            
087200      MOVE SEQB-INV-KVLS-OLD   TO TAB-SORT-KVLS      (SORT-IX)            
087300      MOVE SEQB-INV-KVEFRS-OLD TO TAB-SORT-KVEFRS    (SORT-IX)            
087400      MOVE INVA-ART-IDARTNR    TO TAB-SORT-IDARTNR   (SORT-IX)            
087500      MOVE WS-CONV-BEART       TO TAB-SORT-BENAMNING (SORT-IX)            
087600                                                                          
087700     .                                                                    
087800     EJECT                                                                
087900 FC-CALL-SORT SECTION.                                                    
088000     MOVE 'FC-CALL-SORT    '  TO CURR-SECTION                             
088100                                                                          
088200     CALL WINTSOR USING SORT-TABELL                                       
088300                  TAB-SORT-STEG-LANGD                                     
088400                  TAB-SORT-ANTAL                                          
088500                  TAB-SORT-FAELT(1)                                       
088600                  TAB-SORT-LANGD                                          
088700                                                                          
088800     .                                                                    
088900     EJECT                                                                
089000 FD-FYLL-SIDAN SECTION.                                                   
089100     MOVE 'FD-FYLL-SIDAN   '  TO CURR-SECTION                             
089200                                                                          
089300     MOVE +1   TO INDX                                                    
089400     MOVE ZERO TO WS-KVRADER                                              
089500     SET SORT-IX TO +1                                                    
089600     PERFORM UNTIL  SORT-IX > MAX-INDX OR                                 
089700             TAB-SORT-BENAMNING(SORT-IX) = ALL '9'                        
089800        MOVE TAB-SORT-KVAKS(SORT-IX)     TO RESP-KVAKS     (INDX)         
089900                                            TAB-KVAKS      (INDX)         
090000        MOVE TAB-SORT-KVLS (SORT-IX)     TO RESP-KVLS      (INDX)         
090100                                            TAB-KVLS       (INDX)         
090200        MOVE TAB-SORT-KVEFRS (SORT-IX)   TO RESP-KVEFRS    (INDX)         
090300                                            TAB-KVEFRS     (INDX)         
090400        MOVE TAB-SORT-IDARTNR(SORT-IX)   TO RESP-IDARTNR   (INDX)         
090500                                            TAB-ARTIKEL    (INDX)         
090600        MOVE TAB-SORT-BENAMNING(SORT-IX) TO RESP-BEART-SVE (INDX)         
090700                                            TAB-BENAMNING  (INDX)         
090800*       MOVE TAB-SORT-MESSAGE(SORT-IX)   TO                               
090900*                                    RESP-IDMSG-ERROR-LINE (INDX)         
091000*                                           TAB-MESSAGE    (INDX)         
091100        ADD +1 TO INDX                                                    
091200        ADD 1 TO WS-KVRADER                                               
091300        SET SORT-IX UP BY +1                                              
091400     END-PERFORM                                                          
091500     MOVE WS-KVRADER TO RESP-KVRADER                                      
091600*    IF  (SORT-IX) <  MAX-INDX                                            
091700*       PERFORM UNTIL (SORT-IX) > MAX-INDX                                
091800*          PERFORM MFS-RENSA-RADEN-UT                                     
091900*          MOVE 'N'               TO MOD-FELRAD-UT (INDX)                 
092000*          ADD +1 TO INDX                                                 
092100*          SET SORT-IX UP BY +1                                           
092200*       END-PERFORM                                                       
092300*    END-IF                                                               
092400     .                                                                    
092500     EJECT                                                                
092600 G-KOLLA-INPUT SECTION.                                                   
092700     MOVE 'G-KOLLA-INPUT   '  TO CURR-SECTION                             
092800                                                                          
092900     MOVE JA  TO INDATA-SW                                                
093000     MOVE +1 TO INDX                                                      
093100                                                                          
093200     IF SUB-KDTRANS = 'WLA178'                                            
093300       PERFORM IMS-GU-WDH111-BSEQ                                         
093400     PERFORM IMS-GU-WDH111-BSEQ                                           
093500       IF SEGMENT-FINNS                                                   
093600         PERFORM IMS-GNP-WDH101-BSEQ                                      
093700       END-IF                                                             
093800       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                    
093900         REQU-IDARTNR-ONE = INVA-ART-IDARTNR                              
094000         PERFORM IMS-GN-WDH111-BSEQ                                       
094100         IF SEGMENT-FINNS                                                 
094200           PERFORM IMS-GNP-WDH101-BSEQ                                    
094300         END-IF                                                           
094400       END-PERFORM                                                        
094500     ELSE                                                                 
094600       PERFORM IMS-GU-WDH111-BSEQ                                         
094700     END-IF                                                               
097400                                                                          
097600     IF SEGMENT-FINNS                                                     
097700                                                                          
097800       MOVE SEQB-INV-IDDC        TO W-IDDC-WDH1                           
097900       MOVE SEQB-INV-KDINVKAT    TO W-KDINVKAT-WDH1                       
098000       MOVE SEQB-INV-TISEGKEY    TO W-TISEGKEY-WDH1                       
098100       MOVE SEQB-INV-DAREGDAT-SORT                                        
098200                                 TO W-DAREGDAT-SORT-WDH1                  
098300*-- KONTROLLERAR OM TABELLEN ÄR IFYLLD                                    
098400                                                                          
098500       MOVE NEJ TO TABELL-IFYLLD-SW                                       
098600       PERFORM UNTIL INDX > MAX-INDX                                      
098700         IF REQU-ANTAL-IN(INDX) NOT = ALL '+'                             
098800           MOVE JA  TO TABELL-IFYLLD-SW                                   
098900         END-IF                                                           
099000         IF REQU-DIFFERANS-IN(INDX) NOT = ALL '+'                         
099100           MOVE JA  TO TABELL-IFYLLD-SW                                   
099200         END-IF                                                           
099201         IF REQU-FLOMINV-IN(INDX) NOT = ALL '+'                           
099202           MOVE JA  TO TABELL-IFYLLD-SW                                   
099203         END-IF                                                           
099300         IF TABELL-IFYLLD                                                 
099400           MOVE 10 TO INDX                                                
099500         END-IF                                                           
099600         ADD +1 TO INDX                                                   
099700       END-PERFORM                                                        
099800                                                                          
099900***----KONTROLLERA TABELL RADER-------------                              
100000***----OM VÄRDEN ÄR NUMERISKA   ------------                              
100100                                                                          
100200       IF TABELL-IFYLLD                                                   
100300         MOVE +1                   TO INDX                                
100400         PERFORM UNTIL INDX > MAX-INDX                                    
100500           MOVE REQU-IDARTNR   (INDX) TO TAB-ARTIKEL  (INDX)              
100600           IF SUB-KDTRANS = 'WLA178'                                      
100700             IF REQU-IDARTNR(INDX) NOT = ALL '+' AND ZERO                 
100800               MOVE REQU-IDARTNR (INDX)                                   
100900                                 TO W-IDARTNR                             
101000               PERFORM IMS-GU-WDH111                                      
101100               MOVE INVA-INV-KVAKS-OLD                                    
101200                                 TO REQU-KVAKS (INDX)                     
101300               MOVE INVA-INV-KVLS-OLD                                     
101400                                 TO REQU-KVLS (INDX)                      
101410                                    WS-TAB-KVLS                           
101500               MOVE INVA-INV-KVEFRS-OLD                                   
101600                                 TO REQU-KVEFRS (INDX)                    
101700             END-IF                                                       
101800           END-IF                                                         
101900**** CHECK THAT AVERAGE COST MARKETS HAVE AVERAGE COST                    
102000           MOVE REQU-IDDC-KEY       TO WS-IDDC                            
102100           EVALUATE TRUE                                                  
102200           WHEN XDC-NON-VCC-OWNED OR LDC-CN OR NDC-NA                     
102300             IF REQU-IDARTNR(INDX) NOT = ALL '+' AND ZERO                 
102400               MOVE TAB-ARTIKEL(INDX) TO W-IDARTNR                        
102500               PERFORM IMS-GHU-WDK711                                     
102600               IF SEGMENT-FINNS                                           
102700                                                                          
102800**** CHECK FOR NDC-US DIFFERS AND IS DONE IN SECTION HAC-                 
102900                 IF NOT NDC-US                                            
103000                   IF SLAG-PRAVCOST = ZERO                                
103100                     MOVE NEJ        TO INDATA-SW                         
103200                     MOVE '023'     TO RESP-IDMSG-ERROR-LINE(INDX)        
103300                     MOVE '023'      TO TAB-MESSAGE(INDX)                 
103400                     MOVE '023'      TO RESP-IDMSG-ERROR                  
103500                     MOVE 'PRAVCOST' TO RESP-IDELMT-ERROR                 
103600                   END-IF                                                 
103700                 END-IF                                                   
103800                 MOVE SLAG-PRAVCOST TO WS-PRAVCOST                        
103900               ELSE                                                       
104000                 MOVE NEJ          TO INDATA-SW                           
104100                 MOVE '023'        TO RESP-IDMSG-ERROR-LINE(INDX)         
104200                 MOVE '023'        TO TAB-MESSAGE(INDX)                   
104300                 MOVE '023'        TO RESP-IDMSG-ERROR                    
104400                 MOVE 'PRAVCOST'   TO RESP-IDELMT-ERROR                   
104500               END-IF                                                     
104600             END-IF                                                       
104700           END-EVALUATE                                                   
104800                                                                          
104900***        MOVE REQU-BEART-SVE (INDX) TO TAB-BENAMNING(INDX)              
105000           MOVE ALL X'2B'             TO TAB-BENAMNING(INDX)              
105100                                                                          
105200           MOVE REQU-KVAKS     (INDX) TO TAB-KVAKS    (INDX)              
105300           IF REQU-KVLS(INDX) NOT = ALL '+'                               
105310             IF SUB-KDTRANS = 'WLA178'                                    
105311               MOVE WS-TAB-KVLS    TO TAB-KVLS     (INDX)                 
105312             ELSE                                                         
105400               PERFORM S03-DECEDIT                                        
105500               MOVE WS-TAB-KVLS    TO TAB-KVLS     (INDX)                 
105510             END-IF                                                       
105600           END-IF                                                         
105700           MOVE REQU-KVEFRS    (INDX) TO TAB-KVEFRS   (INDX)              
105800           IF REQU-ANTAL-IN (INDX) NOT = ALL '+' AND ZERO                 
105900              IF REQU-FLDIFF = JA                                         
106000                 MOVE NEJ          TO INDATA-SW                           
106100                 MOVE '033'        TO RESP-IDMSG-ERROR-LINE(INDX)         
106200                 MOVE '033'        TO TAB-MESSAGE(INDX)                   
106300                 MOVE '033'        TO RESP-IDMSG-ERROR                    
106400                 MOVE 'KVANTAL'    TO RESP-IDELMT-ERROR                   
106500*                MUST NOT BE ENTERED ***                                  
106600              ELSE                                                        
106700                 IF REQU-ANTAL-IN (INDX) NOT NUMERIC                      
106800                    MOVE NEJ       TO INDATA-SW                           
106900                    MOVE '024'     TO RESP-IDMSG-ERROR-LINE(INDX)         
107000                    MOVE '024'     TO TAB-MESSAGE(INDX)                   
107100                    MOVE '024'     TO RESP-IDMSG-ERROR                    
107200                    MOVE 'KVANTAL' TO RESP-IDELMT-ERROR                   
107300*                   NOT NUMERIC ***                                       
107400                 ELSE                                                     
107500                    MOVE REQU-ANTAL-IN (INDX)                             
107600                                   TO TAB-ANTAL (INDX)                    
107610                    IF NDC-BR                                             
107611                      COMPUTE WS-KVLS = TAB-KVLS(INDX) +                  
107612                                        TAB-KVEFRS(INDX)                  
107613                      IF TAB-ANTAL(INDX) > WS-KVLS                        
107614                        MOVE NEJ       TO INDATA-SW                       
107615                        MOVE '023'     TO                                 
107616                             RESP-IDMSG-ERROR-LINE(INDX)                  
107617                        MOVE '023'     TO TAB-MESSAGE(INDX)               
107618                        MOVE '023'     TO RESP-IDMSG-ERROR                
107619                        MOVE 'KVANTAL' TO RESP-IDELMT-ERROR               
107620*                       NOT NUMERIC ***                                   
107621                      END-IF                                              
107630                    END-IF                                                
107700                 END-IF                                                   
107800              END-IF                                                      
107900           END-IF                                                         
108000                                                                          
108100           IF INDATA-OK                                                   
108200              IF REQU-DIFFERANS-IN (INDX) NOT = ALL '+' AND ZERO          
108300                 IF REQU-FLDIFF = JA                                      
108400                    MOVE NEJ    TO INDATA-SW                              
108500                    MOVE '033'  TO RESP-IDMSG-ERROR-LINE(INDX)            
108600                    MOVE '033'  TO TAB-MESSAGE(INDX)                      
108700                    MOVE '033'  TO RESP-IDMSG-ERROR                       
108800                    MOVE 'KVDIFF' TO RESP-IDELMT-ERROR                    
108900*                   MUST NOT BE ENTERED ***                               
109000                 ELSE                                                     
109100                    IF REQU-DIFFERANS-IN (INDX) NOT NUMERIC               
109200                       MOVE NEJ    TO INDATA-SW                           
109300                       MOVE '024'  TO RESP-IDMSG-ERROR-LINE(INDX)         
109400                       MOVE '024'  TO TAB-MESSAGE(INDX)                   
109500                       MOVE '024'  TO RESP-IDMSG-ERROR                    
109600*                      NOT NUMERIC ***                                    
109700                       MOVE 'KVDIFF' TO RESP-IDELMT-ERROR                 
109800                    ELSE                                                  
109900                       MOVE REQU-DIFFERANS-IN (INDX)                      
110000                                     TO TAB-DIFFERANS (INDX)              
110100                    END-IF                                                
110200                 END-IF                                                   
110300              END-IF                                                      
110400           END-IF                                                         
110500                                                                          
110600           IF INDATA-OK                                                   
110700              IF REQU-TECKEN-IN (INDX) = ' ' OR '-' OR '+'                
110800               IF ((REQU-TECKEN-IN (INDX) = ' ' OR '+') AND               
110900                (REQU-DIFFERANS-IN (INDX) NOT = ALL '+' AND ZERO)         
111000                 AND NDC-BR)                                              
111001                 MOVE NEJ    TO INDATA-SW                                 
111002                 MOVE '023'  TO RESP-IDMSG-ERROR-LINE(INDX)               
111003                 MOVE '023'  TO TAB-MESSAGE(INDX)                         
111004                 MOVE '023'  TO RESP-IDMSG-ERROR                          
111005*                IS INVALID    ***                                        
111006                 MOVE 'KDAVVTYP'                                          
111007                             TO RESP-IDELMT-ERROR                         
111008               ELSE                                                       
111009                 MOVE REQU-TECKEN-IN (INDX)                               
111010                             TO TAB-TECKEN (INDX)                         
111011               END-IF                                                     
111020              ELSE                                                        
111100                 MOVE NEJ    TO INDATA-SW                                 
111200                 MOVE '023'  TO RESP-IDMSG-ERROR-LINE(INDX)               
111300                 MOVE '023'  TO TAB-MESSAGE(INDX)                         
111400                 MOVE '023'  TO RESP-IDMSG-ERROR                          
111500*                IS INVALID    ***                                        
111600                 MOVE 'KDAVVTYP'                                          
111700                             TO RESP-IDELMT-ERROR                         
111800              END-IF                                                      
111900           END-IF                                                         
111901           IF INDATA-OK                                                   
111902              IF REQU-FLOMINV-IN(INDX) NOT = ALL '+'                      
111903               IF REQU-FLOMINV-IN(INDX) = 'J' OR 'Y'                      
111904                 MOVE REQU-FLOMINV-IN(INDX)                               
111905                             TO TAB-FLOMINV(INDX)                         
111906               ELSE                                                       
111907                 MOVE NEJ    TO INDATA-SW                                 
111908                 MOVE '023'  TO RESP-IDMSG-ERROR-LINE(INDX)               
111909                 MOVE '023'  TO TAB-MESSAGE(INDX)                         
111910                 MOVE '023'  TO RESP-IDMSG-ERROR                          
111911*                IS INVALID    ***                                        
111912                 MOVE 'KDFLOMIN'                                          
111913                             TO RESP-IDELMT-ERROR                         
111914               END-IF                                                     
111915              END-IF                                                      
111916           END-IF                                                         
112000** IDTRACK CHANGES BEGIN ***                                              
112100           IF WS-FLTRACK = 'J'                                            
112101            MOVE 'J' TO WS-FLLOCAL                                        
112102            IF NDC-MX                                                     
112103              MOVE REQU-IDDC-KEY       TO W-IDDC                          
112105              PERFORM IMS-GU-WDK711                                       
112106              IF SLAG-IDDC-REF = SPACE                                    
112107                MOVE 'N' TO WS-FLLOCAL                                    
112108              END-IF                                                      
112109            END-IF                                                        
112113           END-IF                                                         
112120           IF WS-FLTRACK = 'J' AND WS-FLLOCAL = 'J'                       
112200             IF INDATA-OK                                                 
112300               IF TAB-ANTAL (INDX) > 0                                    
112400                 MOVE 'N' TO WS-DIFF                                      
112500                 IF TAB-ANTAL (INDX) >  TAB-KVLS (INDX)                   
112600                    COMPUTE WS-ANTAL = TAB-ANTAL (INDX) -                 
112700                                       TAB-KVLS (INDX)                    
112800                    PERFORM GA-VALIDATE-WDK728                            
112900                    IF IDTRACK-QTY-NOT-DONE                               
113000                       MOVE NEJ    TO INDATA-SW                           
113100                       MOVE '423'  TO RESP-IDMSG-ERROR-LINE(INDX)         
113200                       MOVE '423'  TO TAB-MESSAGE(INDX)                   
113300                       MOVE '423'  TO RESP-IDMSG-ERROR                    
113400                       MOVE SPACES TO RESP-IDELMT-ERROR                   
113500                       MOVE WS-TEMP-KVAVIS   TO                           
113600                                    WS-TEMP-KVAVIS-SHOW                   
113700                       STRING ' :' WS-TEMP-KVAVIS-SHOW                    
113800                       DELIMITED BY SIZE INTO RESP-IDELMT-ERROR           
113900                    END-IF                                                
114000                 ELSE                                                     
114100                    IF TAB-ANTAL (INDX) <  TAB-KVLS (INDX)                
114200                       COMPUTE WS-ANTAL = TAB-KVLS  (INDX) -              
114300                                          TAB-ANTAL (INDX)                
114400                       PERFORM GB-VALIDATE-WDK728                         
114500                    ELSE                                                  
114600                       CONTINUE                                           
114700                    END-IF                                                
114800                 END-IF                                                   
114900               ELSE                                                       
115000                 IF TAB-DIFFERANS (INDX) > 0                              
115100                 MOVE 'J' TO WS-DIFF                                      
115200                 IF TAB-TECKEN (INDX) = '+' OR ' '                        
115300                    PERFORM GA-VALIDATE-WDK728                            
115400                    IF IDTRACK-QTY-NOT-DONE                               
115500                       MOVE NEJ    TO INDATA-SW                           
115600                       MOVE '423'  TO RESP-IDMSG-ERROR-LINE(INDX)         
115700                       MOVE '423'  TO TAB-MESSAGE(INDX)                   
115800                       MOVE '423'  TO RESP-IDMSG-ERROR                    
115900                       MOVE SPACES TO RESP-IDELMT-ERROR                   
116000                       MOVE WS-TEMP-KVAVIS   TO                           
116100                                    WS-TEMP-KVAVIS-SHOW                   
116200                       STRING ' :' WS-TEMP-KVAVIS-SHOW                    
116300                       DELIMITED BY SIZE INTO RESP-IDELMT-ERROR           
116400                    END-IF                                                
116500                 END-IF                                                   
116600                 IF TAB-TECKEN (INDX) = '-'                               
116700                    IF TAB-DIFFERANS (INDX) <= TAB-KVLS (INDX)            
116800                       PERFORM GB-VALIDATE-WDK728                         
116900                    ELSE                                                  
117000                       MOVE NEJ    TO INDATA-SW                           
117100                       MOVE '424'  TO RESP-IDMSG-ERROR-LINE(INDX)         
117200                       MOVE '424'  TO TAB-MESSAGE(INDX)                   
117300                       MOVE '424'  TO RESP-IDMSG-ERROR                    
117400                       MOVE SPACES TO RESP-IDELMT-ERROR                   
117500                       MOVE TAB-KVLS (INDX) TO WS-TEMP-KVAVIS-SHOW        
117600                       STRING ' :' WS-TEMP-KVAVIS-SHOW                    
117700                       DELIMITED BY SIZE INTO RESP-IDELMT-ERROR           
117800                    END-IF                                                
117900                 END-IF                                                   
118000                 END-IF                                                   
118100               END-IF                                                     
118200             END-IF                                                       
118300           END-IF                                                         
118400** IDTRACK CHANGES ENDS  ***                                              
118500           ADD +1                 TO INDX                                 
118600         END-PERFORM                                                      
118700                                                                          
118800*                                                                         
118900*       IF INDATA-FEL                                                     
119000*          MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
119100*          CALL WMEDKONV USING MED-WMEDAREA                               
119200*          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
119300*          MOVE 'FYLL I FYS ANT' TO MOD-TEMFSINF                          
119400*          PERFORM MFS-ROER-EJ-FAELT-UT                                   
119500*          PERFORM MFS-ROER-EJ-FAELT-IN                                   
119600*       END-IF                                                            
119700       ELSE                                                               
119800         IF REQU-FLDIFF = JA                                              
119900           MOVE +1                   TO INDX                              
120000           PERFORM UNTIL INDX > MAX-INDX                                  
120100             MOVE REQU-IDARTNR   (INDX) TO TAB-ARTIKEL  (INDX)            
120200             ADD +1 TO INDX                                               
120300           END-PERFORM                                                    
120400         ELSE                                                             
120500           MOVE NEJ               TO INDATA-SW                            
120600           IF REQU-IDARTNR (1) = ALL '+' OR ZEROES                        
120700             MOVE ERR-INVALID     TO RESP-IDMSG-ERROR                     
120800             MOVE 'IDARTNR'       TO RESP-IDELMT-ERROR                    
120900           ELSE                                                           
121000             MOVE ERR-INVALID     TO RESP-IDMSG-ERROR                     
121100             MOVE 'KVANTAL'       TO RESP-IDELMT-ERROR                    
121200           END-IF                                                         
121300         END-IF                                                           
121400       END-IF                                                             
121500       IF WS-IDPRTOMG  > 2                                                
121600         MOVE JA                 TO FEL-SW                                
121700         MOVE NEJ                TO INDATA-SW                             
121800         MOVE '369'              TO RESP-IDMSG-ERROR                      
121900       END-IF                                                             
122000     ELSE                                                                 
122100       MOVE JA                   TO FEL-SW                                
122200       MOVE NEJ                  TO INDATA-SW                             
122300       MOVE '027'                TO RESP-IDMSG-ERROR                      
122400       MOVE 'LISTNR'             TO RESP-IDELMT-ERROR                     
122500     END-IF                                                               
122600     .                                                                    
122700     EJECT                                                                
122800 GA-VALIDATE-WDK728  SECTION.                                             
122900     MOVE ZERO                TO WS-VALD-KVAVIS                           
123000                                 WS-TEMP-KVAVIS                           
123100     IF NOT-DIFF-OK                                                       
123200        MOVE WS-ANTAL         TO WS-VALD-KVAVIS                           
123300     ELSE                                                                 
123400        MOVE REQU-DIFFERANS-IN (INDX)                                     
123500                              TO WS-VALD-KVAVIS                           
123600     END-IF                                                               
123700     MOVE 'N'                 TO IDTRACK-QTY-SW                           
123800     MOVE REQU-IDDC-KEY       TO W-IDDC                                   
123900     MOVE REQU-IDARTNR (INDX) TO W-IDARTNR                                
124000     PERFORM IMS-GU-WDK711                                                
124100     MOVE 9999999999999999    TO W-DAINLEV                                
124200     PERFORM IMS-GHNP-WDK728-LAST                                         
124300     PERFORM UNTIL SEGMENT-SAKNAS OR IDTRACK-QTY-DONE                     
124400       IF TRCK-KVTRACK-KVAR < TRCK-KVANTMOT                               
124500          COMPUTE WS-TEMP-KVAVIS = WS-TEMP-KVAVIS +                       
124600          (TRCK-KVANTMOT - TRCK-KVTRACK-KVAR)                             
124700          IF WS-TEMP-KVAVIS >= WS-VALD-KVAVIS                             
124800             MOVE 'J' TO IDTRACK-QTY-SW                                   
124900          END-IF                                                          
125000       END-IF                                                             
125100       IF IDTRACK-QTY-NOT-DONE                                            
125200          PERFORM IMS-GU-WDK711                                           
125300          MOVE  TRCK-DAINLEV TO W-DAINLEV                                 
125400          PERFORM IMS-GHNP-WDK728-LAST                                    
125500       END-IF                                                             
125600     END-PERFORM                                                          
125700     .                                                                    
125800     EJECT                                                                
125900 GB-VALIDATE-WDK728  SECTION.                                             
126000     MOVE ZERO                TO WS-VALD-KVAVIS                           
126100                                 WS-TEMP-KVAVIS                           
126200     IF NOT-DIFF-OK                                                       
126300        MOVE WS-ANTAL         TO WS-VALD-KVAVIS                           
126400     ELSE                                                                 
126500        MOVE REQU-DIFFERANS-IN (INDX)                                     
126600                              TO WS-VALD-KVAVIS                           
126700     END-IF                                                               
126800     MOVE 'N'                 TO IDTRACK-QTY-SW                           
126900     MOVE REQU-IDDC-KEY       TO W-IDDC                                   
127000     MOVE REQU-IDARTNR (INDX) TO W-IDARTNR                                
127100     PERFORM IMS-GU-WDK711                                                
127200     PERFORM IMS-GHNP-WDK728                                              
127300     PERFORM UNTIL SEGMENT-SAKNAS OR IDTRACK-QTY-DONE                     
127400       IF TRCK-KVTRACK-KVAR > 0                                           
127500          COMPUTE WS-TEMP-KVAVIS = WS-TEMP-KVAVIS +                       
127600                  TRCK-KVTRACK-KVAR                                       
127700          IF WS-TEMP-KVAVIS >= WS-VALD-KVAVIS                             
127800             MOVE 'J' TO IDTRACK-QTY-SW                                   
127900             MOVE 'N' TO WS-USE-TEMP                                      
128000          END-IF                                                          
128100       END-IF                                                             
128200       IF IDTRACK-QTY-NOT-DONE                                            
128300          PERFORM IMS-GHNP-WDK728                                         
128400       END-IF                                                             
128500     END-PERFORM                                                          
128600     .                                                                    
128700     EJECT                                                                
128800 H-UPPDATERA SECTION.                                                     
128900     MOVE 'H-UPPDATERA     '  TO CURR-SECTION                             
129000                                                                          
129100*------KONTROLLER FÖR INVENTERINGNEN                                      
129200     MOVE +1  TO INDX                                                     
129300     MOVE +1  TO IX                                                       
129400     MOVE NEJ TO UPDATE-SW                                                
129500     MOVE NEJ TO FEL-SW                                                   
129600                                                                          
129700     PERFORM UNTIL INDX > MAX-INDX                                        
129800     IF TAB-ARTIKEL(INDX)  NUMERIC                                        
129900        MOVE TAB-ARTIKEL(INDX) TO W-IDARTNR                               
130000        IF REQU-FLDIFF = JA                                               
130100           IF W-IDARTNR NUMERIC                                           
130200              PERFORM IMS-GET-ARTC-ART                                    
130300              PERFORM IMS-GET-ARTC-CLAG                                   
130400              MOVE CLAG-PRARTSTD   TO WS-PRARTSTD                         
130500              IF DCS-CDC                                                  
130600                 MOVE +0        TO CLAG-KVUTRS                            
130700                 MOVE +0        TO CLAG-KVINVS                            
130800                 MOVE WS-TIINVDAT TO CLAG-TIINVDAT                        
130900                 PERFORM IMS-REPL-ARTC-CLAG                               
131000                 MOVE ZERO      TO WS-ANTAL                               
131100                 MOVE '+'       TO WS-TECKEN                              
131200                 MOVE JA        TO ALLT-OK-SW                             
132200              ELSE                                                        
132300*                IF NOT DCS-NDC-NA                                        
132400                    PERFORM IMS-GHU-WDK711                                
132500                    MOVE SLAG-PRAVCOST TO WS-PRAVCOST                     
132600                    MOVE +0          TO SLAG-KVUTRS                       
132700                    MOVE +0          TO SLAG-KVINVS                       
132800                    MOVE WS-TIINVDAT TO SLAG-TIINVDAT                     
132900                    PERFORM IMS-REPL-SLAGER-SEGM                          
133000                    MOVE ZERO        TO WS-ANTAL                          
133100                    MOVE '+'         TO WS-TECKEN                         
133200                    MOVE JA          TO ALLT-OK-SW                        
133300*                END-IF                                                   
133400              END-IF                                                      
133500           END-IF                                                         
133600        ELSE                                                              
133700           IF REQU-ANTAL-IN (INDX) NOT = ALL '+' OR                       
133800              REQU-DIFFERANS-IN (INDX) NOT = ALL '+' OR                   
133810              REQU-FLOMINV-IN (INDX) NOT = ALL '+'                        
133900              MOVE TAB-ARTIKEL (INDX)    TO  W-IDARTNR                    
134000              PERFORM HA-LAES-WDK6                                        
134100              PERFORM HAA-KOLLA-DIFF                                      
134200              PERFORM HAB-KOLLA-MED-REGLER                                
134300              IF ALLT-FEL OR REQU-FLOMINV-IN (INDX) NOT = ALL '+'         
134400                 MOVE JA  TO FEL-RAD(IX)                                  
134500                 MOVE '290' TO TAB-MESSAGE(IX)                            
134600                 MOVE JA TO OMINVENTERING-SW                              
134700                 PERFORM HF-FYLL-FELTABELL                                
134800                 PERFORM HG-NOLLA-LISTNR                                  
134900                 IF ALLT-FEL                                              
135000                    SUBTRACT 1 FROM IX                                    
135100                    MOVE '290' TO TAB-MESSAGE(IX)                         
135200                    MOVE JA TO OMINVENTERING-SW                           
135300                    ADD 1 TO IX                                           
135400                 END-IF                                                   
135500                 MOVE JA  TO FEL-SW                                       
135600              END-IF                                                      
135700           ELSE                                                           
135800              IF TAB-ARTIKEL (INDX) NUMERIC                               
135900                 MOVE NEJ   TO FEL-RAD(IX)                                
136000                 PERFORM HF-FYLL-FELTABELL                                
136100                 MOVE NEJ TO FORTSATT-SW                                  
136200              END-IF                                                      
136300           END-IF                                                         
136400        END-IF                                                            
136500        IF ALLT-OK AND FORTSATT                                           
136600           PERFORM HAC-KOLLA-MED-WDH1                                     
136700           IF ALLT-OK AND FORTSATT                                        
136800              IF DCS-CDC                                                  
136908                 MOVE +0 TO CLAG-KVUTRS                                   
137000                 PERFORM IMS-REPL-ARTC-CLAG                               
137010**** OM DET ÄR EN REFILLARTIKEL SÅ SKALL FLAGGA SÄTTAS TILL N             
137020**** SÄTTER BARA DEN OM DET ÄR REFILLER FRÅN KINA                         
137030**** BEHÖVER BYGGA EN ANNAN LÖSNING OM DET FINNS REFILLER                 
137040**** FRÅN FLER STÄLLEN ÄN ETT                                             
137050                 PERFORM IMS-GHU-WDK629                                   
137060                 IF SEGMENT-FINNS                                         
137070                   MOVE NEJ           TO CREF-FLREFNYO                    
137080                   PERFORM IMS-REPL-WDK629                                
137090                 END-IF                                                   
137091                 COMPUTE WS-KVANTAL =                                     
137092                  CLAG-KVLS + CLAG-KVEFRS + CLAG-KVAKS-CDC                
137100              ELSE                                                        
137200*                IF NOT DCS-NDC-NA                                        
137300                    MOVE +0 TO SLAG-KVUTRS                                
137400                    PERFORM IMS-REPL-SLAGER-SEGM                          
137500*                END-IF                                                   
137600              END-IF                                                      
137700              PERFORM HB-UPPDATERA-WDH1                                   
137800              IF ALLT-OK                                                  
137900                 PERFORM HC-UPPDATERA-WDH7                                
138000                 IF ALLT-OK                                               
138100                    IF WS-ANTAL NOT = 0                                   
138200                      MOVE REQU-IDDC-KEY TO WS-IDDC                       
138300                      IF NDC-NA                                           
138400                         PERFORM HL-FYLL-WDR8-AREA                        
138500                         PERFORM HLA-UPPDATERA-WDR8                       
138600                      ELSE                                                
138700                         IF XDC-NON-VCC-OWNED                             
138800                         OR LDC-CN                                        
138900                            PERFORM HK-FYLL-WDR8-AREA                     
139000                            PERFORM HKA-UPPDATERA-WDR8                    
139100                         ELSE                                             
139200                            PERFORM HD-FYLL-WDR9-AREA                     
139300                            PERFORM HDA-UPPDATERA-WDR9                    
139400                         END-IF                                           
139500                      END-IF                                              
139600                    END-IF                                                
139700                    IF ALLT-OK                                            
139800                       PERFORM HE-FYLL-WDL9-AREA                          
139900                       PERFORM HEA-UPPDATERA-WDL9                         
140000                       IF ALLT-OK                                         
140100                          IF DCS-CDC                                      
140200                             PERFORM HH-TAECKNING-CDC                     
140300                          END-IF                                          
140400                          PERFORM HI-RADERA-UTREDSALDO                    
140500                          PERFORM HJ-UPPDATERA-WDK7                       
140600                          MOVE JA TO UPDATE-SW                            
140700**** IDTRACK CHANGES BEGIN ****                                           
140800                         IF WS-FLTRACK = 'J' AND WS-FLLOCAL = 'J'         
140900                          IF TAB-ANTAL (INDX) >= 0                        
141000                             IF TAB-ANTAL (INDX) > TAB-KVLS (INDX)        
141100                                COMPUTE WS-KVLS-UPD =                     
141200                                TAB-ANTAL (INDX) - TAB-KVLS (INDX)        
141300                                MOVE WS-KVLS-UPD TO WS-VALD-KVAVIS        
141400                                PERFORM HJA-VALIDATE-IDTRACK              
141500                             ELSE                                         
141600                                IF TAB-KVLS (INDX) >                      
141700                                   TAB-ANTAL (INDX)                       
141800                                   COMPUTE WS-KVLS-UPD =                  
141900                                TAB-KVLS (INDX) - TAB-ANTAL (INDX)        
142000                                MOVE WS-KVLS-UPD TO WS-VALD-KVAVIS        
142100                                PERFORM HJB-VALIDATE-IDTRACK              
142200                                END-IF                                    
142300                             END-IF                                       
142400                          ELSE                                            
142500                             IF TAB-TECKEN (INDX) = '+'                   
142600                                AND TAB-DIFFERANS (INDX) > 0              
142700                                MOVE TAB-DIFFERANS (INDX) TO              
142800                                     WS-VALD-KVAVIS                       
142900                                PERFORM HJA-VALIDATE-IDTRACK              
143000                             ELSE                                         
143100                             IF TAB-TECKEN (INDX) = '-'  AND              
143200                                TAB-DIFFERANS (INDX) > 0                  
143300                                MOVE TAB-DIFFERANS (INDX) TO              
143400                                     WS-VALD-KVAVIS                       
143500                                PERFORM HJB-VALIDATE-IDTRACK              
143600                             END-IF                                       
143700                             END-IF                                       
143800                          END-IF                                          
143900                         END-IF                                           
144000**** IDTRACK CHANGES ENDS  ****                                           
144100                       END-IF                                             
144200                    END-IF                                                
144300                 END-IF                                                   
144400              END-IF                                                      
144500           END-IF                                                         
144600        END-IF                                                            
144700     END-IF                                                               
144800     ADD +1 TO INDX                                                       
144900     END-PERFORM                                                          
145000     IF ALLT-FEL AND NOT FEL-US                                           
145100        MOVE '290'    TO RESP-IDMSG-ERROR                                 
145200        MOVE NEJ      TO ALLT-OK-SW                                       
145300     ELSE                                                                 
145400        IF NOT ALLT-FEL                                                   
145500           MOVE '001' TO RESP-IDMSG-INFO                                  
145600        END-IF                                                            
145700        IF OMINVENTERING                                                  
145800           MOVE '290' TO RESP-IDMSG-ERROR                                 
145900        END-IF                                                            
146000     END-IF                                                               
146100     IF DCS-KDTRADP = 'BR12' AND FIRST-REC-TRANS                          
146200        PERFORM S23-SEND-CLOSE                                            
146201     END-IF                                                               
146202                                                                          
146203     .                                                                    
146300     EJECT                                                                
146400 HA-LAES-WDK6 SECTION.                                                    
146500     MOVE 'HA-LAES-WDK6    '  TO CURR-SECTION                             
146600                                                                          
146700     MOVE JA                  TO ALLT-OK-SW                               
146800     MOVE JA                  TO FORTSATT-SW                              
146900     PERFORM IMS-GET-ARTC-ART                                             
147000     IF SEGMENT-FINNS                                                     
147100        MOVE ART-KDSORT       TO WS-KDSORT                                
147200        PERFORM IMS-GET-ARTC-CLAG                                         
147300        IF SEGMENT-FINNS                                                  
147400           MOVE CLAG-PRARTSTD TO WS-PRARTSTD                              
147500           MOVE CLAG-KVLS     TO WS-XLAG-KVLS                             
147600        ELSE                                                              
147700           MOVE NEJ           TO ALLT-OK-SW                               
147800        END-IF                                                            
147900     ELSE                                                                 
148000        MOVE NEJ              TO ALLT-OK-SW                               
148100     END-IF                                                               
148200     IF DCS-CDC                                                           
148300       CONTINUE                                                           
148400     ELSE                                                                 
148500*      IF NOT DCS-NDC-NA                                                  
148600          PERFORM IMS-GHU-WDK711                                          
148700          MOVE SLAG-KVLS           TO WS-XLAG-KVLS                        
148800                                      WS-SLAG-KVLS                        
148900*      END-IF                                                             
149000     END-IF                                                               
149100     .                                                                    
149200     EJECT                                                                
149300                                                                          
149400 HAA-KOLLA-DIFF SECTION.                                                  
149500     MOVE 'HAA-KOLLA-DIFF  '  TO CURR-SECTION                             
149600                                                                          
149700     IF ALLT-OK                                                           
149800        COMPUTE WS-KVLS = TAB-KVEFRS(INDX) + TAB-KVLS (INDX)              
149900*--- POSITIVT LAGERSALDO                                                  
150000*--- FYSISKT ANTAL IFRÅN BILDEN                                           
150100        IF WS-KVLS >= 0                                                   
150200           IF REQU-ANTAL-IN (INDX) NOT = ALL '+'                          
150300              IF TAB-ANTAL (INDX) >= WS-KVLS                              
150400                 COMPUTE WS-ANTAL = TAB-ANTAL(INDX) - WS-KVLS             
150500                 COMPUTE WS-KVLS-NEW = WS-XLAG-KVLS + WS-ANTAL            
150600              ELSE                                                        
150700                 COMPUTE WS-ANTAL = TAB-ANTAL (INDX) - WS-KVLS            
150800                 COMPUTE WS-KVLS-NEW = WS-ANTAL + WS-XLAG-KVLS            
150900              END-IF                                                      
151000              IF WS-ANTAL >= 0                                            
151100                 MOVE '+' TO WS-TECKEN                                    
151200              ELSE                                                        
151300                 MOVE '-' TO WS-TECKEN                                    
151400              END-IF                                                      
151500           ELSE                                                           
151600*--- DIFFERANS MELLAN KVLS OCH HITTAT ANTAL                               
151700              IF TAB-DIFFERANS (INDX) NOT = ALL '+'                       
151800                 IF TAB-TECKEN (INDX) = '+'                               
151900                    COMPUTE WS-ANTAL    = TAB-DIFFERANS (INDX)            
152000                    COMPUTE WS-KVLS-NEW = WS-ANTAL + WS-XLAG-KVLS         
152100                 ELSE                                                     
152200                    COMPUTE WS-ANTAL = TAB-DIFFERANS(INDX)                
152300                    COMPUTE WS-KVLS-NEW = WS-XLAG-KVLS - WS-ANTAL         
152400                 END-IF                                                   
152500                 IF TAB-TECKEN (INDX) = '+'                               
152600                    MOVE '+'  TO WS-TECKEN                                
152700                 ELSE                                                     
152800                    MOVE '-'  TO WS-TECKEN                                
152900                    COMPUTE WS-ANTAL =  WS-ANTAL * -1                     
153000                 END-IF                                                   
153100              END-IF                                                      
153200           END-IF                                                         
153300        ELSE                                                              
153400*--- NEGATIVT LAGERSALDO                                                  
153500           IF REQU-ANTAL-IN (INDX) NOT = ALL '+'                          
153600***        IF TAB-ANTAL (INDX) NOT = ALL '+'                              
153700              IF TAB-ANTAL (INDX) > WS-KVLS                               
153800                 MOVE '+'    TO WS-TECKEN                                 
153900              ELSE                                                        
154000                 MOVE '-'    TO WS-TECKEN                                 
154100              END-IF                                                      
154200              COMPUTE WS-ANTAL    = TAB-ANTAL(INDX) - WS-KVLS             
154300              COMPUTE WS-KVLS-NEW = WS-XLAG-KVLS + WS-ANTAL               
154400           ELSE                                                           
154500              IF TAB-DIFFERANS (INDX) NOT = ALL '+'                       
154600               IF TAB-TECKEN(INDX) = '-'                                  
154700                   COMPUTE WS-KVLS-NEW = WS-XLAG-KVLS -                   
154800                                    TAB-DIFFERANS(INDX)                   
154900               ELSE                                                       
155000                 COMPUTE WS-KVLS-NEW = WS-XLAG-KVLS +                     
155100                                    TAB-DIFFERANS(INDX)                   
155200               END-IF                                                     
155300               MOVE TAB-TECKEN(INDX)         TO WS-TECKEN                 
155400               MOVE TAB-DIFFERANS(INDX)      TO WS-ANTAL                  
155500               IF TAB-TECKEN(INDX) = '-'                                  
155600                  COMPUTE WS-ANTAL = WS-ANTAL * -1                        
155700               END-IF                                                     
155800              END-IF                                                      
155900           END-IF                                                         
156000        END-IF                                                            
156100     END-IF                                                               
156200***  CALL ABEND                                                           
156300     .                                                                    
156400     EJECT                                                                
156500 HAB-KOLLA-MED-REGLER SECTION.                                            
156600     MOVE 'HAB-KOLLA-MED-RE'  TO CURR-SECTION                             
156700                                                                          
156800     IF ALLT-OK                                                           
156900        IF WS-ANTAL NOT = 0                                               
157000          MOVE WS-ANTAL           TO WS-ANTAL-N                           
157100          COMPUTE WS-BELOPP = WS-ANTAL-N * WS-PRARTSTD                    
157200          MOVE REQU-IDDC-KEY TO WS-IDDC                                   
157300          IF XDC-NON-VCC-OWNED OR LDC-CN                                  
157400            IF WS-IDPRTOMG =  1                                           
157500              IF WS-BELOPP > 100                                          
157600                 MOVE NEJ            TO ALLT-OK-SW                        
157700              ELSE                                                        
157800                 MOVE JA             TO ALLT-OK-SW                        
157900              END-IF                                                      
158000                                                                          
158100            ELSE                                                          
158200              IF WS-BELOPP > 1000                                         
158300                 MOVE NEJ            TO ALLT-OK-SW                        
158400              ELSE                                                        
158500                 MOVE JA             TO ALLT-OK-SW                        
158600              END-IF                                                      
158700            END-IF                                                        
158800          ELSE                                                            
158900            IF WS-IDPRTOMG =  1                                           
159000              IF WS-BELOPP > 5000                                         
159100                 MOVE NEJ            TO ALLT-OK-SW                        
159200              ELSE                                                        
159300                 MOVE JA             TO ALLT-OK-SW                        
159400              END-IF                                                      
159500                                                                          
159600            ELSE                                                          
159700              IF WS-BELOPP > 10000                                        
159800                 MOVE NEJ            TO ALLT-OK-SW                        
159900              ELSE                                                        
160000                IF WS-BELOPP > 5000 AND DCS-CDC                           
160001                  MOVE NEJ           TO ALLT-OK-SW                        
160002                ELSE                                                      
160003                  MOVE JA            TO ALLT-OK-SW                        
160004                END-IF                                                    
160100              END-IF                                                      
160200            END-IF                                                        
160300          END-IF                                                          
160400        ELSE                                                              
160500           MOVE ZERO                TO WS-ANTAL                           
160600           MOVE WS-XLAG-KVLS        TO WS-KVLS-NEW                        
160700           MOVE JA                  TO ALLT-OK-SW                         
160800        END-IF                                                            
160900        IF DCS-CDC                                                        
161000*------ NYA VARDEN TILL WDK611 SEGMENT                                    
161100           MOVE WS-KVLS-NEW        TO CLAG-KVLS                           
161200           MOVE WS-ANTAL           TO CLAG-KVINVS                         
161300           MOVE WS-TIINVDAT        TO CLAG-TIINVDAT                       
161400        ELSE                                                              
161500*          IF NOT DCS-NDC-NA                                              
161600*------ NYA VARDEN TILL WDK711 SEGMENT                                    
161700              MOVE WS-KVLS-NEW      TO SLAG-KVLS                          
161800              MOVE WS-ANTAL         TO SLAG-KVINVS                        
161900              MOVE WS-TIINVDAT      TO SLAG-TIINVDAT                      
162000*          END-IF                                                         
162100        END-IF                                                            
162200     END-IF                                                               
162300                                                                          
162400     .                                                                    
162500     EJECT                                                                
162600 HAC-KOLLA-MED-WDH1 SECTION.                                              
162700     MOVE 'HAC-KOLLA-MED-WD'  TO CURR-SECTION                             
162800                                                                          
162900     MOVE NEJ TO POST-SW                                                  
163000     MOVE NEJ TO ALLT-OK-SW                                               
163100     MOVE NEJ TO FORTSATT-SW                                              
163200     MOVE NEJ TO FEL-US-SW                                                
163300     PERFORM IMS-GET-INVA-ART                                             
163400     IF SEGMENT-FINNS                                                     
163500       PERFORM IMS-GET-INVA-INV                                           
163600       PERFORM UNTIL SEGMENT-SAKNAS OR POST-FINNS OR FEL-US               
163700         IF INVA-INV-IDPRTOMG = WS-IDPRTOMG AND                           
163800           (INVA-INV-IDLOPNR  = WS-IDLOPNR)                               
163900            MOVE JA                TO POST-SW                             
164000            MOVE JA                TO ALLT-OK-SW                          
164100            MOVE JA                TO FORTSATT-SW                         
164200            MOVE INVA-INV-KDINVKAT TO WS-KDINVKAT                         
164300                                                                          
164400           IF NDC-US                                                      
164500             IF SLAG-PRAVCOST = ZERO                                      
164600               IF INVA-INV-KDINVKAT = 3 AND                               
164700                 (REQU-ANTAL-IN (INDX) = ALL '+' OR ZERO) AND             
164800                 (REQU-DIFFERANS-IN (INDX) = ALL '+' OR ZERO)             
164900                 NEXT SENTENCE                                            
165000               ELSE                                                       
165100                 MOVE NEJ        TO ALLT-OK-SW                            
165200                 MOVE JA         TO FEL-US-SW                             
165300                 MOVE '023'      TO RESP-IDMSG-ERROR-LINE(INDX)           
165400                 MOVE '023'      TO RESP-IDMSG-ERROR                      
165500                 MOVE '023'      TO TAB-MESSAGE(IX)                       
165600                 MOVE 'PRAVCOST' TO RESP-IDELMT-ERROR                     
165700                 MOVE JA         TO FEL-RAD(IX)                           
165800                 MOVE JA         TO FEL-SW                                
165900                 PERFORM HF-FYLL-FELTABELL                                
166000               END-IF                                                     
166100             END-IF                                                       
166200           END-IF                                                         
166300                                                                          
166400           IF NOT FEL-US                                                  
166500             MOVE INVA-INV-DAREGDAT-CRE TO WS-DAREGDAT-CRE                
166600             MOVE INVA-INV-DAREGDAT-PR1 TO WS-DAREGDAT-PR1                
166700             MOVE INVA-INV-DAREGDAT-PR2 TO WS-DAREGDAT-PR2                
166800             MOVE INVA-INV-DAREGDAT-PR3 TO WS-DAREGDAT-PR3                
166900             PERFORM IMS-GNP-WDH121                                       
167000             PERFORM UNTIL SEGMENT-SAKNAS                                 
167100               IF INVA-INVL-KDSEGKEY = '0'                                
167200                 MOVE INVA-INVL-IDUSER   TO WS-IDUSER-CRE                 
167300               END-IF                                                     
167400               IF INVA-INVL-KDSEGKEY = '1'                                
167500                 MOVE INVA-INVL-IDUSER   TO WS-IDUSER-PR1                 
167600               END-IF                                                     
167700               IF INVA-INVL-KDSEGKEY = '2'                                
167800                 MOVE INVA-INVL-IDUSER   TO WS-IDUSER-PR2                 
167900               END-IF                                                     
168000               IF INVA-INVL-KDSEGKEY = '3'                                
168100                 MOVE INVA-INVL-IDUSER   TO WS-IDUSER-PR3                 
168200               END-IF                                                     
168300               PERFORM IMS-GNP-WDH121                                     
168400             END-PERFORM                                                  
168700           END-IF                                                         
168701         ELSE                                                             
168702             PERFORM IMS-GET-INVA-INV                                     
168800         END-IF                                                           
168900       END-PERFORM                                                        
169000     END-IF                                                               
169100     .                                                                    
169200     EJECT                                                                
169300 HB-UPPDATERA-WDH1 SECTION.                                               
169400     MOVE 'HB-UPPDATERA-WDH'  TO CURR-SECTION                             
169500                                                                          
169600     MOVE NEJ TO POST-SW                                                  
169700     PERFORM IMS-GET-INVA-ART                                             
169800     IF SEGMENT-FINNS                                                     
169900        PERFORM IMS-GET-INVA-INV                                          
170000        PERFORM UNTIL SEGMENT-SAKNAS OR POST-FINNS                        
170100           IF INVA-INV-IDPRTOMG = WS-IDPRTOMG AND                         
170200             (INVA-INV-IDLOPNR  = WS-IDLOPNR)                             
170300              MOVE JA TO POST-SW                                          
170400              MOVE INVA-INV-KDINVKAT TO WS-KDINVKAT                       
170500              MOVE INVA-INV-DAREGDAT-CRE TO WS-DAREGDAT-CRE               
170600              MOVE INVA-INV-DAREGDAT-PR1 TO WS-DAREGDAT-PR1               
170700              MOVE INVA-INV-DAREGDAT-PR2 TO WS-DAREGDAT-PR2               
170800              MOVE INVA-INV-DAREGDAT-PR3 TO WS-DAREGDAT-PR3               
170900                                                                          
171000              PERFORM IMS-GNP-WDH121                                      
171100              PERFORM UNTIL SEGMENT-SAKNAS                                
171200                IF INVA-INVL-KDSEGKEY = '0'                               
171300                  MOVE INVA-INVL-IDUSER   TO WS-IDUSER-CRE                
171400                END-IF                                                    
171500                IF INVA-INVL-KDSEGKEY = '1'                               
171600                  MOVE INVA-INVL-IDUSER   TO WS-IDUSER-PR1                
171700                END-IF                                                    
171800                IF INVA-INVL-KDSEGKEY = '2'                               
171900                  MOVE INVA-INVL-IDUSER   TO WS-IDUSER-PR2                
172000                END-IF                                                    
172100                IF INVA-INVL-KDSEGKEY = '3'                               
172200                  MOVE INVA-INVL-IDUSER   TO WS-IDUSER-PR3                
172300                END-IF                                                    
172400              PERFORM IMS-GNP-WDH121                                      
172500              END-PERFORM                                                 
172600           ELSE                                                           
172700              PERFORM IMS-GET-INVA-INV                                    
172800           END-IF                                                         
172900        END-PERFORM                                                       
173000     END-IF                                                               
173100                                                                          
173200     PERFORM HBC-RENSA-WDH1                                               
173300                                                                          
173400     PERFORM IMS-GET-INVA-ART                                             
173500     IF SEGMENT-SAKNAS                                                    
173600        MOVE W-IDARTNR   TO INVA-ART-IDARTNR                              
173700        PERFORM IMS-ISRT-INVA-ART                                         
173800     END-IF                                                               
173900     PERFORM HBA-FYLL-INVA-AREA                                           
174000                                                                          
174100     PERFORM IMS-ISRT-INVA-INV                                            
174200     PERFORM UNTIL SEGMENT-FINNS                                          
174300        IF SEGMENT-FINNS-REDAN OR INDEX-FINNS-REDAN                       
174400           ADD 1                TO INVA-INV-TISEGKEY                      
174500           PERFORM IMS-ISRT-INVA-INV                                      
174600        END-IF                                                            
174700     END-PERFORM                                                          
174800     PERFORM HBD-INSERT-WDH121                                            
174900     .                                                                    
175000     EJECT                                                                
175100 HBA-FYLL-INVA-AREA SECTION.                                              
175200     MOVE 'HBA-FYLL-INVA-AR'  TO CURR-SECTION                             
175300                                                                          
175400     MOVE SPACE TO INVA-INV-WDH111                                        
175500     MOVE NEJ             TO INVA-INV-FLINVSKR                            
175600                             INVA-INV-FLINV2B                             
175700                             INVA-INV-FLINV3E                             
175800                             INVA-INV-FLINV4N                             
175900     IF DCS-NDC-NA                                                        
176000       MOVE NEJ           TO INVA-INV-FLINV2C                             
176100                             INVA-INV-FLINV2D                             
176200       MOVE JA            TO INVA-INV-FLINV4R                             
176300                             INVA-INV-FLINV4P                             
176400     ELSE                                                                 
176500       MOVE JA            TO INVA-INV-FLINV2C                             
176600                             INVA-INV-FLINV2D                             
176700       MOVE NEJ           TO INVA-INV-FLINV4R                             
176800                             INVA-INV-FLINV4P                             
176900     END-IF                                                               
177000     MOVE NEJ             TO INVA-INV-FLINV85                             
177100     MOVE SPACE           TO INVA-INV-FILLER                              
177200                             INVA-INV-FILLER1                             
177300     MOVE DCS-IDDC        TO INVA-INV-IDDC                                
177400     MOVE WS-ANTAL        TO INVA-INV-KVJUSTKV                            
177500     MOVE SPACE           TO INVA-INV-TEINVANM                            
177600     MOVE +12             TO INVA-INV-KDINVKAT                            
177700     MOVE WS-KDINVKAT     TO INVA-INV-KDINVKAT-OLD                        
177800     MOVE JA              TO INVA-INV-FLINVBEH                            
177900     MOVE ART-IDFKNGRP    TO INVA-INV-IDFKNGRP                            
178000     MOVE ZERO            TO INVA-INV-KDINVPRIO                           
178100     MOVE ART-KDPRODSL    TO INVA-INV-KDPRODSL                            
178200     MOVE CLAG-KDVVKL     TO INVA-INV-KDVVKL                              
178300     MOVE CLAG-KDPSLLOC   TO INVA-INV-KDPSLLOC                            
178400                                                                          
178500     MOVE ZERO                TO INVA-INV-ADLAGOMR                        
178600                                 INVA-INV-ADGANG                          
178700                                 INVA-INV-ADPLATS                         
178800     IF DCS-CDC                                                           
178900        MOVE CLAG-ADLAGOMR     TO INVA-INV-ADLAGOMR                       
179000        MOVE CLAG-ADGANG       TO INVA-INV-ADGANG                         
179100        MOVE CLAG-ADPLATS      TO INVA-INV-ADPLATS                        
179200     ELSE                                                                 
179300*       IF NOT DCS-NDC-NA                                                 
179400           PERFORM IMS-GU-WDK711                                          
179500           IF SEGMENT-FINNS                                               
179600              MOVE SLAG-ADLAGOMR TO INVA-INV-ADLAGOMR                     
179700              MOVE SLAG-ADGANG   TO INVA-INV-ADGANG                       
179800              MOVE SLAG-ADPLATS  TO INVA-INV-ADPLATS                      
179900           END-IF                                                         
180000*       END-IF                                                            
180100     END-IF                                                               
180200                                                                          
180300     IF DCS-CDC                                                           
180400        IF CLAG-KDERS = 11 OR 14 OR 17 OR 18 OR 19                        
180500           MOVE JA              TO INVA-INV-FLINV85                       
180600        END-IF                                                            
180700     END-IF                                                               
180800     MOVE 0               TO WS-TISEGKEY-LOPNR                            
180900     MOVE WS-DAGENS-DATUM TO WS-TISEGKEY-DAT                              
181000                             INVA-INV-DAREGDAT                            
181100                             INVA-INV-DAREGDAT-CRE                        
181200     MOVE WS-TIAAAAMMDDL  TO INVA-INV-TISEGKEY                            
181300     COMPUTE INVA-INV-DAREGDAT-SORT = WS-DAGENS-DATUM                     
181400                                                                          
181500     MOVE ZERO            TO INVA-INV-IDPRTOMG                            
181600                             INVA-INV-IDLOPNR                             
181700                             INVA-INV-KVAKS-OLD                           
181800                             INVA-INV-KVEFRS-OLD                          
181900                             INVA-INV-KVLS-OLD                            
182000                                                                          
182100     MOVE ZERO            TO INVA-INV-DAREGDAT-PR1                        
182200                             INVA-INV-DAREGDAT-PR2                        
182300                             INVA-INV-DAREGDAT-PR3                        
182400     .                                                                    
182500     EJECT                                                                
182600 HBC-RENSA-WDH1 SECTION.                                                  
182700     MOVE 'HBC-RENSA-WDH1  '  TO CURR-SECTION                             
182800                                                                          
182900*----MÅSTE BÖRJA OM IFRÅN BÖRJAN PÅ ARTIKEL FÖR ATT KUNNA                 
183000*----RENSA ALLA POSTER                                                    
183100     MOVE 001        TO W-KDINVKAT-WDH1-MIN                               
183200     MOVE 099        TO W-KDINVKAT-WDH1-MAX                               
183300                                                                          
183400     PERFORM IMS-GET-INVA-ART                                             
183500     IF SEGMENT-FINNS                                                     
183600        PERFORM IMS-GET-INVA-INV                                          
183700        PERFORM UNTIL SEGMENT-SAKNAS                                      
183800           IF INVA-INV-KDINVKAT = +1 OR +2 OR +3 OR +4 OR +5 OR +9        
183900              MOVE JA    TO INVA-INV-FLINVBEH                             
184000              MOVE ZERO  TO INVA-INV-IDPRTOMG                             
184100              MOVE ZERO  TO INVA-INV-IDLOPNR                              
184200              IF INVA-INV-KDINVKAT = +2                                   
184300                 MOVE WS-ANTAL        TO INVA-INV-KVJUSTKV                
184400              END-IF                                                      
184500              PERFORM IMS-REPL-INVA-INV                                   
184600           END-IF                                                         
184700           PERFORM IMS-GET-INVA-INV                                       
184800        END-PERFORM                                                       
184900     END-IF                                                               
185000     .                                                                    
185100     EJECT                                                                
185200 HBD-INSERT-WDH121 SECTION.                                               
185300     MOVE 'WBD-INSERT-WDH121' TO CURR-SECTION                             
185400     MOVE '0'            TO INVA-INVL-KDSEGKEY                            
185500     MOVE REQU-IDUSER    TO INVA-INVL-IDUSER                              
185600     PERFORM IMS-INSERT-WDH121                                            
185700                                                                          
185800     MOVE '1'            TO INVA-INVL-KDSEGKEY                            
185900     MOVE SPACE          TO INVA-INVL-IDUSER                              
186000     PERFORM IMS-INSERT-WDH121                                            
186100     MOVE '2'            TO INVA-INVL-KDSEGKEY                            
186200     MOVE SPACE          TO INVA-INVL-IDUSER                              
186300     PERFORM IMS-INSERT-WDH121                                            
186400     MOVE '3'            TO INVA-INVL-KDSEGKEY                            
186500     MOVE SPACE          TO INVA-INVL-IDUSER                              
186600     PERFORM IMS-INSERT-WDH121                                            
186700                                                                          
186800     .                                                                    
186900     EJECT                                                                
187000 HC-UPPDATERA-WDH7 SECTION.                                               
187100     MOVE 'HC-UPPDATERA-WDH'  TO CURR-SECTION                             
187200                                                                          
187300     PERFORM IMS-GHU-INVHIST-ROT                                          
187400     IF SEGMENT-FINNS                                                     
187500        MOVE JA              TO ALLT-OK-SW                                
187600     ELSE                                                                 
187700        MOVE W-IDARTNR       TO INVA-IDARTNR                              
187800        PERFORM IMS-ISRT-INVHIST-ROT                                      
187900     END-IF                                                               
188000     PERFORM HCA-SKAPA-TISEGKEY                                           
188100     PERFORM HCB-KOLLA-KDJUSTYP                                           
188200     MOVE WS-TISEGKEY       TO INVH-TISEGKEY                              
188300     MOVE DCS-IDDC          TO INVH-IDDC                                  
188400     MOVE WS-DAGENS-DATUM   TO INVH-DAREGDAT-CLO                          
188500     MOVE JA                TO INVH-FLAUTLSJ                              
188600     MOVE WS-IDUSER         TO INVH-IDPW                                  
188700     MOVE WS-ANTAL          TO INVH-KVJUSTKV                              
188800     MOVE INVH-KDJUSTYP     TO WS-KDJUSTYP                                
188900     MOVE WS-PRARTSTD       TO INVH-PRARTSTD                              
189000     MOVE WS-MSGI-IDUSER    TO INVH-IDUSER-CLO                            
189100     MOVE WS-DAREGDAT-CRE   TO INVH-DAREGDAT-CRE                          
189200     MOVE WS-DAREGDAT-PR1   TO INVH-DAREGDAT-PR1                          
189300     MOVE WS-DAREGDAT-PR2   TO INVH-DAREGDAT-PR2                          
189400     MOVE WS-DAREGDAT-PR3   TO INVH-DAREGDAT-PR3                          
189500     MOVE WS-IDUSER-PR1     TO INVH-IDUSER-PR1                            
189600     MOVE WS-IDUSER-PR2     TO INVH-IDUSER-PR2                            
189700     MOVE WS-IDUSER-PR3     TO INVH-IDUSER-PR3                            
189800     MOVE WS-IDUSER-CRE     TO INVH-IDUSER-CRE                            
189810                                                                          
189910     IF DCS-CDC                                                           
189920       MOVE WS-KVANTAL      TO INVH-KVANTAL                               
189930     ELSE                                                                 
189940       MOVE +0              TO INVH-KVANTAL                               
189950     END-IF                                                               
190000                                                                          
190100     MOVE JA                TO ALLT-OK-SW                                 
190200     PERFORM IMS-ISRT-INVHIST-SEGM                                        
190300     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
190400        IF SEGMENT-FINNS-REDAN                                            
190500           SUBTRACT 1 FROM INVH-TISEGKEY                                  
190600           PERFORM IMS-ISRT-INVHIST-SEGM                                  
190700        END-IF                                                            
190800     END-PERFORM                                                          
190900     MOVE ZERO               TO WS-DAREGDAT-CRE                           
191000                                WS-DAREGDAT-PR1                           
191100                                WS-DAREGDAT-PR2                           
191200                                WS-DAREGDAT-PR3                           
191300     MOVE SPACE              TO WS-IDUSER-PR1                             
191400                                WS-IDUSER-PR2                             
191500                                WS-IDUSER-PR3                             
191600                                WS-IDUSER-CRE                             
191700     .                                                                    
191800     EJECT                                                                
191900 HCA-SKAPA-TISEGKEY SECTION.                                              
192000     MOVE 'HCA-SKAPA-TISEGK'  TO CURR-SECTION                             
192100                                                                          
192200     MOVE 9 TO WS-TISEGKEY-LOPNR                                          
192300     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAGENS-DATUM                   
192400     MOVE WS-DAGENS-DATUM TO WS-TISEGKEY-DAT                              
192500     COMPUTE WS-TISEGKEY = 999999999 - WS-TIAAAAMMDDL                     
192600     .                                                                    
192700     EJECT                                                                
192800 HCB-KOLLA-KDJUSTYP SECTION.                                              
192900     MOVE 'HCB-KOLLA-KDJUST'  TO CURR-SECTION                             
193000                                                                          
193100     EVALUATE TRUE                                                        
193200       WHEN WS-KDINVKAT = +1                                              
193300          MOVE +1 TO INVH-KDJUSTYP                                        
193400          MOVE '1' TO WS-LOGG-UREF1                                       
193500       WHEN WS-KDINVKAT = +2                                              
193600          MOVE +2 TO INVH-KDJUSTYP                                        
193700          MOVE '2' TO WS-LOGG-UREF1                                       
193800       WHEN WS-KDINVKAT = +3                                              
193900          MOVE +3 TO INVH-KDJUSTYP                                        
194000          MOVE '3' TO WS-LOGG-UREF1                                       
194100       WHEN WS-KDINVKAT = +4                                              
194200          MOVE +4 TO INVH-KDJUSTYP                                        
194300          MOVE '4' TO WS-LOGG-UREF1                                       
194400       WHEN WS-KDINVKAT = +5                                              
194500          MOVE +5 TO INVH-KDJUSTYP                                        
194600          MOVE '5' TO WS-LOGG-UREF1                                       
194700       WHEN WS-KDINVKAT = +8                                              
194800          MOVE +8 TO INVH-KDJUSTYP                                        
194900          MOVE '8' TO WS-LOGG-UREF1                                       
195000       WHEN WS-KDINVKAT = +9                                              
195100          MOVE +9 TO INVH-KDJUSTYP                                        
195200          MOVE '9' TO WS-LOGG-UREF1                                       
195300       WHEN OTHER                                                         
195400          MOVE +0 TO INVH-KDJUSTYP                                        
195500          MOVE +0 TO WS-LOGG-UREF1                                        
195600     END-EVALUATE                                                         
195700     .                                                                    
195800     EJECT                                                                
195900 HD-FYLL-WDR9-AREA  SECTION.                                              
196000     MOVE 'HD-FYLL-WDR9-ARE'  TO CURR-SECTION                             
196100                                                                          
196200     MOVE 'W5030800'       TO FIL-IDPGM                                   
196300     MOVE WS-DAGENS-DATUM  TO FIL-DAREGDAT                                
196400     ACCEPT FIL-TIKLOCK    FROM TIME                                      
196500     MOVE 1                TO FIL-IDSEKVNR                                
196600     MOVE 'W510EKHA'       TO FIL-IDCPYTXT                                
196700     MOVE WS-IDUSER        TO FIL-IDUSER                                  
196800     MOVE W-IDARTNR        TO EKH-IDARTNR                                 
196900     MOVE '403'            TO EKH-KDEKHHT                                 
197000     MOVE '40'             TO WS-KDEKSHT-1                                
197100     MOVE WS-KDJUSTYP      TO WS-KDEKSHT-2                                
197200     MOVE WS-EKH-KDEKSHT   TO EKH-KDEKSHT                                 
197300     MOVE 'DET'            TO EKH-KDEKNIVA                                
197400     MOVE DCS-IDDC         TO EKH-IDDC-SEND                               
197500                              EKH-IDDC-REC                                
197600     MOVE ZERO             TO EKH-IDDISTR                                 
197700                              EKH-IDKUNDNR                                
197800     MOVE W-IDARTNR        TO W-EKH-IDARTNR                               
197900                                                                          
198000     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
198100     MOVE W-EKH-IDARTNR    TO CIA-IDARTBET-IN                             
198200     CALL W009CIA USING       CIA-W009CIA                                 
198300     MOVE CIA-IDARTBET-UT TO EKH-IDVERGL                                  
198400                                                                          
198500                                                                          
198600     MOVE WS-DAGENS-DATUM  TO EKH-DAVERDAT                                
198700     MOVE ART-KDPRODSL     TO EKH-KDPRODSL                                
198800     MOVE ZERO             TO EKH-KDPSLLOC                                
198900     MOVE SPACE            TO EKH-FLLSBOK                                 
199000     MOVE 'SEK'            TO EKH-KDVALISO                                
199100     MOVE 1.00             TO EKH-PRKURS                                  
199200     MOVE ZERO             TO EKH-PRARTNTO                                
199300     MOVE ZERO             TO EKH-PRARTSJK                                
199400     MOVE ZERO             TO EKH-PRHEMTAG                                
199500     MOVE WS-PRARTSTD      TO EKH-PRARTSTD                                
199600     MOVE ZERO             TO EKH-PRLANDCO                                
199700                              EKH-PRINK                                   
199800                              EKH-PRDIRLON                                
199900                              EKH-PRDMTRL                                 
200000                              EKH-PROVRPAL                                
200100                              EKH-SUBEL                                   
200200     MOVE WS-ANTAL         TO EKH-KVANTAL                                 
200300     MOVE 'L178'           TO EKH-IDTRANS                                 
200400     MOVE ZERO             TO EKH-BEVAT                                   
200500                              EKH-IDANALYS                                
200600                              EKH-IDKONTO                                 
200700                              EKH-KDANMORS                                
200800                              EKH-KDFRAKT                                 
200900                              EKH-SUVAT                                   
201000     MOVE ZERO             TO EKH-DAAVIDAT                                
201100                              EKH-IDAVINR                                 
201200                              EKH-KDAVVTYP                                
201300                              EKH-KDRT                                    
201400                              EKH-KVANTMOT                                
201500                              EKH-KVAVIS                                  
201600     MOVE WS-KDSORT        TO EKH-KDSORT                                  
201700     MOVE NEJ              TO EKH-FLDCET                                  
201800     MOVE 'SEPV'           TO EKH-KDTRADP                                 
201900     MOVE SPACE            TO EKH-IDLEVNR                                 
202000                              EKH-IDKST                                   
202100     MOVE SPACE            TO EKH-IDKUNDRF                                
202200     MOVE SPACE            TO EKH-IDFAKT-EXP                              
202300     .                                                                    
202400     EJECT                                                                
202500 HDA-UPPDATERA-WDR9 SECTION.                                              
202600     MOVE 'HDA-UPPDATERA-WD'  TO CURR-SECTION                             
202700                                                                          
202800     PERFORM IMS-ISRT-WDR901                                              
202900     PERFORM UNTIL SEGMENT-FINNS                                          
203000       ADD +1 TO FIL-IDSEKVNR                                             
203100       PERFORM IMS-ISRT-WDR901                                            
203200     END-PERFORM                                                          
203300     .                                                                    
203400     EJECT                                                                
203500 HE-FYLL-WDL9-AREA SECTION.                                               
203600     MOVE 'HE-FYLL-WDL9-ARE'  TO CURR-SECTION                             
203700                                                                          
203800* LÄGGER UPP SALDOLOGG I WDL9                                             
203900     MOVE W-IDARTNR             TO LOGG-IDARTNR                           
204000     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - WS-DAGENS-DATUM            
204100     ACCEPT TRANS-TID FROM TIME                                           
204200     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TID                 
204300     MOVE 9                       TO LOGG-IDSEKVNR                        
204400     MOVE DCS-IDDC                TO LOGG-IDDC                            
204500     MOVE 'MISC'                  TO LOGG-IDHUVTYP                        
204600     MOVE 'ADJ'                   TO LOGG-IDSUBTYP                        
204700     MOVE IDPGM                   TO LOGG-IDPGM                           
204800     MOVE 'L178'                  TO LOGG-IDTRANS                         
204900     MOVE WS-MSGI-IDUSER          TO LOGG-IDUSER                          
205000     MOVE SPACE                   TO LOGG-REF                             
205100     MOVE WS-LOGG-UREF1           TO LOGG-UREF1                           
205200     MOVE WS-IDUSER               TO LOGG-UREF2                           
205300     MOVE WS-TECKEN               TO LOGG-IDTECKEN-KVLS                   
205400     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS-PAV              
205500                                     LOGG-IDTECKEN-KVEFRS                 
205600                                     LOGG-IDTECKEN-KVAKS                  
205700     IF WS-ANTAL < 0                                                      
205800        COMPUTE LOGG-KVART-SALDO = WS-ANTAL * -1                          
205900     ELSE                                                                 
206000        MOVE WS-ANTAL              TO LOGG-KVART-SALDO                    
206100     END-IF                                                               
206200     IF DCS-CDC                                                           
206300        MOVE CLAG-KVAKS-PAV        TO LOGG-KVAKS-PAV                      
206400        MOVE CLAG-KVEFRS           TO LOGG-KVEFRS                         
206500        MOVE CLAG-KVLS             TO LOGG-KVLS                           
206600        COMPUTE LOGG-KVAKS = CLAG-KVAKS-CDC +                             
206700                             CLAG-KVAKS-T                                 
206800     ELSE                                                                 
206900*       IF NOT DCS-NDC-NA                                                 
207000           MOVE SLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                     
207100           MOVE SLAG-KVEFRS         TO LOGG-KVEFRS                        
207200           MOVE SLAG-KVLS           TO LOGG-KVLS                          
207300           MOVE SLAG-KVAKS-SDC      TO LOGG-KVAKS                         
207400*       END-IF                                                            
207500     END-IF                                                               
207600     MOVE ZERO                    TO LOGG-DAREGDAT-LADD                   
207700     .                                                                    
207800     EJECT                                                                
207900 HEA-UPPDATERA-WDL9 SECTION.                                              
208000     MOVE 'HEA-UPPDATERA-WD'  TO CURR-SECTION                             
208100                                                                          
208200     PERFORM IMS-ISRT-WDL901                                              
208300     IF SEGMENT-FINNS-REDAN                                               
208400        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
208500           ADD -1 TO LOGG-IDSEKVNR                                        
208600           PERFORM IMS-ISRT-WDL901                                        
208700        END-PERFORM                                                       
208800     END-IF                                                               
208900     .                                                                    
209000     EJECT                                                                
209100 HF-FYLL-FELTABELL SECTION.                                               
209200     MOVE 'HF-FYLL-FELTABEL'  TO CURR-SECTION                             
209300                                                                          
209400     MOVE TAB-ARTIKEL     (INDX)   TO FEL-ARTIKEL   (IX)                  
209500     MOVE TAB-BENAMNING   (INDX)   TO FEL-BENAMNING (IX)                  
209600     MOVE TAB-KVAKS       (INDX)   TO FEL-KVAKS     (IX)                  
209700     MOVE TAB-KVLS        (INDX)   TO FEL-KVLS      (IX)                  
209800     MOVE TAB-KVEFRS      (INDX)   TO FEL-KVEFRS    (IX)                  
209900     MOVE TAB-ANTAL       (INDX)   TO FEL-ANTAL     (IX)                  
210000     IF FEL-RAD (IX) = 'N'                                                
210100        MOVE SPACE                  TO FEL-TECKEN    (IX)                 
210200     ELSE                                                                 
210300        MOVE TAB-TECKEN    (INDX)   TO FEL-TECKEN    (IX)                 
210400     END-IF                                                               
210500     MOVE TAB-DIFFERANS   (INDX)   TO FEL-DIFFERANS (IX)                  
210510     MOVE TAB-FLOMINV     (INDX)   TO FEL-FLOMINV   (IX)                  
210600     MOVE TAB-MESSAGE     (INDX)   TO FEL-MESSAGE   (IX)                  
210700     ADD +1 TO IX                                                         
210800     .                                                                    
210900     EJECT                                                                
211000 HG-NOLLA-LISTNR SECTION.                                                 
211100     MOVE 'HG-NOLLA-LISTNR '  TO CURR-SECTION                             
211200                                                                          
211300     MOVE NEJ TO POST-SW                                                  
211400     PERFORM IMS-GET-INVA-ART                                             
211500     IF SEGMENT-FINNS                                                     
211600        PERFORM IMS-GET-INVA-INV                                          
211700        IF SEGMENT-FINNS                                                  
211800           PERFORM UNTIL SEGMENT-SAKNAS OR POST-FINNS                     
211900              IF INVA-INV-IDLOPNR = WS-IDLOPNR AND                        
212000                (INVA-INV-IDPRTOMG = WS-IDPRTOMG)                         
212100                 MOVE JA                 TO POST-SW                       
212200                 MOVE ZERO               TO INVA-INV-IDLOPNR              
212300*--INGEN ADD,ADD +1                  TO INVA-INV-IDPRTOMG                 
212400*--GÖRS I PGM W5030100                                                    
212500                 MOVE 'N'                TO INVA-INV-FLINVSKR             
212600                 MOVE WS-DAGENS-DATUM    TO INVA-INV-DAREGDAT             
212700                 PERFORM IMS-REPL-INVA-INV                                
212800              ELSE                                                        
212900                 PERFORM IMS-GET-INVA-INV                                 
213000                 IF SEGMENT-SAKNAS                                        
213100                    MOVE NEJ TO ALLT-OK-SW                                
213200                 END-IF                                                   
213300              END-IF                                                      
213400           END-PERFORM                                                    
213500        ELSE                                                              
213600           MOVE NEJ TO ALLT-OK-SW                                         
213700        END-IF                                                            
213800     ELSE                                                                 
213900        MOVE NEJ TO ALLT-OK-SW                                            
214000     END-IF                                                               
214100     .                                                                    
214200     EJECT                                                                
214300 HH-TAECKNING-CDC SECTION.                                                
214400     MOVE 'HH-TAECKNING-CDC'  TO CURR-SECTION                             
214500                                                                          
214600     IF WS-TECKEN =  '+'                                                  
214700        MOVE W-IDARTNR    TO 4506-IDARTNR                                 
214800        MOVE 11           TO 4506-KDTAKORS                                
214900        MOVE ZERO         TO 4506-KVANTMOT                                
215000        MOVE DCS-IDDC     TO W-IDDC-4505                                  
215100        PERFORM IMS-ISRT-450511                                           
215200     END-IF                                                               
215300     .                                                                    
215400     EJECT                                                                
215500 HI-RADERA-UTREDSALDO SECTION.                                            
215600     MOVE 'HI-RADERA-UTREDS'  TO CURR-SECTION                             
215700                                                                          
215800     MOVE W-IDARTNR TO W-IDARTNR-UTR                                      
215900     PERFORM IMS-GHU-ART-UTREDNSALDO                                      
216000                                                                          
216100     IF SEGMENT-FINNS                                                     
216200        PERFORM IMS-DELETE-ART-UTREDNSALDO                                
216300     END-IF                                                               
216400     .                                                                    
216500     EJECT                                                                
216600 HJ-UPPDATERA-WDK7 SECTION.                                               
216700     MOVE 'HJ-UPPDATERA-WDK'  TO CURR-SECTION                             
216800                                                                          
216900     IF W-IDDC NOT = WC-CDC-SE                                            
217000       PERFORM IMS-GHU-WDK711                                             
217100       IF SEGMENT-FINNS                                                   
217200          IF SLAG-FLREFNYO = JA                                           
217300             MOVE NEJ     TO SLAG-FLREFNYO                                
217400             PERFORM IMS-REPL-SLAGER-SEGM                                 
217500          END-IF                                                          
217600       END-IF                                                             
217700     END-IF                                                               
217800     .                                                                    
217900     EJECT                                                                
218000 HJA-VALIDATE-IDTRACK SECTION.                                            
218100     SKIP2                                                                
218200                                                                          
218300     IF WS-FLTRACK = 'J' AND WS-FLLOCAL = 'J'                             
218400        MOVE ZERO                TO WS-TEMP-KVAVIS                        
218500        MOVE 'N'                 TO IDTRACK-QTY-SW                        
218600        PERFORM IMS-GU-WDK711                                             
218700        MOVE 9999999999999999    TO W-DAINLEV                             
218800        PERFORM IMS-GHNP-WDK728-LAST                                      
218900        PERFORM UNTIL SEGMENT-SAKNAS OR IDTRACK-QTY-DONE                  
219000          IF TRCK-KVTRACK-KVAR < TRCK-KVANTMOT                            
219100             COMPUTE WS-TEMP-KVAVIS =                                     
219200              TRCK-KVANTMOT - TRCK-KVTRACK-KVAR                           
219300**** HAVE MORE THAN REQUIRED KVAVIS IN WDK728 SEGMENT                     
219400              IF WS-TEMP-KVAVIS >= WS-VALD-KVAVIS                         
219500                 MOVE 'J' TO IDTRACK-QTY-SW                               
219600                 ADD WS-VALD-KVAVIS TO TRCK-KVTRACK-KVAR                  
219700                 PERFORM IMS-REPL-WDK728                                  
219800                 MOVE WS-VALD-KVAVIS TO WS-TRCK-KVANTMOT                  
219900                 MOVE '+' TO WS-LOGT-SIGN                                 
220000                 MOVE TRCK-IDTRACK TO WS-TRCK-IDTRACK                     
220100                 PERFORM S04-SALDOLOGT-DATA                               
220200                 PERFORM S05-ISRT-SALDOLOGT                               
220300              END-IF                                                      
220400                                                                          
220500**** HAVE LESS THAN REQUIRED KVAVIS IN WDK728 SEGMENT                     
220600              IF WS-TEMP-KVAVIS < WS-VALD-KVAVIS                          
220700                 ADD WS-TEMP-KVAVIS TO TRCK-KVTRACK-KVAR                  
220800                 COMPUTE WS-VALD-KVAVIS =                                 
220900                     WS-VALD-KVAVIS - WS-TEMP-KVAVIS                      
221000                 PERFORM IMS-REPL-WDK728                                  
221100                 MOVE WS-TEMP-KVAVIS TO WS-TRCK-KVANTMOT                  
221200                 MOVE '+' TO WS-LOGT-SIGN                                 
221300                 MOVE TRCK-IDTRACK TO WS-TRCK-IDTRACK                     
221400                 PERFORM S04-SALDOLOGT-DATA                               
221500                 PERFORM S05-ISRT-SALDOLOGT                               
221600                 MOVE 'N' TO IDTRACK-QTY-SW                               
221700              END-IF                                                      
221800          END-IF                                                          
221900          IF IDTRACK-QTY-NOT-DONE                                         
222000             PERFORM IMS-GU-WDK711                                        
222100             MOVE  TRCK-DAINLEV TO W-DAINLEV                              
222200             PERFORM IMS-GHNP-WDK728-LAST                                 
222300          END-IF                                                          
222400        END-PERFORM                                                       
222500     END-IF                                                               
222600                                                                          
222700     .                                                                    
222800     EJECT                                                                
222900 HJB-VALIDATE-IDTRACK SECTION.                                            
223000     SKIP2                                                                
223100                                                                          
223200     IF WS-FLTRACK = 'J' AND WS-FLLOCAL = 'J'                             
223300                                                                          
223400        MOVE ZERO                TO WS-TEMP-KVAVIS                        
223500        MOVE 'N'                 TO IDTRACK-QTY-SW                        
223600        PERFORM IMS-GU-WDK711                                             
223700        PERFORM IMS-GHNP-WDK728                                           
223800        PERFORM UNTIL SEGMENT-SAKNAS OR IDTRACK-QTY-DONE                  
223900          IF TRCK-KVTRACK-KVAR > 0                                        
224000             COMPUTE WS-TEMP-KVAVIS =                                     
224100                     TRCK-KVTRACK-KVAR                                    
224200**** HAVE MORE THAN REQUIRED KVAVIS IN WDK728 SEGMENT                     
224300              IF WS-TEMP-KVAVIS >= WS-VALD-KVAVIS                         
224400                 MOVE 'J' TO IDTRACK-QTY-SW                               
224500                 COMPUTE TRCK-KVTRACK-KVAR =                              
224600                 TRCK-KVTRACK-KVAR - WS-VALD-KVAVIS                       
224700                 PERFORM IMS-REPL-WDK728                                  
224800                 MOVE WS-VALD-KVAVIS TO WS-TRCK-KVANTMOT                  
224900                 MOVE '-' TO WS-LOGT-SIGN                                 
225000                 MOVE TRCK-IDTRACK TO WS-TRCK-IDTRACK                     
225100                 PERFORM S04-SALDOLOGT-DATA                               
225200                 PERFORM S05-ISRT-SALDOLOGT                               
225300              END-IF                                                      
225400                                                                          
225500**** HAVE LESS THAN REQUIRED KVAVIS IN WDK728 SEGMENT                     
225600              IF WS-TEMP-KVAVIS < WS-VALD-KVAVIS                          
225700                 COMPUTE WS-VALD-KVAVIS =                                 
225800                     WS-VALD-KVAVIS - WS-TEMP-KVAVIS                      
225900                 MOVE ZERO TO TRCK-KVTRACK-KVAR                           
226000                 PERFORM IMS-REPL-WDK728                                  
226100                 MOVE WS-TEMP-KVAVIS TO WS-TRCK-KVANTMOT                  
226200                 MOVE '-' TO WS-LOGT-SIGN                                 
226300                 MOVE TRCK-IDTRACK TO WS-TRCK-IDTRACK                     
226400                 PERFORM S04-SALDOLOGT-DATA                               
226500                 PERFORM S05-ISRT-SALDOLOGT                               
226600                 MOVE 'N' TO IDTRACK-QTY-SW                               
226700              END-IF                                                      
226800                                                                          
226900          END-IF                                                          
227000          IF IDTRACK-QTY-NOT-DONE                                         
227100             PERFORM IMS-GHNP-WDK728                                      
227200          END-IF                                                          
227300        END-PERFORM                                                       
227400     END-IF                                                               
227500                                                                          
227600     .                                                                    
227700     EJECT                                                                
227800 HK-FYLL-WDR8-AREA  SECTION.                                              
227900     MOVE 'HK-FYLL-WDR8-ARE'   TO CURR-SECTION                            
228000                                                                          
228100     PERFORM IMS-GHU-WDK711                                               
228200     IF SEGMENT-FINNS                                                     
228300       MOVE SLAG-PRAVCOST TO WS-PRAVCOST                                  
228400     ELSE                                                                 
228500       MOVE ZERO          TO WS-PRAVCOST                                  
228600     END-IF                                                               
228700     MOVE 'WL017800'       TO WDR8-FIL-IDPGM                              
228800     MOVE WS-DAGENS-DATUM  TO WDR8-FIL-TIREGDAT                           
228900     ACCEPT WDR8-FIL-TIKLOCK    FROM TIME                                 
229000     MOVE 1                TO WDR8-FIL-IDSEKVNR                           
229100     MOVE W-IDARTNR        TO WDR8-EKH-IDARTNR                            
229200     MOVE '403'            TO WDR8-EKH-KDEKHHT                            
229300     MOVE '40'             TO WS-KDEKSHT-1                                
229400     MOVE WS-KDJUSTYP      TO WS-KDEKSHT-2                                
229500     MOVE WS-EKH-KDEKSHT   TO WDR8-EKH-KDEKSHT                            
229600     MOVE 'DET'            TO WDR8-EKH-KDEKNIVA                           
229700     MOVE DCS-IDDC         TO WDR8-EKH-IDDC-SEND                          
229800                              WDR8-EKH-IDDC-REC                           
229900     MOVE ZERO             TO WDR8-EKH-IDDISTR                            
230000                              WDR8-EKH-IDKUNDNR                           
230100     MOVE W-IDARTNR        TO W-EKH-IDARTNR                               
230200                                                                          
230300     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
230400     MOVE W-EKH-IDARTNR    TO CIA-IDARTBET-IN                             
230500     CALL W009CIA USING       CIA-W009CIA                                 
230600     MOVE CIA-IDARTBET-UT TO WDR8-EKH-IDVERGL                             
230700                                                                          
230800                                                                          
230900     MOVE WS-DAGENS-DATUM  TO WDR8-EKH-DAVERDAT                           
231000     MOVE ART-KDPRODSL     TO WDR8-EKH-KDPRODSL                           
231100     MOVE ZERO             TO WDR8-EKH-KDPSLLOC                           
231200     MOVE SPACE            TO WDR8-EKH-FLLSBOK                            
231300     MOVE 1.00             TO WDR8-EKH-PRKURS                             
231400     MOVE ZERO             TO WDR8-EKH-PRARTNTO                           
231500     MOVE ZERO             TO WDR8-EKH-PRARTSJK                           
231600     MOVE ZERO             TO WDR8-EKH-PRHEMTAG                           
231700     MOVE WS-PRAVCOST      TO WDR8-EKH-PRARTSTD                           
231800     MOVE ZERO             TO WDR8-EKH-PRLANDCO                           
231900                              WDR8-EKH-PRINK                              
232000                              WDR8-EKH-PRDIRLON                           
232100                              WDR8-EKH-PRDMTRL                            
232200                              WDR8-EKH-PROVRPAL                           
232300                              WDR8-EKH-SUBEL                              
232400     MOVE WS-ANTAL         TO WDR8-EKH-KVANTAL                            
232500     MOVE 'L178'           TO WDR8-EKH-IDTRANS                            
232600     MOVE ZERO             TO WDR8-EKH-BEVAT                              
232700                              WDR8-EKH-IDANALYS                           
232800                              WDR8-EKH-IDKONTO                            
232900                              WDR8-EKH-KDANMORS                           
233000                              WDR8-EKH-KDFRAKT                            
233100                              WDR8-EKH-SUVAT                              
233200     MOVE ZERO             TO WDR8-EKH-DAAVIDAT                           
233300                              WDR8-EKH-IDAVINR                            
233400                              WDR8-EKH-KDAVVTYP                           
233500                              WDR8-EKH-KDRT                               
233600                              WDR8-EKH-KVANTMOT                           
233700                              WDR8-EKH-KVAVIS                             
233800     MOVE WS-KDSORT        TO WDR8-EKH-KDSORT                             
233900     MOVE NEJ              TO WDR8-EKH-FLDCET                             
234000     MOVE SPACE            TO WDR8-EKH-IDLEVNR                            
234100                              WDR8-EKH-IDKST                              
234200     MOVE SPACE            TO WDR8-EKH-IDKUNDRF                           
234300     MOVE SPACE            TO WDR8-EKH-IDFAKT-EXP                         
234400     MOVE DCS-KDVALISO     TO WDR8-EKH-KDVALISO                           
234500     MOVE DCS-KDTRADP      TO WDR8-EKH-KDTRADP                            
234600     IF NDC-CN                                                            
234700       MOVE 'W570'         TO WDR8-FIL-IDCPYTXT(1:4)                      
234800     ELSE                                                                 
234900       IF NDC-IN                                                          
235000         MOVE 'W515'       TO WDR8-FIL-IDCPYTXT(1:4)                      
235100       ELSE                                                               
235200         MOVE DCS-KDTRADP  TO WDR8-FIL-IDCPYTXT(1:4)                      
235300       END-IF                                                             
235400     END-IF                                                               
235500     MOVE 'EKHA'           TO WDR8-FIL-IDCPYTXT(5:4)                      
235600     .                                                                    
235700                                                                          
235800 HKA-UPPDATERA-WDR8 SECTION.                                              
235900     MOVE 'HKA-UPPDATERA-WD'    TO CURR-SECTION                           
236000                                                                          
236100     PERFORM IMS-ISRT-WDR801                                              
236200     PERFORM UNTIL SEGMENT-FINNS                                          
236300       ADD +1 TO WDR8-FIL-IDSEKVNR                                        
236400       PERFORM IMS-ISRT-WDR801                                            
236500     END-PERFORM                                                          
236600     IF DCS-KDTRADP = 'BR12'                                              
236700      IF NOT-FIRST-REC-TRANS                                              
236800       PERFORM S06-FIX-LOCAL-TIME                                         
236900       PERFORM S20-SEND-OPEN                                              
237000       MOVE SEND-IDCOM         TO WZ04-SEND-IDCOM                         
237100       PERFORM S21-SEND-PUT-PROP                                          
237200       MOVE ZERO TO NOTF-IDSEKVNR                                         
237300       MOVE JA  TO FIRST-REC-TRANS-SW                                     
237400      END-IF                                                              
237500      MOVE WDR8-EKH-KDEKHHT   TO NOTF-KDEKHHT                             
237600      MOVE WDR8-EKH-KDEKSHT   TO NOTF-KDEKSHT                             
237700      MOVE WDR8-EKH-DAVERDAT  TO NOTF-DAVERDAT                            
237800      MOVE AKTUELL-TID-X(1:6) TO NOTF-TIREGTID                            
237900      MOVE WDR8-EKH-IDVERGL   TO NOTF-IDVERGL                             
238000      MOVE WDR8-EKH-IDDC-REC  TO NOTF-IDDC                                
238100      MOVE ZERO               TO NOTF-IDFAKT                              
238200                                 NOTF-IDKUNDNR                            
238300                                 NOTF-IDORDER                             
238400                                 NOTF-IDKOLLI                             
238500      MOVE WDR8-EKH-IDARTNR   TO W-IDARTNR-EDIT-X                         
238600      MOVE FUNCTION TRIM(W-IDARTNR-EDIT-X LEADING)                        
238700                              TO NOTF-IDARTNR20                           
238800      MOVE WDR8-EKH-KVANTAL   TO NOTF-KVANTAL                             
238900      ADD +1 TO NOTF-IDSEKVNR                                             
239000      PERFORM S22-SEND-PUT                                                
239001     END-IF                                                               
239002     .                                                                    
239003 HL-FYLL-WDR8-AREA SECTION.                                               
239004     MOVE 'HL-FYLL-WDR8-ARE' TO CURR-SECTION                              
239005                                                                          
239006     PERFORM IMS-GU-WDK711                                                
239007     IF SEGMENT-FINNS                                                     
239008       MOVE SLAG-PRAVCOST TO WS-PRAVCOST                                  
239009     ELSE                                                                 
239010       MOVE ZERO          TO WS-PRAVCOST                                  
239011     END-IF                                                               
239012                                                                          
239013     MOVE 'WL017800'        TO WDR8-FIL-IDPGM                             
239014     MOVE WS-DAGENS-DATUM   TO WDR8-FIL-TIREGDAT                          
239015     ACCEPT WDR8-FIL-TIKLOCK   FROM TIME                                  
239016     MOVE 1                 TO WDR8-FIL-IDSEKVNR                          
239017     MOVE 'W510A08 '        TO WDR8-FIL-IDCPYTXT                          
239018     MOVE 'A08'             TO A08-IDPTYP                                 
239019     MOVE 'M10'             TO A08-KDEKOHT                                
239020     IF NDC-NA                                                            
239021        IF NDC-US                                                         
239022          MOVE 53             TO A08-IDFTG                                
239023        ELSE                                                              
239024          MOVE 54             TO A08-IDFTG                                
239025        END-IF                                                            
239026     ELSE                                                                 
239100       CONTINUE                                                           
239200     END-IF                                                               
239300     MOVE DCS-IDDC          TO A08-IDDC-SEND                              
239400                               A08-IDDC-REC                               
239500     MOVE WS-DAGENS-DATUM   TO A08-DAJUSTDA                               
239600     MOVE W-IDARTNR         TO A08-IDARTNR                                
239700     MOVE ART-KDPRODSL      TO A08-KDPRODSL                               
239800     MOVE CLAG-KDPSLLOC     TO A08-KDPSLLOC                               
239900     MOVE WS-ANTAL          TO A08-KVJUSTKV                               
240000     MOVE WS-PRAVCOST       TO A08-PRAVCOST                               
240100     MOVE INVA-INV-KDINVKAT TO A08-KDINVKAT                               
240200     MOVE INVA-INV-TEINVANM TO A08-TEINVANM                               
240300     .                                                                    
240400                                                                          
240500 HLA-UPPDATERA-WDR8 SECTION.                                              
240600     MOVE 'HLA-UPPDATERA-WDR8' TO CURR-SECTION                            
240700                                                                          
240800     PERFORM IMS-ISRT-WDR801                                              
240900     PERFORM UNTIL SEGMENT-FINNS                                          
241000       ADD +1  TO WDR8-FIL-IDSEKVNR                                       
241100       PERFORM IMS-ISRT-WDR801                                            
241200     END-PERFORM                                                          
241300     .                                                                    
241400                                                                          
241500 I-LAES-VISA-FELTABELL SECTION.                                           
241600     MOVE 'I-LAES-VISA-FELT'  TO CURR-SECTION                             
241700                                                                          
241800     MOVE REQU-IDLISTNR-KEY     TO RESP-IDLISTNR-KEY                      
241900     MOVE REQU-IDUSER-KEY       TO RESP-IDUSER-KEY                        
242000     MOVE +1   TO INDX                                                    
242100     MOVE ZERO TO WS-KVRADER                                              
242200     PERFORM UNTIL INDX > MAX-INDX                                        
242300        IF FEL-ARTIKEL (INDX) NUMERIC AND                                 
242400           FEL-ARTIKEL (INDX) > ZERO                                      
242500           MOVE FEL-ARTIKEL  (INDX)    TO RESP-IDARTNR  (INDX)            
242600           MOVE FEL-BENAMNING(INDX)    TO RESP-BEART-SVE(INDX)            
242700           MOVE FEL-KVAKS    (INDX)    TO RESP-KVAKS    (INDX)            
242800           MOVE FEL-KVLS     (INDX)    TO RESP-KVLS     (INDX)            
242900           MOVE FEL-KVEFRS   (INDX)    TO RESP-KVEFRS   (INDX)            
243000           IF FEL-ANTAL (INDX) NUMERIC                                    
243100              MOVE FEL-ANTAL  (INDX)   TO RESP-ANTAL-IN (INDX)            
243200           END-IF                                                         
243300                                                                          
243400           MOVE FEL-TECKEN   (INDX)    TO RESP-TECKEN-IN(INDX)            
243500                                                                          
243600           IF FEL-DIFFERANS  (INDX) NUMERIC                               
243700              MOVE FEL-DIFFERANS(INDX) TO RESP-DIFFERANS-IN(INDX)         
243800           END-IF                                                         
243801           IF FEL-FLOMINV    (INDX) NUMERIC                               
243802              MOVE FEL-FLOMINV(INDX) TO RESP-FLOMINV-IN(INDX)             
243803           END-IF                                                         
243900           MOVE FEL-MESSAGE (INDX) TO RESP-IDMSG-ERROR-LINE (INDX)        
244000           ADD +1 TO WS-KVRADER                                           
244100        END-IF                                                            
244200                                                                          
244300        ADD +1 TO INDX                                                    
244400     END-PERFORM                                                          
244500     MOVE WS-KVRADER TO RESP-KVRADER                                      
244600     .                                                                    
244700     EJECT                                                                
244800*    --- DISPATCHER SECTIONS                                              
244900 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
245000     MOVE 'S01-FETCH-REQUES'  TO CURR-SECTION                             
245100                                                                          
245200     MOVE 'GETARG'                    TO SUB-KDFUNC                       
245300     MOVE 'CARPARTS.LDC.AUTOMATICADJ' TO SUB-ADDISPABS                    
245400     MOVE LENGTH OF REQU-AREA         TO SUB-KVDLEN                       
245500                                                                          
245600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
245700                                                                          
245800     IF SUB-KDRC > 0                                                      
245900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
246000       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
246100       DELIMITED BY SIZE INTO FELTEXT                                     
246200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
246300     END-IF                                                               
246400     .                                                                    
246500     SKIP3                                                                
246600 S02-RETURN-RESPONSE SECTION.                                             
246700     MOVE 'S02-RETURN-RESPO'  TO CURR-SECTION                             
246800                                                                          
246900     MOVE 'RETURN'                   TO SUB-KDFUNC                        
247000     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
247100                                                                          
247200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
247300                                                                          
247400     IF SUB-KDRC > 0                                                      
247500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
247600       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
247700       DELIMITED BY SIZE INTO FELTEXT                                     
247800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
247900     END-IF                                                               
248000     .                                                                    
248100 S03-DECEDIT SECTION.                                                     
248200        MOVE REQU-KVLS(INDX)     TO DEC-IDFRIDATA                         
248300        MOVE 7                   TO DEC-KVHELTAL                          
248400        MOVE 0                   TO DEC-KVDECIMAL                         
248500                                                                          
248600        CALL WDECEDIT USING DEC-WDECAREA                                  
248700                                                                          
248800        IF DEC-KDSVAR-OK                                                  
248900          MOVE DEC-IDEDITDATA    TO WS-TAB-KVLS                           
249000        ELSE                                                              
249100          CALL ABEND                                                      
249200        END-IF                                                            
249300                                                                          
249400                                                                          
249500     .                                                                    
249600 S04-SALDOLOGT-DATA SECTION.                                              
249700     INITIALIZE LOGT-WDL301                                               
249800                                                                          
249900     COMPUTE LOGT-DAREGDAT-9KOMPL = 99999999 - WS-DAGENS-DATUM            
250000     ACCEPT TRANS-TID FROM TIME                                           
250100     COMPUTE LOGT-TIKLOCK-9KOMPL  = 999999999 - TRANS-TID                 
250200     MOVE W-IDARTNR          TO LOGT-IDARTNR                              
250300     MOVE 9                  TO LOGT-IDSEKVNR                             
250400     MOVE 'MISC'             TO LOGT-IDHUVTYP                             
250500     MOVE 'ADJ'              TO LOGT-IDSUBTYP                             
250600     MOVE 'WL017800'         TO LOGT-IDPGM                                
250700     MOVE 'L178'             TO LOGT-IDTRANS                              
250800     MOVE MSG-SIGNON-USERID  TO LOGT-IDUSER                               
250900     MOVE +0                 TO LOGT-IDKUNDNR                             
251000     MOVE '00000000'         TO LOGT-DAREGDAT-LADD                        
251100     MOVE W-IDDC             TO LOGT-IDDC                                 
251200     MOVE WS-TRCK-KVANTMOT   TO LOGT-KVART-SALDO                          
251300     IF WS-LOGT-SIGN = '+'                                                
251400        COMPUTE WS-SLAG-KVLS = WS-SLAG-KVLS +                             
251500                               WS-TRCK-KVANTMOT                           
251600     ELSE                                                                 
251700        COMPUTE WS-SLAG-KVLS = WS-SLAG-KVLS -                             
251800                               WS-TRCK-KVANTMOT                           
251900     END-IF                                                               
252000     MOVE WS-SLAG-KVLS       TO LOGT-KVLS                                 
252100     MOVE WS-LOGT-SIGN       TO LOGT-IDTECKEN-KVLS                        
252200                                LOGT-IDTECKEN-KVTRACK-KVAR                
252300     MOVE TRCK-KVTRACK-KVAR  TO LOGT-KVTRACK-KVAR                         
252400     MOVE WS-TRCK-IDTRACK    TO LOGT-IDTRACK                              
252500     .                                                                    
252600     EJECT                                                                
252700                                                                          
252800 S05-ISRT-SALDOLOGT SECTION.                                              
252900     PERFORM IMS-ISRT-WDL301                                              
253000     IF SEGMENT-FINNS-REDAN                                               
253100       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
253200         SUBTRACT 1 FROM LOGT-IDSEKVNR                                    
253300         PERFORM IMS-ISRT-WDL301                                          
253400       END-PERFORM                                                        
253500     END-IF                                                               
253600     .                                                                    
253700     EJECT                                                                
253800 S06-FIX-LOCAL-TIME SECTION.                                              
253900                                                                          
254000******* ADAPT DATE AND TIME FOR TIMEZONES                                 
254100     PERFORM IMS-GU-WDB601                                                
254200                                                                          
254300     MOVE '011'                TO MSGI-KDCALL                             
254400     MOVE DCS-IDTIDZON         TO MSGI-IDTIDZON                           
254500     MOVE DCS-IDDC             TO MSGI-IDDC                               
254600     MOVE DAGENS-AAMMDD        TO MSGI-TILOKDAT                           
254700     MOVE WS-DAGENS-TID        TO MSGI-TILOKTID                           
254800     CALL WL01TIDZ USING          MSGI-WL01TIDZ                           
254900       MOVE MSGI-TILOKDAT(1:6) TO W-DATUM-Y                               
255000       MOVE MSGI-TILOKTID(1:4) TO AKTUELL-TID-X(1:4)                      
255100     .                                                                    
255101     EJECT                                                                
255102                                                                          
255103 S11-MSG-CONV SECTION.                                                    
255104     MOVE SPACES                  TO RESP-MESSAGES (1)                    
255105                                     RESP-MESSAGES (2)                    
255106     MOVE 1                       TO MSG-IX                               
255107*    REQUEST OK                                                           
255108     MOVE 200                     TO RESP-KDSTATUS-API                    
255109                                                                          
255110     IF RESP-IDMSG-INFO = '001' AND                                       
255111        RESP-IDMSG-ERROR = '290'                                          
255112       MOVE RESP-IDMSG-ERROR      TO RESP-IDMSG-INFO                      
255113       MOVE SPACES                TO RESP-IDMSG-ERROR                     
255114     END-IF                                                               
255115                                                                          
255200     IF RESP-IDMSG-INFO > SPACE                                           
255300       MOVE SPACES                TO MSG-CONV-AREA                        
255400       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
255500       CALL WMSGCONV           USING MSG-CONV-AREA                        
255600       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
255700       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
255800       ADD 1                      TO MSG-IX                               
255900     END-IF                                                               
256000                                                                          
256100     IF RESP-IDMSG-ERROR > SPACE                                          
256200*      BAD REQUEST                                                        
256300       MOVE 400                   TO RESP-KDSTATUS-API                    
256400       MOVE SPACES                TO MSG-CONV-AREA                        
256500       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
256600       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
256700       CALL WMSGCONV           USING MSG-CONV-AREA                        
256800       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
256900       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
257000     END-IF                                                               
257100     .                                                                    
257200 S20-SEND-OPEN SECTION.                                                   
257300     MOVE 'OPEN'                        TO SEND-KDFUNC                    
257400     MOVE WS-ADDRESS-MQASYNC            TO SEND-ADDISPABS                 
257500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
257600                         SEND-OPEN-AREA                                   
257700     IF SEND-KDRC > 0                                                     
257800       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
257900       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
258000       DELIMITED BY SIZE INTO FELTEXT                                     
258100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
258200     END-IF                                                               
258300     .                                                                    
258400     EJECT                                                                
258500 S21-SEND-PUT-PROP SECTION.                                               
258600                                                                          
258700     SET PROP-IX                 TO +1                                    
258800*    MANDATORY PROPERTY THAT SPECIFIES THE ACTUAL DESTINATION             
258900     MOVE 'ADDRESS'              TO PROP-IDPROPTYPE  (PROP-IX)            
259000     MOVE 'ADDISPABS'            TO PROP-IDPROPNAME  (PROP-IX)            
259100     MOVE WS-ADDRESS-WHSTOCKA    TO PROP-BEPROPVALUE (PROP-IX)            
259200                                                                          
259300     SET PROP-IX              UP BY +1                                    
259400*    OPTIONAL MQ MESSAGE PROPERTIES. CAN BE CASE-SENSITIVE                
259500     MOVE 'MQMPROP'              TO PROP-IDPROPTYPE  (PROP-IX)            
259600     MOVE 'CountryCode'          TO PROP-IDPROPNAME  (PROP-IX)            
259700     MOVE 'BR'                   TO PROP-BEPROPVALUE (PROP-IX)            
259800                                                                          
259900*    SET THE NUMBER OF PROPERTIES (KVANTAL) SO CORRECT LENGTH             
260000*    IS CALCULATED.                                                       
260100     SET PROP-KVANTAL            TO PROP-IX                               
260200                                                                          
260300     MOVE 'PUT'                            TO SEND-KDFUNC                 
260400     MOVE LENGTH OF PROP-WZ04PROP          TO SEND-KVDLEN                 
260500     MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
260600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
260700                         SEND-KVDLEN                                      
260800                         PROP-WZ04PROP                                    
260900     IF SEND-KDRC > 1                                                     
261000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
261100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
261200       DELIMITED BY SIZE INTO FELTEXT                                     
261300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
261400     END-IF                                                               
261500     .                                                                    
261600     EJECT                                                                
261700 S22-SEND-PUT SECTION.                                                    
261800                                                                          
261900     MOVE 'PUT'                            TO SEND-KDFUNC                 
262000     MOVE LENGTH OF NOTF-AREA              TO SEND-KVDLEN                 
262100     MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
262200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
262300                         SEND-KVDLEN                                      
262400                         NOTF-AREA                                        
262500     IF SEND-KDRC > 1                                                     
262600       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
262700       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
262800       DELIMITED BY SIZE INTO FELTEXT                                     
262900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
263000     END-IF                                                               
263100     .                                                                    
263200     EJECT                                                                
263300 S23-SEND-CLOSE SECTION.                                                  
263400     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
263500     MOVE WZ04-SEND-IDCOM            TO SEND-IDCOM                        
263600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
263700                                                                          
263800     IF SEND-KDRC > 0                                                     
263900       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
264000       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
264100       DELIMITED BY SIZE INTO FELTEXT                                     
264200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
264300     END-IF                                                               
264400     .                                                                    
264401     EJECT                                                                
264402                                                                          
264403 IMS-GET-ARTC-ART SECTION.                                                
264404     MOVE 'IMS-GET-ARTC-ART'  TO CURR-IMS-SECTION                         
264405                                                                          
264406     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
264407          DELIMITED BY SIZE INTO SSA1                                     
264408     MOVE '    ' TO GODK-STATUSKODER                                      
264409     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
264410     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
264411     PERFORM IMS-STATUSKONTROLL                                           
264412     .                                                                    
264413     EJECT                                                                
264414 IMS-GET-ARTC-CLAG SECTION.                                               
264415     MOVE 'IMS-GET-ARTC-CLA'  TO CURR-IMS-SECTION                         
264416                                                                          
264417     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
264418          DELIMITED BY SIZE INTO SSA1                                     
264419     MOVE '    ' TO GODK-STATUSKODER                                      
264420     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                  
264421     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
264422     PERFORM IMS-STATUSKONTROLL                                           
264423     .                                                                    
264424     SKIP3                                                                
264425 IMS-REPL-ARTC-CLAG SECTION.                                              
264426     MOVE 'IMS-REPL-ARTC-CL'  TO CURR-IMS-SECTION                         
264427                                                                          
264428     MOVE '  ' TO GODK-STATUSKODER                                        
264429     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
264430     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
264431     PERFORM IMS-STATUSKONTROLL                                           
264432     .                                                                    
264433     EJECT                                                                
264434                                                                          
264435 IMS-GHU-WDK629  SECTION.                                                 
264436     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
264437          DELIMITED BY SIZE INTO SSA1                                     
264438     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
264439          DELIMITED BY SIZE INTO SSA2                                     
264440     MOVE   'WDK629  '        TO SSA3                                     
264441     MOVE '  GE' TO GODK-STATUSKODER                                      
264442     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK629 SSA1 SSA2 SSA3         
264443     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
264444     PERFORM IMS-STATUSKONTROLL                                           
264445     .                                                                    
264446     SKIP3                                                                
264447 IMS-REPL-WDK629 SECTION.                                                 
264448     MOVE '  ' TO GODK-STATUSKODER                                        
264449     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK629                       
264450     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
264451     PERFORM IMS-STATUSKONTROLL                                           
264452     .                                                                    
264453     EJECT                                                                
264454 IMS-GET-INVA-ART SECTION.                                                
264455     MOVE 'IMS-GET-INVA-ART'  TO CURR-IMS-SECTION                         
264456                                                                          
264457     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
264458          DELIMITED BY SIZE INTO SSA1                                     
264459     MOVE '  GE' TO GODK-STATUSKODER                                      
264460     CALL CBLTDLI USING GHU INVA-PCB DLI-IO-WDH101   SSA1                 
264461     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
264462     PERFORM IMS-STATUSKONTROLL                                           
264463     .                                                                    
264464     EJECT                                                                
264465 IMS-REPL-INVA-INV SECTION.                                               
264466     MOVE 'IMS-REPL-INVA-IN'  TO CURR-IMS-SECTION                         
264467                                                                          
264468     MOVE '  ' TO GODK-STATUSKODER                                        
264469     CALL CBLTDLI USING REPL INVA-PCB DLI-IO-WDH111                       
264470     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
264471     PERFORM IMS-STATUSKONTROLL                                           
264472     .                                                                    
264473     EJECT                                                                
264474 IMS-ISRT-INVA-ART    SECTION.                                            
264500     MOVE 'IMS-ISRT-INVA-AR'  TO CURR-IMS-SECTION                         
264600                                                                          
264700     MOVE 'WDH101   ' TO SSA1                                             
264800     MOVE '  ' TO GODK-STATUSKODER                                        
264900     CALL  CBLTDLI  USING ISRT INVA-PCB DLI-IO-WDH101   SSA1              
265000     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
265100     PERFORM IMS-STATUSKONTROLL                                           
265200     .                                                                    
265300     EJECT                                                                
265400 IMS-ISRT-INVA-INV    SECTION.                                            
265500     MOVE 'IMS-ISRT-INVA-IN'  TO CURR-IMS-SECTION                         
265600                                                                          
265700     MOVE 'WDH111   ' TO SSA1                                             
265800     MOVE '  IINI' TO GODK-STATUSKODER                                    
265900     CALL  CBLTDLI  USING ISRT INVA-PCB DLI-IO-WDH111   SSA1              
266000     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
266100     PERFORM IMS-STATUSKONTROLL                                           
266200     .                                                                    
266300     EJECT                                                                
266400 IMS-GET-INVA-INV SECTION.                                                
266500     MOVE 'IMS-GET-INVA-INV'  TO CURR-IMS-SECTION                         
266600                                                                          
266700     STRING 'WDH111  (WDH111KY=>' W-WDH111KY-MIN-X                        
266800                    '&WDH111KY=<' W-WDH111KY-MAX-X                        
266900                    '&FLINVBEH =' NEJ ')'                                 
267000            DELIMITED BY SIZE INTO SSA1                                   
267100     MOVE '  GE' TO GODK-STATUSKODER                                      
267200     CALL CBLTDLI USING GHNP INVA-PCB DLI-IO-WDH111   SSA1                
267300     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
267400     PERFORM IMS-STATUSKONTROLL                                           
267500     .                                                                    
267600     SKIP3                                                                
267700 IMS-GNP-WDH121   SECTION.                                                
267800     MOVE 'GNP-WDH121       ' TO CURR-IMS-SECTION                         
267900     MOVE 'WDH121  ' TO SSA1                                              
268000     MOVE '  GE' TO GODK-STATUSKODER                                      
268100     CALL CBLTDLI USING GNP INVA-PCB DLI-IO-WDH121   SSA1                 
268200     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
268300     PERFORM IMS-STATUSKONTROLL                                           
268400     .                                                                    
268500     SKIP3                                                                
268600 IMS-INSERT-WDH121    SECTION.                                            
268700     MOVE 'ISRT-WDH121         ' TO CURR-IMS-SECTION                      
268800     MOVE 'WDH121  ' TO SSA1                                              
268900     MOVE '  II' TO GODK-STATUSKODER                                      
269000     CALL  CBLTDLI  USING ISRT INVA-PCB DLI-IO-WDH121 SSA1                
269100     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
269200     PERFORM IMS-STATUSKONTROLL                                           
269300     .                                                                    
269400     EJECT                                                                
269500 IMS-GHU-INVHIST-ROT SECTION.                                             
269600     MOVE 'IMS-GHU-INVHIST-'  TO CURR-IMS-SECTION                         
269700                                                                          
269800     STRING 'WLINVC01(IDARTNR  =' W-IDARTNR-X ')'                         
269900            DELIMITED BY SIZE INTO SSA1                                   
270000     MOVE '  GE' TO GODK-STATUSKODER                                      
270100     CALL  CBLTDLI  USING GHU INVC-PCB DLI-IO-WLINVC01 SSA1               
270200     MOVE INVC-STATUS-CODE TO STATUS-WS                                   
270300     PERFORM IMS-STATUSKONTROLL                                           
270400     .                                                                    
270500     SKIP2                                                                
270600 IMS-ISRT-INVHIST-ROT SECTION.                                            
270700     MOVE 'IMS-ISRT-INVHROT'  TO CURR-IMS-SECTION                         
270800                                                                          
270900     MOVE 'WLINVC01 ' TO SSA1                                             
271000     MOVE '  ' TO GODK-STATUSKODER                                        
271100     CALL  CBLTDLI  USING ISRT INVC-PCB DLI-IO-WLINVC01 SSA1              
271200     MOVE INVC-STATUS-CODE TO STATUS-WS                                   
271300     PERFORM IMS-STATUSKONTROLL                                           
271400     .                                                                    
271500     EJECT                                                                
271600 IMS-ISRT-INVHIST-SEGM SECTION.                                           
271700     MOVE 'IMS-ISRT-INVSEGM'  TO CURR-IMS-SECTION                         
271800                                                                          
271900     MOVE 'WLINVC11 ' TO SSA1                                             
272000     MOVE '  II' TO GODK-STATUSKODER                                      
272100     CALL  CBLTDLI  USING ISRT INVC-PCB DLI-IO-WLINVC11 SSA1              
272200     MOVE INVC-STATUS-CODE TO STATUS-WS                                   
272300     PERFORM IMS-STATUSKONTROLL                                           
272400     .                                                                    
272500     EJECT                                                                
272600 IMS-GET-BENA SECTION.                                                    
272700     MOVE 'IMS-GET-BENA    '  TO CURR-IMS-SECTION                         
272800                                                                          
272900     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-WDD3-X ')'                    
273000          DELIMITED BY SIZE INTO SSA1                                     
273100     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
273200          DELIMITED BY SIZE INTO SSA2                                     
273300     MOVE '  GE' TO GODK-STATUSKODER                                      
273400     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA01 SSA1 SSA2             
273500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
273600     PERFORM IMS-STATUSKONTROLL                                           
273700     .                                                                    
273800     EJECT                                                                
273900 IMS-GU-WDH111 SECTION.                                                   
274000     MOVE 'IMS-GU-WDH111'        TO CURR-IMS-SECTION                      
274100                                                                          
274200     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
274300             DELIMITED BY SIZE INTO SSA1                                  
274400     STRING 'WDH111  (WDH111KY =' W-WDH111KY-X ')'                        
274500             DELIMITED BY SIZE INTO SSA2                                  
274600     MOVE '  GE'                 TO GODK-STATUSKODER                      
274700     CALL CBLTDLI             USING GU                                    
274800                                    INVA-PCB                              
274900                                    DLI-IO-WDH111                         
275000                                    SSA1                                  
275100                                    SSA2                                  
275200     MOVE INVA-STATUS-CODE       TO STATUS-WS                             
275300     PERFORM IMS-STATUSKONTROLL                                           
275400     .                                                                    
275500                                                                          
275600 IMS-GU-WDH111-BSEQ   SECTION.                                            
275700     MOVE 'IMS-GU-WDH111-BS'  TO CURR-IMS-SECTION                         
275800                                                                          
275900     STRING 'WDH111  (WDH1BSEQ=>' W-WDH1BSEQ-MIN-X                        
276000                    '&WDH1BSEQ=<' W-WDH1BSEQ-MAX-X                        
276100                    '&IDPRTINV =' W-IDPRTINV-X ')'                        
276200            DELIMITED BY SIZE INTO SSA1                                   
276300     MOVE '  GE' TO GODK-STATUSKODER                                      
276400     CALL CBLTDLI USING GU WDH1B-PCB DLI-IO-WDH1B1 SSA1                   
276500     MOVE WDH1B-STATUS-CODE TO STATUS-WS                                  
276600     PERFORM IMS-STATUSKONTROLL                                           
276700     .                                                                    
276800                                                                          
276900 IMS-GN-WDH111-BSEQ   SECTION.                                            
277000     MOVE 'IMS-GN-WDH111-BS'  TO CURR-IMS-SECTION                         
277100                                                                          
277200     STRING 'WDH111  (WDH1BSEQ=>' W-WDH1BSEQ-MIN-X                        
277300                    '&WDH1BSEQ=<' W-WDH1BSEQ-MAX-X                        
277400                    '&IDPRTINV =' W-IDPRTINV-X ')'                        
277500            DELIMITED BY SIZE INTO SSA1                                   
277600                                                                          
277700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
277800     CALL CBLTDLI USING GN WDH1B-PCB DLI-IO-WDH1B1 SSA1                   
277900     MOVE WDH1B-STATUS-CODE TO STATUS-WS                                  
278000     PERFORM IMS-STATUSKONTROLL                                           
278100     .                                                                    
278200                                                                          
278300 IMS-GNP-WDH101-BSEQ   SECTION.                                           
278400     MOVE 'IMS-GNP-WDH101-B'  TO CURR-IMS-SECTION                         
278500                                                                          
278600     MOVE   'WDH101' TO SSA1                                              
278700     MOVE '  ' TO GODK-STATUSKODER                                        
278800     CALL CBLTDLI USING GNP WDH1B-PCB DLI-IO-WDH101   SSA1                
278900     MOVE WDH1B-STATUS-CODE TO STATUS-WS                                  
279000     PERFORM IMS-STATUSKONTROLL                                           
279100     .                                                                    
279200                                                                          
279300********** WDR9 PEDAL*********************************************        
279400 IMS-ISRT-WDR901 SECTION.                                                 
279500     MOVE 'IMS-ISRT-WDR901 '  TO CURR-IMS-SECTION                         
279600                                                                          
279700     MOVE 'WLSAPA01 ' TO SSA1                                             
279800     MOVE '  II' TO GODK-STATUSKODER                                      
279900     CALL CBLTDLI USING ISRT SAPA-PCB WLSAPA01 SSA1                       
280000     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
280100     PERFORM IMS-STATUSKONTROLL                                           
280200     .                                                                    
280300     EJECT                                                                
280400********** WDL9 SALDOLOGG*****************************************        
280500 IMS-ISRT-WDL901 SECTION.                                                 
280600     MOVE 'IMS-ISRT-WDL901 '  TO CURR-IMS-SECTION                         
280700                                                                          
280800     MOVE 'WLLOGA01 ' TO SSA1                                             
280900     MOVE '  II' TO GODK-STATUSKODER                                      
281000     CALL CBLTDLI USING ISRT LOGA-PCB WLLOGA01 SSA1                       
281100     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
281200     PERFORM IMS-STATUSKONTROLL                                           
281300     .                                                                    
281400     EJECT                                                                
281500                                                                          
281600 IMS-ISRT-WDR801 SECTION.                                                 
281700     MOVE 'IMS-ISRT-WDR801 '  TO CURR-IMS-SECTION                         
281800                                                                          
281900     MOVE 'WDR801   ' TO SSA1                                             
282000     MOVE '  II' TO GODK-STATUSKODER                                      
282100     CALL CBLTDLI USING ISRT WDR8-PCB WDR8-WDR801 SSA1                    
282200     MOVE WDR8-STATUS-CODE TO STATUS-WS                                   
282300     PERFORM IMS-STATUSKONTROLL                                           
282400     .                                                                    
282500************ WDK7 ARTIKELREGISTER SDC  ***************************        
282600                                                                          
282700 IMS-GU-WDK711 SECTION.                                                   
282800     MOVE 'IMS-GU-WDK711   '  TO CURR-IMS-SECTION                         
282900                                                                          
283000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
283100            DELIMITED BY SIZE INTO SSA1                                   
283200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
283300            DELIMITED BY SIZE INTO SSA2                                   
283400     MOVE '    ' TO GODK-STATUSKODER                                      
283500     CALL  CBLTDLI  USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2             
283600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
283700     PERFORM IMS-STATUSKONTROLL                                           
283800     .                                                                    
283900     SKIP2                                                                
284000 IMS-GHU-WDK711 SECTION.                                                  
284100     MOVE 'IMS-GHU-WDK711  '  TO CURR-IMS-SECTION                         
284200                                                                          
284300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
284400            DELIMITED BY SIZE INTO SSA1                                   
284500     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
284600            DELIMITED BY SIZE INTO SSA2                                   
284700     MOVE '  GE' TO GODK-STATUSKODER                                      
284800     CALL  CBLTDLI  USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2            
284900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
285000     PERFORM IMS-STATUSKONTROLL                                           
285100     .                                                                    
285200     SKIP2                                                                
285300                                                                          
285400 IMS-GHNP-WDK728 SECTION.                                                 
285500                                                                          
285600     MOVE 'WDK728 ' TO SSA1                                               
285700     MOVE '  GE' TO GODK-STATUSKODER                                      
285800     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK728 SSA1                  
285900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
286000     PERFORM IMS-STATUSKONTROLL                                           
286100     .                                                                    
286200     EJECT                                                                
286300 IMS-GHNP-WDK728-LAST SECTION.                                            
286400                                                                          
286500     STRING 'WDK728  *L(DAINLEV < ' W-DAINLEV ')'                         
286600          DELIMITED BY SIZE INTO SSA1                                     
286700     MOVE '  GE' TO GODK-STATUSKODER                                      
286800     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK728 SSA1                  
286900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
287000     PERFORM IMS-STATUSKONTROLL                                           
287100     .                                                                    
287200     EJECT                                                                
287300 IMS-REPL-WDK728 SECTION.                                                 
287400                                                                          
287500     MOVE '  ' TO GODK-STATUSKODER                                        
287600     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK728                       
287700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
287800     PERFORM IMS-STATUSKONTROLL                                           
287900     .                                                                    
288000     EJECT                                                                
288100 IMS-REPL-SLAGER-SEGM SECTION.                                            
288200     MOVE 'IMS-REPL-SLAGER-'  TO CURR-IMS-SECTION                         
288300                                                                          
288400     MOVE '  ' TO GODK-STATUSKODER                                        
288500     CALL  CBLTDLI  USING REPL WDK7-PCB DLI-IO-WDK711                     
288600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
288700     PERFORM IMS-STATUSKONTROLL                                           
288800     .                                                                    
288900     EJECT                                                                
289000 IMS-ISRT-WDL301 SECTION.                                                 
289100     MOVE 'WDL301 ' TO SSA1                                               
289200     MOVE '  II' TO GODK-STATUSKODER                                      
289300     CALL CBLTDLI USING ISRT WDL3-PCB DLI-IO-WDL301 SSA1                  
289400     MOVE WDL3-STATUS-CODE TO STATUS-WS                                   
289500     PERFORM IMS-STATUSKONTROLL                                           
289600     .                                                                    
289700     EJECT                                                                
289800************* WDR4 4505 HÄNDELSEBAS ( TÄCKNING ORDERADSREG.) ****         
289900                                                                          
290000 IMS-ISRT-450511 SECTION.                                                 
290100     MOVE 'IMS-ISRT-450511 '  TO CURR-IMS-SECTION                         
290200                                                                          
290300     STRING 'WL450501(WDGXKEY  =' W-WDGX-4505-KEY-X ')'                   
290400            DELIMITED BY SIZE INTO SSA1                                   
290500     MOVE 'WL450511 '                   TO SSA2                           
290600     MOVE '  ' TO GODK-STATUSKODER                                        
290700     CALL  CBLTDLI  USING ISRT 4505-PCB WL450511 SSA1 SSA2                
290800     MOVE 4505-STATUS-CODE TO STATUS-WS                                   
290900     PERFORM IMS-STATUSKONTROLL                                           
291000     .                                                                    
291100     EJECT                                                                
291200                                                                          
291300************ WDG2 XXEF HÄNDELSEBAS (UTREDNINGSSALDO) *************        
291400                                                                          
291500 IMS-GHU-ART-UTREDNSALDO SECTION.                                         
291600     MOVE 'IMS-GHU-ART-UTRE'  TO CURR-IMS-SECTION                         
291700                                                                          
291800     STRING 'WLXXEF01(WDGXKEY  =' W-WDGXKEY-ROT-X ')'                     
291900           DELIMITED BY SIZE INTO SSA1                                    
292000     STRING 'WLXXEF11(WDGXKEY  =' W-IDARTNR-UTR-X ')'                     
292100           DELIMITED BY SIZE INTO SSA2                                    
292200     MOVE '  GE' TO GODK-STATUSKODER                                      
292300     CALL  CBLTDLI  USING GHU XXEF-PCB WLXXEF11 SSA1 SSA2                 
292400     MOVE XXEF-STATUS-CODE TO STATUS-WS                                   
292500     PERFORM IMS-STATUSKONTROLL                                           
292600     .                                                                    
292700     SKIP2                                                                
292800 IMS-DELETE-ART-UTREDNSALDO SECTION.                                      
292900     MOVE 'IMS-DELETE-ART-U'  TO CURR-IMS-SECTION                         
293000                                                                          
293100     MOVE '  ' TO GODK-STATUSKODER                                        
293200     CALL  CBLTDLI  USING DLET XXEF-PCB WLXXEF11                          
293300     MOVE XXEF-STATUS-CODE TO STATUS-WS                                   
293400     PERFORM IMS-STATUSKONTROLL                                           
293500     .                                                                    
293600     EJECT                                                                
293700                                                                          
293800 IMS-GU-WDB601    SECTION.                                                
293900     MOVE 'IMS-GU-WDB601   '  TO CURR-IMS-SECTION                         
294000                                                                          
294100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
294200          DELIMITED BY SIZE INTO SSA1                                     
294300     MOVE '  ' TO GODK-STATUSKODER                                        
294400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
294500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
294600     PERFORM IMS-STATUSKONTROLL                                           
294700     .                                                                    
294800     EJECT                                                                
294900 IMS-STATUSKONTROLL SECTION.                                              
295000                                                                          
295100     SET STATUS-IX TO 1                                                   
295200     SEARCH GODK-STATUS                                                   
295300       AT END                                                             
295400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
295500         DELIMITED BY SIZE INTO FELTEXT                                   
295600         CALL FELLOG                                                      
295700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
295800         CONTINUE                                                         
295900     END-SEARCH                                                           
296000     .                                                                    
