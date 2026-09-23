000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5010900.                                                
000400 AUTHOR.         THOMAS LARSSON.                                          
000500 DATE-WRITTEN.   95/01/17.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        NY BILD FÖR FIKTIV CLEARING MELLAN CDC OCH SDC/LDC.              
001000*        DET ÄR BARA OK ATT GÖRA DETTA MELLAN DC INOM SAMMA IDFTG.        
001100*        SÅ INOM KINA KAN MAN GÖRA DET MELLAN OLIKA NDC-CN.               
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR WLINLC (WDL6)                              
001400*        PROGRAMMET UPPDATERAR WLINLE (WDL2)                              
001500*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001600*        PROGRAMMET UPPDATERAR WLARTS (WDK7)                              
001700*        PROGRAMMET LOGGAR SALDOFÖRÄNDRINGAR PÅ WLLOGA (WDL9)             
001800*        PROGRAMMET UPPDATERAR WLSAPA (WDR9)-EKONOMISK HÄNDELSE           
001900*        PROGRAMMET UPPDATERAR WDR8         -EKONOMISK HÄNDELSE           
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSAKTION: W5T109                                              
002300*        MID:         W5I10901                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        MOD:         W5O10901                                            
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                       PIC X(08)   VALUE 'W5010900'.            
003600                                                                          
003700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003900                                                                          
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200                                                                          
004300 77  WS-IDDC-SEND                PIC X(2)    VALUE SPACE.                 
004400 77  WS-IDDC-MOT                 PIC X(2)    VALUE SPACE.                 
004500                                                                          
004600 01  FILLER                      PIC X(15) VALUE 'ABENDKODER '.           
004700 01  ABENDKODER.                                                          
004800  03 ABEND-MED-DUMP              PIC S9(4)  VALUE +1000 COMP SYNC.        
004900  03 ABEND-UTAN-DUMP             PIC S9(4)  VALUE +16   COMP SYNC.        
005000                                                                          
005100*01  -COPY WWDCKONS                                                       
005200                                                                          
005300 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005400*    --- DET RÄTTA VÄRDET PÅ NEDANSTÅENDE FÄLT SÄTTS I A-INIT             
005500 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +147  COMP SYNC.        
005600                                                                          
005700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005800 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
005900 77  WS-KVCLEAR                  PIC 9(7)    VALUE ZERO.                  
006000 77  WS-KVCLEAR-RED              PIC S9(7)   VALUE ZERO.                  
006100 77  WS-IDCLEARREF               PIC 9(7)    VALUE ZERO.                  
006200 77  WS-SUMMA                    PIC S9(11)  VALUE ZERO.                  
006300 77  W-IDSEKVNR                  PIC S9(3)   VALUE ZERO COMP-3.           
006400 77  WS-KDSORT                   PIC X(2)    VALUE SPACE.                 
006500 77  WS-PRAVCOST-SEND            PIC S9(7)V9(2) VALUE +0 COMP-3.          
006600 77  WS-PRAVCOST-MOT             PIC S9(7)V9(2) VALUE +0 COMP-3.          
006700 77  WS-KVLS                     PIC S9(7)           COMP-3.              
006800 77  WS-KVLS-MOT                 PIC S9(7)           COMP-3.              
006900 77  WS-KVEFRS-MOT               PIC S9(7)           COMP-3.              
007000                                                                          
007100*01  -COPY  WWDC99                                                        
007200                                                                          
007300 01  WS-IDLOPNRM                 PIC S9(9)   VALUE ZERO.                  
007400 01  WS-IDLOPNRM-RED REDEFINES WS-IDLOPNRM.                               
007500     03  W-LOPNR1                PIC 9(1).                                
007600     03  W-VV                    PIC 9(2).                                
007700     03  W-D                     PIC 9(1).                                
007800     03  W-LOPNR2                PIC 9(5).                                
007900                                                                          
008000 01  WS-ARBAREA.                                                          
008100*    DATE + TIME FÖR ATT SKAPA INLEVERANSNUMMER                           
008200     03  WS-TIAAAAMMDDHHMMSSTH   PIC 9(16)   VALUE ZERO.                  
008300     03  FILLER REDEFINES WS-TIAAAAMMDDHHMMSSTH.                          
008400         05  WS-TISEKEL          PIC 9(2).                                
008500         05  WS-TIAAMMDD         PIC 9(6).                                
008600         05  WS-HHMMSSTH         PIC 9(8).                                
008700     03  WS-DAINLEV              PIC 9(16)   VALUE ZERO.                  
008800                                                                          
008900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009000 01  W-DAGENS-DATUM              PIC 9(8)    VALUE ZERO.                  
009100                                                                          
009200 01  WLOGG-TID                   PIC S9(9)   VALUE ZERO.                  
009300                                                                          
009400 01  LOGG-DATUM                  PIC S9(8)   VALUE ZERO.                  
009500                                                                          
009600 01  W-PRARTSTD                  PIC 9(7)V9(2) COMP-3 VALUE ZERO.         
009700                                                                          
009800 01  W-EKH-IDARTNR               PIC X(9)    VALUE SPACE.                 
009900                                                                          
010000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
010100     88  INDATA-OK                           VALUE 'J'.                   
010200     88  INDATA-FEL                          VALUE 'N'.                   
010300                                                                          
010400 77  AVG-COST-SW                 PIC X       VALUE 'J'.                   
010500     88  AVGCOST-FOUND                       VALUE 'J'.                   
010600     88  AVGCOST-MISSING                     VALUE 'N'.                   
010700                                                                          
010800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
010900     88  NYCKLAR-OK                          VALUE 'J'.                   
011000     88  NYCKLAR-FEL                         VALUE 'N'.                   
011100                                                                          
011200 77  ARTIKEL-SW                  PIC X       VALUE 'J'.                   
011300     88  ARTIKEL-FINNS                       VALUE 'J'.                   
011400     88  ARTIKEL-SAKNAS                      VALUE 'N'.                   
011500                                                                          
011600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
011700     88  EGEN-MID                            VALUE '5109'.                
011800     88  GODK-MID                            VALUE '5101' '5102'          
011900                                                   '5103' '5104'          
012000                                                   '5105' '5106'          
012100                                                   '5107' '5108'          
012200                                                   '5109'.                
012300     88  HELP-MID                            VALUE '0551'.                
012400     EJECT                                                                
012500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012600 01  GENERELLA-SUBPROGRAM.                                                
012700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013200     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
013300     03  W510AVG                 PIC X(8)    VALUE 'W510AVG '.            
013400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
013500     EJECT                                                                
013600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013700*01 -COPY WMSGINIT                                                        
013800     EJECT                                                                
013900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
014000*01 -COPY WMEDAREA                                                        
014100     SKIP3                                                                
014200*    --- PARAMETRAR TILL SUBPROGRAM W009CIA                               
014300*01 -COPY W009CIA                                                         
014400                                                                          
014500     EJECT                                                                
014600 01  MESSAGE-CODES.                                                       
014700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
014800     03  ERR-AVG-COST-MISSING    PIC X(3)    VALUE '300'.                 
014900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
015000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
015100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
015200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
015300     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
015400     EJECT                                                                
015500*01 -COPY WDATAREA                                                        
015600     EJECT                                                                
015700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015800*                                                                         
015900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016000     SKIP3                                                                
016100*01  MID -COPY W5I10901                                                   
016200     EJECT                                                                
016300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016400     SKIP3                                                                
016500*01  -COPY WMSGAREA                                                       
016600     EJECT                                                                
016700     03  MOD REDEFINES MSG-AREA.                                          
016800*      05  -COPY W5O10901                                                 
016900     EJECT                                                                
017000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
017100     SKIP3                                                                
017200*01  -COPY WMFSAREA                                                       
017300     EJECT                                                                
017400*    --- AREA FÖR W510AVG                                                 
017500 01  FILLER                    PIC X(16) VALUE 'W510AVGAREA*****'.        
017600                                                                          
017700*01  -COPY W510AVG                                                        
017800     EJECT                                                                
017900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018000*                                                                         
018100     EJECT                                                                
018200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018300     SKIP3                                                                
018400 01  NYCKLAR-TILL-DLI.                                                    
018500     03  W-IDARTNR-X.                                                     
018600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
018700     03  W-DAINLEV-X.                                                     
018800         05  W-DAINLEV           PIC 9(16)   VALUE ZERO.                  
018900     03  W-KDSEGKEY-X.                                                    
019000         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
019100     03  W-IDDC-X.                                                        
019200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
019300     03  W-IDDC-B6-X.                                                     
019400         05 W-IDDC-B6                  PIC X(2).                          
019500     SKIP2                                                                
019600*    --- STATUS-KOD FRÅN IMS                                              
019700 01  STATUS-WS                   PIC XX.                                  
019800     88  SEGMENT-FINNS                       VALUE '  '.                  
019900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
020000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
020100     SKIP2                                                                
020200 01  GODK-STATUSKODER.                                                    
020300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020400     SKIP3                                                                
020500 01  SSA1                        PIC X(64).                               
020600 01  SSA2                        PIC X(64).                               
020700 01  SSA3                        PIC X(64).                               
020800     EJECT                                                                
020900*    --- IMS FUNKTIONSKODER                                               
021000*01  -COPY W0003                                                          
021100     EJECT                                                                
021200*    ---  DLI INPUT-OUTPUT AREA                                           
021300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
021400     SKIP3                                                                
021500 01  DLI-IO-AREA.                                                         
021600     03  IO-AREA                 PIC X(200)  VALUE SPACE.                 
021700     SKIP3                                                                
021800     03  WLINLC01 REDEFINES IO-AREA.                                      
021900*        05  -COPY WDL601  -PRE INLC-                                     
022000     SKIP3                                                                
022100     03  WLINLC11 REDEFINES IO-AREA.                                      
022200*        05  -COPY WDL611  -PRE INLC-                                     
022300     EJECT                                                                
022400     03  WLINLE01 REDEFINES IO-AREA.                                      
022500*        05  -COPY WDL201  -PRE INLE-                                     
022600     SKIP3                                                                
022700     03  WLINLE11 REDEFINES IO-AREA.                                      
022800*        05  -COPY WDL211  -PRE INLE-                                     
022900     SKIP3                                                                
023000     03  WLINLE21 REDEFINES IO-AREA.                                      
023100*        05  -COPY WDL221  -PRE INLE-                                     
023200     EJECT                                                                
023300 01  FILLER                      PIC X(16)   VALUE 'WLARTC01'.            
023400 01  WLARTC01.                                                            
023500*    03  WLARTC01 -COPY WDK601 -PRE ARTC-                                 
023600     EJECT                                                                
023700                                                                          
023800 01  FILLER                      PIC X(16)   VALUE 'WLARTC11'.            
023900 01  WLARTC11.                                                            
024000*    03  WLARTC11 -COPY WDK611 -PRE ARTC-                                 
024100     EJECT                                                                
024200                                                                          
024300 01  DLI-IO-AREA3.                                                        
024400     03  IO-AREA3                PIC X(400)  VALUE SPACE.                 
024500     SKIP2                                                                
024600     03  WLARTS01 REDEFINES IO-AREA3.                                     
024700*        05  -COPY WDK701  -PRE ARTS-                                     
024800     SKIP3                                                                
024900     03  WLARTS11 REDEFINES IO-AREA3.                                     
025000*        05  -COPY WDK711  -PRE ARTS-                                     
025100     EJECT                                                                
025200 01  DLI-IO-AREA4.                                                        
025300     03  IO-AREA4                PIC X(400)  VALUE SPACE.                 
025400     EJECT                                                                
025500 01  FILLER                      PIC X(16)  VALUE 'WLLOGA01'.             
025600*01  WLLOGA01 -COPY WDL901                                                
025700     EJECT                                                                
025800 01  FILLER                      PIC X(16)  VALUE 'WDR801  '.             
025900*01  WDR801   -COPY WDR801 -PRE EKO-                                      
026000*    05 -COPY W510EKHA -RED EKO-FIL-WDR801-DATA -PRE EKO-                 
026100                                                                          
026200 01  FILLER                      PIC X(16)  VALUE 'WLSAPA01'.             
026300*01  WLSAPA01 -COPY WDR901                                                
026400*    05 -COPY W510EKHA -RED FIL-WDR901-DATA                               
026500                                                                          
026600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
026700 01   DLI-IO-AREA-B601.                                                   
026800*     03  -COPY WDB601                                                    
026900                                                                          
027000 01  FILLER               PIC X(16)   VALUE 'WDB601 SEND'.                
027100 01   DLI-IO-AREA-B601-SEND.                                              
027200*     03  -COPY WDB601 -PRE SEND-                                         
027300                                                                          
027400 01  FILLER               PIC X(16)   VALUE 'WDB601 MOT'.                 
027500 01   DLI-IO-AREA-B601-MOT.                                               
027600*     03  -COPY WDB601 -PRE MOT-                                          
027700     EJECT                                                                
027800                                                                          
027900 LINKAGE SECTION.                                                         
028000                                                                          
028100*01  -COPY W0009   -PRE MSG-                                              
028200     EJECT                                                                
028300*01  -COPY W0008  -PRE USEA-                                              
028400     05  FILLER                  PIC X.                                   
028500     EJECT                                                                
028600*01  -COPY W0008  -PRE INLC-                                              
028700     05  FILLER                  PIC X.                                   
028800     EJECT                                                                
028900*01  -COPY W0008  -PRE INLE-                                              
029000     05  FILLER                  PIC X.                                   
029100     EJECT                                                                
029200*01  -COPY W0008  -PRE ARTC-                                              
029300     05  FILLER                  PIC X.                                   
029400     EJECT                                                                
029500*01  -COPY W0008  -PRE ARTS-                                              
029600     05  FILLER                  PIC X.                                   
029700     EJECT                                                                
029800*01  -COPY W0008  -PRE LOGA-                                              
029900     05  FILLER                  PIC X.                                   
030000*01  -COPY W0008  -PRE SAPA-                                              
030100     05  FILLER                  PIC X.                                   
030200     EJECT                                                                
030300*01  -COPY W0008  -PRE WDB6-                                              
030400     05  FILLER                  PIC X.                                   
030500     EJECT                                                                
030600*01  -COPY W0008  -PRE WDR8-                                              
030700     05  FILLER                  PIC X.                                   
030800*01  -COPY W0008  -PRE 9305-                                              
030900     05  FILLER                  PIC X.                                   
031000*01  -COPY W0008  -PRE AVG-WDB6-                                          
031100     05  FILLER                  PIC X.                                   
031200     EJECT                                                                
031300 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
031400         INLC-PCB INLE-PCB ARTC-PCB ARTS-PCB          LOGA-PCB            
031500         SAPA-PCB WDB6-PCB WDR8-PCB 9305-PCB AVG-WDB6-PCB.                
031600 MAIN SECTION.                                                            
031700     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
031800         INLC-PCB INLE-PCB ARTC-PCB ARTS-PCB          LOGA-PCB            
031900         SAPA-PCB WDB6-PCB WDR8-PCB 9305-PCB AVG-WDB6-PCB.                
032000                                                                          
032100     PERFORM IMS-GET-MSG                                                  
032200     IF SEGMENT-FINNS                                                     
032300       PERFORM A-INIT                                                     
032400       PERFORM B-KOLLA-NYCKLAR                                            
032500       IF NYCKLAR-OK                                                      
032600         IF MFS-UPDATE                                                    
032700           PERFORM C-KOLL-ARTIKEL-FINNS                                   
032800           IF ARTIKEL-FINNS                                               
032900             PERFORM G-KOLLA-INPUT                                        
033000             IF INDATA-OK                                                 
033100               PERFORM H-UPPDATERA                                        
033200             END-IF                                                       
033300           ELSE                                                           
033400             MOVE 'ARTIKEL SAKNAS PÅ ARTREG ' TO MOD-TEMFSINF             
033500           END-IF                                                         
033600         ELSE                                                             
033700           PERFORM E-SAMMA-SIDA                                           
033800         END-IF                                                           
033900       END-IF                                                             
034000       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
034100       PERFORM IMS-INSERT-MSG                                             
034200     END-IF                                                               
034300                                                                          
034400     MOVE ZERO TO RETURN-CODE                                             
034500     GOBACK                                                               
034600     .                                                                    
034700     EJECT                                                                
034800 A-INIT SECTION.                                                          
034900                                                                          
035000     IF MSG-DUBBLA-TRANSKODER                                             
035100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I10901                 
035200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
035300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
035400     ELSE                                                                 
035500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I10901                  
035600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
035700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
035800     END-IF                                                               
035900                                                                          
036000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
036100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
036200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
036300                                                                          
036400     MOVE LOW-VALUE TO MSG-AREA                                           
036500     MOVE 'W5O109N1' TO MFS-IDMOD                                         
036600     MOVE '5109' TO MOD-IDTRANS                                           
036700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
036800                                                                          
036900*    --- OM SVAR TILL SKÄRM: MAX-MOD-LAENGD = MOD-LÄNGD + 4               
037000*    --- OM PROGRAM-TILL-PROGRAM-SWITCH:    = MOD-LÄNGD + 17              
037100                                                                          
037200     IF EGEN-MID OR HELP-MID                                              
037300       CONTINUE                                                           
037400     ELSE                                                                 
037500       MOVE SPACE TO MFS-KDTRTYP                                          
037600       MOVE '7' TO MFS-IDPFK                                              
037700     END-IF                                                               
037800                                                                          
037900     MOVE 'IDAG  '       TO DAT-KDDATFORM                                 
038000                                                                          
038100     CALL WDATKONV USING DAT-KDDATFORM                                    
038200                         DAT-I-TIDATUM                                    
038300                         DAT-O-TIDATUM                                    
038400                         DAT-KDSVAR                                       
038500     IF DAT-KDSVAR-OK                                                     
038600       MOVE DAT-TIVV TO W-VV                                              
038700       MOVE DAT-TID  TO W-D                                               
038800     ELSE                                                                 
038900       CALL FELLOG                                                        
039000     END-IF                                                               
039100                                                                          
039200     ACCEPT DAGENS-DATUM FROM DATE                                        
039300     .                                                                    
039400     EJECT                                                                
039500 B-KOLLA-NYCKLAR SECTION.                                                 
039600                                                                          
039700     MOVE JA TO NYCKLAR-SW                                                
039800                                                                          
039900*    -- KONTROLL AV IDARTNR                                               
040000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
040100                                                                          
040200     MOVE ALL '+' TO MSGI-WMSGINIT                                        
040300     MOVE '001'             TO MSGI-KDCALL                                
040400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
040500     MOVE '5109'               TO MSGI-IDTRANS                            
040600     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
040700                                                                          
040800     IF MFS-IDTRANS = '5109'                                              
040900     OR (MID-IDARTNR-IN NUMERIC                                           
041000     AND MID-IDARTNR-IN > ZERO)                                           
041100         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
041200     END-IF                                                               
041300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
041400                                                                          
041500     IF MSGI-IDLAND-SPR  = 'GB'                                           
041600       MOVE +2 TO SPRAK-IX                                                
041700       MOVE 'GB ' TO MED-IDSKYLT                                          
041800     ELSE                                                                 
041900       MOVE +1 TO SPRAK-IX                                                
042000       MOVE 'S  ' TO MED-IDSKYLT                                          
042100     END-IF                                                               
042200                                                                          
042300     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
042400     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
042500                                                                          
042600     IF MID-IDARTNR-IN = ALL '+'                                          
042700       CONTINUE                                                           
042800     ELSE                                                                 
042900       MOVE '7'         TO MFS-IDPFK                                      
043000       MOVE SPACE       TO MFS-KDTRTYP                                    
043100     END-IF                                                               
043200     IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                          
043300       MOVE WS-IDARTNR TO W-IDARTNR                                       
043400     ELSE                                                                 
043500       MOVE NEJ TO NYCKLAR-SW                                             
043600     END-IF                                                               
043700                                                                          
043800     MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                    
043900     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
044000                                                                          
044100     IF NYCKLAR-FEL                                                       
044200       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
044300       CALL WMEDKONV USING MED-WMEDAREA                                   
044400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
044500       PERFORM MFS-RENSA-FAELT-IN                                         
044600       PERFORM MFS-RENSA-FAELT-UT                                         
044700     END-IF                                                               
044800     .                                                                    
044900     EJECT                                                                
045000 C-KOLL-ARTIKEL-FINNS SECTION.                                            
045100                                                                          
045200     MOVE JA TO ARTIKEL-SW                                                
045300                                                                          
045400     PERFORM IMS-GU-WDK601                                                
045500     MOVE ARTC-ART-KDSORT TO WS-KDSORT                                    
045600     PERFORM IMS-GHU-WDK611                                               
045700     IF SEGMENT-SAKNAS                                                    
045800       MOVE NEJ TO ARTIKEL-SW                                             
045900     END-IF                                                               
046000                                                                          
046100     MOVE MID-IDDC-SEND    TO WS-IDDC-SEND                                
046200     IF WS-IDDC-SEND NOT = SEND-DCS-IDDC                                  
046300        MOVE WS-IDDC-SEND TO W-IDDC-B6                                    
046400        PERFORM IMS-GU-WDB601-SEND                                        
046500     END-IF                                                               
046600     IF NOT SEND-DCS-CDC                                                  
046700       MOVE MID-IDDC-SEND TO W-IDDC                                       
046800     ELSE                                                                 
046900       MOVE MID-IDDC-MOT TO W-IDDC                                        
047000     END-IF                                                               
047100                                                                          
047200     PERFORM IMS-GHU-WDK711                                               
047300     IF SEGMENT-SAKNAS                                                    
047400       MOVE NEJ TO ARTIKEL-SW                                             
047500     END-IF                                                               
047600     .                                                                    
047700     EJECT                                                                
047800 E-SAMMA-SIDA SECTION.                                                    
047900     MOVE 'FICTIVE CLEARING FOR WAREHOUSES WITH SAME COMPANY CODE'        
048000           TO MOD-TEMFSINF                                                
048100     IF EGEN-MID OR HELP-MID                                              
048200       IF MID-INPUT = ALL '+'                                             
048300         PERFORM MFS-RENSA-FAELT-IN                                       
048400       ELSE                                                               
048500         IF MID-IDARTNR-IN NOT = ALL '+'                                  
048600            PERFORM MFS-RENSA-FAELT-IN                                    
048700         ELSE                                                             
048800            MOVE INF-PRESS-PF11 TO MED-IDMFSINF                           
048900            CALL WMEDKONV USING MED-WMEDAREA                              
049000            MOVE MED-MFSINF TO MOD-TEMFSINF                               
049100            PERFORM MFS-ROER-EJ-FAELT-IN                                  
049200            PERFORM MFS-ROER-EJ-FAELT-UT                                  
049300            MOVE MFS-FORMATETS-ATTR TO MOD-KVCLEAR                        
049400            PERFORM EA-MID-INDATA-TILL-MOD                                
049500         END-IF                                                           
049600       END-IF                                                             
049700     ELSE                                                                 
049800       PERFORM MFS-RENSA-FAELT-IN                                         
049900     END-IF                                                               
050000     .                                                                    
050100     EJECT                                                                
050200 EA-MID-INDATA-TILL-MOD SECTION.                                          
050300                                                                          
050400* * * * * FÖR VARJE MID-FÄLT                                              
050500* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
050600* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
050700* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
050800                                                                          
050900     IF MID-KVCLEAR NOT = ALL '+'                                         
051000       MOVE MID-KVCLEAR           TO MOD-KVCLEAR                          
051100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVCLEAR-ATTR                     
051200     ELSE                                                                 
051300       MOVE MFS-RENSA-FAELT       TO MOD-KVCLEAR                          
051400     END-IF                                                               
051500                                                                          
051600     IF MID-IDDC-SEND NOT = ALL '+'                                       
051700       MOVE MID-IDDC-SEND         TO MOD-IDDC-SEND                        
051800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDC-SEND-ATTR                   
051900     ELSE                                                                 
052000       MOVE MFS-RENSA-FAELT       TO MOD-IDDC-SEND                        
052100     END-IF                                                               
052200                                                                          
052300     IF MID-IDDC-MOT NOT = ALL '+'                                        
052400       MOVE MID-IDDC-MOT          TO MOD-IDDC-MOT                         
052500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDC-MOT-ATTR                    
052600     ELSE                                                                 
052700       MOVE MFS-RENSA-FAELT       TO MOD-IDDC-MOT                         
052800     END-IF                                                               
052900                                                                          
053000     IF MID-IDCLEARREF NOT = ALL '+'                                      
053100       MOVE MID-IDCLEARREF        TO MOD-IDCLEARREF                       
053200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDCLEARREF-ATTR                  
053300     ELSE                                                                 
053400       MOVE MFS-RENSA-FAELT       TO MOD-IDCLEARREF                       
053500     END-IF                                                               
053600     .                                                                    
053700     EJECT                                                                
053800 G-KOLLA-INPUT SECTION.                                                   
053900                                                                          
054000     MOVE JA  TO INDATA-SW                                                
054100     IF MID-INPUT = ALL '+'                                               
054200       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
054300       CALL WMEDKONV USING MED-WMEDAREA                                   
054400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
054500       PERFORM MFS-ROER-EJ-FAELT-IN                                       
054600       PERFORM MFS-ROER-EJ-FAELT-UT                                       
054700       MOVE NEJ TO INDATA-SW                                              
054800     ELSE                                                                 
054900       MOVE SPACES       TO  WS-IDDC-SEND                                 
055000                             WS-IDDC-MOT                                  
055100                             MOT-DCS-KDDC                                 
055200       IF MID-IDDC-SEND NOT = ALL '+'                                     
055300         MOVE MID-IDDC-SEND        TO WS-IDDC-SEND                        
055400         IF WS-IDDC-SEND NOT = SEND-DCS-IDDC                              
055500            MOVE WS-IDDC-SEND TO W-IDDC-B6                                
055600            PERFORM IMS-GU-WDB601-SEND                                    
055700         END-IF                                                           
055800         IF SEND-DCS-CDC OR SEND-DCS-SDC OR SEND-DCS-NDC-PF OR            
055900            SEND-DCS-NDC-CN OR SEND-DCS-NDC-OTHERS OR                     
056000            SEND-DCS-NDC-NA OR SEND-DCS-NDC-SA                            
056100           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-SEND-ATTR                
056200         ELSE                                                             
056300           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-SEND-ATTR                  
056400           MOVE NEJ TO INDATA-SW                                          
056500           MOVE SPACE              TO WS-IDDC-SEND                        
056600         END-IF                                                           
056700       END-IF                                                             
056800                                                                          
056900       IF MID-IDDC-MOT NOT = ALL '+'                                      
057000         MOVE MID-IDDC-MOT          TO WS-IDDC-MOT                        
057100         IF WS-IDDC-MOT NOT = MOT-DCS-IDDC                                
057200            MOVE WS-IDDC-MOT TO W-IDDC-B6                                 
057300            PERFORM IMS-GU-WDB601-MOT                                     
057400         END-IF                                                           
057500         IF MOT-DCS-CDC OR MOT-DCS-SDC OR MOT-DCS-NDC-PF OR               
057600            MOT-DCS-NDC-CN OR MOT-DCS-NDC-OTHERS OR                       
057700            MOT-DCS-NDC-NA OR MOT-DCS-NDC-SA                              
057800           IF MID-IDDC-MOT NOT = MID-IDDC-SEND                            
057900             IF MOT-DCS-CDC OR MOT-DCS-NDC-CN                             
058000             OR MOT-DCS-NDC-NA                                            
058100               MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-MOT-ATTR             
058200             ELSE                                                         
058300               IF SEND-DCS-CDC OR SEND-DCS-NDC-CN                         
058400               OR SEND-DCS-NDC-NA                                         
058500                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-MOT-ATTR           
058600               ELSE                                                       
058700                 MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-MOT-ATTR             
058800                 MOVE NEJ TO INDATA-SW                                    
058900                 MOVE SPACE        TO WS-IDDC-MOT                         
059000                                      MOT-DCS-KDDC                        
059100               END-IF                                                     
059200             END-IF                                                       
059300           ELSE                                                           
059400             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-MOT-ATTR                 
059500             MOVE NEJ TO INDATA-SW                                        
059600             MOVE SPACE              TO WS-IDDC-MOT                       
059700           END-IF                                                         
059800         ELSE                                                             
059900           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-MOT-ATTR                   
060000           MOVE NEJ TO INDATA-SW                                          
060100           MOVE SPACE              TO WS-IDDC-MOT                         
060200         END-IF                                                           
060300       ELSE                                                               
060400         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-MOT-ATTR                     
060500         MOVE NEJ TO INDATA-SW                                            
060600         MOVE SPACE              TO WS-IDDC-MOT                           
060700       END-IF                                                             
060800                                                                          
060900       IF MID-IDDC-SEND NOT = ALL '+'                                     
061000       AND MID-IDDC-MOT NOT = ALL '+'                                     
061100        IF MID-IDDC-MOT NOT = MID-IDDC-SEND                               
061200          MOVE MID-IDDC-SEND             TO WS-IDDC                       
061300          IF NDC-CN OR NDC-US                                             
061400             MOVE MID-IDDC-MOT           TO WS-IDDC                       
061500             IF NDC-CN OR NDC-US                                          
061600               MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-MOT-ATTR             
061700             ELSE                                                         
061800               MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDDC-MOT-ATTR             
061900               MOVE NEJ TO INDATA-SW                                      
062000               MOVE SPACE                TO WS-IDDC-MOT                   
062100             END-IF                                                       
062200          END-IF                                                          
062300          MOVE MID-IDDC-MOT              TO WS-IDDC                       
062400          IF NDC-CN OR NDC-US                                             
062500             MOVE MID-IDDC-SEND          TO WS-IDDC                       
062600             IF NDC-CN OR NDC-US                                          
062700               MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-SEND-ATTR            
062800             ELSE                                                         
062900               MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDDC-SEND-ATTR            
063000               MOVE NEJ TO INDATA-SW                                      
063100               MOVE SPACE                TO WS-IDDC-SEND                  
063200             END-IF                                                       
063300          END-IF                                                          
063400        ELSE                                                              
063500          MOVE MFS-ALFA-FAELT-FEL        TO MOD-IDDC-MOT-ATTR             
063600          MOVE NEJ                       TO INDATA-SW                     
063700          MOVE SPACE                     TO WS-IDDC-MOT                   
063800        END-IF                                                            
063900       END-IF                                                             
064000                                                                          
064100       IF MID-KVCLEAR NOT = ALL '+'                                       
064200         MOVE MID-KVCLEAR TO WS-KVCLEAR                                   
064300         INSPECT WS-KVCLEAR REPLACING LEADING SPACE BY ZERO               
064400         IF WS-KVCLEAR NUMERIC AND                                        
064500            WS-KVCLEAR > ZERO                                             
064600           IF SEND-DCS-CDC                                                
064700             PERFORM IMS-GHU-WDK611                                       
064800             IF SEGMENT-FINNS                                             
064900               MOVE MID-KVCLEAR           TO WS-KVCLEAR-RED               
065000               COMPUTE WS-SUMMA = ARTC-CLAG-KVLS - WS-KVCLEAR-RED         
065100               IF WS-SUMMA >= +0                                          
065200                 MOVE MFS-NUM-FAELT-RAETT TO MOD-KVCLEAR-ATTR             
065300                 MOVE MID-KVCLEAR         TO WS-KVCLEAR                   
065400               ELSE                                                       
065500                 MOVE MFS-NUM-FAELT-FEL   TO MOD-KVCLEAR-ATTR             
065600                 MOVE NEJ TO INDATA-SW                                    
065700               END-IF                                                     
065800             END-IF                                                       
065900           ELSE                                                           
066000             MOVE WS-IDDC-SEND TO W-IDDC                                  
066100             PERFORM IMS-GHU-WDK711                                       
066200             IF SEGMENT-FINNS                                             
066300               MOVE MID-KVCLEAR           TO WS-KVCLEAR-RED               
066400               COMPUTE WS-SUMMA = ARTS-SLAG-KVLS - WS-KVCLEAR-RED         
066500               IF WS-SUMMA >= +0                                          
066600                 MOVE MFS-NUM-FAELT-RAETT TO MOD-KVCLEAR-ATTR             
066700                 MOVE MID-KVCLEAR         TO WS-KVCLEAR                   
066800               ELSE                                                       
066900                 MOVE MFS-NUM-FAELT-FEL TO MOD-KVCLEAR-ATTR               
067000                 MOVE NEJ TO INDATA-SW                                    
067100               END-IF                                                     
067200             ELSE                                                         
067300**** ARTIKELN MÅSTE FINNAS PÅ DET SÄNDANDE LAGRET                         
067400               MOVE MFS-NUM-FAELT-FEL TO MOD-IDDC-SEND-ATTR               
067500               MOVE NEJ TO INDATA-SW                                      
067600             END-IF                                                       
067700           END-IF                                                         
067800           IF MOT-DCS-CDC                                                 
067900             CONTINUE                                                     
068000           ELSE                                                           
068100             MOVE WS-IDDC-MOT TO W-IDDC                                   
068200             PERFORM IMS-GHU-WDK711                                       
068300             IF SEGMENT-FINNS                                             
068400               CONTINUE                                                   
068500             ELSE                                                         
068600**** ARTIKELN MÅSTE FINNAS PÅ DET MOTTAGANDE LAGRET                       
068700               MOVE MFS-NUM-FAELT-FEL TO MOD-IDDC-MOT-ATTR                
068800               MOVE NEJ TO INDATA-SW                                      
068900             END-IF                                                       
069000           END-IF                                                         
069100         ELSE                                                             
069200           MOVE MFS-NUM-FAELT-FEL TO MOD-KVCLEAR-ATTR                     
069300           MOVE NEJ TO INDATA-SW                                          
069400         END-IF                                                           
069500       ELSE                                                               
069600         MOVE MFS-NUM-FAELT-FEL TO MOD-KVCLEAR-ATTR                       
069700         MOVE NEJ TO INDATA-SW                                            
069800       END-IF                                                             
069900                                                                          
070000       IF MID-IDCLEARREF NOT = ALL '+'                                    
070100         MOVE MID-IDCLEARREF TO WS-IDCLEARREF                             
070200         INSPECT WS-IDCLEARREF REPLACING LEADING SPACE BY ZERO            
070300         IF WS-IDCLEARREF NUMERIC                                         
070400           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCLEARREF-ATTR                
070500           MOVE MID-IDCLEARREF      TO WS-IDCLEARREF                      
070600         ELSE                                                             
070700           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCLEARREF-ATTR                  
070800           MOVE NEJ TO INDATA-SW                                          
070900         END-IF                                                           
071000       ELSE                                                               
071100         MOVE ZERO TO WS-IDCLEARREF                                       
071200                      MID-IDCLEARREF                                      
071300       END-IF                                                             
071400                                                                          
071500       MOVE WS-IDDC-SEND       TO WS-IDDC                                 
071600       IF NDC-CN OR NDC-US                                                
071700        IF MID-IDDC-SEND NOT = ALL '+' AND                                
071800          MID-IDDC-MOT  NOT = ALL '+' AND                                 
071900          INDATA-OK                                                       
072000          MOVE ZEROS TO WS-PRAVCOST-SEND                                  
072100                        WS-PRAVCOST-MOT                                   
072200                        WS-KVLS                                           
072300                        WS-KVLS-MOT                                       
072400                        WS-KVEFRS-MOT                                     
072500          MOVE WS-IDDC-SEND TO W-IDDC                                     
072600          PERFORM IMS-GU-WDK711                                           
072700          IF ARTS-SLAG-PRAVCOST NOT = ZERO                                
072800             MOVE ARTS-SLAG-PRAVCOST    TO WS-PRAVCOST-SEND               
072900          ELSE                                                            
073000             MOVE WS-IDDC-MOT           TO W-IDDC                         
073100             PERFORM IMS-GU-WDK711                                        
073200             IF ARTS-SLAG-PRAVCOST NOT = ZERO                             
073300                MOVE ARTS-SLAG-PRAVCOST TO WS-PRAVCOST-MOT                
073400             ELSE                                                         
073500               IF ARTS-SLAG-PRAVCOST = ZERO                               
073600                  MOVE NEJ TO INDATA-SW                                   
073700                  MOVE MFS-NUM-FAELT-FEL TO MOD-IDDC-SEND-ATTR            
073800                  MOVE NEJ TO AVG-COST-SW                                 
073900               END-IF                                                     
074000             END-IF                                                       
074100          END-IF                                                          
074200        END-IF                                                            
074300       END-IF                                                             
074400       IF AVGCOST-MISSING AND INDATA-FEL AND (NDC-CN                      
074500       OR NDC-US)                                                         
074600          MOVE ERR-AVG-COST-MISSING TO MED-IDMFSFEL                       
074700          CALL WMEDKONV USING MED-WMEDAREA                                
074800          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
074900          PERFORM MFS-ROER-EJ-FAELT-UT                                    
075000          PERFORM MFS-ROER-EJ-FAELT-IN                                    
075100       ELSE                                                               
075200         IF INDATA-FEL                                                    
075300           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
075400           CALL WMEDKONV USING MED-WMEDAREA                               
075500           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
075600           PERFORM MFS-ROER-EJ-FAELT-UT                                   
075700           PERFORM MFS-ROER-EJ-FAELT-IN                                   
075800         END-IF                                                           
075900       END-IF                                                             
076000     END-IF                                                               
076100     .                                                                    
076200     EJECT                                                                
076300 H-UPPDATERA SECTION.                                                     
076400     IF SEND-DCS-CDC                                                      
076500       PERFORM IMS-GHU-WDK611                                             
076600       IF SEGMENT-FINNS                                                   
076700                                                                          
076800         COMPUTE ARTC-CLAG-KVLS = ARTC-CLAG-KVLS - WS-KVCLEAR             
076900         MOVE ARTC-CLAG-PRARTSTD TO W-PRARTSTD                            
077000         PERFORM IMS-REPL-ARTC                                            
077100         MOVE '-' TO LOGG-IDTECKEN-KVLS                                   
077200         MOVE WS-IDDC-SEND TO LOGG-IDDC                                   
077300         PERFORM HD-SKAPA-SALDOLOGG-WDK6                                  
077400                                                                          
077500       END-IF                                                             
077600       PERFORM HB-CDC-HISTORIK                                            
077700     ELSE                                                                 
077800       MOVE WS-IDDC-SEND TO W-IDDC                                        
077900       PERFORM IMS-GHU-WDK711                                             
078000       IF SEGMENT-FINNS                                                   
078100         COMPUTE ARTS-SLAG-KVLS = ARTS-SLAG-KVLS - WS-KVCLEAR             
078200         IF WS-PRAVCOST-SEND = ZERO AND                                   
078300            WS-PRAVCOST-MOT > ZERO                                        
078400            MOVE WS-PRAVCOST-MOT TO ARTS-SLAG-PRAVCOST                    
078500         END-IF                                                           
078600         PERFORM IMS-REPL-ARTS                                            
078700         MOVE '-' TO LOGG-IDTECKEN-KVLS                                   
078800         MOVE WS-IDDC-SEND TO LOGG-IDDC                                   
078900         PERFORM HF-SKAPA-SALDOLOGG-WDK7                                  
079000                                                                          
079100       END-IF                                                             
079200                                                                          
079300       PERFORM HC-SDC-HISTORIK-SEND                                       
079400     END-IF                                                               
079500                                                                          
079600     IF MOT-DCS-CDC                                                       
079700       PERFORM IMS-GHU-WDK611                                             
079800       IF SEGMENT-FINNS                                                   
079900         COMPUTE ARTC-CLAG-KVLS = ARTC-CLAG-KVLS + WS-KVCLEAR             
080000         MOVE ARTC-CLAG-PRARTSTD TO W-PRARTSTD                            
080100         PERFORM IMS-REPL-ARTC                                            
080200         MOVE '+' TO LOGG-IDTECKEN-KVLS                                   
080300         MOVE WS-IDDC-MOT TO LOGG-IDDC                                    
080400         PERFORM HD-SKAPA-SALDOLOGG-WDK6                                  
080500                                                                          
080600       END-IF                                                             
080700       PERFORM HB-CDC-HISTORIK                                            
080800     ELSE                                                                 
080900       MOVE WS-IDDC-MOT TO W-IDDC                                         
081000       PERFORM IMS-GHU-WDK711                                             
081100       IF SEGMENT-FINNS                                                   
081200         IF MOT-DCS-LAND-NON-VCC-OWNED                                    
081300           MOVE ARTS-SLAG-PRAVCOST TO WS-PRAVCOST-MOT                     
081400           MOVE ARTS-SLAG-KVLS   TO WS-KVLS-MOT                           
081500           MOVE ARTS-SLAG-KVEFRS TO WS-KVEFRS-MOT                         
081600                                                                          
081700           MOVE WS-TIAAMMDD (1:2)  TO AVG-TIAA                            
081800           MOVE WS-TIAAMMDD (3:2)  TO AVG-TIMM                            
081900           MOVE 021                TO AVG-KDCALL                          
082000           MOVE WS-PRAVCOST-MOT    TO AVG-PRAVCOST-OLD                    
082100           COMPUTE WS-KVLS = WS-KVLS-MOT + WS-KVEFRS-MOT                  
082200           IF WS-KVLS < ZERO                                              
082300             MOVE ZERO             TO AVG-KVLS-OLD                        
082400           ELSE                                                           
082500             MOVE WS-KVLS          TO AVG-KVLS-OLD                        
082600           END-IF                                                         
082700           MOVE ZERO               TO AVG-PRAVCOST-NEW                    
082800           MOVE WS-PRAVCOST-SEND   TO AVG-PRARTNTO                        
082900           MOVE ZERO               TO AVG-PRKURS                          
083000           MOVE WS-IDDC-MOT        TO AVG-IDDC                            
083100           MOVE SPACE              TO AVG-KDVALISO                        
083200           MOVE WS-KVCLEAR         TO AVG-KVANTMOT                        
083300           MOVE ZERO               TO AVG-KDPSLLOC                        
083400           MOVE ZERO               TO AVG-KDPRODSL                        
083500                                      AVG-IDFKNGRP                        
083600           CALL W510AVG USING AVG-W510AVG 9305-PCB                        
083700                              AVG-WDB6-PCB                                
083800           IF AVG-KDSVAR = ' '                                            
083900              MOVE AVG-PRAVCOST-NEW TO ARTS-SLAG-PRAVCOST                 
084000              COMPUTE ARTS-SLAG-KVLS = ARTS-SLAG-KVLS + WS-KVCLEAR        
084100           ELSE                                                           
084200              MOVE AVG-PRAVCOST-OLD TO ARTS-SLAG-PRAVCOST                 
084300           END-IF                                                         
084400           PERFORM IMS-REPL-ARTS                                          
084500           MOVE '+' TO LOGG-IDTECKEN-KVLS                                 
084600           MOVE WS-IDDC-MOT TO LOGG-IDDC                                  
084700           PERFORM HF-SKAPA-SALDOLOGG-WDK7                                
084800         ELSE                                                             
084900           MOVE ARTS-SLAG-KVLS   TO WS-KVLS-MOT                           
085000           MOVE ARTS-SLAG-KVEFRS TO WS-KVEFRS-MOT                         
085100           COMPUTE WS-KVLS = WS-KVLS-MOT + WS-KVEFRS-MOT                  
085200           COMPUTE ARTS-SLAG-KVLS = ARTS-SLAG-KVLS + WS-KVCLEAR           
085300           PERFORM IMS-REPL-ARTS                                          
085400           MOVE '+' TO LOGG-IDTECKEN-KVLS                                 
085500           MOVE WS-IDDC-MOT TO LOGG-IDDC                                  
085600           PERFORM HF-SKAPA-SALDOLOGG-WDK7                                
085700         END-IF                                                           
085800       END-IF                                                             
085900       PERFORM HC-SDC-HISTORIK-MOT                                        
086000     END-IF                                                               
086100                                                                          
086200                                                                          
086300     PERFORM S03-UPPDATERA-WDR8-WDR9                                      
086400                                                                          
086500     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
086600     CALL WMEDKONV USING MED-WMEDAREA                                     
086700     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
086800     PERFORM MFS-RENSA-FAELT-IN                                           
086900* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
087000     .                                                                    
087100     EJECT                                                                
087200 HB-CDC-HISTORIK SECTION.                                                 
087300                                                                          
087400     PERFORM IMS-GHU-WDL201                                               
087500     IF SEGMENT-SAKNAS                                                    
087600       MOVE WS-IDARTNR      TO INLE-ART-IDARTNR                           
087700                               W-IDARTNR                                  
087800       PERFORM IMS-ISRT-WDL201                                            
087900                                                                          
088000     END-IF                                                               
088100                                                                          
088200     MOVE ZERO         TO W-LOPNR1                                        
088300                        W-LOPNR2                                          
088400                                                                          
088500     PERFORM S01-SKAPA-IDINLEV                                            
088600     MOVE WS-DAINLEV        TO INLE-INL-DAINLEV                           
088700                               W-DAINLEV                                  
088800     PERFORM IMS-ISRT-WDL211                                              
088900     MOVE 'R32'             TO INLE-MOT-IDPTYP                            
089000     MOVE WS-IDLOPNRM       TO INLE-MOT-IDLOPNRM                          
089100     MOVE WS-IDCLEARREF     TO INLE-MOT-IDAVINR                           
089200     PERFORM HBA-SET-IDLEVNR                                              
089300     IF SEND-DCS-CDC                                                      
089400       MOVE WS-IDDC-MOT     TO INLE-MOT-IDKONTO                           
089500       MOVE 99              TO INLE-MOT-KDRT                              
089600       COMPUTE WS-KVCLEAR-RED = WS-KVCLEAR * -1                           
089700       MOVE WS-KVCLEAR-RED  TO INLE-MOT-KVANTMOT                          
089800     ELSE                                                                 
089900       MOVE ZERO            TO INLE-MOT-IDKONTO                           
090000       MOVE 08              TO INLE-MOT-KDRT                              
090100       MOVE WS-KVCLEAR      TO INLE-MOT-KVANTMOT                          
090200     END-IF                                                               
090300     MOVE DAGENS-DATUM      TO INLE-MOT-TIAVIDAT                          
090400                               INLE-MOT-TIUPPDAT                          
090500     MOVE WC-CDC-SE         TO INLE-MOT-IDDC                              
090600     MOVE SPACE             TO INLE-MOT-IDFS                              
090700                               INLE-MOT-IDKST                             
090800     MOVE +0                TO INLE-MOT-ADLAGOMR                          
090900                               INLE-MOT-ADGANG                            
091000                               INLE-MOT-ADPLATS                           
091100                               INLE-MOT-KDAVVANT                          
091200                               INLE-MOT-KDAVVKV                           
091300                               INLE-MOT-KVAVIS                            
091400                               INLE-MOT-KVFORDEL                          
091500                               INLE-MOT-KVRETUR                           
091600                               INLE-MOT-KVFORV                            
091700                               INLE-MOT-IDANALYS                          
091800                               INLE-MOT-IDSHIPM                           
091900     PERFORM IMS-ISRT-WDL221                                              
092000     .                                                                    
092100     SKIP2                                                                
092200 HBA-SET-IDLEVNR               SECTION.                                   
092300                                                                          
092400     IF WS-IDDC-SEND NOT = DCS-IDDC                                       
092500        MOVE WS-IDDC-SEND TO W-IDDC-B6                                    
092600        PERFORM IMS-GU-WDB601                                             
092700     END-IF                                                               
092800     MOVE DCS-IDLEVNR-DC  TO INLE-MOT-IDLEVNR                             
092900     .                                                                    
093000     EJECT                                                                
093100 HC-SDC-HISTORIK-SEND SECTION.                                            
093200                                                                          
093300     PERFORM IMS-GHU-WDL601                                               
093400     IF SEGMENT-SAKNAS                                                    
093500       MOVE WS-IDARTNR      TO INLC-ART-IDARTNR                           
093600                               W-IDARTNR                                  
093700       PERFORM IMS-ISRT-WDL601                                            
093800     END-IF                                                               
093900                                                                          
094000     PERFORM S01-SKAPA-IDINLEV-SEND                                       
094100     MOVE WS-DAINLEV        TO INLC-INL-DAINLEV                           
094200     MOVE WS-IDCLEARREF     TO INLC-INL-IDFAKT                            
094300                                                                          
094400     COMPUTE WS-KVCLEAR-RED = WS-KVCLEAR * -1                             
094500     MOVE WS-KVCLEAR-RED    TO INLC-INL-KVANTMOT                          
094600     MOVE WS-IDDC-SEND      TO INLC-INL-IDDC                              
094700                                                                          
094800     MOVE 'R32'             TO INLC-INL-IDPTYP                            
094900     MOVE DAGENS-DATUM      TO INLC-INL-TIINLMOT                          
095000                               INLC-INL-TIINLINL                          
095100     MOVE ZERO              TO INLC-INL-ADLAGOMR                          
095200                               INLC-INL-ADGANG                            
095300                               INLC-INL-ADPLATS                           
095400                               INLC-INL-IDDISTR                           
095500                               INLC-INL-IDKUNDNR                          
095600                               INLC-INL-IDKOLLI                           
095700                               INLC-INL-KVAVIS                            
095800                               INLC-INL-TIINLMTI                          
095900                               INLC-INL-TIINLITI                          
096000                               INLC-INL-IDLOPNRM                          
096100                               INLC-INL-KDFRAKT                           
096200                               INLC-INL-KVART-SKROT                       
096300                               INLC-INL-PRARTNTO                          
096400                               INLC-INL-PRKURS                            
096500                               INLC-INL-TIBERANK                          
096600     MOVE +8                TO INLC-INL-KDRT                              
096700     MOVE NEJ               TO INLC-INL-FLPRIO                            
096800                               INLC-INL-FLMAKUL                           
096900                               INLC-INL-FLSKAKOL                          
097000                               INLC-INL-FLTULLST                          
097100     MOVE SPACE             TO INLC-INL-IDANALYS                          
097200                               INLC-INL-IDKST                             
097300     MOVE ZERO              TO INLC-INL-IDKONTO                           
097400                               INLC-INL-KVTULRET                          
097500                               INLC-INL-KVRETUR                           
097600                               INLC-INL-KDAVVANT                          
097700                               INLC-INL-TIAVIDAT                          
097800     MOVE SPACE             TO INLC-INL-KDKOLLI                           
097900                               INLC-INL-IDKUNDRF                          
098000                               INLC-INL-KDVALISO                          
098100                               INLC-INL-IDUSER-003                        
098200                               INLC-INL-IDDC-LEV                          
098300     MOVE '1441 '           TO INLC-INL-IDLEVNR                           
098400     PERFORM IMS-ISRT-WDL611                                              
098500     .                                                                    
098600     EJECT                                                                
098700 HC-SDC-HISTORIK-MOT SECTION.                                             
098800                                                                          
098900     PERFORM IMS-GHU-WDL601                                               
099000     IF SEGMENT-SAKNAS                                                    
099100       MOVE WS-IDARTNR      TO INLC-ART-IDARTNR                           
099200                               W-IDARTNR                                  
099300       PERFORM IMS-ISRT-WDL601                                            
099400     END-IF                                                               
099500                                                                          
099600     PERFORM S01-SKAPA-IDINLEV-MOT                                        
099700     MOVE WS-DAINLEV        TO INLC-INL-DAINLEV                           
099800     MOVE WS-IDCLEARREF     TO INLC-INL-IDFAKT                            
099900                                                                          
100000     MOVE WS-KVCLEAR        TO INLC-INL-KVANTMOT                          
100100     MOVE WS-IDDC-MOT       TO INLC-INL-IDDC                              
100200                                                                          
100300     MOVE 'R32'             TO INLC-INL-IDPTYP                            
100400     MOVE DAGENS-DATUM      TO INLC-INL-TIINLMOT                          
100500                               INLC-INL-TIINLINL                          
100600     MOVE ZERO              TO INLC-INL-ADLAGOMR                          
100700                               INLC-INL-ADGANG                            
100800                               INLC-INL-ADPLATS                           
100900                               INLC-INL-IDDISTR                           
101000                               INLC-INL-IDKUNDNR                          
101100                               INLC-INL-IDKOLLI                           
101200                               INLC-INL-KVAVIS                            
101300                               INLC-INL-TIINLMTI                          
101400                               INLC-INL-TIINLITI                          
101500                               INLC-INL-IDLOPNRM                          
101600                               INLC-INL-KDFRAKT                           
101700                               INLC-INL-KVART-SKROT                       
101800                               INLC-INL-PRARTNTO                          
101900                               INLC-INL-PRKURS                            
102000                               INLC-INL-TIBERANK                          
102100     MOVE +8                TO INLC-INL-KDRT                              
102200     MOVE NEJ               TO INLC-INL-FLPRIO                            
102300                               INLC-INL-FLMAKUL                           
102400                               INLC-INL-FLSKAKOL                          
102500                               INLC-INL-FLTULLST                          
102600     MOVE SPACE             TO INLC-INL-IDANALYS                          
102700                               INLC-INL-IDKST                             
102800     MOVE ZERO              TO INLC-INL-IDKONTO                           
102900                               INLC-INL-KVTULRET                          
103000                               INLC-INL-KVRETUR                           
103100                               INLC-INL-KDAVVANT                          
103200                               INLC-INL-TIAVIDAT                          
103300     MOVE SPACE             TO INLC-INL-KDKOLLI                           
103400                               INLC-INL-IDKUNDRF                          
103500                               INLC-INL-KDVALISO                          
103600                               INLC-INL-IDUSER-003                        
103700                               INLC-INL-IDDC-LEV                          
103800     MOVE '1441 '           TO INLC-INL-IDLEVNR                           
103900     PERFORM IMS-ISRT-WDL611                                              
104000     .                                                                    
104100     EJECT                                                                
104200 HD-SKAPA-SALDOLOGG-WDK6 SECTION.                                         
104300                                                                          
104400     MOVE W-IDARTNR                 TO LOGG-IDARTNR                       
104500     MOVE 9                         TO LOGG-IDSEKVNR                      
104600     MOVE 'MISC'                    TO LOGG-IDHUVTYP                      
104700     MOVE 'R32'                     TO LOGG-IDSUBTYP                      
104800     MOVE 'W5010900'                TO LOGG-IDPGM                         
104900     MOVE '5109'                    TO LOGG-IDTRANS                       
105000     MOVE MSG-SIGNON-USERID         TO LOGG-IDUSER                        
105100     MOVE SPACE                     TO LOGG-REF                           
105200     MOVE MID-IDDC-SEND             TO LOGG-IDDC-SEND                     
105300     MOVE MID-IDCLEARREF            TO LOGG-IDCLEARREF                    
105400     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS                
105500     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS-PAV            
105600     MOVE SPACE                     TO LOGG-IDTECKEN-KVEFRS               
105700     MOVE WS-KVCLEAR                TO LOGG-KVART-SALDO                   
105800     COMPUTE LOGG-KVAKS = ARTC-CLAG-KVAKS-CDC +                           
105900                          ARTC-CLAG-KVAKS-T                               
106000                                                                          
106100     MOVE ARTC-CLAG-KVAKS-PAV       TO LOGG-KVAKS-PAV                     
106200     MOVE ARTC-CLAG-KVEFRS          TO LOGG-KVEFRS                        
106300     MOVE ARTC-CLAG-KVLS            TO LOGG-KVLS                          
106400     MOVE ZERO                      TO LOGG-DAREGDAT-LADD                 
106500     MOVE FUNCTION CURRENT-DATE(1:8) TO LOGG-DATUM                        
106600     COMPUTE LOGG-DAREGDAT-9KOMPL = 999999999 - LOGG-DATUM                
106700     ACCEPT WLOGG-TID FROM TIME                                           
106800     COMPUTE LOGG-TIKLOCK-9KOMPL = 999999999 - WLOGG-TID                  
106900                                                                          
107000     PERFORM IMS-ISRT-WDL901                                              
107100     IF SEGMENT-FINNS-REDAN                                               
107200       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
107300          ADD -1 TO LOGG-IDSEKVNR                                         
107400          PERFORM IMS-ISRT-WDL901                                         
107500       END-PERFORM                                                        
107600     END-IF                                                               
107700     .                                                                    
107800     EJECT                                                                
107900                                                                          
108000 HF-SKAPA-SALDOLOGG-WDK7 SECTION.                                         
108100                                                                          
108200     MOVE W-IDARTNR                 TO LOGG-IDARTNR                       
108300     MOVE 9                         TO LOGG-IDSEKVNR                      
108400     MOVE 'MISC'                    TO LOGG-IDHUVTYP                      
108500     MOVE 'R32'                     TO LOGG-IDSUBTYP                      
108600     MOVE 'W5010900'                TO LOGG-IDPGM                         
108700     MOVE '5109'                    TO LOGG-IDTRANS                       
108800     MOVE MSG-SIGNON-USERID         TO LOGG-IDUSER                        
108900     MOVE SPACE                     TO LOGG-REF                           
109000     MOVE MID-IDDC-SEND             TO LOGG-IDDC-SEND                     
109100     MOVE MID-IDCLEARREF            TO LOGG-IDCLEARREF                    
109200     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS                
109300     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS-PAV            
109400     MOVE SPACE                     TO LOGG-IDTECKEN-KVEFRS               
109500     MOVE WS-KVCLEAR                TO LOGG-KVART-SALDO                   
109600     MOVE ARTS-SLAG-KVAKS-SDC       TO LOGG-KVAKS                         
109700     MOVE ARTS-SLAG-KVAKS-PAV       TO LOGG-KVAKS-PAV                     
109800     MOVE ARTS-SLAG-KVEFRS          TO LOGG-KVEFRS                        
109900     MOVE ARTS-SLAG-KVLS            TO LOGG-KVLS                          
110000     MOVE ZERO                      TO LOGG-DAREGDAT-LADD                 
110100     MOVE FUNCTION CURRENT-DATE(1:8) TO LOGG-DATUM                        
110200     COMPUTE LOGG-DAREGDAT-9KOMPL = 999999999 - LOGG-DATUM                
110300     ACCEPT WLOGG-TID FROM TIME                                           
110400     COMPUTE LOGG-TIKLOCK-9KOMPL = 999999999 - WLOGG-TID                  
110500                                                                          
110600     PERFORM IMS-ISRT-WDL901                                              
110700     IF SEGMENT-FINNS-REDAN                                               
110800       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
110900          ADD -1 TO LOGG-IDSEKVNR                                         
111000          PERFORM IMS-ISRT-WDL901                                         
111100       END-PERFORM                                                        
111200     END-IF                                                               
111300     .                                                                    
111400     EJECT                                                                
111500                                                                          
111600 S01-SKAPA-IDINLEV SECTION.                                               
111700                                                                          
111800     ACCEPT WS-TIAAMMDD FROM DATE                                         
111900     ACCEPT WS-HHMMSSTH FROM TIME                                         
112000     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-TISEKEL                       
112100                                                                          
112200     COMPUTE WS-DAINLEV = 9999999999999999 - WS-TIAAAAMMDDHHMMSSTH        
112300     .                                                                    
112400     EJECT                                                                
112500 S01-SKAPA-IDINLEV-SEND SECTION.                                          
112600                                                                          
112700     ACCEPT WS-TIAAMMDD FROM DATE                                         
112800     ACCEPT WS-HHMMSSTH FROM TIME                                         
112900     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-TISEKEL                       
113000                                                                          
113100     COMPUTE WS-DAINLEV = 9999999999999993 - WS-TIAAAAMMDDHHMMSSTH        
113200     .                                                                    
113300     EJECT                                                                
113400 S01-SKAPA-IDINLEV-MOT SECTION.                                           
113500                                                                          
113600     ACCEPT WS-TIAAMMDD FROM DATE                                         
113700     ACCEPT WS-HHMMSSTH FROM TIME                                         
113800     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-TISEKEL                       
113900                                                                          
114000     COMPUTE WS-DAINLEV = 9999999999999997 - WS-TIAAAAMMDDHHMMSSTH        
114100     .                                                                    
114200     EJECT                                                                
114300 S03-UPPDATERA-WDR8-WDR9  SECTION.                                        
114400                                                                          
114500     MOVE WS-IDDC-SEND       TO WS-IDDC                                   
114600     IF NDC-CN                                                            
114700       MOVE 'W5010900'       TO EKO-FIL-IDPGM                             
114800       MOVE FUNCTION CURRENT-DATE (1:8) TO W-DAGENS-DATUM                 
114900       MOVE W-DAGENS-DATUM   TO EKO-FIL-TIREGDAT                          
115000       ACCEPT EKO-FIL-TIKLOCK FROM TIME                                   
115100       MOVE 1                TO EKO-FIL-IDSEKVNR                          
115200       MOVE 'W570EKHA'       TO EKO-FIL-IDCPYTXT                          
115300       PERFORM S03A-UPPDATERA-WDR8                                        
115400     ELSE                                                                 
115500       IF NDC-US                                                          
115600         MOVE 'W5010900'       TO EKO-FIL-IDPGM                           
115700         MOVE FUNCTION CURRENT-DATE (1:8) TO W-DAGENS-DATUM               
115800         MOVE W-DAGENS-DATUM   TO EKO-FIL-TIREGDAT                        
115900         ACCEPT EKO-FIL-TIKLOCK FROM TIME                                 
116000         MOVE 1                TO EKO-FIL-IDSEKVNR                        
116100         MOVE 'W561EKHA'       TO EKO-FIL-IDCPYTXT                        
116200         PERFORM S03A-UPPDATERA-WDR8                                      
116300       ELSE                                                               
116400         MOVE 'W5010900'     TO FIL-IDPGM IN FIL-WDR901                   
116500         MOVE FUNCTION CURRENT-DATE (1:8) TO W-DAGENS-DATUM               
116600         MOVE W-DAGENS-DATUM TO FIL-DAREGDAT                              
116700         ACCEPT FIL-TIKLOCK IN FIL-WDR901 FROM TIME                       
116800         MOVE 1              TO FIL-IDSEKVNR IN FIL-WDR901                
116900         MOVE 'W510EKHA'     TO FIL-IDCPYTXT IN FIL-WDR901                
117000         MOVE MSG-SIGNON-USERID TO FIL-IDUSER                             
117100         PERFORM S03B-UPPDATERA-WDR9                                      
117200       END-IF                                                             
117300     END-IF                                                               
117400     .                                                                    
117500     EJECT                                                                
117600 S03A-UPPDATERA-WDR8  SECTION.                                            
117700     MOVE W-IDARTNR        TO EKO-EKH-IDARTNR                             
117800     MOVE '403'            TO EKO-EKH-KDEKHHT                             
117900     MOVE '410'            TO EKO-EKH-KDEKSHT                             
118000     MOVE 'DET'            TO EKO-EKH-KDEKNIVA                            
118100     MOVE WS-IDDC-SEND     TO EKO-EKH-IDDC-SEND                           
118200     MOVE WS-IDDC-MOT      TO EKO-EKH-IDDC-REC                            
118300     MOVE +0               TO EKO-EKH-IDDISTR                             
118400     MOVE +0               TO EKO-EKH-IDKUNDNR                            
118500                                                                          
118600     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
118700     MOVE W-IDARTNR        TO CIA-IDARTBET-IN                             
118800     CALL W009CIA USING       CIA-W009CIA                                 
118900     MOVE CIA-IDARTBET-UT  TO EKO-EKH-IDVERGL                             
119000                                                                          
119100     MOVE W-DAGENS-DATUM   TO EKO-EKH-DAVERDAT                            
119200     MOVE ARTC-ART-KDPRODSL TO EKO-EKH-KDPRODSL                           
119300     MOVE ZERO             TO EKO-EKH-KDPSLLOC                            
119400     MOVE SPACE            TO EKO-EKH-FLLSBOK                             
119500     MOVE 'CNY'            TO EKO-EKH-KDVALISO                            
119600     MOVE 1.00             TO EKO-EKH-PRKURS                              
119700     MOVE ZERO             TO EKO-EKH-PRARTNTO                            
119800     MOVE ZERO             TO EKO-EKH-PRARTSJK                            
119900     MOVE ZERO             TO EKO-EKH-PRHEMTAG                            
120000     IF WS-PRAVCOST-SEND > 0                                              
120100        MOVE WS-PRAVCOST-SEND  TO EKO-EKH-PRARTSTD                        
120200     ELSE                                                                 
120300       IF WS-PRAVCOST-MOT > 0                                             
120400          MOVE WS-PRAVCOST-MOT  TO EKO-EKH-PRARTSTD                       
120500       END-IF                                                             
120600     END-IF                                                               
120700     MOVE ZERO             TO EKO-EKH-PRLANDCO                            
120800     MOVE ZERO             TO EKO-EKH-PRINK                               
120900     MOVE ZERO             TO EKO-EKH-PRDIRLON                            
121000     MOVE ZERO             TO EKO-EKH-PRDMTRL                             
121100     MOVE ZERO             TO EKO-EKH-PROVRPAL                            
121200     MOVE ZERO             TO EKO-EKH-SUBEL                               
121300     MOVE WS-KVCLEAR       TO EKO-EKH-KVANTAL                             
121400     MOVE '5109'           TO EKO-EKH-IDTRANS                             
121500     MOVE ZERO                   TO EKO-EKH-BEVAT                         
121600                                    EKO-EKH-IDANALYS                      
121700                                    EKO-EKH-IDKONTO                       
121800                                    EKO-EKH-KDANMORS                      
121900                                    EKO-EKH-KDFRAKT                       
122000                                    EKO-EKH-SUVAT                         
122100     MOVE ZERO                   TO EKO-EKH-DAAVIDAT                      
122200                                    EKO-EKH-IDAVINR                       
122300                                    EKO-EKH-KDAVVTYP                      
122400                                    EKO-EKH-KDRT                          
122500                                    EKO-EKH-KVANTMOT                      
122600                                    EKO-EKH-KVAVIS                        
122700     MOVE WS-KDSORT              TO EKO-EKH-KDSORT                        
122800     MOVE 'CN05'                 TO EKO-EKH-KDTRADP                       
122900     MOVE SPACE                  TO EKO-EKH-IDLEVNR                       
123000                                    EKO-EKH-IDKST                         
123100     MOVE SPACE                  TO EKO-EKH-FLDCET                        
123200     MOVE SPACE                  TO EKO-EKH-IDKUNDRF                      
123300     MOVE SPACE                  TO EKO-EKH-IDFAKT-EXP                    
123400                                                                          
123500     PERFORM IMS-ISRT-WDR801                                              
123600     PERFORM UNTIL SEGMENT-FINNS                                          
123700      ADD +1  TO EKO-FIL-IDSEKVNR                                         
123800      PERFORM IMS-ISRT-WDR801                                             
123900     END-PERFORM                                                          
124000     .                                                                    
124100     EJECT                                                                
124200 S03B-UPPDATERA-WDR9  SECTION.                                            
124300                                                                          
124400     MOVE W-IDARTNR        TO EKH-IDARTNR                                 
124500     MOVE '403'            TO EKH-KDEKHHT                                 
124600     MOVE '410'            TO EKH-KDEKSHT                                 
124700     MOVE 'DET'            TO EKH-KDEKNIVA                                
124800     MOVE WS-IDDC-SEND     TO EKH-IDDC-SEND                               
124900     MOVE WS-IDDC-MOT      TO EKH-IDDC-REC                                
125000     MOVE +0               TO EKH-IDDISTR                                 
125100     MOVE +0               TO EKH-IDKUNDNR                                
125200                                                                          
125300     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
125400     MOVE W-IDARTNR        TO CIA-IDARTBET-IN                             
125500     CALL W009CIA USING       CIA-W009CIA                                 
125600     MOVE CIA-IDARTBET-UT TO EKH-IDVERGL                                  
125700                                                                          
125800     MOVE W-DAGENS-DATUM   TO EKH-DAVERDAT                                
125900     MOVE ARTC-ART-KDPRODSL TO EKH-KDPRODSL                               
126000     MOVE ZERO             TO EKH-KDPSLLOC                                
126100     MOVE SPACE            TO EKH-FLLSBOK                                 
126200     MOVE 'SEK'            TO EKH-KDVALISO                                
126300     MOVE 1.00             TO EKH-PRKURS                                  
126400     MOVE ZERO             TO EKH-PRARTNTO                                
126500     MOVE ZERO             TO EKH-PRARTSJK                                
126600     MOVE ZERO             TO EKH-PRHEMTAG                                
126700     MOVE W-PRARTSTD       TO EKH-PRARTSTD                                
126800     MOVE ZERO             TO EKH-PRLANDCO                                
126900     MOVE ZERO             TO EKH-PRINK                                   
127000     MOVE ZERO             TO EKH-PRDIRLON                                
127100     MOVE ZERO             TO EKH-PRDMTRL                                 
127200     MOVE ZERO             TO EKH-PROVRPAL                                
127300     MOVE ZERO             TO EKH-SUBEL                                   
127400     MOVE WS-KVCLEAR       TO EKH-KVANTAL                                 
127500     MOVE '5109'           TO EKH-IDTRANS                                 
127600     MOVE ZERO                   TO EKH-BEVAT                             
127700                                    EKH-IDANALYS                          
127800                                    EKH-IDKONTO                           
127900                                    EKH-KDANMORS                          
128000                                    EKH-KDFRAKT                           
128100                                    EKH-SUVAT                             
128200     MOVE ZERO                   TO EKH-DAAVIDAT                          
128300                                    EKH-IDAVINR                           
128400                                    EKH-KDAVVTYP                          
128500                                    EKH-KDRT                              
128600                                    EKH-KVANTMOT                          
128700                                    EKH-KVAVIS                            
128800     MOVE WS-KDSORT              TO EKH-KDSORT                            
128900     MOVE 'SEPV'                 TO EKH-KDTRADP                           
129000     MOVE SPACE                  TO EKH-IDLEVNR                           
129100                                    EKH-IDKST                             
129200     MOVE SPACE                  TO EKH-FLDCET                            
129300     MOVE SPACE                  TO EKH-IDKUNDRF                          
129400     MOVE SPACE                  TO EKH-IDFAKT-EXP                        
129500                                                                          
129600     PERFORM IMS-ISRT-WDR901                                              
129700     PERFORM UNTIL SEGMENT-FINNS                                          
129800       ADD +1  TO FIL-IDSEKVNR IN FIL-WDR901                              
129900       PERFORM IMS-ISRT-WDR901                                            
130000     END-PERFORM                                                          
130100     .                                                                    
130200     EJECT                                                                
130300 MFS-RENSA-FAELT-UT SECTION.                                              
130400                                                                          
130500*    --- ALLA UTDATA-FÄLT                                                 
130600     MOVE MFS-RENSA-FAELT TO MOD-KVCLEAR                                  
130700                             MOD-IDDC-SEND                                
130800                             MOD-IDDC-MOT                                 
130900                             MOD-IDCLEARREF                               
131000     .                                                                    
131100     SKIP3                                                                
131200 MFS-RENSA-FAELT-IN SECTION.                                              
131300                                                                          
131400*    --- ALLA INDATA-FÄLT                                                 
131500     MOVE MFS-RENSA-FAELT TO MOD-KVCLEAR                                  
131600                             MOD-IDDC-SEND                                
131700                             MOD-IDDC-MOT                                 
131800                             MOD-IDCLEARREF                               
131900     .                                                                    
132000     EJECT                                                                
132100 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
132200                                                                          
132300*    --- ALLA UTDATA-FÄLT                                                 
132400     MOVE MFS-ROER-EJ-FAELT TO MOD-KVCLEAR                                
132500                             MOD-IDDC-SEND                                
132600                             MOD-IDDC-MOT                                 
132700                             MOD-IDCLEARREF                               
132800     .                                                                    
132900     SKIP3                                                                
133000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
133100                                                                          
133200*    --- ALLA INDATA-FÄLT                                                 
133300     MOVE MFS-ROER-EJ-FAELT TO MOD-KVCLEAR                                
133400                             MOD-IDDC-SEND                                
133500                             MOD-IDDC-MOT                                 
133600                             MOD-IDCLEARREF                               
133700     .                                                                    
133800     EJECT                                                                
133900* --- IMS SEKTIONER ---                                                   
134000     SKIP3                                                                
134100 IMS-GET-MSG SECTION.                                                     
134200                                                                          
134300     MOVE '  QC' TO GODK-STATUSKODER                                      
134400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
134500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
134600     PERFORM IMS-STATUSKONTROLL                                           
134700     .                                                                    
134800     SKIP3                                                                
134900 IMS-INSERT-MSG SECTION.                                                  
135000                                                                          
135100     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
135200       MOVE '0' TO MFS-KDHUVOMR                                           
135300     END-IF                                                               
135400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
135500     MOVE SPACE TO GODK-STATUSKODER                                       
135600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
135700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
135800     PERFORM IMS-STATUSKONTROLL                                           
135900     .                                                                    
136000     EJECT                                                                
136100 IMS-GHU-WDL601 SECTION.                                                  
136200                                                                          
136300     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
136400          DELIMITED BY SIZE INTO SSA1                                     
136500     MOVE '  GE' TO GODK-STATUSKODER                                      
136600     CALL CBLTDLI USING GHU INLC-PCB DLI-IO-AREA SSA1                     
136700     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
136800     PERFORM IMS-STATUSKONTROLL                                           
136900     .                                                                    
137000     EJECT                                                                
137100 IMS-ISRT-WDL601 SECTION.                                                 
137200     MOVE 'WLINLC01 ' TO SSA1                                             
137300     MOVE '  II' TO GODK-STATUSKODER                                      
137400     CALL CBLTDLI USING ISRT INLC-PCB DLI-IO-AREA SSA1                    
137500     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
137600     PERFORM IMS-STATUSKONTROLL                                           
137700     .                                                                    
137800     SKIP2                                                                
137900 IMS-ISRT-WDL611 SECTION.                                                 
138000     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
138100          DELIMITED BY SIZE INTO SSA1                                     
138200     MOVE 'WLINLC11 ' TO SSA2                                             
138300     MOVE '  II' TO GODK-STATUSKODER                                      
138400     CALL CBLTDLI USING ISRT INLC-PCB DLI-IO-AREA SSA1 SSA2               
138500     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
138600     PERFORM IMS-STATUSKONTROLL                                           
138700     .                                                                    
138800     EJECT                                                                
138900 IMS-GHU-WDL201 SECTION.                                                  
139000                                                                          
139100     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
139200          DELIMITED BY SIZE INTO SSA1                                     
139300     MOVE '  GE' TO GODK-STATUSKODER                                      
139400     CALL CBLTDLI USING GHU INLE-PCB DLI-IO-AREA SSA1                     
139500     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
139600     PERFORM IMS-STATUSKONTROLL                                           
139700     .                                                                    
139800     SKIP2                                                                
139900 IMS-ISRT-WDL201 SECTION.                                                 
140000     MOVE 'WLINLE01 ' TO SSA1                                             
140100     MOVE '  II' TO GODK-STATUSKODER                                      
140200     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA SSA1                    
140300     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
140400     PERFORM IMS-STATUSKONTROLL                                           
140500     .                                                                    
140600     SKIP2                                                                
140700 IMS-ISRT-WDL211 SECTION.                                                 
140800     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
140900          DELIMITED BY SIZE INTO SSA1                                     
141000     MOVE 'WLINLE11 ' TO SSA2                                             
141100     MOVE '  II' TO GODK-STATUSKODER                                      
141200     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA SSA1 SSA2               
141300     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
141400     PERFORM IMS-STATUSKONTROLL                                           
141500     .                                                                    
141600     SKIP2                                                                
141700 IMS-ISRT-WDL221 SECTION.                                                 
141800     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
141900          DELIMITED BY SIZE INTO SSA1                                     
142000     STRING 'WLINLE11(DAINLEV  =' W-DAINLEV-X ')'                         
142100          DELIMITED BY SIZE INTO SSA2                                     
142200     MOVE 'WLINLE21 ' TO SSA3                                             
142300     MOVE '  II' TO GODK-STATUSKODER                                      
142400     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
142500     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
142600     PERFORM IMS-STATUSKONTROLL                                           
142700     .                                                                    
142800     EJECT                                                                
142900 IMS-GU-WDK601 SECTION.                                                   
143000                                                                          
143100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
143200          DELIMITED BY SIZE INTO SSA1                                     
143300     MOVE '  GE' TO GODK-STATUSKODER                                      
143400     CALL CBLTDLI USING GU ARTC-PCB WLARTC01 SSA1                         
143500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
143600     PERFORM IMS-STATUSKONTROLL                                           
143700     .                                                                    
143800     SKIP2                                                                
143900 IMS-GHU-WDK611 SECTION.                                                  
144000                                                                          
144100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
144200          DELIMITED BY SIZE INTO SSA1                                     
144300     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
144400          DELIMITED BY SIZE INTO SSA2                                     
144500     MOVE '  GE' TO GODK-STATUSKODER                                      
144600     CALL CBLTDLI USING GHU ARTC-PCB WLARTC11 SSA1 SSA2                   
144700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
144800     PERFORM IMS-STATUSKONTROLL                                           
144900     .                                                                    
145000     SKIP2                                                                
145100 IMS-REPL-ARTC SECTION.                                                   
145200     MOVE '  ' TO GODK-STATUSKODER                                        
145300     CALL CBLTDLI USING REPL ARTC-PCB WLARTC11                            
145400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
145500     PERFORM IMS-STATUSKONTROLL                                           
145600     .                                                                    
145700     EJECT                                                                
145800 IMS-GU-WDK711 SECTION.                                                   
145900                                                                          
146000     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
146100          DELIMITED BY SIZE INTO SSA1                                     
146200     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
146300          DELIMITED BY SIZE INTO SSA2                                     
146400     MOVE '  GE' TO GODK-STATUSKODER                                      
146500     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA3 SSA1 SSA2                
146600     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
146700     PERFORM IMS-STATUSKONTROLL                                           
146800     .                                                                    
146900     SKIP3                                                                
147000 IMS-GHU-WDK711 SECTION.                                                  
147100                                                                          
147200     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
147300          DELIMITED BY SIZE INTO SSA1                                     
147400     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
147500          DELIMITED BY SIZE INTO SSA2                                     
147600     MOVE '  GE' TO GODK-STATUSKODER                                      
147700     CALL CBLTDLI USING GHU ARTS-PCB DLI-IO-AREA3 SSA1 SSA2               
147800     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
147900     PERFORM IMS-STATUSKONTROLL                                           
148000     .                                                                    
148100     SKIP3                                                                
148200 IMS-REPL-ARTS SECTION.                                                   
148300     MOVE '  ' TO GODK-STATUSKODER                                        
148400     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-AREA3                        
148500     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
148600     PERFORM IMS-STATUSKONTROLL                                           
148700     .                                                                    
148800     EJECT                                                                
148900 IMS-ISRT-WDL901 SECTION.                                                 
149000     MOVE 'WLLOGA01 ' TO SSA1                                             
149100     MOVE '  II' TO GODK-STATUSKODER                                      
149200     CALL CBLTDLI USING ISRT LOGA-PCB WLLOGA01 SSA1                       
149300     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
149400     PERFORM IMS-STATUSKONTROLL                                           
149500     .                                                                    
149600     EJECT                                                                
149700 IMS-ISRT-WDR801 SECTION.                                                 
149800     MOVE 'WDR801   ' TO SSA1                                             
149900     MOVE '  II' TO GODK-STATUSKODER                                      
150000     CALL CBLTDLI USING ISRT WDR8-PCB EKO-WDR801 SSA1                     
150100     MOVE WDR8-STATUS-CODE TO STATUS-WS                                   
150200     PERFORM IMS-STATUSKONTROLL                                           
150300     .                                                                    
150400     EJECT                                                                
150500                                                                          
150600 IMS-ISRT-WDR901 SECTION.                                                 
150700     MOVE 'WLSAPA01 ' TO SSA1                                             
150800     MOVE '  II' TO GODK-STATUSKODER                                      
150900     CALL CBLTDLI USING ISRT SAPA-PCB WLSAPA01 SSA1                       
151000     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
151100     PERFORM IMS-STATUSKONTROLL                                           
151200     .                                                                    
151300     EJECT                                                                
151400                                                                          
151500 IMS-GU-WDB601-SEND   SECTION.                                            
151600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
151700          DELIMITED BY SIZE INTO SSA1                                     
151800     MOVE '  GE' TO GODK-STATUSKODER                                      
151900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-SEND SSA1            
152000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
152100     PERFORM IMS-STATUSKONTROLL                                           
152200     IF SEGMENT-SAKNAS                                                    
152300         MOVE SPACE TO SEND-DCS-KDDC                                      
152400     END-IF                                                               
152500     .                                                                    
152600                                                                          
152700 IMS-GU-WDB601        SECTION.                                            
152800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
152900          DELIMITED BY SIZE INTO SSA1                                     
153000     MOVE '  GE' TO GODK-STATUSKODER                                      
153100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
153200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
153300     PERFORM IMS-STATUSKONTROLL                                           
153400     IF SEGMENT-SAKNAS                                                    
153500         MOVE SPACE TO DCS-KDDC                                           
153600                       DCS-IDLEVNR-DC                                     
153700     END-IF                                                               
153800     .                                                                    
153900                                                                          
154000 IMS-GU-WDB601-MOT    SECTION.                                            
154100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
154200          DELIMITED BY SIZE INTO SSA1                                     
154300     MOVE '  GE' TO GODK-STATUSKODER                                      
154400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-MOT  SSA1            
154500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
154600     PERFORM IMS-STATUSKONTROLL                                           
154700     IF SEGMENT-SAKNAS                                                    
154800         MOVE SPACE TO MOT-DCS-KDDC                                       
154900     END-IF                                                               
155000     .                                                                    
155100                                                                          
155200 IMS-STATUSKONTROLL SECTION.                                              
155300                                                                          
155400     SET STATUS-IX TO 1                                                   
155500     SEARCH GODK-STATUS                                                   
155600       AT END                                                             
155700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
155800         DELIMITED BY SIZE INTO FELTEXT                                   
155900         CALL FELLOG                                                      
156000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
156100         CONTINUE                                                         
156200     END-SEARCH                                                           
156300     .                                                                    
