000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3018210.                                                
000300 AUTHOR.         GAVIN SMITH.                                             
000400 DATE-WRITTEN.   98/12/13.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700****************************************************************          
000800*    FUNKTION:                                                            
000900*        THIS PGM IS USED TO GENERATE CLEARING TRANSACTIONS               
001000*        BETWEEN DIFFERENT SDC'S FOR EXCHANGE PARTS.                      
001100*                                                                         
001200*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001300*        PROGRAMMET UPPDATERAR WDK7                                       
001400*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001500*        PROGRAMMET UPPDATERAR WL3171 (WDGX) PÅ WDR4                      
001600*        PROGRAMMET UPPDATERAR WL3169 (WDGX) (KOLLI-LÖPNR/DC)             
001700*        PROGRAMMET UPPDATERAR WLXXLD (WDGX) PÅ WDR1                      
001900*        PROGRAMMET UPPDATERAR WLLOGA (WDL9)                              
002000*                                                                         
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSAKTION: W3T182                                              
002400*        MID:         W3I182N1                                            
002500*                                                                         
002600*    UTDATA.                                                              
002700*        MOD:         W3O182N1                                            
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W3018210'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300 01  ALL-SPACE.                                                           
004400     03 FILLER                   PIC X(80)   VALUE SPACE.                 
004500 01  ALL-PLUS.                                                            
004600     03 FILLER                   PIC X(80)   VALUE ALL '+'.               
004700 01  ALL-SPACE-UTF8.                                                      
004800     03  FILLER                  PIC X(25)  VALUE ALL X'20'.              
004900 01  ALL-PLUS-UTF8.                                                       
005000     03  FILLER                  PIC X(25)  VALUE ALL X'2B'.              
005100                                                                          
005200 01  WS-IDSKYLT-SE               PIC X(3)   VALUE 'S  '.                  
005300 01  WS-IDSKYLT-GB               PIC X(3)   VALUE 'GB '.                  
005400 01  WS-IDSKYLT-CN               PIC X(3)   VALUE 'RCN'.                  
005500 01  WS-CP-UNICODE               PIC X(4)   VALUE 'UTF8'.                 
005600 01  WS-CP-EBCDIC                PIC X(3)   VALUE '278'.                  
005700                                                                          
005800 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
005900 77  WS-SDC-91                   PIC X(2)    VALUE '91'.                  
006000                                                                          
006100*    --- ARBETSFÄLT FÖR DIVERSE INDEX                                     
006200 77  W-SUM-OF                    PIC S9(7)   VALUE ZERO.                  
006300 77  INDX                        PIC 999     VALUE ZERO.                  
006400 77  W-KDFAKT                    PIC 9999999 VALUE ZERO.                  
006500 77  W-KVANTAL                   PIC S9(6)   VALUE ZERO.                  
006600 77  W-DCTEST                    PIC X       VALUE 'N'.                   
006700 77  W-NY-KOLLI                  PIC X       VALUE SPACE.                 
006800 77  W-TOT-CURS                  PIC X       VALUE SPACE.                 
006900 77  W-KOLLI-VIKT-IN             PIC S9(6)V9 COMP-3 VALUE ZERO.           
007000 77  W-KOLLI-VOL-IN           PIC S9(4)V9(3) COMP-3 VALUE ZERO.           
007100 77  W-TOT-VIKT-IN               PIC S9(6)V9 COMP-3 VALUE ZERO.           
007200 77  WS-TOT-VIKT-IN              PIC S9(6)V9 COMP-3 VALUE ZERO.           
007300 77  WS-TOT-VOL-IN            PIC S9(4)V9(3) COMP-3 VALUE ZERO.           
007400 77  W-TOT-VOL-IN             PIC S9(4)V9(3) COMP-3 VALUE ZERO.           
007500*                                                                         
007600 77  RED-INDX1                   PIC S9(1)   VALUE ZERO  COMP-3.          
007700*                                                                         
007800 01  TEST-IDARTNR                PIC 9(9)  COMP-3.                        
007900*01  FILLER  -COPY WWBYT03  -RED  TEST-IDARTNR.                           
008600 77  IX                          PIC S9(7)   COMP-3 VALUE ZERO.           
008700 77  IX2                         PIC S9(7)   COMP-3 VALUE ZERO.           
008800 77  W-SAVE-NEXT-IDKOLLI         PIC 9(5).                                
008900 77  W-SAVE-NEXT-IDARTNO         PIC 9(9).                                
009000 01  WS-FAKTVAL                  PIC S9(7)V99 COMP-3 VALUE ZERO.          
009100*    --- DATUMFÄLT                                                        
009200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009300 01  FILLER REDEFINES DAGENS-DATUM.                                       
009400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009700                                                                          
009800 01  W-DAGENS-DATUM              PIC 9(8).                                
009900 01  INVOICE-DATE                PIC S9(7)   VALUE ZERO COMP-3.           
010000 01  TRANS-TID                   PIC  9(9).                               
010100                                                                          
010200     EJECT                                                                
010300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
010400                                                                          
010500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
010600     88  INDATA-OK                           VALUE 'J'.                   
010700     88  INDATA-FEL                          VALUE 'N'.                   
010800     88  INDATA-FIPPLE                       VALUE 'Z'.                   
010900                                                                          
011000 77  INVOICE-SW                  PIC X       VALUE 'J'.                   
011100     88  INVOICE-FOUND                       VALUE 'J'.                   
011200     88  INVOICE-NOT-FOUND                   VALUE 'N'.                   
011300                                                                          
011400 77  LINE-DATA-ALL-PLUS-SW       PIC X       VALUE 'J'.                   
011500     88  LINE-DATA-ALL-PLUS                  VALUE 'J'.                   
011600     88  LINE-DATA-NOT-ALL-PLUS              VALUE 'N'.                   
011700                                                                          
011800 77  IN-DATA-ALL-PLUS-SW         PIC X       VALUE 'J'.                   
011900     88  IN-DATA-ALL-PLUS                    VALUE 'J'.                   
012000     88  IN-DATA-NOT-ALL-PLUS                VALUE 'N'.                   
012100                                                                          
012200 77  CASE-SW                     PIC X       VALUE 'J'.                   
012300     88  CASE-FOUND                          VALUE 'J'.                   
012400     88  CASE-NOT-FOUND                      VALUE 'N'.                   
012500                                                                          
012600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
012700     88  NYCKLAR-OK                          VALUE 'J'.                   
012800     88  NYCKLAR-FEL                         VALUE 'N'.                   
012900                                                                          
013000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
013100 01  GENERELLA-SUBPROGRAM.                                                
013200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013400     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
013500     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
013600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.             
013700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.              
013800     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
013900     EJECT                                                                
014000 01  MESSAGE-CODES.                                                       
014100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '020'.                 
014200     03  INF-PRESS-PF11          PIC X(3)    VALUE '013'.                 
014300     03  INF-FIRST-PAGE          PIC X(3)    VALUE '010'.                 
014400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '014'.                 
014500     03  ERR-NOT-ALLOWED         PIC X(3)    VALUE '007'.                 
014600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
014700     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '011'.                 
014800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
014900     03  PARTNO-MISSING          PIC X(3)    VALUE '025'.                 
015000     03  INF-NEW-CASE-CREATED    PIC X(3)    VALUE '405'.                 
015100     03  ERR-INV-WITHOUT-ARTNR   PIC X(3)    VALUE '406'.                 
015200     03  ERR-UPD-WEIGHT-FOR-CASE PIC X(3)    VALUE '126'.                 
015300     03  ERR-UPDATE-WEIGHT       PIC X(3)    VALUE '041'.                 
015400     03  ERR-UPDATE-VOLUME       PIC X(3)    VALUE '041'.                 
015500     03  INF-TOT-WEIGHT-UPDATED  PIC X(3)    VALUE '407'.                 
015600     03  INF-WEIGHT-OF-CASE-UPDATED                                       
015700                                 PIC X(3)    VALUE '407'.                 
015800     03  INF-CANT-DEL-ALL-CASES  PIC X(3)    VALUE '408'.                 
015900     03  INF-CASE-DEL-ENTER-LAST-CASE                                     
016000                                 PIC X(3)    VALUE '409'.                 
016100     03  INF-PARTS-DEL-FROM-CASE PIC X(3)    VALUE '410'.                 
016200     03  INF-PARTS-ADDED-TO-CASE PIC X(3)    VALUE '411'.                 
016300     03  INF-PROFORMA-PRINTING   PIC X(3)    VALUE '412'.                 
016400     EJECT                                                                
016500 01  FILLER                      PIC X(16)   VALUE 'W005WDK7'.            
016600*01  -COPY W005WDK7                                                       
016700     EJECT                                                                
016800*    --- PARAMETRAR TILL SUBPROGRAM WTRAUTF8                              
016900*                                                                         
017000 01  FILLER                 PIC X(16)   VALUE 'WTRAUTF8-AREA   '.         
017100*01 -COPY WTRAUTF8                                                        
017200     SKIP3                                                                
017300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
017400*                                                                         
017500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
017600     SKIP3                                                                
017700*01 -COPY WMSGINIT                                                        
017800*                                                                         
017900 01  SPAR-AREA.                                                           
018000        03 FILLER                PIC X(200).                              
018100        03 SAVE-IDFAKT-X                            .                     
018200        05 SAVE-IDFAKT           PIC 9(7) VALUE ZERO.                     
018300        03 SAVE-IDKOLLI-X                           .                     
018400        05 SAVE-IDKOLLI          PIC 9(5) VALUE ZERO.                     
018500        03 SAVE-ENTER-IDKOLLI-X                     .                     
018600        05 SAVE-ENTER-IDKOLLI    PIC 9(5) VALUE ZERO.                     
018700        03 SAVE-ENTER-IDARTNO-X                     .                     
018800        05 SAVE-ENTER-IDARTNO    PIC 9(9) VALUE ZERO.                     
018900        03 SAVE-NEXT-IDKOLLI-X                      .                     
019000        05 SAVE-NEXT-IDKOLLI    PIC 9(5)  VALUE ZERO.                     
019100        03 SAVE-NEXT-IDARTNO-X                      .                     
019200        05 SAVE-NEXT-IDARTNO    PIC 9(9)  VALUE ZERO.                     
019300        03 SAVE-SWITCH          PIC X(3)  VALUE '777'.                    
019400     EJECT                                                                
019500*- - - - - - - - - - - - - -  PARAMETRAR TILL WDECEDIT                    
019600                                                                          
019700 01  FILLER                      PIC X(16)   VALUE 'WDECAREA'.            
019800     SKIP2                                                                
019900*01  -COPY WDECAREA.                                                      
020000     EJECT                                                                
020100*- - - - - - - - - - - - - -  PARAMETRAR TILL WDATKONV                    
020200                                                                          
020300 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
020400     SKIP2                                                                
020500*01  -COPY WDATAREA.                                                      
020600     EJECT                                                                
020700 01  PROG-TO-PROG-SW.                                                     
020800*03 -COPY WMSGSOP                                                         
020900     EJECT                                                                
021000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
021100*                                                                         
021200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
021300     SKIP3                                                                
021400*01  -COPY WMFSAREA                                                       
021500     EJECT                                                                
021600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021700*                                                                         
021800     EJECT                                                                
021900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022000     SKIP3                                                                
022100 01  NYCKLAR-TILL-DLI.                                                    
022200     03  W-IDUSER-X.                                                      
022300         05  W-IDUSER            PIC X(8)    VALUE SPACE.                 
022400     03  W-IDARTNB-X.                                                     
022500         05  W-IDARTNB           PIC S9(9)   VALUE ZERO COMP-3.           
022600     03  W-IDSKYLT-X.                                                     
022700         05  W-IDSKYLT           PIC X(3)    VALUE 'GB '.                 
022800     03  W-IDHTYP-X.                                                      
022900         05  FILLER              PIC X(4)    VALUE '3171'.                
023000         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
023100     03  W-IDFAKT-X.                                                      
023200         05  W-IDFAKT            PIC S9(7)   VALUE ZERO COMP-3.           
023300     03  W-IDKOLLI-X.                                                     
023400         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
023500     03  W-IDARTNR-X.                                                     
023600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
023700     03  W-KEYSEG-Y.                                                      
023800         05  FILLER              PIC X(4)    VALUE '3169'.                
023900         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
024000     03  W-KEYSEG-X.                                                      
024100         05  FILLER              PIC X(4)    VALUE '4741'.                
024200         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
024300     03  W-KDSEGKEY-X.                                                    
024400         05  FILLER              PIC X(1)    VALUE '1'  .                 
024500     03  W-IDDC-X.                                                        
024600         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
024700     03  W-SAMMANSA-X.                                                    
024800         05  W-SAMMANSA          PIC X(20)    VALUE SPACE.                
024900     03  W-IDDC-B6-X.                                                     
025000         05 W-IDDC-B6                  PIC X(2).                          
025100     03  W-IDDC-B6-REC-X.                                                 
025200         05 W-IDDC-B6-REC              PIC X(2).                          
025300     SKIP2                                                                
025400*    --- STATUS-KOD FRÅN IMS                                              
025500 01  STATUS-WS                   PIC XX.                                  
025600     88  SEGMENT-FINNS                       VALUE '  '.                  
025700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
025800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
025900     88  END-OF-DATABASE                     VALUE 'GB'.                  
026000     SKIP2                                                                
026100 01  GODK-STATUSKODER.                                                    
026200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026300     SKIP3                                                                
026400 01  SSA1                        PIC X(64).                               
026500 01  SSA2                        PIC X(64).                               
026600 01  SSA3                        PIC X(64).                               
026700 01  SSA4                        PIC X(64).                               
026800     EJECT                                                                
026900*    --- IMS FUNKTIONSKODER                                               
027000*01  -COPY W0003                                                          
027100     EJECT                                                                
027200*    ---  DLI INPUT-OUTPUT AREA                                           
027300                                                                          
027400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC01'.                    
027500 01  DLI-IO-WLARTC01.                                                     
027600*    03  -COPY WDK601                                                     
027700     EJECT                                                                
027800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC11'.                    
027900 01  DLI-IO-WLARTC11.                                                     
028000*    03  -COPY WDK611                                                     
028100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
028200 01  DLI-IO-WDK711.                                                       
028300*    03  -COPY WDK711                                                     
028400     EJECT                                                                
028500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBENA11'.                    
028600 01  DLI-IO-WLBENA11.                                                     
028700*    03  -COPY WDD311  -PRE BENA-                                         
028800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL317101'.                    
028900 01  DLI-IO-WL317101.                                                     
029000*    03  -COPY WDGX01   -PRE 3171-                                        
029100     EJECT                                                                
029200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL317111'.                    
029300 01  DLI-IO-WL317111.                                                     
029400*    03  -COPY WDGX3172                                                   
029500     EJECT                                                                
029600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL317121'.                    
029700 01  DLI-IO-WL317121.                                                     
029800*    03  -COPY WDGX3174                                                   
029900     EJECT                                                                
030000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL317131'.                    
030100 01  DLI-IO-WL317131.                                                     
030200*    03  -COPY WDGX3176                                                   
030300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL316901'.                    
030400 01  DLI-IO-WL316901.                                                     
030500*    03  -COPY WDGX01   -PRE 3169-                                        
030600     EJECT                                                                
030700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL316911'.                    
030800 01  DLI-IO-WL316911.                                                     
030900*    03  -COPY WDGX3170                                                   
031000     EJECT                                                                
031100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLXXLD01'.                    
031200 01  DLI-IO-WLXXLD01.                                                     
031300*    03  -COPY WDGX01   INVNO-                                            
031400     EJECT                                                                
031500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLXXLD11'.                    
031600 01  DLI-IO-WLXXLD11.                                                     
031700*    03  -COPY WDGX4742                                                   
032500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLOGA01'.                    
032600 01  DLI-IO-WLLOGA01.                                                     
032700*    03  -COPY WDL901                                                     
032800                                                                          
032900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
033000 01   DLI-IO-AREA-B601.                                                   
033100*     03  -COPY WDB601                                                    
033200                                                                          
033300 01  FILLER               PIC X(16)   VALUE 'WDB601 REC '.                
033400 01   DLI-IO-AREA-B601-REC.                                               
033500*     03  -COPY WDB601   -PRE REC-                                        
033600     EJECT                                                                
033700 LINKAGE SECTION.                                                         
033800 01  REQU-AREA.                                                           
033900*    03 -COPY WZ01REQU                                                    
034000*    03 -COPY W30182I1                                                    
034100     EJECT                                                                
034200 01  RESP-AREA.                                                           
034300*    03 -COPY WZ01RESP                                                    
034400*    03 -COPY W30182O1                                                    
034500     EJECT                                                                
034600 01  MAX-KVRADER                 PIC S9(4) COMP.                          
034700*01  -COPY W0009  -PRE ALT-                                               
034800                                                                          
034900*01  -COPY W0008  -PRE USEA-                                              
035000     05  FILLER                  PIC X.                                   
035100                                                                          
035200*01  -COPY W0008  -PRE ARTC-                                              
035300     05  FILLER                  PIC X.                                   
035400                                                                          
035500*01  -COPY W0008  -PRE WDK7-                                              
035600     05  FILLER                  PIC X.                                   
035700                                                                          
035800*01  -COPY W0008  -PRE BENA-                                              
035900     05  FILLER                  PIC X.                                   
036000                                                                          
036100*01  -COPY W0008  -PRE 3171-                                              
036200     05  FILLER                  PIC X.                                   
036300                                                                          
036400*01  -COPY W0008  -PRE XXLD-                                              
036500     05  FILLER                  PIC X.                                   
036600                                                                          
037000*01  -COPY W0008  -PRE LOGA-                                              
037100     05  FILLER                  PIC X.                                   
037200*01  -COPY W0008  -PRE 3169-                                              
037300     05  FILLER                  PIC X.                                   
037400*01  -COPY W0008  -PRE WDB6-                                              
037500     05  FILLER                  PIC X.                                   
037600     EJECT                                                                
037700 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA MAX-KVRADER                
037800                                   ALT-PCB USEA-PCB ARTC-PCB              
037900     WDK7-PCB BENA-PCB 3171-PCB XXLD-PCB LOGA-PCB                         
038000     3169-PCB WDB6-PCB.                                                   
038100 MAIN SECTION.                                                            
038200     ENTRY 'DLITCBL' USING REQU-AREA RESP-AREA MAX-KVRADER                
038300                                   ALT-PCB USEA-PCB ARTC-PCB              
038400     WDK7-PCB BENA-PCB 3171-PCB XXLD-PCB LOGA-PCB                         
038500     3169-PCB WDB6-PCB.                                                   
038600                                                                          
038700     PERFORM A-INIT                                                       
038800     PERFORM B-KOLLA-NYCKLAR                                              
038900     IF NYCKLAR-OK                                                        
039000       IF REQU-UPDATE                                                     
039100         PERFORM G-KOLLA-INPUT                                            
039200         IF INDATA-OK                                                     
039300           PERFORM H-UPPDATERA                                            
039400         END-IF                                                           
039500       ELSE                                                               
039600         IF INDATA-OK                                                     
039700           IF REQU-FIRST                                                  
039800             PERFORM C-FOERSTA-SIDA                                       
039900           ELSE                                                           
040000             IF REQU-NEXT                                                 
040100               PERFORM D-NAESTA-SIDA                                      
040200             ELSE                                                         
040300               PERFORM E-SAMMA-SIDA                                       
040400             END-IF                                                       
040500           END-IF                                                         
040600         END-IF                                                           
040700       END-IF                                                             
040800       IF INDATA-OK OR INDATA-FIPPLE                                      
040900        PERFORM F-LAES-VISA-INFO                                          
041000       END-IF                                                             
041100     END-IF                                                               
041200     PERFORM  S20-WEB-DIALOG-SWITCH                                       
041300     GOBACK                                                               
041400     .                                                                    
041500     EJECT                                                                
041600 A-INIT SECTION.                                                          
041700                                                                          
041800     IF REQU-KVRADER NOT NUMERIC                                          
041900        MOVE ZERO TO REQU-KVRADER                                         
042000     END-IF                                                               
042100                                                                          
042200     MOVE ALL '+'                TO RESP-W30182O1                         
042300     MOVE REQU-KVRADER           TO RESP-KVRADER                          
042400     PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > RESP-KVRADER           
042500        MOVE ALL-PLUS-UTF8       TO RESP-BEART (INDX)                     
042600     END-PERFORM                                                          
042700     PERFORM MFS-FORM-ATTR                                                
042800                                                                          
042900     MOVE 001                    TO RESP-IDMSGVER                         
043000     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
043100                                    RESP-IDMSG-INFO                       
043200                                    RESP-IDELMT-ERROR                     
043300     EVALUATE REQU-IDSPRAK                                                
043400       WHEN 'SV'                                                          
043500        MOVE WS-IDSKYLT-SE       TO W-IDSKYLT                             
043600        MOVE WS-CP-EBCDIC        TO TRAUTF8-KDCP                          
043700                                                                          
043800       WHEN 'ZH'                                                          
043900        MOVE WS-IDSKYLT-CN       TO W-IDSKYLT                             
044000        MOVE WS-CP-UNICODE       TO TRAUTF8-KDCP                          
044100                                                                          
044200       WHEN OTHER                                                         
044300        MOVE WS-IDSKYLT-GB       TO W-IDSKYLT                             
044400        MOVE WS-CP-EBCDIC        TO TRAUTF8-KDCP                          
044500     END-EVALUATE                                                         
044600                                                                          
044700                                                                          
044800     MOVE FUNCTION CURRENT-DATE(3:6) TO DAGENS-DATUM                      
044900* --- FIX, CHANGE OF FROM UPDATE TO ENTER KEY FOR CREATE NEW              
045000* --- CASE WHICH IS CURRENTLY IN VALIDATION OF KEY SECTION                
045100     IF REQU-UPDATE AND                                                   
045200             (REQU-NYTT-KOLLI-UPD = 'Y' OR 'y')                           
045300       SET REQU-QUERY TO TRUE                                             
045400     END-IF                                                               
045500     .                                                                    
045600     EJECT                                                                
045700 B-KOLLA-NYCKLAR SECTION.                                                 
045800                                                                          
045900* --- GET USER INFO FROM USERBASE                                         
046000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
046100     MOVE '001'             TO MSGI-KDCALL                                
046200     MOVE '3182'            TO MSGI-IDTRANS                               
046300     MOVE REQU-IDUSER       TO MSGI-IDUSER                                
046400     MOVE REQU-IDKOLLI-KEY  TO MSGI-IDKOLLI                               
046500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
046600     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
046700     IF SAVE-IDFAKT-X  NOT NUMERIC                                        
046800       MOVE ZERO TO SAVE-IDFAKT                                           
046900     END-IF                                                               
047000     IF SAVE-IDKOLLI-X  NOT NUMERIC                                       
047100       MOVE ZERO TO SAVE-IDKOLLI                                          
047200     END-IF                                                               
047300     IF SAVE-ENTER-IDKOLLI-X NOT NUMERIC                                  
047400       MOVE ZERO TO SAVE-ENTER-IDKOLLI                                    
047500     END-IF                                                               
047600     IF SAVE-ENTER-IDARTNO-X NOT NUMERIC                                  
047700       MOVE ZERO TO SAVE-ENTER-IDARTNO                                    
047800     END-IF                                                               
047900     IF SAVE-NEXT-IDKOLLI-X  NOT NUMERIC                                  
048000       MOVE ZERO TO SAVE-NEXT-IDKOLLI                                     
048100     END-IF                                                               
048200     IF SAVE-NEXT-IDARTNO-X  NOT NUMERIC                                  
048300       MOVE ZERO TO SAVE-NEXT-IDARTNO                                     
048400     END-IF                                                               
048500     MOVE REQU-IDDC-KEY  TO W-IDDC                                        
048600                            WS-IDDC                                       
048610     IF MSGI-IDKOLLI NUMERIC                                              
048700        MOVE MSGI-IDKOLLI   TO W-IDKOLLI                                  
048710     ELSE                                                                 
048720        MOVE ZERO           TO W-IDKOLLI                                  
048730     END-IF                                                               
048800                                                                          
048900     MOVE JA TO NYCKLAR-SW                                                
049000* --- CHECK THAT USER HAS CORRECT IDDC CODE IN USERBASE.                  
049100* --- IF NOT SEND ERRORMESSAGE TO SCREEN.                                 
049200* --- IF  NOT  (SDC-NL-ET AND NDC-JP AND NDC-AU)                          
049300     MOVE WS-IDDC TO W-IDDC-B6                                            
049400     PERFORM IMS-GU-WDB601                                                
049500     IF  NOT (DCS-SDC AND DCS-HOLLAND) AND                                
049600         NOT (DCS-NDC AND DCS-JAPAN) AND                                  
049600         NOT (DCS-NDC AND DCS-AUSTRALIA)                                  
049700       MOVE ERR-NOT-ALLOWED TO RESP-IDMSG-ERROR                           
049800       MOVE NEJ TO NYCKLAR-SW                                             
049900     END-IF                                                               
050000     IF NYCKLAR-OK                                                        
050100* --- GET INVOICE KEY FROM USERBASE AND CHECK THAT IT'S A VALID           
050200* --- NUMERIC, THEN LOCATE THIS INVOICE ON HÄNDELSE-REGISTER              
050300       INSPECT MSGI-IDFAKT REPLACING LEADING SPACES BY ZEROS              
050400* --- FIX, SÅ ATT VI ALLTID VISAR DET SISTA KOLLIT OCH                    
050500* --- INFO-SIDAN AV BILDEN OM VI KOMMER FRÅN ANNAN BILD.                  
050600       IF (MSGI-IDFAKT NOT NUMERIC)                                       
050700         MOVE 'N' TO INVOICE-SW                                           
050800         MOVE '888' TO SAVE-SWITCH                                        
050900       ELSE                                                               
051000         MOVE MSGI-IDFAKT TO  W-IDFAKT                                    
051100         PERFORM   IMS-GU-3171-CL11                                       
051200         IF (SEGMENT-SAKNAS) OR                                           
051300            ((SEGMENT-FINNS) AND ((3172-KDTRSTAT NOT = 1)                 
051400                OR (3172-IDDC-SEND NOT = REQU-IDDC-KEY)))                 
051500           MOVE 'N' TO INVOICE-SW                                         
051600         ELSE                                                             
051700* --- IF FIELD CREATE NEW CASE HAS VALUE Y OR J THEN                      
051800* --- WE READ LAST CASE RECORD WITHIN THIS INVOICE, EXTRACT               
051900* --- CASE NO. ADD 1 TO CASE NO. AND GENARATE A NEW CASE.                 
052000                                                                          
052100           IF REQU-NYTT-KOLLI-UPD = 'Y' OR 'y'                            
052200             MOVE MFS-ADD-SET-CURSOR TO RESP-KOLLI-VIKT-UPD-ATTR          
052300             IF REQU-IDKOLLI-KEY = ALL '+'                                
052400               SET REQU-FIRST    TO TRUE                                  
052500* --- UTGÅR PGA LÖPNR. PERFORM IMS-GHNPL-3171-CL21                        
052600* --- FÖR KOLLI *ADD 1 TO 3174-IDKOLLI GIVING SAVE-IDKOLLI                
052700* --- HERE WE INSERT NEW CASE NO.                                         
052800               PERFORM IMS-GU-3169-CL01                                   
052900               PERFORM IMS-GHU-3169-CL11                                  
053000                IF SEGMENT-SAKNAS                                         
053100                 MOVE 1 TO 3170-IDKOLLI                                   
053200                           SAVE-IDKOLLI                                   
053300                 MOVE W-IDDC TO 3170-IDDC                                 
053400                 MOVE 999 TO 3170-IDKOLLI-TOM                             
053500                 PERFORM IMS-ISRT-3169-CL11                               
053600                ELSE                                                      
053700                 ADD 1 TO 3170-IDKOLLI GIVING 3170-IDKOLLI                
053800                 IF 3170-IDKOLLI > 3170-IDKOLLI-TOM                       
053900                   MOVE 1 TO 3170-IDKOLLI                                 
054000                 END-IF                                                   
054100                 MOVE 3170-IDKOLLI TO SAVE-IDKOLLI                        
054200                 PERFORM IMS-REPL-3169-CL11                               
054300                END-IF                                                    
054400                                                                          
054500               INITIALIZE 3174-WDGX3174                                   
054600               MOVE SAVE-IDKOLLI TO 3174-IDKOLLI                          
054700                                  W-IDKOLLI                               
054800               PERFORM IMS-ISRT-3171-CL21                                 
054900               MOVE INF-NEW-CASE-CREATED TO RESP-IDMSG-INFO               
055000             END-IF                                                       
055100             MOVE  ALL-SPACE TO RESP-NYTT-KOLLI-UPD                       
055200             MOVE  MFS-ADD-LAES-IN-FAELT TO                               
055300                                      RESP-NYTT-KOLLI-UPD-ATTR            
055400             IF REQU-NYTT-KOLLI-UPD NOT = '+'                             
055500             MOVE 'Y' TO W-NY-KOLLI                                       
055600             MOVE '+' TO REQU-NYTT-KOLLI-UPD                              
055700             END-IF                                                       
055800           END-IF                                                         
055900* --- READ CASE SEGMENT, AND IF NOT FOUND, SEND ERRORMESSAGE              
056000* --- TO INPUT SCREEN. IF MID-KOLLI-NR IS NUMERIC AND NOT ++              
056100* --- GIVE MESSAGE 'KOLLI THIS KOLLI DOES NOT EXIST' AND UPDATE           
056200* --- ERROR FLAG.                                                         
056300             IF REQU-IDKOLLI-KEY NOT = ALL '+'                            
056400*              SET REQU-FIRST    TO TRUE                                  
056500               INSPECT REQU-IDKOLLI-KEY REPLACING LEADING SPACE           
056600               BY ZERO                                                    
056700               IF REQU-IDKOLLI-KEY NUMERIC                                
056800                 MOVE REQU-IDKOLLI-KEY TO W-IDKOLLI                       
056900               ELSE                                                       
057000                 MOVE ZERO TO W-IDKOLLI                                   
057100               END-IF                                                     
057200               PERFORM IMS-GU-3171-CL21                                   
057300               IF SEGMENT-SAKNAS                                          
057400                 MOVE ALL-PLUS          TO RESP-IDKOLLI-KEY               
057500                 MOVE MFS-NUM-FAELT-FEL TO RESP-IDKOLLI-KEY-ATTR          
057600                 MOVE ALL-PLUS          TO RESP-IDFAKT                    
057700                 MOVE ALL-SPACE         TO RESP-NYTT-KOLLI-UPD            
057800                 MOVE ERR-WRONG-KEY     TO RESP-IDMSG-ERROR               
057900                 MOVE 'IDKOLLI'         TO RESP-IDELMT-ERROR              
058000                 MOVE '+++++' TO REQU-IDKOLLI-KEY                         
058100                 MOVE '+'     TO REQU-NYTT-KOLLI-UPD                      
058200                 PERFORM MFS-RENSA-FAELT-IN                               
058300                 MOVE NEJ TO NYCKLAR-SW                                   
058400               ELSE                                                       
058500                 MOVE '+++++' TO REQU-IDKOLLI-KEY                         
058600                 MOVE '+'     TO REQU-NYTT-KOLLI-UPD                      
058700                 MOVE W-IDKOLLI TO RESP-IDKOLLI-KEY                       
058800               END-IF                                                     
058900             ELSE                                                         
059000               PERFORM IMS-GU-3171-CL21                                   
059100               IF SEGMENT-SAKNAS                                          
059200                 MOVE 'N' TO INVOICE-SW                                   
059300               ELSE                                                       
059400                 MOVE '+++++' TO REQU-IDKOLLI-KEY                         
059500                 MOVE W-IDKOLLI TO RESP-IDKOLLI-KEY                       
059600               END-IF                                                     
059700             END-IF                                                       
059800         END-IF                                                           
059900       END-IF                                                             
060000* --- IF DATABASE ACCESS WITH USERBASE DATA DIDN'T GIVA A VALID           
060100* --- INVOICE TO WORK WITH, REACCESS THE DATABASE AND FIND THE            
060200* --- LAST INVOICE FOR THE RELEVANT DC, WITH STATUSCODE = 1.              
060300                                                                          
060400       IF INVOICE-NOT-FOUND AND  NYCKLAR-OK                               
060500         PERFORM IMS-GU-3171-CL                                           
060600         PERFORM IMS-GNP-3171-CL11                                        
060700         PERFORM UNTIL (SEGMENT-SAKNAS  OR W-DCTEST = 'Y')                
060800           IF 3171-SEG-LEVEL = '02'                                       
060900             IF (3172-KDTRSTAT = 1 )                                      
061000                AND (3172-IDDC-SEND = REQU-IDDC-KEY)                      
061100               MOVE 'Y' TO W-DCTEST                                       
061200               MOVE 3172-IDFAKT TO W-IDFAKT                               
061300             END-IF                                                       
061400           END-IF                                                         
061500           IF W-DCTEST NOT = 'Y'                                          
061600             PERFORM IMS-GNP-3171-CL11                                    
061700           END-IF                                                         
061800         END-PERFORM                                                      
061900* --- FIND THE LAST CASE REGISTERED FOR THE INVOICE YOU FOUND.            
062000         IF W-DCTEST  = 'Y'                                               
062100           PERFORM IMS-GHNPL-3171-CL21                                    
062200           MOVE 3174-IDKOLLI TO W-IDKOLLI                                 
062300* --- IF WE STILL HAVEN'T FOUND A VALID INVOICE FOR THIS DC WE'LL         
062400* --- HAVE TO CREATE A NEW INVOICE FOR THIS DC, AND UPDATE THE            
062500* --- CLEARING HÄNDELSE-REGISTER WITH THE NEW INVOICE DATA AND THE        
062600* --- 1ST CASE FOR THIS INVOICE.                                          
062700         ELSE                                                             
062800* --- READ FAKTURANR-REGISTER AND GET NEW INVOICE NO.                     
062900           PERFORM IMS-GHU-XXLD-INV                                       
063000           MOVE +03 TO INDX                                               
063100* --- INCREASE INVOICE NO. BY 1 AND UPDATE THE FAKTURANR-REGISTER         
063200           ADD 1 TO 4742-IDFAKT-AKT (INDX) GIVING                         
063300                    4742-IDFAKT-AKT (INDX)                                
063400           IF 4742-IDFAKT-AKT (INDX) > 4742-IDFAKT-MAX (INDX)             
063500             MOVE  4742-IDFAKT-MIN (INDX) TO                              
063600                   4742-IDFAKT-AKT (INDX)                                 
063700                   W-IDFAKT                                               
063800           END-IF                                                         
063900           PERFORM IMS-REPL-XXLD-INV                                      
064000* --- UPDATE CLEARING HÄNDELSEBAS WITH NEW SEGMENT FOR THE NEW            
064100* --- INVOICE AND CASE NO. 1.                                             
064200           INITIALIZE 3172-WDGX3172                                       
064300           MOVE 4742-IDFAKT-AKT (INDX) TO 3172-IDFAKT                     
064400                                          W-IDFAKT                        
064500           MOVE W-IDDC TO  3172-IDDC-SEND                                 
064600           IF (DCS-SDC AND DCS-HOLLAND) OR                                
064600              (DCS-NDC AND DCS-JAPAN) OR                                  
064600              (DCS-NDC AND DCS-AUSTRALIA)                                 
064700             MOVE WS-SDC-91 TO 3172-IDDC-REC                              
064800           END-IF                                                         
064900           MOVE 1 TO 3172-KDTRSTAT                                        
065000           PERFORM IMS-ISRT-3171-CL11                                     
065100* --- HERE WE INSERT NEW CASE NO.                                         
065200               PERFORM IMS-GU-3169-CL01                                   
065300               PERFORM IMS-GHU-3169-CL11                                  
065400                IF SEGMENT-SAKNAS                                         
065500                 MOVE 1 TO 3170-IDKOLLI                                   
065600                           SAVE-IDKOLLI                                   
065700                 MOVE W-IDDC TO 3170-IDDC                                 
065800                 MOVE 999 TO 3170-IDKOLLI-TOM                             
065900                 PERFORM IMS-ISRT-3169-CL11                               
066000                ELSE                                                      
066100                 ADD 1 TO 3170-IDKOLLI GIVING 3170-IDKOLLI                
066200                 IF 3170-IDKOLLI > 3170-IDKOLLI-TOM                       
066300                   MOVE 1 TO 3170-IDKOLLI                                 
066400                 END-IF                                                   
066500                 MOVE 3170-IDKOLLI TO SAVE-IDKOLLI                        
066600                 PERFORM IMS-REPL-3169-CL11                               
066700                END-IF                                                    
066800                                                                          
066900           INITIALIZE 3174-WDGX3174                                       
067000           MOVE SAVE-IDKOLLI   TO W-IDKOLLI                               
067100           MOVE W-IDKOLLI TO 3174-IDKOLLI                                 
067200           PERFORM IMS-ISRT-3171-CL21                                     
067300           MOVE INF-NEW-CASE-CREATED TO RESP-IDMSG-INFO                   
067400         END-IF                                                           
067500       END-IF                                                             
067600     END-IF                                                               
067700* --- HÄR STÄLLS BILDSWITCHEN MELLAN INFOBILD OCH INPUT-BILDEN            
067800     IF SAVE-SWITCH   = '777' OR '888'                                    
067900        CONTINUE                                                          
068000     ELSE                                                                 
068100        MOVE '777' TO SAVE-SWITCH                                         
068200     END-IF                                                               
068300     IF REQU-SPLIT                                                        
068400       IF SAVE-SWITCH = '888'                                             
068500         PERFORM S01-INIT-LINE-DATA                                       
068600       END-IF                                                             
068700       IF  SAVE-SWITCH = '777'                                            
068800         MOVE '888' TO SAVE-SWITCH                                        
068900       ELSE                                                               
069000         MOVE '777' TO SAVE-SWITCH                                        
069100       END-IF                                                             
069200       SET REQU-FIRST TO TRUE                                             
069300     END-IF                                                               
069400*  MOVE + TO INDEX 10 FIELDS IF SWITCH = '888'                            
069500     IF SAVE-SWITCH = '888'                                               
069600       PERFORM S01-INIT-LINE-DATA                                         
069700     END-IF                                                               
069800* --- FIX, TO SET CURSOR                                                  
069900     IF REQU-NYTT-KOLLI-UPD = 'Y' OR 'y'                                  
070000       MOVE '777' TO SAVE-SWITCH                                          
070100     END-IF                                                               
070200                                                                          
070300     IF NYCKLAR-OK                                                        
070400* --- UPDATE THE USERBASE WITH THE CURRENT KEY VALUES!!                   
070500       MOVE ALL '+'           TO MSGI-WMSGINIT                            
070600       MOVE '001'             TO MSGI-KDCALL                              
070700       MOVE REQU-IDUSER       TO MSGI-IDUSER                              
070800       MOVE '3182'            TO MSGI-IDTRANS                             
070900       MOVE W-IDFAKT          TO MSGI-IDFAKT                              
071000       MOVE W-IDKOLLI         TO MSGI-IDKOLLI                             
071100       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
071200* --- UPDATE THE MOD-KEY FIELDS!!                                         
071300       MOVE MSGI-IDFAKT         TO RESP-IDFAKT                            
071400       MOVE MSGI-IDKOLLI        TO RESP-IDKOLLI-KEY                       
071500       MOVE ALL-SPACE           TO RESP-NYTT-KOLLI-UPD                    
071600     ELSE                                                                 
071700       MOVE ZERO                 TO RESP-KVRADER                          
071800     END-IF                                                               
071900     SET LINE-DATA-ALL-PLUS      TO TRUE                                  
072000     PERFORM VARYING INDX FROM +1 BY +1                                   
072100       UNTIL INDX > REQU-KVRADER OR                                       
072200             LINE-DATA-NOT-ALL-PLUS                                       
072300       IF REQU-NYRAD (INDX) = ALL '+'                                     
072400         CONTINUE                                                         
072500       ELSE                                                               
072600         SET LINE-DATA-NOT-ALL-PLUS  TO TRUE                              
072700       END-IF                                                             
072800     END-PERFORM                                                          
072900                                                                          
073000     SET IN-DATA-NOT-ALL-PLUS    TO TRUE                                  
073100     IF REQU-NYTT-KOLLI-UPD = ALL '+' AND                                 
073200        REQU-KOLLI-VIKT-UPD = ALL '+' AND                                 
073300        REQU-KOLLI-VOL-UPD  = ALL '+' AND                                 
073400        REQU-TOT-VIKT-UPD   = ALL '+' AND                                 
073500        REQU-TOT-VOL-UPD    = ALL '+' AND                                 
073600        REQU-SKAPA-PROFORMA = ALL '+' AND                                 
073700        REQU-DEL-IDARTNR-OBJ = ALL '+' AND                                
073800        REQU-DEL-KVANTAL    = ALL '+' AND                                 
073900       (REQU-DEL-IDKOLLI    = ALL '+' OR '0000+')                         
074000       SET IN-DATA-ALL-PLUS      TO TRUE                                  
074100     END-IF                                                               
074200     .                                                                    
074300     EJECT                                                                
074400 C-FOERSTA-SIDA SECTION.                                                  
074500                                                                          
074600     MOVE 0 TO W-IDARTNR                                                  
074700*    -- SET SHOW DATA MODE                                                
074800     MOVE '888' TO SAVE-SWITCH                                            
074900                                                                          
075000     .                                                                    
075100     EJECT                                                                
075200 D-NAESTA-SIDA SECTION.                                                   
075300                                                                          
075400     IF MSGI-IDTRANS = '3182'                                             
075500       MOVE SAVE-NEXT-IDARTNO TO W-IDARTNR                                
075600     END-IF                                                               
075700     .                                                                    
075800     EJECT                                                                
075900 E-SAMMA-SIDA SECTION.                                                    
076000                                                                          
076100     IF IN-DATA-ALL-PLUS AND                                              
076200        LINE-DATA-ALL-PLUS                                                
076300* --- SWITCH BY ENTER                                                     
076400       IF REQU-QUERY                                                      
076500         IF SAVE-SWITCH = '888'                                           
076600           PERFORM S01-INIT-LINE-DATA                                     
076700         END-IF                                                           
076800         IF  SAVE-SWITCH = '777'                                          
076900           MOVE '888' TO SAVE-SWITCH                                      
077000         ELSE                                                             
077100           MOVE '777' TO SAVE-SWITCH                                      
077200         END-IF                                                           
077300         IF REQU-NYTT-KOLLI-UPD = 'Y' OR 'y'                              
077400           MOVE '777' TO SAVE-SWITCH                                      
077500         END-IF                                                           
077600         SET REQU-FIRST TO TRUE                                           
077700       END-IF                                                             
077800                                                                          
077900       MOVE SAVE-ENTER-IDARTNO TO W-IDARTNR                               
078000     ELSE                                                                 
078100       MOVE INF-PRESS-PF11 TO RESP-IDMSG-INFO                             
078200       PERFORM EA-MID-INDATA-TILL-MOD                                     
078300       PERFORM EB-MOD-UTDATA-TILL-MOD                                     
078400       MOVE NEJ TO INDATA-SW                                              
078500     END-IF                                                               
078600     .                                                                    
078700     EJECT                                                                
078800 EA-MID-INDATA-TILL-MOD SECTION.                                          
078900                                                                          
079000* --- FÖR VARJE MID-FÄLT                                                  
079100* --- OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT            
079200* --- FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR                   
079300* --- ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                       
079400     IF    REQU-NYTT-KOLLI-UPD NOT = ALL '+'                              
079500       MOVE ALL-PLUS              TO RESP-NYTT-KOLLI-UPD                  
079600       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-NYTT-KOLLI-UPD-ATTR             
079700     ELSE                                                                 
079800       MOVE ALL-SPACE TO       RESP-NYTT-KOLLI-UPD                        
079900     END-IF                                                               
080000     IF    REQU-KOLLI-VIKT-UPD NOT = ALL '+'                              
080100       MOVE ALL-PLUS              TO RESP-KOLLI-VIKT-UPD                  
080200       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-KOLLI-VIKT-UPD-ATTR             
080300     ELSE                                                                 
080400       MOVE ALL-SPACE TO         RESP-KOLLI-VIKT-UPD                      
080500     END-IF                                                               
080600     IF    REQU-KOLLI-VOL-UPD NOT = ALL '+'                               
080700       MOVE ALL-PLUS           TO   RESP-KOLLI-VOL-UPD                    
080800       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-KOLLI-VOL-UPD-ATTR              
080900     ELSE                                                                 
081000       MOVE ALL-SPACE TO         RESP-KOLLI-VOL-UPD                       
081100     END-IF                                                               
081200     IF    REQU-TOT-VIKT-UPD NOT = ALL '+'                                
081300       MOVE ALL-PLUS           TO   RESP-TOT-VIKT-UPD                     
081400       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-TOT-VIKT-UPD-ATTR               
081500     ELSE                                                                 
081600       MOVE ALL-SPACE TO         RESP-TOT-VIKT-UPD                        
081700     END-IF                                                               
081800     IF    REQU-TOT-VOL-UPD NOT = ALL '+'                                 
081900       MOVE ALL-PLUS TO            RESP-TOT-VOL-UPD                       
082000       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-TOT-VOL-UPD-ATTR                
082100     ELSE                                                                 
082200       MOVE ALL-SPACE TO         RESP-TOT-VOL-UPD                         
082300     END-IF                                                               
082400     IF    REQU-SKAPA-PROFORMA NOT = ALL '+'                              
082500      MOVE ALL-PLUS             TO   RESP-SKAPA-PROFORMA                  
082600      MOVE MFS-ADD-LAES-IN-FAELT TO RESP-SKAPA-PROFORMA-ATTR              
082700     ELSE                                                                 
082800      MOVE ALL-SPACE TO         RESP-SKAPA-PROFORMA                       
082900     END-IF                                                               
083000     IF    REQU-DEL-IDARTNR-OBJ NOT = ALL '+'                             
083100       MOVE ALL-PLUS TO            RESP-DEL-IDARTNR-OBJ                   
083200       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-DEL-IDARTNR-OBJ-ATTR            
083300     ELSE                                                                 
083400      MOVE ALL-SPACE TO         RESP-DEL-IDARTNR-OBJ                      
083500     END-IF                                                               
083600     IF    REQU-DEL-KVANTAL NOT = ALL '+'                                 
083700       MOVE ALL-PLUS TO            RESP-DEL-KVANTAL                       
083800       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-DEL-KVANTAL-ATTR                
083900     ELSE                                                                 
084000      MOVE ALL-SPACE TO         RESP-DEL-KVANTAL                          
084100     END-IF                                                               
084200*    -- NOTE: FORMAT IS X(5) / Z(4)9 NOT X FOR THIS "FLAG"                
084300     IF    REQU-DEL-IDKOLLI NOT = ALL '+' AND '0000+'                     
084400      MOVE ALL-PLUS           TO   RESP-DEL-IDKOLLI                       
084500      MOVE MFS-ADD-LAES-IN-FAELT TO RESP-DEL-IDKOLLI-ATTR                 
084600     ELSE                                                                 
084700      MOVE ALL-SPACE TO         RESP-DEL-IDKOLLI                          
084800     END-IF                                                               
084900     IF SAVE-SWITCH = '777'                                               
085000       PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > REQU-KVRADER         
085100        IF REQU-IDARTNR-OBJ (INDX) NOT = ALL '+'                          
085200          MOVE ALL-PLUS              TO RESP-IDARTNR-OBJ (INDX)           
085300          MOVE MFS-ADD-LAES-IN-FAELT TO                                   
085400                                   RESP-IDARTNR-OBJ-ATTR (INDX)           
085500        ELSE                                                              
085600          MOVE ALL-SPACE             TO RESP-IDARTNR-OBJ (INDX)           
085700        END-IF                                                            
085800                                                                          
085900        IF REQU-KVANTAL (INDX) NOT = ALL '+'                              
086000          MOVE ALL-PLUS              TO RESP-KVANTAL (INDX)               
086100          MOVE MFS-ADD-LAES-IN-FAELT TO RESP-KVANTAL-ATTR (INDX)          
086200        ELSE                                                              
086300          MOVE ALL-SPACE             TO RESP-KVANTAL (INDX)               
086400        END-IF                                                            
086500                                                                          
086600       END-PERFORM                                                        
086700     END-IF                                                               
086800     .                                                                    
086900     EJECT                                                                
087000 EB-MOD-UTDATA-TILL-MOD SECTION.                                          
087100                                                                          
087200* --- ALLA UTDATA-FÄLT                                                    
087300* --- INCL SCROLL KEYS AND LINEDATA                                       
087400     MOVE ALL-PLUS TO                                                     
087500                                                                          
087600                                    RESP-KOLLI-VOL-UT                     
087700                                    RESP-KOLLI-VIKT-UT                    
087800                                    RESP-TOT-VIKT-UT                      
087900                                    RESP-TOT-VOL-UT                       
088000     PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > REQU-KVRADER           
088100       MOVE ALL-PLUS TO           RESP-IDARTNR-OBJ (INDX)                 
088200       MOVE ALL-PLUS TO           RESP-KVANTAL (INDX)                     
088300       IF REQU-IDMSGVER = '101'                                           
088400         MOVE ALL-PLUS         TO RESP-BEART (INDX)                       
088500       ELSE                                                               
088600         MOVE ALL-PLUS-UTF8    TO RESP-BEART (INDX)                       
088700       END-IF                                                             
088800       MOVE ALL-PLUS TO           RESP-KVLS (INDX)                        
088900       IF   SAVE-SWITCH = '888'                                           
089000         MOVE MFS-CLOSE-FIELD TO  RESP-IDARTNR-OBJ-ATTR (INDX)            
089100         MOVE MFS-CLOSE-FIELD TO  RESP-KVANTAL-ATTR (INDX)                
089200         MOVE MFS-ADD-SET-CURSOR TO RESP-IDKOLLI-KEY-ATTR                 
089300       END-IF                                                             
089400     END-PERFORM                                                          
089500     .                                                                    
089600     EJECT                                                                
089700 F-LAES-VISA-INFO SECTION.                                                
089800                                                                          
089900     MOVE ZERO                   TO RESP-KVRADER                          
090000* --- FOLLOWING FIELDS ARE SHOWN IRRESPECTIVE OF WHICH SCREEN IS          
090100* --- SHOWN (EITHER THE UPDATE OR THE VIEW SCREEN)                        
090200     MOVE  3172-VKORDBTO-FAKT  TO RESP-TOT-VIKT-UT                        
090300     MOVE  3172-VLORDBTO-FAKT  TO RESP-TOT-VOL-UT                         
090400     MOVE  3174-VKORDBTO-KOLLI TO RESP-KOLLI-VIKT-UT                      
090500     MOVE  3174-VLORDBTO-KOLLI TO RESP-KOLLI-VOL-UT                       
090600* --- HERE WE CHOOSE TO ADJUST SCREEN VIEW MODE DEPENDING ON THE          
090700* --- VALUE OF THE SAVE-SWITCH FIELD                                      
090800     IF   SAVE-SWITCH = '888'                                             
090900* --- MOVE CORRECT DISPLAY FORMATS TO INDEXED FIELDS WHICH                
091000* --- NORMALLY HAVE BEEN USED AS INPUTFIELDS.                             
091100       PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-KVRADER          
091200         MOVE MFS-CLOSE-FIELD TO  RESP-IDARTNR-OBJ-ATTR (INDX)            
091300         MOVE MFS-CLOSE-FIELD TO  RESP-KVANTAL-ATTR (INDX)                
091400       END-PERFORM                                                        
091500* --- MOVE CURSOR TO IN-COLLI'N                                           
091600       IF NOT INDATA-FIPPLE                                               
091700        IF W-NY-KOLLI     = 'Y'                                           
091800         MOVE MFS-ADD-SET-CURSOR TO RESP-KOLLI-VIKT-UPD-ATTR              
091900        ELSE                                                              
092000         IF W-TOT-CURS     = 'Y'                                          
092100           MOVE MFS-ADD-SET-CURSOR TO RESP-SKAPA-PROFORMA-ATTR            
092200         ELSE                                                             
092300           MOVE MFS-ADD-SET-CURSOR TO RESP-IDKOLLI-KEY-ATTR               
092400         END-IF                                                           
092500        END-IF                                                            
092600       END-IF                                                             
092700* --- LÄS IN POSTERNA TILL MOD-FÄLT                                       
092800* --- FIRST READ IN THE INVOICE CASE AND PARTNO. (EITHER 1ST)             
092900* --- PARTNO. OR ONE SPECIFIED IN USERBASE.                               
093000       IF W-IDARTNR > 0                                                   
093100         PERFORM IMS-GU-3171-CL31                                         
093200       ELSE                                                               
093300         PERFORM IMS-GU-3171-CL21                                         
093400         PERFORM IMS-GN-3171-CL21                                         
093500       END-IF                                                             
093600       MOVE 3176-IDARTNR-OBJ TO   SAVE-ENTER-IDARTNO                      
093700       IF SEGMENT-SAKNAS                                                  
093800         PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-KVRADER        
093900           MOVE ALL-SPACE TO        RESP-IDARTNR-OBJ (INDX)               
094000           MOVE ALL-SPACE TO        RESP-KVANTAL (INDX)                   
094100           IF REQU-IDMSGVER = '101'                                       
094200             MOVE ALL-SPACE         TO RESP-BEART (INDX)                  
094300           ELSE                                                           
094400             MOVE ALL-SPACE-UTF8    TO RESP-BEART (INDX)                  
094500           END-IF                                                         
094600           MOVE ALL-SPACE TO        RESP-KVLS (INDX)                      
094700         END-PERFORM                                                      
094800       ELSE                                                               
094900         PERFORM VARYING INDX FROM 1 BY 1 UNTIL                           
095000                 (INDX > MAX-KVRADER OR SEGMENT-SAKNAS)                   
095100* --- MOVE VALUES TO MOD-FIELDS                                           
095200           MOVE 3176-IDARTNR-OBJ TO RESP-IDARTNR-OBJ (INDX)               
095300                                    W-IDARTNR                             
095400           MOVE 3176-KVANTAL-DEB TO RESP-KVANTAL    (INDX)                
095500                                                                          
                 MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                        
                 IF DCS-UNICODE-IDSKYLT                                         
                    MOVE 'UTF8'             TO TRAUTF8-KDCP                     
                 ELSE                                                           
                    MOVE '278 '             TO TRAUTF8-KDCP                     
                 END-IF                                                         
                                                                                
095600           PERFORM IMS-GU-BENA-BEN                                        
095700           IF SEGMENT-FINNS                                               
095800              MOVE BENA-TEXT-BEART TO TRAUTF8-TECONV-FROM                 
095900           ELSE                                                           
096000              MOVE SPACE          TO TRAUTF8-TECONV-FROM                  
096100              MOVE WS-CP-EBCDIC TO TRAUTF8-KDCP                           
096200           END-IF                                                         
                 IF TRAUTF8-TECONV-FROM = SPACES                                
                  MOVE 'GB'  TO W-IDSKYLT                                       
                  MOVE '278' TO TRAUTF8-KDCP                                    
                  PERFORM IMS-GU-BENA-BEN                                       
                  MOVE BENA-TEXT-BEART    TO TRAUTF8-TECONV-FROM                
                 END-IF                                                         
096300           IF REQU-IDMSGVER = '101'                                       
096400*            -- CLASSIC IMS - NO TRANSLATION TO UTF8                      
096500             MOVE TRAUTF8-TECONV-FROM TO RESP-BEART (INDX)                
096600           ELSE                                                           
096700*            -- WEB CALL                                                  
096800             CALL WTRAUTF8 USING TRAUTF8-AREA                             
096900             MOVE TRAUTF8-TECONV-TO TO RESP-BEART (INDX)                  
097000           END-IF                                                         
097100                                                                          
097200           MOVE '  ' TO STATUS-WS                                         
097300           MOVE ALL-SPACE TO        RESP-KVLS (INDX)                      
097400           PERFORM IMS-GN-3171-CL21                                       
097500           ADD +1                TO RESP-KVRADER                          
097600         END-PERFORM                                                      
097700         PERFORM UNTIL INDX > MAX-KVRADER                                 
097800           MOVE ALL-SPACE TO        RESP-IDARTNR-OBJ (INDX)               
097900           MOVE ALL-SPACE TO        RESP-KVANTAL (INDX)                   
098000           IF REQU-IDMSGVER = '101'                                       
098100             MOVE ALL-SPACE         TO RESP-BEART (INDX)                  
098200           ELSE                                                           
098300             MOVE ALL-SPACE-UTF8    TO RESP-BEART (INDX)                  
098400           END-IF                                                         
098500           MOVE ALL-SPACE TO        RESP-KVLS (INDX)                      
098600           ADD 1 TO INDX GIVING INDX                                      
098700         END-PERFORM                                                      
098800       END-IF                                                             
098900* --- SEND MESSAGE TO SCREEN IF MORE POSTS EXIST.                         
099000         IF STATUS-WS NOT = 'GE'                                          
099100           MOVE 3176-IDARTNR-OBJ TO   SAVE-NEXT-IDARTNO                   
099200           MOVE INF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO                   
099300         ELSE                                                             
099400           MOVE   SAVE-ENTER-IDARTNO TO   SAVE-NEXT-IDARTNO               
099500         END-IF                                                           
099600* --- SAVE VALUE OF FIRST AND LAST PAGE PARTNUMBERS.                      
099700* --- UPDATE THE USERBASE WITH THE CURRENT ROLLING VALS!                  
099800         MOVE '002'      TO MSGI-KDCALL                                   
099900         MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                
100000         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
100100* --- UPDATE THE USERBASE WITH THE NEW PAGING KEYS.                       
100200     END-IF                                                               
100300* --- SAVE VALUE OF FIRST AND LAST PAGE PARTNUMBERS.                      
100400* --- UPDATE THE USERBASE WITH THE CURRENT ROLLING VALS!                  
100500     MOVE '002'      TO MSGI-KDCALL                                       
100600     MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                    
100700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
100800* --- UPDATE THE USERBASE WITH THE NEW PAGING KEYS.                       
100900* --- RENSA INFÄLTEN                                                      
101000     IF SAVE-SWITCH =  '777'  AND (INDATA-OK OR INDATA-FIPPLE)            
101100      PERFORM MFS-RENSA-FAELT-IN                                          
101200     END-IF                                                               
101300     IF SAVE-SWITCH =  '888'  AND (INDATA-OK OR INDATA-FIPPLE)            
101400      PERFORM MFS-RENSA-FAELT-IN-X                                        
101500     END-IF                                                               
101600     IF INDATA-FIPPLE                                                     
101700       MOVE MFS-ADD-SET-CURSOR  TO RESP-KOLLI-VIKT-UPD-ATTR               
101800     END-IF                                                               
101900     CONTINUE                                                             
102000     .                                                                    
102100     EJECT                                                                
102200 G-KOLLA-INPUT SECTION.                                                   
102300                                                                          
102400     MOVE JA  TO INDATA-SW                                                
102500* --- CHECK THAT INPUT HAS BEEN ACTUALLY MADE ON SCREEN!!                 
102600     IF IN-DATA-ALL-PLUS AND                                              
102700        LINE-DATA-ALL-PLUS                                                
102800       MOVE ERR-PF11-AND-NO-DATA TO RESP-IDMSG-ERROR                      
102900       PERFORM MFS-ROER-EJ-FAELT-IN                                       
103000       PERFORM MFS-LAES-IN-IGEN                                           
103100       PERFORM MFS-ROER-EJ-FAELT-UT                                       
103200       MOVE NEJ TO INDATA-SW                                              
103300     END-IF                                                               
103400* --- CHECK THAT INPUT COMBINATIONS ARE CORRECT                           
103500     IF INDATA-OK                                                         
103600       IF (( REQU-DEL-IDARTNR-OBJ NOT = ALL '+' ) OR                      
103700           ( REQU-DEL-IDKOLLI NOT =  ALL '+' AND '0000+' ) )              
103800                  AND                                                     
103900          (( LINE-DATA-NOT-ALL-PLUS )                                     
104000                   OR                                                     
104100          ( REQU-SKAPA-PROFORMA NOT = ALL '+'))                           
104200                                                                          
104300         MOVE ERR-NOT-ALLOWED      TO RESP-IDMSG-ERROR                    
104400         PERFORM MFS-ROER-EJ-FAELT-IN                                     
104500         PERFORM MFS-LAES-IN-IGEN                                         
104600         PERFORM MFS-ROER-EJ-FAELT-UT                                     
104700         MOVE NEJ TO INDATA-SW                                            
104800       END-IF                                                             
104900       IF ( REQU-SKAPA-PROFORMA NOT = ALL '+')                            
105000                      AND                                                 
105100          (( LINE-DATA-NOT-ALL-PLUS )                                     
105200                      OR                                                  
105300          ( REQU-DEL-IDARTNR-OBJ NOT = ALL '+'))                          
105400                                                                          
105500         MOVE ERR-NOT-ALLOWED      TO RESP-IDMSG-ERROR                    
105600         PERFORM MFS-ROER-EJ-FAELT-IN                                     
105700         PERFORM MFS-LAES-IN-IGEN                                         
105800         PERFORM MFS-ROER-EJ-FAELT-UT                                     
105900         MOVE NEJ TO INDATA-SW                                            
106000       END-IF                                                             
106100     END-IF                                                               
106200* --- CHECK ON DELETE OF CASE CURRENT CASE                                
106300     IF REQU-DEL-IDKOLLI NOT = ALL '+' AND '0000+'                        
106400       IF REQU-DEL-IDKOLLI = '0000y' OR '0000Y'                           
106500          MOVE MFS-NUM-FAELT-RAETT TO RESP-DEL-IDKOLLI-ATTR               
106600       ELSE                                                               
106700          MOVE MFS-ALFA-FAELT-FEL TO  RESP-DEL-IDKOLLI-ATTR               
106800          MOVE NEJ TO INDATA-SW                                           
106900       END-IF                                                             
107000     END-IF                                                               
107100* --- CHECK IF IT'S OK TO CREATE THE PROFORMA INVOICE                     
107200     IF REQU-SKAPA-PROFORMA = ALL '+' OR SPACE OR 'N'                     
107300       MOVE  MFS-ALFA-FAELT-RAETT TO RESP-SKAPA-PROFORMA-ATTR             
107400     ELSE                                                                 
107500       MOVE  MFS-ALFA-FAELT-RAETT TO RESP-SKAPA-PROFORMA-ATTR             
107600       MOVE  'Y'                TO REQU-SKAPA-PROFORMA                    
107700     END-IF                                                               
107800     IF REQU-SKAPA-PROFORMA    =     'Y'   OR  'y'                        
107900* --- CHECK THAT PARTNUMBERS EXIST IN THE INVOICE!!!                      
108000       PERFORM IMS-GU-3171-CL                                             
108100       PERFORM IMS-GNP-3171-CL11X                                         
108200       IF SEGMENT-SAKNAS                                                  
108300         MOVE MFS-NUM-FAELT-FEL TO RESP-SKAPA-PROFORMA-ATTR               
108400         MOVE ERR-INV-WITHOUT-ARTNR  TO RESP-IDMSG-ERROR                  
108500         MOVE NEJ TO INDATA-SW                                            
108600       ELSE                                                               
108700* --- CHECK THAT WEIGHT CORRECTLY FILLED IN                               
108800         IF (3172-VLORDBTO-FAKT  = 0)  OR (3172-SUFKTNTO NOT = 1)         
108900           PERFORM IMS-GU-3171-CL                                         
109000           PERFORM IMS-GNP-3171-CL11Y                                     
109100           PERFORM UNTIL (SEGMENT-SAKNAS)                                 
109200                   OR (INDATA-SW = 'N' OR 'Z')                            
109300             IF 3171-SEG-LEVEL = '03'                                     
109400               IF 3174-VKORDBTO-KOLLI = 0                                 
109500                 MOVE 'Z' TO INDATA-SW                                    
109600                 MOVE MFS-ALFA-FAELT-RAETT TO                             
109700                   RESP-SKAPA-PROFORMA-ATTR                               
109800                 MOVE MFS-NUM-FAELT-FEL TO                                
109900                    RESP-KOLLI-VIKT-UPD-ATTR                              
110000                 MOVE ERR-UPD-WEIGHT-FOR-CASE                             
110100                                 TO RESP-IDMSG-ERROR                      
110200                 MOVE 'VKART'      TO RESP-IDELMT-ERROR                   
110300                 MOVE 3174-IDKOLLI TO MSGI-IDKOLLI                        
110400                                    W-IDKOLLI                             
110500                 MOVE 3174-IDKOLLI TO RESP-IDKOLLI-KEY                    
110600* --- UPDATE THE USERBASE WITH THE CURRENT KEY VALUES!!                   
110700                 MOVE ALL '+'           TO MSGI-WMSGINIT                  
110800                 MOVE '001'             TO MSGI-KDCALL                    
110900                 MOVE REQU-IDUSER       TO MSGI-IDUSER                    
111000                 MOVE '3182'            TO MSGI-IDTRANS                   
111100                 MOVE W-IDFAKT          TO MSGI-IDFAKT                    
111200                 MOVE W-IDKOLLI         TO MSGI-IDKOLLI                   
111300                 CALL W005INIT USING MSGI-WMSGINIT USEA-PCB               
111400* --- UPDATE THE MOD-KEY FIELDS!!                                         
111500                 MOVE MSGI-IDFAKT         TO RESP-IDFAKT                  
111600                 MOVE MSGI-IDKOLLI        TO RESP-IDKOLLI-KEY             
111700               ELSE                                                       
111800                 MOVE MFS-ALFA-FAELT-RAETT TO                             
111900                               RESP-SKAPA-PROFORMA-ATTR                   
112000               END-IF                                                     
112100             END-IF                                                       
112200             IF INDATA-SW NOT = 'Z'                                       
112300               PERFORM IMS-GNP-3171-CL11Y                                 
112400             END-IF                                                       
112500           END-PERFORM                                                    
112600         ELSE                                                             
112700           MOVE MFS-ALFA-FAELT-RAETT  TO RESP-SKAPA-PROFORMA-ATTR         
112800         END-IF                                                           
112900* --- IF FOR INVOICE WITHOUT PARTNUMBERS                                  
113000       END-IF                                                             
113100     END-IF                                                               
113200* --- CHECK INPUT FOR SUM BY CASEINFORMATION. FOR FIELDS                  
113300* --- REQU-KOLLI-VIKT-UPD AND REQU-KOLLI-VOL-UPD. BOTH MUST HAVE          
113400* --- EITHER VALUE '+' OR BOTH MUST HAVE A POSITIVE VALUE.                
113500     IF REQU-KOLLI-VIKT-UPD = ALL '+'                                     
113600     AND REQU-KOLLI-VOL-UPD NOT = ALL '+'                                 
113700       MOVE MFS-NUM-FAELT-RAETT TO RESP-KOLLI-VOL-UPD-ATTR                
113800       MOVE MFS-NUM-FAELT-FEL TO RESP-KOLLI-VIKT-UPD-ATTR                 
113900       MOVE ERR-UPDATE-WEIGHT   TO RESP-IDMSG-ERROR                       
114000       MOVE 'VKART'              TO RESP-IDELMT-ERROR                     
114100       MOVE NEJ TO INDATA-SW                                              
114200     ELSE                                                                 
114300       IF REQU-KOLLI-VOL-UPD = ALL '+'                                    
114400       AND REQU-KOLLI-VIKT-UPD NOT = ALL '+'                              
114500         MOVE MFS-NUM-FAELT-RAETT TO RESP-KOLLI-VIKT-UPD-ATTR             
114600         MOVE MFS-NUM-FAELT-FEL TO RESP-KOLLI-VOL-UPD-ATTR                
114700         MOVE ERR-UPDATE-VOLUME       TO RESP-IDMSG-ERROR                 
114800         MOVE 'VLART'            TO RESP-IDELMT-ERROR                     
114900         MOVE NEJ TO INDATA-SW                                            
115000       ELSE                                                               
115100         MOVE MFS-NUM-FAELT-RAETT TO RESP-KOLLI-VIKT-UPD-ATTR             
115200                                     RESP-KOLLI-VOL-UPD-ATTR              
115300       END-IF                                                             
115400     END-IF                                                               
115500* --- FIX, DECIMALCONVERSION OF INPUT                                     
115600     IF REQU-KOLLI-VIKT-UPD  NOT = ALL '+'                                
115700       MOVE REQU-KOLLI-VIKT-UPD     TO DEC-IDFRIDATA                      
115800       MOVE +6                      TO DEC-KVHELTAL                       
115900       MOVE +1                      TO DEC-KVDECIMAL                      
116000       CALL WDECEDIT USING DEC-WDECAREA                                   
116100       IF  DEC-KDSVAR-OK                                                  
116200          MOVE DEC-IDEDITDATA      TO W-KOLLI-VIKT-IN                     
116300       ELSE                                                               
116400          MOVE MFS-NUM-FAELT-FEL   TO RESP-KOLLI-VIKT-UPD-ATTR            
116500          MOVE NEJ                 TO INDATA-SW                           
116600       END-IF                                                             
116700     END-IF                                                               
116800* --- FIX, DECIMALER EN ELLER TVÅ ELLER TRE                               
116900     IF REQU-KOLLI-VOL-UPD  NOT = ALL '+'                                 
117000       MOVE REQU-KOLLI-VOL-UPD     TO DEC-IDFRIDATA                       
117100       MOVE +4                      TO DEC-KVHELTAL                       
117200       MOVE +3                      TO DEC-KVDECIMAL                      
117300       CALL WDECEDIT USING DEC-WDECAREA                                   
117400       IF  DEC-KDSVAR-OK                                                  
117500          MOVE DEC-IDEDITDATA      TO W-KOLLI-VOL-IN                      
117600       ELSE                                                               
117700          MOVE MFS-NUM-FAELT-FEL   TO RESP-KOLLI-VOL-UPD-ATTR             
117800          MOVE NEJ                 TO INDATA-SW                           
117900       END-IF                                                             
118000     END-IF                                                               
118100* --- CHECK INPUT FOR SUM BY INVOICEINFORMATION, FOR FIELDS               
118200* --- REQU-TOT-VIKT-UPD AND REQU-TOT-VOL-UPD. BOTH MUST HAVE              
118300* --- EITHER VALUE '+' OR BOTH MUST HAVE A POSITIVE VALUE.                
118400     IF REQU-TOT-VIKT-UPD = ALL '+'                                       
118500        AND REQU-TOT-VOL-UPD NOT = ALL '+'                                
118600        MOVE MFS-NUM-FAELT-RAETT TO RESP-TOT-VOL-UPD-ATTR                 
118700        MOVE MFS-NUM-FAELT-FEL TO RESP-TOT-VIKT-UPD-ATTR                  
118800        MOVE ERR-UPDATE-WEIGHT       TO RESP-IDMSG-ERROR                  
118900        MOVE 'VKART'             TO RESP-IDELMT-ERROR                     
119000        MOVE NEJ TO INDATA-SW                                             
119100     ELSE                                                                 
119200       IF REQU-TOT-VOL-UPD = ALL '+'                                      
119300          AND REQU-TOT-VIKT-UPD NOT = ALL '+'                             
119400          MOVE MFS-NUM-FAELT-RAETT TO RESP-TOT-VIKT-UPD-ATTR              
119500          MOVE MFS-NUM-FAELT-FEL TO RESP-TOT-VOL-UPD-ATTR                 
119600          MOVE ERR-UPDATE-VOLUME       TO RESP-IDMSG-ERROR                
119700          MOVE 'VLART'           TO RESP-IDELMT-ERROR                     
119800          MOVE NEJ TO INDATA-SW                                           
119900       ELSE                                                               
120000          MOVE MFS-NUM-FAELT-RAETT TO RESP-TOT-VIKT-UPD-ATTR              
120100                                      RESP-TOT-VOL-UPD-ATTR               
120200       END-IF                                                             
120300     END-IF                                                               
120400* --- FIX, DECIMALCONVERSION OF INPUT                                     
120500     IF REQU-TOT-VIKT-UPD        NOT = ALL '+'                            
120600       MOVE REQU-TOT-VIKT-UPD       TO DEC-IDFRIDATA                      
120700       MOVE +6                      TO DEC-KVHELTAL                       
120800       MOVE +1                      TO DEC-KVDECIMAL                      
120900       CALL WDECEDIT USING DEC-WDECAREA                                   
121000       IF  DEC-KDSVAR-OK                                                  
121100          MOVE DEC-IDEDITDATA      TO W-TOT-VIKT-IN                       
121200       ELSE                                                               
121300          MOVE MFS-NUM-FAELT-FEL   TO RESP-TOT-VIKT-UPD-ATTR              
121400          MOVE NEJ                 TO INDATA-SW                           
121500       END-IF                                                             
121600     END-IF                                                               
121700* --- FIX, DECIMALER EN ELLER TVÅ ELLER TRE TOTAL VOLYM                   
121800     IF REQU-TOT-VOL-UPD  NOT = ALL '+'                                   
121900       MOVE REQU-TOT-VOL-UPD     TO DEC-IDFRIDATA                         
122000       MOVE +4                      TO DEC-KVHELTAL                       
122100       MOVE +3                      TO DEC-KVDECIMAL                      
122200       CALL WDECEDIT USING DEC-WDECAREA                                   
122300       IF  DEC-KDSVAR-OK                                                  
122400          MOVE DEC-IDEDITDATA      TO W-TOT-VOL-IN                        
122500       ELSE                                                               
122600          MOVE MFS-NUM-FAELT-FEL   TO RESP-TOT-VOL-UPD-ATTR               
122700          MOVE NEJ                 TO INDATA-SW                           
122800       END-IF                                                             
122900     END-IF                                                               
123000* --- CHECK INPUT FOR DELETE FIELDS. BOTH FIELDS MUST BE COMPLETED        
123100* --- CHECK AGAINST BASE THAT PARTNO. EXISTS AND ENOUGH VOLUME            
123200* --- ON BASE TO ENABLE QUANTITY ENTERED TO BE DELETED.                   
123300     IF REQU-DEL-IDARTNR-OBJ not = all '+'                                
123400* --- CHECK THAT THIS IDARTNO EXISTS ON BASE (WITHIN CASE)                
123500       INSPECT REQU-DEL-IDARTNR-OBJ REPLACING LEADING SPACE               
123600                                  BY   ZERO                               
123700       IF REQU-DEL-IDARTNR-OBJ NUMERIC                                    
123800        MOVE    REQU-DEL-IDARTNR-OBJ TO W-IDARTNR                         
123900       ELSE                                                               
124000        MOVE    ZERO            TO  W-IDARTNR                             
124100       END-IF                                                             
124200       PERFORM  IMS-GHU-3171-CL31                                         
124300* --- IF IT DOESN'T EXIST, CREATE ERRORMESSAGE                            
124400       IF SEGMENT-SAKNAS                                                  
124500         MOVE PARTNO-MISSING     TO RESP-IDMSG-ERROR                      
124600         MOVE 'IDARTNR'          TO RESP-IDELMT-ERROR                     
124700         MOVE MFS-NUM-FAELT-FEL TO RESP-DEL-IDARTNR-OBJ-ATTR              
124800         MOVE NEJ TO INDATA-SW                                            
124900       ELSE                                                               
125000*         ELSE                                                            
125100         MOVE MFS-NUM-FAELT-RAETT TO RESP-DEL-IDARTNR-OBJ-ATTR            
125200         INSPECT REQU-DEL-KVANTAL REPLACING LEADING SPACE                 
125300                                    BY   ZERO                             
125400         IF REQU-DEL-KVANTAL NUMERIC                                      
125500          MOVE REQU-DEL-KVANTAL TO W-KVANTAL                              
125600         ELSE                                                             
125700          MOVE ZERO            TO W-KVANTAL                               
125800         END-IF                                                           
125900* --- CHECK THAT QUANTITY > 0 AND THAT QUANTITY ON                        
126000* --- SCREEN < OR = QUANTITY ON BASE.                                     
126100         IF ((W-KVANTAL > 3176-KVANTAL-DEB)                               
126200                    OR                                                    
126300             (W-KVANTAL = 0))                                             
126400           MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                  
126500           MOVE MFS-NUM-FAELT-FEL TO RESP-DEL-KVANTAL-ATTR                
126600           MOVE NEJ TO INDATA-SW                                          
126700         ELSE                                                             
126800           MOVE MFS-NUM-FAELT-RAETT TO RESP-DEL-IDARTNR-OBJ-ATTR          
126900                                         RESP-DEL-KVANTAL-ATTR            
127000                                                                          
127100         END-IF                                                           
127200       END-IF                                                             
127300     END-IF                                                               
127400* --- CHECK ON UPDATE OF PARTNO DATA                                      
127500     IF LINE-DATA-NOT-ALL-PLUS                                            
127600       PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > REQU-KVRADER         
127700        IF REQU-IDARTNR-OBJ (INDX) NOT = ALL '+'                          
127800         INSPECT REQU-IDARTNR-OBJ (INDX)                                  
127900                 REPLACING LEADING SPACE BY ZEROS                         
128000         IF REQU-IDARTNR-OBJ (INDX) NUMERIC                               
128100* --- KONTROLLERA ATT DET ÄR ETT BYTESNR OCH ATT ARTIKELNR                
128200* --- FINNS PÅ RESPECTIVE IDDC ARTIKELBAS SOM SKICKAR.                    
128300           MOVE REQU-IDARTNR-OBJ (INDX) TO W-IDARTNR                      
128400         ELSE                                                             
128500           MOVE ZERO                   TO W-IDARTNR                       
128600         END-IF                                                           
128700         MOVE W-IDDC             TO WS-IDDC                               
128800* --- VALIDATE SJK                                                        
128900         PERFORM IMS-GU-ARTC-C11                                          
129000         IF SEGMENT-SAKNAS                                                
129100           MOVE PARTNO-MISSING    TO RESP-IDMSG-ERROR                     
129200           MOVE 'IDARTNR'         TO RESP-IDELMT-ERROR                    
129300           MOVE MFS-NUM-FAELT-FEL TO RESP-IDARTNR-OBJ-ATTR(INDX)          
129400           MOVE NEJ               TO INDATA-SW                            
129500           MOVE ZERO              TO CLAG-PRARTSJK                        
129600         END-IF                                                           
129700                                                                          
129800         IF CLAG-PRARTSJK > ZERO                                          
129900           IF NOT DCS-CDC                                                 
130000             PERFORM IMS-GHU-WDK711                                       
130100           END-IF                                                         
130200           MOVE REQU-IDARTNR-OBJ (INDX) TO TEST-IDARTNR                   
130300* --- OM ARTIKELNR VERKAR OK GÄLLER DET ATT KOLLA ATT KVANTEN             
130400* --- OCKSÅ ÄR OK                                                         
130500           IF BYT03-OBJEKT  AND SEGMENT-FINNS                             
130600* --- EFTERSOM DET KAN FINNAS RADER MED SAMMA ARTIKELNR MÅSTE EN          
130700* --- KONTROLL GÖRAS MOT HÄNDELSEBASEN FÖR AKTUELL ARTIKEL.               
130800* --- SUMMAN FÅR EJ ÖVERSTIGA TILLGÄNGLIG I LAGER. SUMMAN LAGRAS          
130900* --- I FÄLT W-SUM-OF SOM ÄR KVLS MINUS SUMMAN I HÄNDELSEBASEN.           
131000* --- MOVE RÄTT VÄRDEN TILL NYCKELFÄLT                                    
131100             MOVE MSGI-IDFAKT TO W-IDFAKT                                 
131200             PERFORM IMS-GU-3171-CL11                                     
131300* --- SÖK IGENOM                                                          
131400             PERFORM UNTIL SEGMENT-SAKNAS                                 
131500               IF 3171-SEG-LEVEL = '04'                                   
131600                 IF 3176-IDARTNR-OBJ = W-IDARTNR                          
131700                   ADD W-SUM-OF TO                                        
131800                   3176-KVANTAL-DEB GIVING W-SUM-OF                       
131900                 END-IF                                                   
132000               END-IF                                                     
132100               PERFORM IMS-GNP-3171-CL11X                                 
132200             END-PERFORM                                                  
132300* --- UTRÄKNING AV KVANTITET SOM FÅR PACKAS I KOLLITSTART                 
132400             COMPUTE W-SUM-OF =                                           
132500                     SLAG-KVLS - W-SUM-OF                                 
132600* --- UTRÄKNING AV KVANTITET SOM FÅR PACKAS I KOLLITSLUT                  
132700*              IF (REQU-KVANTAL (INDX) > W-SUM-OF )                       
132800             INSPECT REQU-KVANTAL (INDX) REPLACING                        
132900                     LEADING SPACE BY ZERO                                
133000                     LEADING  '+'  BY ZERO                                
133100* --- STOP ZERO INPUT OF QUANTITY                                         
133200             IF REQU-KVANTAL (INDX) = '0000000'                           
133300               MOVE ALL SPACE TO  REQU-KVANTAL (INDX)                     
133400             END-IF                                                       
133500                                                                          
133600             IF REQU-KVANTAL (INDX) NUMERIC                               
133700               MOVE REQU-KVANTAL (INDX) TO                                
133800                                W-KVANTAL                                 
133900               MOVE W-SUM-OF  TO RESP-KVLS (INDX)                         
134000               IF (W-KVANTAL  > W-SUM-OF )                                
134100                                                                          
134200                 MOVE WS-CP-EBCDIC      TO TRAUTF8-KDCP                   
134300                 MOVE 'Too many!'       TO TRAUTF8-TECONV-FROM            
134400                                           TRAUTF8-TECONV-TO              
134500                 IF REQU-IDMSGVER = '001'                                 
134600                   CALL WTRAUTF8 USING TRAUTF8-AREA                       
134700                 END-IF                                                   
134800                 MOVE TRAUTF8-TECONV-TO TO RESP-BEART (INDX)              
134900                                                                          
135000                 MOVE NEJ TO INDATA-SW                                    
135100                 MOVE MFS-NUM-FAELT-RAETT TO                              
135200                                RESP-IDARTNR-OBJ-ATTR (INDX)              
135300                 MOVE MFS-NUM-FAELT-FEL TO                                
135400                                RESP-KVANTAL-ATTR (INDX)                  
135500               ELSE                                                       
135600                 MOVE MFS-NUM-FAELT-RAETT TO                              
135700                                RESP-IDARTNR-OBJ-ATTR (INDX)              
135800                                RESP-KVANTAL-ATTR (INDX)                  
135900                 MOVE ALL-SPACE TO RESP-KVLS (INDX)                       
136000                                RESP-KVLS (INDX)                          
136100               END-IF                                                     
136200             ELSE                                                         
136300               MOVE NEJ TO INDATA-SW                                      
136400               MOVE MFS-NUM-FAELT-FEL TO                                  
136500                                RESP-KVANTAL-ATTR (INDX)                  
136600               MOVE MFS-NUM-FAELT-RAETT TO                                
136700                                RESP-IDARTNR-OBJ-ATTR (INDX)              
136800               MOVE WS-CP-EBCDIC      TO TRAUTF8-KDCP                     
136900               MOVE 'Not numeric!'    TO TRAUTF8-TECONV-FROM              
137000                                         TRAUTF8-TECONV-TO                
137100               IF REQU-IDMSGVER = '001'                                   
137200                 CALL WTRAUTF8 USING TRAUTF8-AREA                         
137300               END-IF                                                     
137400               MOVE TRAUTF8-TECONV-TO TO RESP-BEART (INDX)                
137500             END-IF                                                       
137600             MOVE ZERO TO W-SUM-OF                                        
137700           ELSE                                                           
137800* --- OM DET INTE ÄR BYTESNR FELMARKERA FÄLTET                            
137900             MOVE MFS-NUM-FAELT-FEL TO                                    
138000                                RESP-IDARTNR-OBJ-ATTR (INDX)              
138100             MOVE MFS-NUM-FAELT-FEL TO                                    
138200                                RESP-KVANTAL-ATTR (INDX)                  
138300             MOVE WS-CP-EBCDIC      TO TRAUTF8-KDCP                       
138400             MOVE 'Invalid P/N!'    TO TRAUTF8-TECONV-FROM                
138500                                       TRAUTF8-TECONV-TO                  
138600             IF REQU-IDMSGVER = '001'                                     
138700               CALL WTRAUTF8 USING TRAUTF8-AREA                           
138800             END-IF                                                       
138900             MOVE TRAUTF8-TECONV-TO TO RESP-BEART (INDX)                  
139000             MOVE NEJ TO INDATA-SW                                        
139100           END-IF                                                         
139200         ELSE                                                             
139300* --- OM DET INTE ÄR FELAKTIG ARTIKEL                                     
139400             MOVE MFS-NUM-FAELT-FEL TO                                    
139500                                RESP-IDARTNR-OBJ-ATTR (INDX)              
139600             MOVE MFS-NUM-FAELT-FEL TO                                    
139700                                RESP-KVANTAL-ATTR (INDX)                  
139800             MOVE WS-CP-EBCDIC      TO TRAUTF8-KDCP                       
139900             MOVE 'Invalid P/N!'    TO TRAUTF8-TECONV-FROM                
140000                                       TRAUTF8-TECONV-TO                  
140100             IF REQU-IDMSGVER = '001'                                     
140200               CALL WTRAUTF8 USING TRAUTF8-AREA                           
140300             END-IF                                                       
140400             MOVE TRAUTF8-TECONV-TO TO RESP-BEART (INDX)                  
140500             MOVE NEJ TO INDATA-SW                                        
140600         END-IF                                                           
140700        ELSE                                                              
140800          MOVE MFS-NUM-FAELT-RAETT TO                                     
140900                                    RESP-IDARTNR-OBJ-ATTR (INDX)          
141000                                    RESP-KVANTAL-ATTR (INDX)              
141100        END-IF                                                            
141200       END-PERFORM                                                        
141300     ELSE                                                                 
141400       PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > REQU-KVRADER         
141500         MOVE MFS-NUM-FAELT-RAETT TO RESP-IDARTNR-OBJ-ATTR (INDX)         
141600                                     RESP-KVANTAL-ATTR (INDX)             
141700       END-PERFORM                                                        
141800     END-IF                                                               
141900                                                                          
142000     IF INDATA-FEL                                                        
142100       PERFORM MFS-ROER-EJ-FAELT-UT                                       
142200       PERFORM MFS-ROER-EJ-FAELT-IN                                       
142300       PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > REQU-KVRADER         
142400         IF   SAVE-SWITCH = '888'                                         
142500           MOVE MFS-CLOSE-FIELD TO  RESP-IDARTNR-OBJ-ATTR (INDX)          
142600           MOVE MFS-CLOSE-FIELD TO  RESP-KVANTAL-ATTR (INDX)              
142700         END-IF                                                           
142800       END-PERFORM                                                        
142900     END-IF                                                               
143000     .                                                                    
143100     EJECT                                                                
143200 H-UPPDATERA SECTION.                                                     
143300                                                                          
143400* --- UPDATE CURRENT INVOICE WITH NEW WEIGHT OR VOLUME                    
143500     MOVE MSGI-IDFAKT TO W-IDFAKT                                         
143600     PERFORM IMS-GU-3171-CL11                                             
143700     IF REQU-TOT-VIKT-UPD NOT = ALL '+'                                   
143800       MOVE W-TOT-VIKT-IN   TO 3172-VKORDBTO-FAKT                         
143900     END-IF                                                               
144000     IF REQU-TOT-VOL-UPD NOT = ALL '+'                                    
144100       MOVE W-TOT-VOL-IN   TO 3172-VLORDBTO-FAKT                          
144200       IF W-TOT-VOL-IN  = 0                                               
144300        MOVE 0              TO 3172-SUFKTNTO                              
144400       ELSE                                                               
144500        MOVE 1              TO 3172-SUFKTNTO                              
144600       END-IF                                                             
144700       MOVE INF-TOT-WEIGHT-UPDATED                                        
144800                            TO RESP-IDMSG-INFO                            
144900       MOVE 'VKORDBTO-FAKT'      TO RESP-IDELMT-ERROR                     
145000       MOVE MFS-ADD-SET-CURSOR TO RESP-SKAPA-PROFORMA-ATTR                
145100       MOVE 'Y' TO W-TOT-CURS                                             
145200     END-IF                                                               
145300* --- UPDATE CURRENT CASE WITH NEW WEIGHT OR VOLUME                       
145400     MOVE MSGI-IDKOLLI TO W-IDKOLLI                                       
145500     PERFORM IMS-GHU-3171-CL21                                            
145600     IF REQU-KOLLI-VIKT-UPD NOT = ALL '+'                                 
145700* --- CALCULATE TOTAL WEIGHT AND UPDATE TOTAL WEIGHT* SECTIONA            
145800      IF 3172-SUFKTNTO NOT  = 1                                           
145900        COMPUTE                                                           
146000        WS-TOT-VIKT-IN = W-KOLLI-VIKT-IN  - 3174-VKORDBTO-KOLLI           
146100        COMPUTE                                                           
146200        WS-TOT-VOL-IN  = W-KOLLI-VOL-IN  - 3174-VLORDBTO-KOLLI            
146300      END-IF                                                              
146400      MOVE W-KOLLI-VIKT-IN   TO 3174-VKORDBTO-KOLLI                       
146500     END-IF                                                               
146600     IF REQU-KOLLI-VOL-UPD NOT = ALL '+'                                  
146700       MOVE W-KOLLI-VOL-IN    TO 3174-VLORDBTO-KOLLI                      
146800       PERFORM IMS-REPL-3171-CL21                                         
146900       MOVE                                                               
147000       INF-WEIGHT-OF-CASE-UPDATED                                         
147100                       TO RESP-IDMSG-INFO                                 
147200       MOVE 'VKARTBTO-KOLLI'     TO RESP-IDELMT-ERROR                     
147300* --- MOVE CURSOR TO INPUT OF PARTNO LINE.                                
147400           MOVE '777' TO SAVE-SWITCH                                      
147500* --- CALCULATE TOTAL WEIGHT AND UPDATE TOTAL WEIGHT* SECTIONB            
147600      IF 3172-SUFKTNTO NOT  = 1                                           
147700       PERFORM IMS-GHU-3171-CL11                                          
147800       COMPUTE                                                            
147900       3172-VKORDBTO-FAKT = 3172-VKORDBTO-FAKT + WS-TOT-VIKT-IN           
148000       COMPUTE                                                            
148100       3172-VLORDBTO-FAKT = 3172-VLORDBTO-FAKT + WS-TOT-VOL-IN            
148200       PERFORM IMS-REPL-3171-CL11                                         
148300      END-IF                                                              
148400     END-IF                                                               
148500* --- DELETE CURRENT CASE FROM CURRENT INVOICE                            
148600     IF REQU-DEL-IDKOLLI = '0000y' OR '0000Y'                             
148700       MOVE 3174-VKORDBTO-KOLLI TO WS-TOT-VIKT-IN                         
148800       MOVE 3174-VLORDBTO-KOLLI TO WS-TOT-VOL-IN                          
148900       PERFORM IMS-DLET-3171-CL21                                         
149000       PERFORM IMS-GU-3171-CL11                                           
149100       PERFORM IMS-GHNPL-3171-CL21                                        
149200        IF SEGMENT-SAKNAS                                                 
149300          PERFORM IMS-ISRT-3171-CL21                                      
149400          MOVE INF-CANT-DEL-ALL-CASES TO RESP-IDMSG-INFO                  
149500        ELSE                                                              
149600         MOVE                                                             
149700         INF-CASE-DEL-ENTER-LAST-CASE                                     
149800                       TO RESP-IDMSG-INFO                                 
149900         IF  SAVE-SWITCH = '777'                                          
150000           MOVE '888' TO SAVE-SWITCH                                      
150100         ELSE                                                             
150200           MOVE '777' TO SAVE-SWITCH                                      
150300         END-IF                                                           
150400         IF 3172-SUFKTNTO NOT = 1                                         
150500          PERFORM IMS-GHU-3171-CL11                                       
150600          COMPUTE                                                         
150700                     3172-VKORDBTO-FAKT = 3172-VKORDBTO-FAKT              
150800                                        - WS-TOT-VIKT-IN                  
150900          COMPUTE                                                         
151000                     3172-VLORDBTO-FAKT = 3172-VLORDBTO-FAKT              
151100                                        - WS-TOT-VOL-IN                   
151200          PERFORM IMS-REPL-3171-CL11                                      
151300         END-IF                                                           
151400        END-IF                                                            
151500* --- OBS! INNAN DELETE JUSTERA TOTALVIKTEN FÖR FAKTURAN                  
151600* --- OBS! INNAN DELETE JUSTERA TOTALVIKTEN FÖR FAKTURAN                  
151700     END-IF                                                               
151800                                                                          
151900* --- DELETE PARTS AND QUANTITY FROM CURRRENT CASE                        
152000     IF REQU-DEL-IDARTNR-OBJ not = all '+'                                
152100       MOVE    REQU-DEL-IDARTNR-OBJ TO W-IDARTNR                          
152200       PERFORM  IMS-GHU-3171-CL31                                         
152300       MOVE REQU-DEL-KVANTAL TO W-KVANTAL                                 
152400       IF  (W-KVANTAL = 3176-KVANTAL-DEB)                                 
152500         PERFORM IMS-DLET-3171-CL31                                       
152600       ELSE                                                               
152700         SUBTRACT W-KVANTAL FROM 3176-KVANTAL-DEB                         
152800                          GIVING 3176-KVANTAL-DEB                         
152900         PERFORM IMS-REPL-3171-CL31                                       
153000       END-IF                                                             
153100       MOVE                                                               
153200       INF-PARTS-DEL-FROM-CASE                                            
153300                       TO RESP-IDMSG-INFO                                 
153400     END-IF                                                               
153500                                                                          
153600* --- ADD NEW PARTS TO THE CURRENT CASE                                   
153700     IF LINE-DATA-NOT-ALL-PLUS                                            
153800       PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > REQU-KVRADER         
153900       PERFORM IMS-GU-3171-CL21                                           
154000         IF REQU-IDARTNR-OBJ (INDX) NOT = ALL '+'                         
154100           MOVE REQU-IDARTNR-OBJ (INDX) TO W-IDARTNR                      
154200                                           3176-IDARTNR-OBJ               
154300           MOVE REQU-KVANTAL (INDX) TO 3176-KVANTAL-DEB                   
154400           MOVE 3176-KVANTAL-DEB  TO  3176-KVANTMOT                       
154500           PERFORM IMS-ISRT-3171-CL31                                     
154600           IF SEGMENT-FINNS-REDAN                                         
154700              PERFORM IMS-GHU-3171-CL31                                   
154800              MOVE REQU-KVANTAL (INDX) TO W-KVANTAL                       
154900              ADD W-KVANTAL TO 3176-KVANTAL-DEB                           
155000                        GIVING 3176-KVANTAL-DEB                           
155100              PERFORM IMS-REPL-3171-CL31                                  
155200           END-IF                                                         
155300         END-IF                                                           
155400       END-PERFORM                                                        
155500       MOVE                                                               
155600       INF-PARTS-ADDED-TO-CASE                                            
155700                       TO RESP-IDMSG-INFO                                 
155800     END-IF                                                               
155900* --- COMPLETE THE INVOICE, AND CREATE PROFORMA ETC...                    
156000     IF REQU-SKAPA-PROFORMA = 'Y' OR 'y'                                  
156100* --- CHECK THAT PART EXISTS AT RECEIVING WAREHOUSE.                      
156200       PERFORM IMS-GHU-3171-CL11                                          
156300       PERFORM IMS-GHNP-3171-CL31                                         
156400       PERFORM UNTIL SEGMENT-SAKNAS                                       
156500         IF 3171-SEG-LEVEL = '04'                                         
156600           MOVE 3172-IDDC-REC       TO WS-IDDC                            
156700                                       W-IDDC-B6-REC                      
156800           PERFORM IMS-GU-WDB601-REC                                      
156900           IF REC-DCS-SDC AND REC-DCS-HOLLAND                             
157000             MOVE WS-SDC-91   TO  W-IDDC                                  
157100             MOVE 3176-IDARTNR-OBJ TO W-IDARTNR                           
157200             PERFORM IMS-GHU-WDK711                                       
157300* --- IF PART DOESNT EXIST, CREATE ON WDK7.                               
157500             IF SEGMENT-SAKNAS                                            
157600               PERFORM HA-NEW-WDK7                                        
157800             END-IF                                                       
157900             MOVE REQU-IDDC-KEY TO W-IDDC                                 
158000           END-IF                                                         
158100* --- UPDATE STOCK SALDO ON WDK6 AND WDK7.                                
158200           PERFORM HC-STOCK-UPDATE                                        
158300* --- UPDATE SOL SYSTEM ON WDL9.                                          
158400*          PERFORM HDA-SOL-UPDATE-RECV                                    
158500*          PERFORM HDB-SOL-UPDATE-SEND                                    
158600         END-IF                                                           
158700         PERFORM IMS-GHNP-3171-CL31                                       
158800       END-PERFORM                                                        
158900* --- CALL PROGRAM TO CREATE PROFORMA INVOICE.                            
159000* --- ALT TRANS--ANROP!!!!!!!!!!                                          
159100* --- UPDATE STATUSCODE ON XCHANGE CLEARING  BASE WL3171.                 
159200       PERFORM IMS-GHU-3171-CL11                                          
159300       MOVE '2' TO 3172-KDTRSTAT                                          
159400       MOVE FUNCTION CURRENT-DATE (1:8) TO 3172-DASNDDAT                  
159500       MOVE WS-FAKTVAL TO  3172-SUFKTNTO                                  
159600       PERFORM IMS-REPL-3171-CL11                                         
159700       PERFORM HE-SOP-PROFORMA                                            
159800       MOVE                                                               
159900       INF-PROFORMA-PRINTING                                              
160000                       TO RESP-IDMSG-INFO                                 
160100     END-IF                                                               
160200* --- AFTER UPDATE , MOVE INFO TO RELEVANT MOD-FIELDS.                    
160300       PERFORM MFS-FORM-ATTR                                              
160400       PERFORM MFS-RENSA-FAELT-IN                                         
160500       PERFORM MFS-RENSA-FAELT-OUT                                        
160600       MOVE SAVE-ENTER-IDARTNO TO W-IDARTNR                               
160700* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
160800*    END-IF                                                               
160900       CONTINUE                                                           
161000     .                                                                    
161100     EJECT                                                                
161200 HA-NEW-WDK7 SECTION.                                                     
161300                                                                          
161400     MOVE ALL '+'      TO WDK7-W005WDK7                                   
161500     MOVE 'WDK711'     TO WDK7-IDSEGM                                     
161600     MOVE W-IDARTNR    TO WDK7-IDARTNR-KFB                                
161700     MOVE WS-SDC-91    TO WDK7-IDDC-KFB                                   
161800                          WDK7-IDDC                                       
161810     MOVE 'N'          TO WDK7-FLREFILL                                   
161900                                                                          
162000     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB ARTC-PCB WDK7-PCB         
162100     .                                                                    
162200     EJECT                                                                
169000                                                                          
174300 HC-STOCK-UPDATE SECTION.                                                 
174400                                                                          
174500     MOVE 3172-IDDC-SEND      TO W-IDDC                                   
174600     MOVE 3176-IDARTNR-OBJ    TO W-IDARTNR                                
174700     PERFORM IMS-GU-ARTC-C11                                              
174800     PERFORM IMS-GHU-WDK711                                               
174900     IF SEGMENT-FINNS                                                     
175000       COMPUTE                                                            
175100        SLAG-KVLS = SLAG-KVLS - 3176-KVANTAL-DEB                          
175200       PERFORM IMS-REPL-WDK711                                            
175300       PERFORM HDA-SOL-UPDATE-SEND                                        
175400     END-IF                                                               
175500                                                                          
175600     MOVE 3172-IDDC-REC       TO W-IDDC                                   
175700     MOVE 3176-IDARTNR-OBJ    TO W-IDARTNR                                
175800     PERFORM IMS-GHU-WDK711                                               
175900     IF SEGMENT-FINNS                                                     
176000       COMPUTE                                                            
176100        SLAG-KVAKS-SDC = SLAG-KVAKS-SDC + 3176-KVANTAL-DEB                
176200       PERFORM IMS-REPL-WDK711                                            
176300       PERFORM HDB-SOL-UPDATE-RECV                                        
176400     END-IF                                                               
176500* --- HERE WE CALCULATE THE INVOICE VALUE FOR UPDATE OF                   
176600* --- THE TRANSACTION REGISTER-3171                                       
176700     COMPUTE                                                              
176800     WS-FAKTVAL = WS-FAKTVAL                                              
176900            + (3176-KVANTAL-DEB * CLAG-PRARTSJK)                          
177000     .                                                                    
177100     EJECT                                                                
177200 HDA-SOL-UPDATE-SEND SECTION.                                             
177300                                                                          
177400* --- FIXA SÄNDANDE SLAGER BOKNING I SOL                                  
177500     MOVE 3176-IDARTNR-OBJ        TO LOGG-IDARTNR                         
177600     MOVE FUNCTION CURRENT-DATE(1:8) TO W-DAGENS-DATUM                    
177700     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - W-DAGENS-DATUM             
177800     ACCEPT TRANS-TID FROM TIME                                           
177900     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TID                 
178000     MOVE 9                       TO LOGG-IDSEKVNR                        
178100     MOVE SLAG-IDDC               TO LOGG-IDDC                            
178200     MOVE 'EXCH'                  TO LOGG-IDHUVTYP                        
178300     MOVE 'OBJ'                   TO LOGG-IDSUBTYP                        
178400     MOVE IDPGM                   TO LOGG-IDPGM                           
178500     MOVE '3182'                  TO LOGG-IDTRANS                         
178600     MOVE REQU-IDUSER             TO LOGG-IDUSER                          
178700     MOVE SPACE                   TO LOGG-REF                             
178800     MOVE W-IDFAKT                TO LOGG-IDFAKT                          
178900     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS-PAV              
179000     MOVE SPACE                   TO LOGG-IDTECKEN-KVEFRS                 
179100     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS                  
179200     MOVE SPACE                   TO LOGG-IDTECKEN-KVLS                   
179300     MOVE 3176-KVANTAL-DEB        TO LOGG-KVART-SALDO                     
179400     MOVE SLAG-KVLS               TO LOGG-KVLS                            
179500     MOVE SLAG-KVAKS-SDC          TO LOGG-KVAKS                           
179600     MOVE SLAG-KVAKS-PAV          TO LOGG-KVAKS-PAV                       
179700     MOVE SLAG-KVEFRS             TO LOGG-KVEFRS                          
179800     MOVE ZERO                    TO LOGG-DAREGDAT-LADD                   
179900     MOVE 3172-IDDC-SEND          TO WS-IDDC                              
180000     MOVE '-'                     TO LOGG-IDTECKEN-KVLS                   
180100                                                                          
180200     PERFORM S16-UPDATE-SOL                                               
180300     .                                                                    
180400     EJECT                                                                
180500 HDB-SOL-UPDATE-RECV SECTION.                                             
180600                                                                          
180700* --- FIXA MOTTAGANDE SLAGER BOKNING I SOL                                
180800     MOVE 3176-IDARTNR-OBJ      TO LOGG-IDARTNR                           
180900     MOVE FUNCTION CURRENT-DATE(1:8) TO W-DAGENS-DATUM                    
181000     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - W-DAGENS-DATUM             
181100     ACCEPT TRANS-TID FROM TIME                                           
181200     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TID                 
181300     MOVE 9                       TO LOGG-IDSEKVNR                        
181400     MOVE '91'                    TO LOGG-IDDC                            
181500     MOVE 'EXCH'                  TO LOGG-IDHUVTYP                        
181600     MOVE 'OBJ'                   TO LOGG-IDSUBTYP                        
181700     MOVE IDPGM                   TO LOGG-IDPGM                           
181800     MOVE '3182'                  TO LOGG-IDTRANS                         
181900     MOVE REQU-IDUSER             TO LOGG-IDUSER                          
182000     MOVE SPACE                   TO LOGG-REF                             
182100     MOVE W-IDFAKT                TO LOGG-IDFAKT                          
182200     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS-PAV              
182300     MOVE SPACE                   TO LOGG-IDTECKEN-KVEFRS                 
182400     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS                  
182500     MOVE SPACE                   TO LOGG-IDTECKEN-KVLS                   
182600     MOVE 3176-KVANTAL-DEB        TO LOGG-KVART-SALDO                     
182700     MOVE SLAG-KVLS               TO LOGG-KVLS                            
182800     MOVE SLAG-KVAKS-SDC          TO LOGG-KVAKS                           
182900     MOVE SLAG-KVAKS-PAV          TO LOGG-KVAKS-PAV                       
183000     MOVE SLAG-KVEFRS             TO LOGG-KVEFRS                          
183100     MOVE ZERO                    TO LOGG-DAREGDAT-LADD                   
183200     MOVE 3172-IDDC-REC           TO WS-IDDC                              
183300     MOVE '+'                     TO LOGG-IDTECKEN-KVAKS                  
183400                                                                          
183500     PERFORM S16-UPDATE-SOL                                               
183600     .                                                                    
183700     EJECT                                                                
183800 HE-SOP-PROFORMA SECTION.                                                 
183900                                                                          
184000* --- AKTIVERING AV RUTIN W371S4 I SOP                                    
184100* --- IDFAKT SKICKAS MED SOM SYMBOLISK PARAMETER                          
184200     MOVE '3182'   TO MSGSOP-IDTRANS                                      
184300     MOVE '1'      TO MSGSOP-KDMFSFOR                                     
184400     MOVE 'W371S4' TO MSGSOP-IDPROCESS                                    
184500     MOVE 'O'      TO MSGSOP-KDSOPFUNK                                    
184600     MOVE W-IDFAKT TO W-KDFAKT                                            
184700     STRING 'IDFAKT(' W-KDFAKT ') '                                       
184800            'IDDC(' REQU-IDDC-KEY ') '                                    
184900            'IDUSER(' REQU-IDUSER ')'                                     
185000            DELIMITED BY SIZE INTO MSGSOP-TESYMBV                         
185100                                                                          
185200     PERFORM IMS-ISRT-ALTMSG                                              
185300     .                                                                    
185400     EJECT                                                                
185500 S01-INIT-LINE-DATA SECTION.                                              
185600                                                                          
185700     PERFORM                                                              
185800     VARYING INDX FROM +1 BY +1                                           
185900       UNTIL INDX > MAX-KVRADER                                           
186000       MOVE ALL-PLUS             TO REQU-NYRAD (INDX)                     
186100     END-PERFORM                                                          
186200     .                                                                    
186300     EJECT                                                                
186400 S16-UPDATE-SOL SECTION.                                                  
186500                                                                          
186600     PERFORM IMS-ISRT-LOGA-SALDO                                          
186700     IF SEGMENT-FINNS-REDAN                                               
186800       PERFORM UNTIL SEGMENT-FINNS                                        
186900         ADD -1 TO LOGG-IDSEKVNR                                          
187000         PERFORM IMS-ISRT-LOGA-SALDO                                      
187100       END-PERFORM                                                        
187200     END-IF                                                               
187300     .                                                                    
187400     EJECT                                                                
187500 S20-WEB-DIALOG-SWITCH SECTION.                                           
187600                                                                          
187700     IF SAVE-SWITCH = '888'                                               
187800*      888 - SHOW DATA MODE                                               
187900       MOVE 'S' TO RESP-KDDIAVAR                                          
188000     ELSE                                                                 
188100*      777 - INPUT DATA MODE                                              
188200       MOVE 'I' TO RESP-KDDIAVAR                                          
188300     END-IF                                                               
188400     .                                                                    
188500     EJECT                                                                
188600 MFS-RENSA-FAELT-OUT SECTION.                                             
188700                                                                          
188800* --- ALLA ICKE NYCKEL-OUT FÄLT                                           
188900     MOVE ALL-SPACE TO                                                    
189000                               RESP-KOLLI-VIKT-UT                         
189100                               RESP-KOLLI-VOL-UT                          
189200                               RESP-TOT-VIKT-UT                           
189300                               RESP-TOT-VOL-UT                            
189400     PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-KVRADER            
189500       MOVE ALL-SPACE TO RESP-BEART (INDX)                                
189600       IF REQU-IDMSGVER = '101'                                           
189700         MOVE ALL-SPACE         TO RESP-BEART (INDX)                      
189800       ELSE                                                               
189900         MOVE ALL-SPACE-UTF8    TO RESP-BEART (INDX)                      
190000       END-IF                                                             
190100       MOVE ALL-SPACE TO RESP-KVLS (INDX)                                 
190200     END-PERFORM                                                          
190300     .                                                                    
190400     SKIP3                                                                
190500 MFS-RENSA-FAELT-IN SECTION.                                              
190600                                                                          
190700* --- ALLA INDATA-FÄLT                                                    
190800     MOVE ALL-SPACE        TO                                             
190900*                            RESP-IDKOLLI-KEY                             
191000*                            RESP-NYTT-KOLLI-UPD                          
191100                             RESP-KOLLI-VIKT-UPD                          
191200                             RESP-KOLLI-VOL-UPD                           
191300                             RESP-TOT-VIKT-UPD                            
191400                             RESP-TOT-VOL-UPD                             
191500                             RESP-SKAPA-PROFORMA                          
191600                             RESP-DEL-IDARTNR-OBJ                         
191700                             RESP-DEL-KVANTAL                             
191800                             RESP-DEL-IDKOLLI                             
191900     PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-KVRADER            
192000       MOVE ALL-SPACE TO RESP-IDARTNR-OBJ (INDX)                          
192100                             RESP-KVANTAL (INDX)                          
192200     END-PERFORM                                                          
192300     .                                                                    
192400     EJECT                                                                
192500 MFS-RENSA-FAELT-IN-X SECTION.                                            
192600                                                                          
192700* --- ALLA INDATA-FÄLT                                                    
192800     MOVE ALL-SPACE        TO                                             
192900                             RESP-KOLLI-VIKT-UPD                          
193000                             RESP-KOLLI-VOL-UPD                           
193100                             RESP-TOT-VIKT-UPD                            
193200                             RESP-TOT-VOL-UPD                             
193300                             RESP-SKAPA-PROFORMA                          
193400                             RESP-DEL-IDARTNR-OBJ                         
193500                             RESP-DEL-KVANTAL                             
193600                             RESP-DEL-IDKOLLI                             
193700     .                                                                    
193800     EJECT                                                                
193900 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
194000                                                                          
194100* --- ALLA UTDATA-FÄLT                                                    
194200     MOVE ALL-PLUS TO                                                     
194300                               RESP-IDKOLLI-KEY                           
194400                               RESP-IDFAKT                                
194500                               RESP-KOLLI-VIKT-UT                         
194600                               RESP-KOLLI-VOL-UT                          
194700                               RESP-TOT-VIKT-UT                           
194800                               RESP-TOT-VOL-UT                            
194900     IF SAVE-SWITCH = '888'                                               
195000       PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-KVRADER          
195100         IF REQU-IDMSGVER = '101'                                         
195200           MOVE ALL-PLUS          TO RESP-BEART (INDX)                    
195300         ELSE                                                             
195400           MOVE ALL-PLUS-UTF8     TO RESP-BEART (INDX)                    
195500         END-IF                                                           
195600         MOVE ALL-PLUS TO RESP-KVLS (INDX)                                
195700       END-PERFORM                                                        
195800     END-IF                                                               
195900     .                                                                    
196000     SKIP3                                                                
196100 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
196200                                                                          
196300* --- ALLA INDATA-FÄLT                                                    
196400     MOVE ALL-PLUS TO RESP-IDKOLLI-KEY                                    
196500                               RESP-NYTT-KOLLI-UPD                        
196600                               RESP-KOLLI-VIKT-UPD                        
196700                               RESP-KOLLI-VOL-UPD                         
196800                               RESP-TOT-VIKT-UPD                          
196900                               RESP-TOT-VOL-UPD                           
197000                               RESP-SKAPA-PROFORMA                        
197100                               RESP-DEL-IDARTNR-OBJ                       
197200                               RESP-DEL-KVANTAL                           
197300                               RESP-DEL-IDKOLLI                           
197400     PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-KVRADER            
197500       MOVE ALL-PLUS TO RESP-IDARTNR-OBJ (INDX)                           
197600                                 RESP-KVANTAL (INDX)                      
197700     END-PERFORM                                                          
197800     .                                                                    
197900     EJECT                                                                
198000 MFS-FORM-ATTR SECTION.                                                   
198100                                                                          
198200* --- ALLA INDATA-FÄLT                                                    
198300     MOVE MFS-FORMATETS-ATTR TO RESP-IDKOLLI-KEY-ATTR                     
198400                                RESP-NYTT-KOLLI-UPD-ATTR                  
198500                                RESP-KOLLI-VIKT-UPD-ATTR                  
198600                                RESP-KOLLI-VOL-UPD-ATTR                   
198700                                RESP-TOT-VIKT-UPD-ATTR                    
198800                                RESP-TOT-VOL-UPD-ATTR                     
198900                                RESP-SKAPA-PROFORMA-ATTR                  
199000                                RESP-DEL-IDARTNR-OBJ-ATTR                 
199100                                RESP-DEL-KVANTAL-ATTR                     
199200                                RESP-DEL-IDKOLLI-ATTR                     
199300     PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-KVRADER            
199400       MOVE MFS-FORMATETS-ATTR TO RESP-IDARTNR-OBJ-ATTR (INDX)            
199500                                  RESP-KVANTAL-ATTR (INDX)                
199600     END-PERFORM                                                          
199700     .                                                                    
199800     SKIP2                                                                
199900 MFS-LAES-IN-IGEN SECTION.                                                
200000                                                                          
200100* --- ALLA INDATA-FÄLT                                                    
200200     MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDKOLLI-KEY-ATTR                  
200300                                   RESP-NYTT-KOLLI-UPD-ATTR               
200400                                   RESP-KOLLI-VIKT-UPD-ATTR               
200500                                   RESP-KOLLI-VOL-UPD-ATTR                
200600                                   RESP-TOT-VIKT-UPD-ATTR                 
200700                                   RESP-TOT-VOL-UPD-ATTR                  
200800                                   RESP-SKAPA-PROFORMA-ATTR               
200900                                   RESP-DEL-IDARTNR-OBJ-ATTR              
201000                                   RESP-DEL-KVANTAL-ATTR                  
201100                                   RESP-DEL-IDKOLLI-ATTR                  
201200     PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-KVRADER            
201300       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDARTNR-OBJ-ATTR (INDX)         
201400                                     RESP-KVANTAL-ATTR (INDX)             
201500     END-PERFORM                                                          
201600     .                                                                    
201700     EJECT                                                                
201800* --- IMS SEKTIONER                                                       
201900     SKIP3                                                                
202000 IMS-GU-ARTC-C11 SECTION.                                                 
202100                                                                          
202200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
202300          DELIMITED BY SIZE INTO SSA1                                     
202400     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
202500          DELIMITED BY SIZE INTO SSA2                                     
202600     MOVE '  GE' TO GODK-STATUSKODER                                      
202700     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC11 SSA1  SSA2            
202800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
202900     PERFORM IMS-STATUSKONTROLL                                           
203000     .                                                                    
203100     EJECT                                                                
203200 IMS-GHU-WDK711   SECTION.                                                
203300                                                                          
203400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
203500          DELIMITED BY SIZE INTO SSA1                                     
203600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
203700          DELIMITED BY SIZE INTO SSA2                                     
203800     MOVE '  GE' TO GODK-STATUSKODER                                      
203900     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
204000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
204100     PERFORM IMS-STATUSKONTROLL                                           
204200     .                                                                    
204300     SKIP3                                                                
204400 IMS-REPL-WDK711 SECTION.                                                 
204500                                                                          
204600     MOVE '  ' TO GODK-STATUSKODER                                        
204700     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
204800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
204900     PERFORM IMS-STATUSKONTROLL                                           
205000     .                                                                    
205100     EJECT                                                                
205200 IMS-GU-BENA-BEN SECTION.                                                 
205300                                                                          
205400     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
205500     DELIMITED BY SIZE INTO SSA1                                          
205600     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
205700     DELIMITED BY SIZE INTO SSA2                                          
205800     MOVE '  GE' TO GODK-STATUSKODER                                      
205900     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA11 SSA1 SSA2             
206000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
206100     PERFORM IMS-STATUSKONTROLL                                           
206200     .                                                                    
206300     SKIP2                                                                
206400 IMS-GU-3171-CL SECTION.                                                  
206500                                                                          
206600     STRING 'WL317101(WDGXKEY  =' W-IDHTYP-X ')'                          
206700          DELIMITED BY SIZE INTO SSA1                                     
206800     MOVE '  GE' TO GODK-STATUSKODER                                      
206900     CALL CBLTDLI USING GU 3171-PCB DLI-IO-WL317101 SSA1                  
207000     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
207100     PERFORM IMS-STATUSKONTROLL                                           
207200     .                                                                    
207300     EJECT                                                                
207400 IMS-GU-3171-CL11 SECTION.                                                
207500                                                                          
207600     STRING 'WL317101(WDGXKEY  =' W-IDHTYP-X ')'                          
207700          DELIMITED BY SIZE INTO SSA1                                     
207800     STRING 'WL317111(IDFAKT   =' W-IDFAKT-X ')'                          
207900          DELIMITED BY SIZE INTO SSA2                                     
208000     MOVE '  GE' TO GODK-STATUSKODER                                      
208100     CALL CBLTDLI USING GU 3171-PCB DLI-IO-WL317111 SSA1 SSA2             
208200     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
208300     PERFORM IMS-STATUSKONTROLL                                           
208400     .                                                                    
208500     SKIP3                                                                
208600 IMS-GHU-3171-CL11 SECTION.                                               
208700                                                                          
208800     STRING 'WL317101(WDGXKEY  =' W-IDHTYP-X ')'                          
208900          DELIMITED BY SIZE INTO SSA1                                     
209000     STRING 'WL317111(IDFAKT   =' W-IDFAKT-X ')'                          
209100          DELIMITED BY SIZE INTO SSA2                                     
209200     MOVE '  GE' TO GODK-STATUSKODER                                      
209300     CALL CBLTDLI USING GHU 3171-PCB DLI-IO-WL317111 SSA1 SSA2            
209400     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
209500     PERFORM IMS-STATUSKONTROLL                                           
209600     .                                                                    
209700     SKIP3                                                                
209800 IMS-GHU-3171-CL31 SECTION.                                               
209900                                                                          
210000     STRING 'WL317101(WDGXKEY  =' W-IDHTYP-X ')'                          
210100          DELIMITED BY SIZE INTO SSA1                                     
210200     STRING 'WL317111(IDFAKT   =' W-IDFAKT-X ')'                          
210300          DELIMITED BY SIZE INTO SSA2                                     
210400     STRING 'WL317121(IDKOLLI  =' W-IDKOLLI-X ')'                         
210500          DELIMITED BY SIZE INTO SSA3                                     
210600     STRING 'WL317131(IDARTNRO =' W-IDARTNR-X ')'                         
210700          DELIMITED BY SIZE INTO SSA4                                     
210800     MOVE '  GE' TO GODK-STATUSKODER                                      
210900     CALL CBLTDLI USING GHU 3171-PCB DLI-IO-WL317131 SSA1 SSA2            
211000                                                    SSA3 SSA4             
211100     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
211200     PERFORM IMS-STATUSKONTROLL                                           
211300     .                                                                    
211400     SKIP3                                                                
211500 IMS-GU-3171-CL31 SECTION.                                                
211600                                                                          
211700     STRING 'WL317101(WDGXKEY  =' W-IDHTYP-X ')'                          
211800          DELIMITED BY SIZE INTO SSA1                                     
211900     STRING 'WL317111(IDFAKT   =' W-IDFAKT-X ')'                          
212000          DELIMITED BY SIZE INTO SSA2                                     
212100     STRING 'WL317121(IDKOLLI  =' W-IDKOLLI-X ')'                         
212200          DELIMITED BY SIZE INTO SSA3                                     
212300     STRING 'WL317131(IDARTNRO =' W-IDARTNR-X ')'                         
212400          DELIMITED BY SIZE INTO SSA4                                     
212500     MOVE '  GE' TO GODK-STATUSKODER                                      
212600     CALL CBLTDLI USING GU 3171-PCB DLI-IO-WL317131 SSA1 SSA2             
212700                                                    SSA3 SSA4             
212800     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
212900     PERFORM IMS-STATUSKONTROLL                                           
213000     .                                                                    
213100     SKIP3                                                                
213200 IMS-GN-3171-CL21 SECTION.                                                
213300                                                                          
213400     STRING 'WL317101(WDGXKEY  =' W-IDHTYP-X ')'                          
213500          DELIMITED BY SIZE INTO SSA1                                     
213600     STRING 'WL317111(IDFAKT   =' W-IDFAKT-X ')'                          
213700          DELIMITED BY SIZE INTO SSA2                                     
213800     STRING 'WL317121(IDKOLLI  =' W-IDKOLLI-X ')'                         
213900          DELIMITED BY SIZE INTO SSA3                                     
214000     MOVE   'WL317131 '   TO     SSA4                                     
214100     MOVE '  GAGE' TO GODK-STATUSKODER                                    
214200     CALL CBLTDLI USING GN 3171-PCB DLI-IO-WL317131 SSA1 SSA2             
214300                                                     SSA3 SSA4            
214400     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
214500     PERFORM IMS-STATUSKONTROLL                                           
214600     .                                                                    
214700     SKIP3                                                                
214800 IMS-GNP-3171-CL11 SECTION.                                               
214900                                                                          
215000     STRING 'WL317101(WDGXKEY  =' W-IDHTYP-X ')'                          
215100          DELIMITED BY SIZE INTO SSA1                                     
215200     MOVE 'WL317111 ' TO SSA2                                             
215300*    STRING 'WL317111(IDFAKT   =' W-IDFAKT-X ')'                          
215400*         DELIMITED BY SIZE INTO SSA2                                     
215500     MOVE '  GEGAGB' TO GODK-STATUSKODER                                  
215600     CALL CBLTDLI USING GHNP 3171-PCB DLI-IO-WL317111 SSA1 SSA2           
215700     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
215800     PERFORM IMS-STATUSKONTROLL                                           
215900     .                                                                    
216000     SKIP3                                                                
216100* --- SPECIAL FOR USE WHEN SUMMING VOLUMES IN INPUT CHECK SECTION         
216200 IMS-GNP-3171-CL11X SECTION.                                              
216300                                                                          
216400     STRING 'WL317101(WDGXKEY  =' W-IDHTYP-X ')'                          
216500          DELIMITED BY SIZE INTO SSA1                                     
216600*    MOVE 'WL317111 ' TO SSA2                                             
216700     STRING 'WL317111(IDFAKT   =' W-IDFAKT-X ')'                          
216800          DELIMITED BY SIZE INTO SSA2                                     
216900     MOVE 'WL317121 ' TO SSA3                                             
217000     MOVE 'WL317131 ' TO SSA4                                             
217100     MOVE '  GEGAGB' TO GODK-STATUSKODER                                  
217200     CALL CBLTDLI USING GNP 3171-PCB DLI-IO-WL317131 SSA1 SSA2            
217300                                                     SSA3 SSA4            
217400     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
217500     PERFORM IMS-STATUSKONTROLL                                           
217600     .                                                                    
217700     SKIP3                                                                
217800 IMS-GHNP-3171-CL31 SECTION.                                              
217900                                                                          
218000     STRING 'WL317101(WDGXKEY  =' W-IDHTYP-X ')'                          
218100          DELIMITED BY SIZE INTO SSA1                                     
218200*    MOVE 'WL317111 ' TO SSA2                                             
218300     STRING 'WL317111(IDFAKT   =' W-IDFAKT-X ')'                          
218400          DELIMITED BY SIZE INTO SSA2                                     
218500     MOVE 'WL317121 ' TO SSA3                                             
218600     MOVE 'WL317131 ' TO SSA4                                             
218700     MOVE '  GEGAGB' TO GODK-STATUSKODER                                  
218800     CALL CBLTDLI USING GHNP 3171-PCB DLI-IO-WL317131 SSA1 SSA2           
218900                                                     SSA3 SSA4            
219000     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
219100     PERFORM IMS-STATUSKONTROLL                                           
219200     .                                                                    
219300     SKIP3                                                                
219400* --- SPECIAL FOR USE WHEN CHECKING IF WEIGHT IS FILLED BY KOLLI          
219500 IMS-GNP-3171-CL11Y SECTION.                                              
219600                                                                          
219700     STRING 'WL317101(WDGXKEY  =' W-IDHTYP-X ')'                          
219800          DELIMITED BY SIZE INTO SSA1                                     
219900*    MOVE 'WL317111 ' TO SSA2                                             
220000     STRING 'WL317111(IDFAKT   =' W-IDFAKT-X ')'                          
220100          DELIMITED BY SIZE INTO SSA2                                     
220200     MOVE 'WL317121 ' TO SSA3                                             
220300     MOVE '  GEGAGB' TO GODK-STATUSKODER                                  
220400     CALL CBLTDLI USING GNP 3171-PCB DLI-IO-WL317121 SSA1 SSA2            
220500                                                     SSA3                 
220600     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
220700     PERFORM IMS-STATUSKONTROLL                                           
220800     .                                                                    
220900     SKIP3                                                                
221000 IMS-ISRT-3171-CL11 SECTION.                                              
221100                                                                          
221200     STRING 'WL317101(WDGXKEY  =' W-IDHTYP-X ')'                          
221300          DELIMITED BY SIZE INTO SSA1                                     
221400     MOVE 'WL317111 ' TO SSA2                                             
221500     MOVE '  ' TO GODK-STATUSKODER                                        
221600     CALL CBLTDLI USING ISRT 3171-PCB DLI-IO-WL317111 SSA1 SSA2           
221700     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
221800     PERFORM IMS-STATUSKONTROLL                                           
221900     .                                                                    
222000     SKIP3                                                                
222100 IMS-REPL-3171-CL11 SECTION.                                              
222200                                                                          
222300     MOVE '  ' TO GODK-STATUSKODER                                        
222400     CALL CBLTDLI USING REPL 3171-PCB DLI-IO-WL317111                     
222500     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
222600     PERFORM IMS-STATUSKONTROLL                                           
222700     .                                                                    
222800     SKIP3                                                                
222900 IMS-GHNPL-3171-CL21 SECTION.                                             
223000     STRING 'WL317101(WDGXKEY  =' W-IDHTYP-X ')'                          
223100          DELIMITED BY SIZE INTO SSA1                                     
223200     STRING 'WL317111(IDFAKT   =' W-IDFAKT-X ')'                          
223300          DELIMITED BY SIZE INTO SSA2                                     
223400     MOVE 'WL317121*L' TO         SSA3                                    
223500     MOVE '  GE' TO GODK-STATUSKODER                                      
223600     CALL CBLTDLI USING GHNP 3171-PCB DLI-IO-WL317121                     
223700                                          SSA1 SSA2 SSA3                  
223800     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
223900     PERFORM IMS-STATUSKONTROLL                                           
224000     .                                                                    
224100     SKIP3                                                                
224200 IMS-GU-3171-CL21 SECTION.                                                
224300     STRING 'WL317101(WDGXKEY  =' W-IDHTYP-X ')'                          
224400          DELIMITED BY SIZE INTO SSA1                                     
224500     STRING 'WL317111(IDFAKT   =' W-IDFAKT-X ')'                          
224600          DELIMITED BY SIZE INTO SSA2                                     
224700     STRING 'WL317121(IDKOLLI  =' W-IDKOLLI-X ')'                         
224800          DELIMITED BY SIZE INTO SSA3                                     
224900     MOVE '  GE' TO GODK-STATUSKODER                                      
225000     CALL CBLTDLI USING GHU 3171-PCB DLI-IO-WL317121                      
225100                                          SSA1 SSA2 SSA3                  
225200     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
225300     PERFORM IMS-STATUSKONTROLL                                           
225400     .                                                                    
225500     SKIP3                                                                
225600 IMS-GHU-3171-CL21 SECTION.                                               
225700     STRING 'WL317101(WDGXKEY  =' W-IDHTYP-X ')'                          
225800          DELIMITED BY SIZE INTO SSA1                                     
225900     STRING 'WL317111(IDFAKT   =' W-IDFAKT-X ')'                          
226000          DELIMITED BY SIZE INTO SSA2                                     
226100     STRING 'WL317121(IDKOLLI  =' W-IDKOLLI-X ')'                         
226200          DELIMITED BY SIZE INTO SSA3                                     
226300     MOVE '  GE' TO GODK-STATUSKODER                                      
226400     CALL CBLTDLI USING GHU 3171-PCB DLI-IO-WL317121                      
226500                                          SSA1 SSA2 SSA3                  
226600     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
226700     PERFORM IMS-STATUSKONTROLL                                           
226800     .                                                                    
226900     SKIP3                                                                
227000 IMS-ISRT-3171-CL21 SECTION.                                              
227100                                                                          
227200     STRING 'WL317101(WDGXKEY  =' W-IDHTYP-X ')'                          
227300          DELIMITED BY SIZE INTO SSA1                                     
227400     STRING 'WL317111(IDFAKT   =' W-IDFAKT-X ')'                          
227500          DELIMITED BY SIZE INTO SSA2                                     
227600     MOVE 'WL317121 ' TO         SSA3                                     
227700     MOVE '  ' TO GODK-STATUSKODER                                        
227800     CALL CBLTDLI USING ISRT 3171-PCB DLI-IO-WL317121                     
227900                                          SSA1 SSA2 SSA3                  
228000     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
228100     PERFORM IMS-STATUSKONTROLL                                           
228200     .                                                                    
228300     SKIP3                                                                
228400 IMS-ISRT-3171-CL31 SECTION.                                              
228500                                                                          
228600     STRING 'WL317101(WDGXKEY  =' W-IDHTYP-X ')'                          
228700          DELIMITED BY SIZE INTO SSA1                                     
228800     STRING 'WL317111(IDFAKT   =' W-IDFAKT-X ')'                          
228900          DELIMITED BY SIZE INTO SSA2                                     
229000     STRING 'WL317121(IDKOLLI  =' W-IDKOLLI-X ')'                         
229100          DELIMITED BY SIZE INTO SSA3                                     
229200     MOVE 'WL317131 ' TO         SSA4                                     
229300     MOVE '  II' TO GODK-STATUSKODER                                      
229400     CALL CBLTDLI USING ISRT 3171-PCB DLI-IO-WL317131                     
229500                                          SSA1 SSA2 SSA3 SSA4             
229600     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
229700     PERFORM IMS-STATUSKONTROLL                                           
229800     .                                                                    
229900     SKIP3                                                                
230000 IMS-REPL-3171-CL21 SECTION.                                              
230100                                                                          
230200     MOVE '  ' TO GODK-STATUSKODER                                        
230300     CALL CBLTDLI USING REPL 3171-PCB DLI-IO-WL317121                     
230400     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
230500     PERFORM IMS-STATUSKONTROLL                                           
230600     .                                                                    
230700     SKIP3                                                                
230800 IMS-DLET-3171-CL21 SECTION.                                              
230900                                                                          
231000     MOVE '  ' TO GODK-STATUSKODER                                        
231100     CALL CBLTDLI USING DLET 3171-PCB DLI-IO-WL317121                     
231200     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
231300     PERFORM IMS-STATUSKONTROLL                                           
231400     .                                                                    
231500     EJECT                                                                
231600 IMS-REPL-3171-CL31 SECTION.                                              
231700                                                                          
231800     MOVE '  ' TO GODK-STATUSKODER                                        
231900     CALL CBLTDLI USING REPL 3171-PCB DLI-IO-WL317131                     
232000     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
232100     PERFORM IMS-STATUSKONTROLL                                           
232200     .                                                                    
232300     SKIP3                                                                
232400 IMS-DLET-3171-CL31 SECTION.                                              
232500                                                                          
232600     MOVE '  ' TO GODK-STATUSKODER                                        
232700     CALL CBLTDLI USING DLET 3171-PCB DLI-IO-WL317131                     
232800     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
232900     PERFORM IMS-STATUSKONTROLL                                           
233000     .                                                                    
233100     EJECT                                                                
233200 IMS-GHU-XXLD-INV SECTION.                                                
233300                                                                          
233400     STRING 'WLXXLD01(WDGXKEY  =' W-KEYSEG-X ')'                          
233500          DELIMITED BY SIZE INTO SSA1                                     
233600     MOVE 'WLXXLD11 '  TO SSA2                                            
233700*    STRING 'WLXXLD11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
233800*         DELIMITED BY SIZE INTO SSA2                                     
233900     MOVE '  ' TO GODK-STATUSKODER                                        
234000     CALL CBLTDLI USING GHU XXLD-PCB DLI-IO-WLXXLD11 SSA1 SSA2            
234100     MOVE XXLD-STATUS-CODE TO STATUS-WS                                   
234200     PERFORM IMS-STATUSKONTROLL                                           
234300     .                                                                    
234400     SKIP3                                                                
234500 IMS-REPL-XXLD-INV SECTION.                                               
234600                                                                          
234700     MOVE '  ' TO GODK-STATUSKODER                                        
234800     CALL CBLTDLI USING REPL XXLD-PCB DLI-IO-WLXXLD11                     
234900     MOVE XXLD-STATUS-CODE TO STATUS-WS                                   
235000     PERFORM IMS-STATUSKONTROLL                                           
235100     .                                                                    
235200     EJECT                                                                
238300 IMS-ISRT-LOGA-SALDO SECTION.                                             
238400                                                                          
238500     MOVE 'WLLOGA01 ' TO SSA1                                             
238600     MOVE '  II' TO GODK-STATUSKODER                                      
238700     CALL CBLTDLI USING ISRT LOGA-PCB DLI-IO-WLLOGA01 SSA1                
238800     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
238900     PERFORM IMS-STATUSKONTROLL                                           
239000     .                                                                    
239100     EJECT                                                                
239200 IMS-ISRT-ALTMSG SECTION.                                                 
239300                                                                          
239400      MOVE SPACE TO GODK-STATUSKODER                                      
239500      CALL CBLTDLI USING ISRT ALT-PCB PROG-TO-PROG-SW                     
239600      MOVE ALT-STATUS-CODE TO STATUS-WS                                   
239700      PERFORM IMS-STATUSKONTROLL                                          
239800      .                                                                   
239900      EJECT                                                               
240000 IMS-GU-3169-CL01 SECTION.                                                
240100                                                                          
240200     STRING 'WL316901(WDGXKEY  =' W-KEYSEG-Y ')'                          
240300          DELIMITED BY SIZE INTO SSA1                                     
240400     MOVE '    ' TO GODK-STATUSKODER                                      
240500     CALL CBLTDLI USING GU 3169-PCB DLI-IO-WL316901 SSA1                  
240600     MOVE 3169-STATUS-CODE TO STATUS-WS                                   
240700     PERFORM IMS-STATUSKONTROLL                                           
240800     .                                                                    
240900     EJECT                                                                
241000 IMS-GHU-3169-CL11 SECTION.                                               
241100                                                                          
241200     STRING 'WL316901(WDGXKEY  =' W-KEYSEG-Y ')'                          
241300          DELIMITED BY SIZE INTO SSA1                                     
241400     STRING 'WL316911(IDDC     =' W-IDDC-X ')'                            
241500          DELIMITED BY SIZE INTO SSA2                                     
241600     MOVE '  GE' TO GODK-STATUSKODER                                      
241700     CALL CBLTDLI USING GHU 3169-PCB DLI-IO-WL316911 SSA1 SSA2            
241800     MOVE 3169-STATUS-CODE TO STATUS-WS                                   
241900     PERFORM IMS-STATUSKONTROLL                                           
242000     .                                                                    
242100     SKIP3                                                                
242200 IMS-ISRT-3169-CL11 SECTION.                                              
242300                                                                          
242400     STRING 'WL316901(WDGXKEY  =' W-KEYSEG-Y ')'                          
242500          DELIMITED BY SIZE INTO SSA1                                     
242600     MOVE   'WL316911 ' TO SSA2                                           
242700     MOVE '  ' TO GODK-STATUSKODER                                        
242800     CALL CBLTDLI USING ISRT 3169-PCB DLI-IO-WL316911 SSA1 SSA2           
242900     MOVE 3169-STATUS-CODE TO STATUS-WS                                   
243000     PERFORM IMS-STATUSKONTROLL                                           
243100     .                                                                    
243200     SKIP3                                                                
243300 IMS-REPL-3169-CL11 SECTION.                                              
243400                                                                          
243500     MOVE '  ' TO GODK-STATUSKODER                                        
243600     CALL CBLTDLI USING REPL 3169-PCB DLI-IO-WL316911                     
243700     MOVE 3169-STATUS-CODE TO STATUS-WS                                   
243800     PERFORM IMS-STATUSKONTROLL                                           
243900     .                                                                    
244000     SKIP3                                                                
244100 IMS-GU-WDB601    SECTION.                                                
244200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
244300          DELIMITED BY SIZE INTO SSA1                                     
244400     MOVE '  GE' TO GODK-STATUSKODER                                      
244500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
244600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
244700     PERFORM IMS-STATUSKONTROLL                                           
244800     IF SEGMENT-SAKNAS                                                    
244900         MOVE SPACE TO DCS-KDDC                                           
245000     END-IF                                                               
245100     .                                                                    
245200                                                                          
245300 IMS-GU-WDB601-REC   SECTION.                                             
245400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-REC-X ')'                     
245500          DELIMITED BY SIZE INTO SSA1                                     
245600     MOVE '  GE' TO GODK-STATUSKODER                                      
245700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-REC SSA1             
245800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
245900     PERFORM IMS-STATUSKONTROLL                                           
246000     IF SEGMENT-SAKNAS                                                    
246100         MOVE SPACE TO REC-DCS-KDDC                                       
246200     END-IF                                                               
246300     .                                                                    
246400                                                                          
246500 IMS-STATUSKONTROLL SECTION.                                              
246600                                                                          
246700     SET STATUS-IX TO 1                                                   
246800     SEARCH GODK-STATUS                                                   
246900       AT END                                                             
247000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
247100         DELIMITED BY SIZE INTO FELTEXT                                   
247200         CALL FELLOG                                                      
247300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
247400         CONTINUE                                                         
247500     END-SEARCH                                                           
247600     .                                                                    
