000100**********************************************************                
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2010500.                                                
000400 AUTHOR.         STEFAN ANDREASSON.                                       
000500 DATE-WRITTEN.   99/11/12.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SÄSONGSANALYS                                                    
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WDK6                                       
001200*                              WDP7                                       
001300*                              WDG3                                       
001400*        PROGRAMMET LÄSER      WDD3                                       
001500*                                                                         
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W2T105                                              
001900*        MID:         W2I10501                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W2O10501                                            
002300*                                                                         
002400*   ÄNDRINGAR:                                                            
002500*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
002600*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
002700*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
002800*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
002900*                                                                         
002910*      2016-03-11  E'TRACKER 10243132  KINA EXPORT 2015,                  
002920*                  KRAV 70011.SÄTT NEJ PÅ FLREFNYO PÅ WDK629 VID          
002930*                  CLAG-KVPB-SEP FORECAST-ÄNDRING.                        
002980*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003600*    -COPY WY2000W1                                                       
003700     SKIP3                                                                
003800                                                                          
003900*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'W2010500'.            
004100                                                                          
004200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004400                                                                          
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  YES                         PIC X       VALUE 'Y'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  IX                          PIC 9(3)    VALUE ZERO.                  
004900 77  IX2                         PIC 9(3)    VALUE ZERO.                  
005000 77  WS-IDLEVNR-8                PIC X(8)    VALUE SPACE.                 
005100 77  SPRAK-IX                    PIC 9(3)    VALUE ZERO.                  
005200 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005300 01  WS.                                                                  
005400  03 WS-TEST.                                                             
005500    05 WS-TEST-A                 PIC X(9)    VALUE SPACE.                 
005600    05 FILLER                    PIC X       VALUE '/'.                   
005700    05 WS-TEST-B                 PIC X(9)    VALUE SPACE.                 
005800    05 FILLER                    PIC X       VALUE '/'.                   
005900    05 WS-TEST-C                 PIC X(1)    VALUE SPACE.                 
006000    05 FILLER                    PIC X       VALUE '/'.                   
006100    05 WS-TEST-D                 PIC X(5)    VALUE SPACE.                 
006200    05 FILLER                    PIC X       VALUE '/'.                   
006300    05 WS-TEST-E                 PIC X(6)    VALUE SPACE.                 
006400  03 WS-ANTAL                    PIC 9(9)    VALUE ZERO.                  
006500  03 WS-TOT-VALANT               PIC 9(9)    VALUE ZERO.                  
006600  03 WS-TOT-SIMANT               PIC 9(9)    VALUE ZERO.                  
006700  03 WS-TOT-HISANT               PIC 9(9)    VALUE ZERO.                  
006800  03 WS-INDEX                    PIC 9(3)    VALUE ZERO.                  
006900  03 WS-INDEX-DEC                PIC 9(3)V9(3)                            
007000                                             VALUE ZERO.                  
007100  03 WS-DASPSEA                  PIC 9(7)    VALUE ZERO.                  
007200  03 WS-KVOI                     PIC 9(7)    VALUE ZERO.                  
007300  03 WS-OSAKERHET                PIC Z(2)9.9 VALUE ZERO.                  
007400  03 WS-SIMIX-SUM                PIC 9(4)    VALUE ZERO.                  
007500  03 WS-SIMIX                    PIC 9(3)    VALUE ZERO.                  
007600  03 WS-JUSTERA                  PIC S9V9(2) VALUE ZERO  COMP-3.          
007700  03 WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
007710  03 WS-IDDC-REF                 PIC X(2)    VALUE SPACE.                 
007800                                                                          
008500  03 WS-MSGI-AREA.                                                        
008600    10 WS-MSGI-IDTRANS-2105      PIC X(4)    VALUE '2105'.                
008700    10 WS-MSGI-SIMIX             OCCURS 12                                
008800                                 PIC 9(3)    VALUE ZERO  COMP-3.          
008900    10 WS-MSGI-SIMANT            OCCURS 12                                
009000                                 PIC 9(7)    VALUE ZERO  COMP-3.          
009100    10 FILLER                    PIC X(124)  VALUE SPACE.                 
009200  03   WS-TEST-SIMIX-GRP.                                                 
009300   05  WS-TEST-SIMIX             OCCURS 12                                
009400                                 PIC 9(3)    VALUE ZERO.                  
009500                                                                          
009600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
009700                                                                          
009800                                                                          
009900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
010000     88  INDATA-OK                           VALUE 'J'.                   
010100     88  INDATA-FEL                          VALUE 'N'.                   
010200                                                                          
010300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
010400     88  NYCKLAR-OK                          VALUE 'J'.                   
010500     88  NYCKLAR-FEL                         VALUE 'N'.                   
010600                                                                          
010700 77  SIM-INDEX-SW                PIC X       VALUE 'N'.                   
010800     88  SIM-INDEX-JA                        VALUE 'J'.                   
010900     88  SIM-INDEX-NEJ                       VALUE 'N'.                   
011000                                                                          
011100 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
011200     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
011300     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
011400                                                                          
011500 77  SIM-ANTAL-SW                PIC X       VALUE 'N'.                   
011600     88  SIM-ANTAL-JA                        VALUE 'J'.                   
011700     88  SIM-ANTAL-NEJ                       VALUE 'N'.                   
011800                                                                          
011900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
012000     88  EGEN-MID                            VALUE '2105'.                
012100     88  GODK-MID                            VALUE '2101' '2102'          
012200                                                   '2103' '2104'          
012300                                                   '2105' '2106'          
012400                                                   '2107' '2108'          
012500                                                   '2109'.                
012600     88  HELP-MID                            VALUE '0551'.                
012700     EJECT                                                                
012900* DC KONSTANTER                                                           
013000*01    -COPY WWDCKONS                                                     
013100       EJECT                                                              
013200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
013300 01  GENERELLA-SUBPROGRAM.                                                
013400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
013500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013900     03  W222SEAS                PIC X(8)    VALUE 'W222SEAS'.            
014000     EJECT                                                                
014100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
014200*01 -COPY WMEDAREA                                                        
014300     EJECT                                                                
014400 01  MESSAGE-CODES.                                                       
014500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
014600     03  CONFLICT                PIC X(3)    VALUE '002'.                 
014700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
014800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
014900     03  ARTIKEL-UTGANGEN        PIC X(3)    VALUE '018'.                 
015000     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
015100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
015200     03  ARTIKEL-ERSATT          PIC X(3)    VALUE '220'.                 
015300     03  PRIS-SAKNAS             PIC X(3)    VALUE '301'.                 
015400     03  ARTIKEL-SAKNAS-SDC      PIC X(3)    VALUE '305'.                 
015500     03  DIREKTLEV               PIC X(3)    VALUE '306'.                 
015600     03  EJ-GODK-REFILL          PIC X(3)    VALUE '307'.                 
015700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
015800     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
015810     03  INF-REFILL-PART         PIC X(3)    VALUE '434'.                 
015900                                                                          
016000 01  MEDDELANDE.                                                          
016100                                                                          
016200     03 MED-1-AREA.                                                       
016300        05 FILLER                PIC X(34)                                
016400           VALUE 'KONFLIKT PF11 OCH RENSA INDATA    '.                    
016500        05 FILLER                PIC X(34)                                
016600           VALUE 'CONFLICT PF11 AND CLEAN INPUT DATA'.                    
016700     03 FILLER REDEFINES MED-1-AREA.                                      
016800        05 MED-1 OCCURS 2        PIC X(34).                               
016900                                                                          
017000     03 MED-2-AREA.                                                       
017100        05 FILLER                PIC X(20)                                
017200           VALUE 'TOTALEN AV INDEX ÄR '.                                  
017300        05 MED-2-INDEX-SE        PIC Z(3)9.                               
017400        05 FILLER                PIC X(16)                                
017500           VALUE ' MÅSTE VARA 1200'.                                      
017600        05 FILLER                PIC X(20)                                
017700           VALUE 'TOTAL OF INDEX IS   '.                                  
017800        05 MED-2-INDEX-GB        PIC Z(3)9.                               
017900        05 FILLER                PIC X(16)                                
018000           VALUE ' MUST BE 1200   '.                                      
018100     03 FILLER REDEFINES MED-2-AREA.                                      
018200        05 MED-2 OCCURS 2        PIC X(40).                               
018300                                                                          
018400     03 MED-3-AREA.                                                       
018500        05 FILLER                PIC X(30)                                
018600           VALUE 'INDATA MÅSTE VARA ÅÅMMDD  '.                            
018700        05 FILLER                PIC X(30)                                
018800           VALUE 'INPUT SHOULD BE YYMMDD    '.                            
018900     03 FILLER REDEFINES MED-3-AREA.                                      
019000        05 MED-3 OCCURS 2        PIC X(30).                               
019100                                                                          
019200                                                                          
019300     EJECT                                                                
019400*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
019500*01  -COPY WDATAREA                                                       
019600     EJECT                                                                
019700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
019800*                                                                         
019900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
020000     SKIP3                                                                
020100*01 -COPY WMSGINIT                                                        
020200     EJECT                                                                
020300*    --- PARAMETRAR TILL SUBPROGRAM W222SEAS                              
020400*                                                                         
020500 01  FILLER                      PIC X(16)   VALUE 'W222SEAS'.            
020600     SKIP3                                                                
020700*01 -COPY W222SEAS                                                        
020800     SKIP3                                                                
020900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
021000*                                                                         
021100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
021200     SKIP3                                                                
021300*01  MID -COPY W2I10501                                                   
021400     EJECT                                                                
021500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
021600     SKIP3                                                                
021700*01  -COPY WMSGAREA                                                       
021800     EJECT                                                                
021900     03  MOD REDEFINES MSG-AREA.                                          
022000*      05  -COPY W2O10501                                                 
022100     EJECT                                                                
022200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
022300     SKIP3                                                                
022400*01  -COPY WMFSAREA                                                       
022500     EJECT                                                                
022600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022700*                                                                         
022800     EJECT                                                                
022900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023000     SKIP3                                                                
023100 01  NYCKLAR-TILL-DLI.                                                    
023200     03  W-IDARTNR-X.                                                     
023300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
023600     03  W-IDUSER-X.                                                      
023700         05  W-IDUSER            PIC X(8)    VALUE SPACE.                 
023800     03  W-IDSKYLT-X.                                                     
023900         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
024000     03 W-IDLEVNR-X.                                                      
024100         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
024200     03 W-KDSEGKEY-X.                                                     
024300         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
024400     03  W-WDG3KEY.                                                       
024500         05  W-IDHTYP        PIC X(4)    VALUE '2203'.                    
024510         05  W-IDDC-2203     PIC X(2)    VALUE '  '.                      
024600         05  W-NYCKEL-VALFRI PIC X(24)   VALUE LOW-VALUE.                 
024700                                                                          
024800     SKIP2                                                                
024900*    --- STATUS-KOD FRÅN IMS                                              
025000 01  STATUS-WS                   PIC XX.                                  
025100     88  SEGMENT-FINNS                       VALUE '  '.                  
025200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
025300     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
025400                                                   'GB'.                  
025500     SKIP2                                                                
025600 01  GODK-STATUSKODER.                                                    
025700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025800     SKIP3                                                                
025900 01  SSA1                        PIC X(64).                               
026000 01  SSA2                        PIC X(64).                               
026100 01  SSA3                        PIC X(64).                               
026200     EJECT                                                                
026300*    --- IMS FUNKTIONSKODER                                               
026400*01  -COPY W0003                                                          
026500     SKIP3                                                                
026600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD301'.             
026700     SKIP3                                                                
026800 01  DLI-IO-WDD301.                                                       
026900*    03  -COPY WDD301                                                     
027000     EJECT                                                                
027100     SKIP3                                                                
027200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD311'.             
027300     SKIP3                                                                
027400 01  DLI-IO-WDD311.                                                       
027500*    03  -COPY WDD311                                                     
027600     EJECT                                                                
027700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK601'.             
027800     SKIP3                                                                
027900 01  DLI-IO-WDK601.                                                       
028000*    03  -COPY WDK601                                                     
028100     EJECT                                                                
028200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK611'.             
028300     SKIP3                                                                
028400 01  DLI-IO-WDK611.                                                       
028500*    03  -COPY WDK611                                                     
028600     EJECT                                                                
028700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK626'.             
028800     SKIP3                                                                
028900 01  DLI-IO-WDK626.                                                       
029000*    03  -COPY WDK626                                                     
029100     EJECT                                                                
029120 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK629'.             
029130     SKIP3                                                                
029140 01  DLI-IO-WDK629.                                                       
029150*    03  -COPY WDK629                                                     
029160     EJECT                                                                
029200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDG302'.             
029300     SKIP3                                                                
029400 01  DLI-IO-WDG302.                                                       
029500*    03  -COPY WDGX2204                                                   
029600     EJECT                                                                
029700 LINKAGE SECTION.                                                         
029800                                                                          
029900*01  -COPY W0009   -PRE MSG-                                              
030000     EJECT                                                                
030100*01  -COPY W0008  -PRE  USEA-                                             
030200     05  FILLER                  PIC X.                                   
030300     EJECT                                                                
030400*01  -COPY W0008  -PRE  WDK6-                                             
030500     05  FILLER                  PIC X.                                   
030600     EJECT                                                                
030700*01  -COPY W0008  -PRE  WDD3-                                             
030800     05  FILLER                  PIC X.                                   
030900     EJECT                                                                
031000*01  -COPY W0008  -PRE  WDG3-                                             
031100     05  FILLER                  PIC X.                                   
031200     EJECT                                                                
031300*01  -COPY W0008  -PRE  WDL8-                                             
031400     05  FILLER                  PIC X.                                   
031500     EJECT                                                                
031600 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDK6-PCB                      
031700                           WDD3-PCB WDG3-PCB WDL8-PCB.                    
031800 MAIN SECTION.                                                            
031900     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDK6-PCB                      
032000                           WDD3-PCB WDG3-PCB WDL8-PCB.                    
032100                                                                          
032200     PERFORM IMS-GET-MSG                                                  
032300     IF SEGMENT-FINNS                                                     
032400       PERFORM A-INIT                                                     
032500       PERFORM B-KOLLA-NYCKLAR                                            
032600       IF NYCKLAR-OK                                                      
032700         PERFORM SEC-URITY                                                
032800         IF MED-IDMFSFEL = ARTIKEL-SAKNAS                                 
032900           CONTINUE                                                       
033000         ELSE                                                             
033100           IF PASSED-SECURITY-CHECK                                       
033200              IF MFS-UPDATE                                               
033300                PERFORM G-KOLLA-INPUT                                     
033400                IF INDATA-OK                                              
033500                  PERFORM H-UPPDATERA                                     
033600                  PERFORM F-HAEMTA-INFO                                   
033700                END-IF                                                    
033800*               PERFORM MFS-ROER-EJ-FAELT-UT                              
033900              ELSE                                                        
034000                PERFORM E-ENTER-TRYCKNING                                 
034100              END-IF                                                      
034200                                                                          
034300              IF NYCKLAR-OK AND INDATA-OK                                 
034400*               UPPDATERA BILDEN                                          
034500                MOVE ZERO    TO WS-TOT-SIMANT                             
034600                MOVE 1       TO IX                                        
034700                PERFORM UNTIL IX > 12                                     
034800                  IF WS-MSGI-SIMIX(IX) NUMERIC                            
034900                     MOVE WS-MSGI-SIMIX (IX) TO MOD-SIMIX (IX)            
035000                  END-IF                                                  
035001                  IF WS-MSGI-SIMANT(IX) NUMERIC                           
035002                    MOVE WS-MSGI-SIMANT (IX) TO MOD-SIMANT (IX)           
035003                    ADD WS-MSGI-SIMANT (IX) TO WS-TOT-SIMANT              
035004                  END-IF                                                  
035100                  ADD 1      TO IX                                        
035200                END-PERFORM                                               
035210                IF WS-TOT-SIMANT NUMERIC                                  
035300                   MOVE WS-TOT-SIMANT TO MOD-TOT-SIMANT                   
035400                END-IF                                                    
035500              END-IF                                                      
035600                                                                          
035700**** MAN SPARAR SÖNDER NYCKLARNA FÖR ANVÄNDANDET AV HOPP MELLAN           
035710**** 2147 -> 2103 -> 2105 -> 2103                                         
035800              MOVE '002'            TO MSGI-KDCALL                        
035900              MOVE MSG-LTERM-NAME   TO MSGI-IDLTERM-USER                  
036000              MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                       
036100              MOVE '2105'           TO MSGI-IDTRANS                       
036300              CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                  
036400           END-IF                                                         
036500         END-IF                                                           
036600       END-IF                                                             
036900       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O10501 + 4                      
037000       PERFORM IMS-INSERT-MSG                                             
037100     END-IF                                                               
037200                                                                          
037300     MOVE ZERO TO RETURN-CODE                                             
037400     GOBACK                                                               
037500     .                                                                    
037600     EJECT                                                                
037700 A-INIT SECTION.                                                          
037800                                                                          
037900     IF MSG-DUBBLA-TRANSKODER                                             
038000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I10501                 
038100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
038200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
038300     ELSE                                                                 
038400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I10501                  
038500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
038600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
038700     END-IF                                                               
038800                                                                          
038900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
039000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
039100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
039200                                                                          
039300     MOVE LOW-VALUE TO MSG-AREA                                           
039400     MOVE 'W2O105N1' TO MFS-IDMOD                                         
039500     MOVE '2105' TO MOD-IDTRANS                                           
039600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
039700                                                                          
039800     IF EGEN-MID OR HELP-MID                                              
039900       CONTINUE                                                           
040000     ELSE                                                                 
040100       MOVE SPACE TO MFS-KDTRTYP                                          
040200       MOVE '7' TO MFS-IDPFK                                              
040300     END-IF                                                               
040400                                                                          
040500     ACCEPT DAGENS-DATUM FROM DATE                                        
040600     PERFORM MFS-LAES-IN-IGEN                                             
040700     .                                                                    
040800     EJECT                                                                
040810                                                                          
040900 B-KOLLA-NYCKLAR SECTION.                                                 
041100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
041200     MOVE '001'             TO MSGI-KDCALL                                
041300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
041400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
041500     MOVE '2105'            TO MSGI-IDTRANS                               
041600     IF EGEN-MID                                                          
041700       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
041800     ELSE                                                                 
041900       IF  MID-IDARTNR-IN NUMERIC                                         
042000       AND MID-IDARTNR-IN > ZERO                                          
042100         MOVE MID-IDARTNR-IN                                              
042200                            TO MSGI-IDARTNR                               
042300       END-IF                                                             
042400     END-IF                                                               
042500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
042600                                                                          
042700     IF MSGI-IDLAND-SPR = 'SE'                                            
042800        MOVE '0'             TO MFS-KDHUVOMR                              
042900        MOVE +1              TO SPRAK-IX                                  
043000     ELSE                                                                 
043100        MOVE +2              TO SPRAK-IX                                  
043200     END-IF                                                               
043300     MOVE MSGI-IDLAND-SPR    TO MED-IDSKYLT                               
043400     IF MSGI-IDLAND-SPR = 'SE'                                            
043500        MOVE 'S  '          TO W-IDSKYLT                                  
043600     ELSE                                                                 
043700        MOVE 'GB '          TO W-IDSKYLT                                  
043800     END-IF                                                               
045100                                                                          
045101     MOVE +1 TO IX                                                        
045102     PERFORM UNTIL IX > 12                                                
045103        INSPECT MID-SIMIX(IX) REPLACING LEADING SPACE BY ZERO             
045104        IF MID-SIMIX(IX) NOT NUMERIC                                      
045104          MOVE 0           TO MID-SIMIX(IX)                               
045104        END-IF                                                            
045104        MOVE MID-SIMIX(IX) TO WS-MSGI-SIMIX(IX)                           
045105        INSPECT MID-SIMANT(IX) REPLACING LEADING SPACE BY ZERO            
045106        IF MID-SIMANT(IX) NOT NUMERIC                                     
045106          MOVE 0           TO MID-SIMANT(IX)                              
045106        END-IF                                                            
045106        MOVE MID-SIMANT(IX) TO WS-MSGI-SIMANT(IX)                         
045107        ADD +1 TO IX                                                      
045108     END-PERFORM                                                          
045109                                                                          
045110     IF EGEN-MID                                                          
045111       CONTINUE                                                           
045112     ELSE                                                                 
045113       INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO               
045114       MOVE MSGI-IDARTNR     TO MID-IDARTNR-IN                            
045115     END-IF                                                               
045300                                                                          
045400     MOVE JA TO NYCKLAR-SW                                                
045401                INDATA-SW                                                 
045402                                                                          
045403*    -- KONTROLL AV IDARTNR                                               
045500     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
045600                                                                          
045700     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
045800     IF MSGI-IDARTNR NUMERIC                                              
045900       MOVE MSGI-IDARTNR     TO WS-IDARTNR                                
046000     ELSE                                                                 
046100       MOVE NEJ              TO NYCKLAR-SW                                
046200     END-IF                                                               
046300                                                                          
046400     MOVE WS-IDARTNR       TO MOD-IDARTNR-UT                              
046500                              W-IDARTNR                                   
046600     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
046700                                                                          
046800     IF NYCKLAR-FEL                                                       
046900       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
047000       CALL WMEDKONV USING MED-WMEDAREA                                   
047100       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
047200       PERFORM MFS-RENSA-FAELT-IN                                         
047300       PERFORM MFS-RENSA-FAELT-UT                                         
047400     END-IF                                                               
047500     .                                                                    
047600     EJECT                                                                
047610                                                                          
047700 E-ENTER-TRYCKNING SECTION.                                               
047800     PERFORM IMS-GU-K601                                                  
047900                                                                          
048000     PERFORM IMS-GU-K611                                                  
048100     IF SEGMENT-FINNS                                                     
048102       IF CLAG-IDDC-REF NOT = SPACE                                       
048103          MOVE INF-REFILL-PART  TO MED-IDMFSFEL                           
048104          CALL WMEDKONV      USING MED-WMEDAREA                           
048105          MOVE MED-MFSFEL       TO MOD-TEMFSFEL                           
048107       END-IF                                                             
048110                                                                          
048200       PERFORM IMS-GU-K626                                                
048300       IF SEGMENT-SAKNAS                                                  
048400          MOVE ZERO          TO JUST-DAMANSEA                             
048500                                JUST-DASPSEA                              
048600          MOVE 1.0           TO JUST-RESEASON (1)                         
048700                                JUST-RESEASON (2)                         
048800                                JUST-RESEASON (3)                         
048900                                JUST-RESEASON (4)                         
049000                                JUST-RESEASON (5)                         
049100                                JUST-RESEASON (6)                         
049200                                JUST-RESEASON (7)                         
049300                                JUST-RESEASON (8)                         
049400                                JUST-RESEASON (9)                         
049500                                JUST-RESEASON (10)                        
049600                                JUST-RESEASON (11)                        
049700                                JUST-RESEASON (12)                        
049800       END-IF                                                             
049900       PERFORM EA-BEHANDLA                                                
050000                                                                          
050100     ELSE                                                                 
050200       MOVE NEJ              TO NYCKLAR-SW                                
050300       MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                                
050400       CALL WMEDKONV USING MED-WMEDAREA                                   
050500       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
050600       PERFORM MFS-RENSA-FAELT-IN                                         
050700       PERFORM MFS-RENSA-FAELT-UT                                         
050800     END-IF                                                               
050900     .                                                                    
051000     EJECT                                                                
051100                                                                          
051200 EA-BEHANDLA SECTION.                                                     
051400     IF NOT EGEN-MID                                                      
051500     OR MID-IDARTNR-IN NOT = ALL '+'                                      
051600                                                                          
051700******************************************************************        
051800*      -      HÄMTA SÄSONGSINDEX FRÅN HISTORIK MHA SUBPGM W222SEAS        
051900*      -      LÄGG UT HISTORIKINFO ÄVEN I SIMULERINGSDELEN                
052000*      -      HÄMTA INFO TILL VALID-DELEN FRÅN WDK6                       
052100*                       JUST-RESEASON                                     
052200*                       JUST-RESEASON * CLAG-KVPB-SEP                     
052300******************************************************************        
052400                                                                          
052500       MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
052600       CALL WMEDKONV USING MED-WMEDAREA                                   
052700       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
052800                                                                          
052900       IF JUST-DASPSEA > ZERO                                             
053000         MOVE JUST-DASPSEA (3:6)                                          
053100                             TO MOD-DASPSEA                               
053200       ELSE                                                               
053300         MOVE MFS-RENSA-FAELT                                             
053400                             TO MOD-DASPSEA                               
053500       END-IF                                                             
053600       MOVE MFS-ADD-LAES-IN-FAELT                                         
053700                           TO MOD-DASPSEA-ATTR                            
053800                                                                          
053900       IF JUST-DAMANSEA > ZERO                                            
054000         MOVE JUST-DAMANSEA (3:6)                                         
054100                             TO MOD-DAMANSEA                              
054200       ELSE                                                               
054300         MOVE MFS-RENSA-FAELT                                             
054400                             TO MOD-DAMANSEA                              
054500       END-IF                                                             
054600                                                                          
054700       PERFORM IMS-GU-D301-BSEQ                                           
054800       IF SEGMENT-FINNS                                                   
054900         PERFORM IMS-GNP-D311                                             
055000         IF SEGMENT-FINNS                                                 
055100           MOVE TEXT-BEART TO MOD-BEART-ENG                               
055200         ELSE                                                             
055300           MOVE MFS-RENSA-FAELT TO MOD-BEART-ENG                          
055400         END-IF                                                           
055500       END-IF                                                             
055600                                                                          
055700       MOVE W-IDARTNR        TO SEAS-IDARTNR                              
055800       MOVE NEJ              TO SEAS-FLKVARTAL                            
055900       MOVE ART-IDFKNGRP     TO SEAS-IDFKNGRP                             
056000                                                                          
056100       CALL W222SEAS USING SEAS-W222SEAS WDL8-PCB                         
056200                                                                          
056300       IF SEAS-KDSVAR = SPACE                                             
056400         MOVE SEAS-ANT-HIST-AR   TO MOD-ANT-HIST-AR                       
056500         MOVE SEAS-OSAKERHET     TO WS-OSAKERHET                          
056600         MOVE WS-OSAKERHET       TO MOD-OSAKERHET                         
056700         IF SEAS-SEASON-ARTIKEL = JA                                      
056800            IF MSGI-IDLAND-SPR = 'SE'                                     
056900               MOVE 'J'          TO MOD-SEASON-ARTIKEL                    
057000            ELSE                                                          
057100               MOVE 'Y'          TO MOD-SEASON-ARTIKEL                    
057200             END-IF                                                       
057300         ELSE                                                             
057400           MOVE 'N'              TO MOD-SEASON-ARTIKEL                    
057500         END-IF                                                           
057600                                                                          
057700         MOVE ZERO               TO WS-TOT-VALANT                         
057800         MOVE 1                  TO IX                                    
057900         PERFORM UNTIL IX > 12                                            
058000                                                                          
058100           COMPUTE WS-INDEX = 100 * JUST-RESEASON (IX)                    
058200           MOVE WS-INDEX         TO MOD-VALIX (IX)                        
058300           COMPUTE WS-ANTAL ROUNDED = JUST-RESEASON (IX) *                
058400                                    CLAG-KVPB-SEP                         
058500                                                                          
058600           MOVE WS-ANTAL         TO MOD-VALANT (IX)                       
058700           ADD WS-ANTAL          TO WS-TOT-VALANT                         
058800           ADD 1                 TO IX                                    
058900         END-PERFORM                                                      
059000                                                                          
059100         MOVE WS-TOT-VALANT      TO MOD-TOT-VALANT                        
059200                                                                          
059300         MOVE ZERO               TO WS-TOT-SIMANT                         
059400                                      WS-TOT-HISANT                       
059500         MOVE 1                  TO IX                                    
059600         PERFORM UNTIL IX > 12                                            
059700                                                                          
059800           MOVE SEAS-RESEASON (IX) TO MOD-HISIX (IX)                      
059900                                      WS-MSGI-SIMIX (IX)                  
060000                                                                          
060100           MOVE SEAS-KVOI (IX)   TO WS-KVOI                               
060200           MOVE WS-KVOI          TO MOD-HISANT (IX)                       
060300           ADD WS-KVOI           TO WS-TOT-HISANT                         
060400           COMPUTE WS-MSGI-SIMANT (IX) ROUNDED =                          
060500*     MULTIPLICERA FÖRST FÖR ATT INTE TAPPA DECIMALER                     
060600*     WS-INDEX ÄR I %, DÄRFÖR / 100                                       
060700                   (WS-MSGI-SIMIX (IX) * CLAG-KVPB-SEP) / 100             
060800           ADD WS-MSGI-SIMANT (IX)                                        
060900                               TO WS-TOT-SIMANT                           
061000           ADD 1                 TO IX                                    
061100         END-PERFORM                                                      
061200                                                                          
061300         MOVE WS-TOT-SIMANT      TO MOD-TOT-SIMANT                        
061400         MOVE WS-TOT-HISANT      TO MOD-TOT-HISANT                        
061500                                                                          
061600       ELSE                                                               
061700         MOVE NEJ            TO NYCKLAR-SW                                
061800         MOVE ARTIKEL-SAKNAS-SDC TO MED-IDMFSFEL                          
061900         CALL WMEDKONV USING MED-WMEDAREA                                 
062000         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
062100         PERFORM MFS-RENSA-FAELT-IN                                       
062200         PERFORM MFS-RENSA-FAELT-UT                                       
062300       END-IF                                                             
062400                                                                          
062500     ELSE                                                                 
062600       IF MID-INPUT = ALL '+'                                             
062700         PERFORM MFS-RENSA-FAELT-IN                                       
062800         PERFORM MFS-ROER-EJ-FAELT-IN                                     
062900         PERFORM MFS-ROER-EJ-FAELT-UT                                     
063000       ELSE                                                               
063100         PERFORM S01-KOLLA-MID-RADER                                      
063200         IF INDATA-OK                                                     
063300           PERFORM EAA-KTRL-INPUT                                         
063400           IF INDATA-OK                                                   
063500             PERFORM EAB-SIMULERA-INDEX                                   
063600             MOVE INF-PRESS-PF11 TO MED-IDMFSINF                          
063700             CALL WMEDKONV USING MED-WMEDAREA                             
063800             MOVE MED-TEMFSINF TO MOD-TEMFSINF                            
063900           END-IF                                                         
064000         END-IF                                                           
064001       END-IF                                                             
064002       PERFORM MFS-ROER-EJ-FAELT-UT                                       
064003     END-IF                                                               
064100     .                                                                    
064200     EJECT                                                                
064300 EAA-KTRL-INPUT SECTION.                                                  
064400                                                                          
064500     MOVE MFS-ADD-LAES-IN-FAELT                                           
064600                           TO MOD-DASPSEA-ATTR                            
064700     IF MID-DASPSEA = ALL '+'                                             
064800       MOVE MFS-RENSA-FAELT  TO MOD-DASPSEA                               
064900                                                                          
065000     ELSE                                                                 
065100       IF MID-DASPSEA(1:1) = SPACE                                        
065200       OR MID-DASPSEA(2:1) = SPACE                                        
065300       OR MID-DASPSEA(3:1) = SPACE                                        
065400       OR MID-DASPSEA(4:1) = SPACE                                        
065500       OR MID-DASPSEA(5:1) = SPACE                                        
065600       OR MID-DASPSEA(6:1) = SPACE                                        
065700         MOVE MID-DASPSEA    TO MOD-DASPSEA                               
065800         MOVE NEJ            TO INDATA-SW                                 
065900         MOVE MFS-ADD-LYS-UPP-FAELT                                       
066000                             TO MOD-DASPSEA-ATTR                          
066100         MOVE ERR-CORR-HILITE-FLDS                                        
066200                             TO MED-IDMFSFEL                              
066300         CALL WMEDKONV USING MED-WMEDAREA                                 
066400         MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                              
066500         MOVE MED-3 (SPRAK-IX)                                            
066600                             TO MOD-TEMFSINF                              
066700         PERFORM MFS-ROER-EJ-FAELT-UT                                     
066800       ELSE                                                               
066900         MOVE 'AAMMDD'     TO DAT-KDDATFORM                               
067000         MOVE MID-DASPSEA  TO DAT-I-TIDATUM                               
067100                              MOD-DASPSEA                                 
067200                                                                          
067300                                                                          
067400         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
067500                         DAT-O-TIDATUM DAT-KDSVAR                         
067600                                                                          
067700         IF DAT-KDSVAR-OK                                                 
067800            MOVE MID-DASPSEA TO TMP1-YYMMDD                               
067900            MOVE DAGENS-DATUM                                             
068000                             TO TMP2-YYMMDD                               
068100            PERFORM WY2000P1                                              
068200            IF TMP1-YYMMDD < TMP2-YYMMDD                                  
068300               MOVE MID-DASPSEA                                           
068400                             TO MOD-DASPSEA                               
068500               MOVE NEJ      TO INDATA-SW                                 
068600               MOVE MFS-ADD-LYS-UPP-FAELT                                 
068700                             TO MOD-DASPSEA-ATTR                          
068800               MOVE ERR-CORR-HILITE-FLDS                                  
068900                             TO MED-IDMFSFEL                              
069000               CALL WMEDKONV USING MED-WMEDAREA                           
069100               MOVE MED-TEMFSFEL                                          
069200                             TO MOD-TEMFSFEL                              
069300               MOVE MED-3 (SPRAK-IX)                                      
069400                             TO MOD-TEMFSINF                              
069500               PERFORM MFS-ROER-EJ-FAELT-UT                               
069600            END-IF                                                        
069700         ELSE                                                             
069800            MOVE MID-DASPSEA TO MOD-DASPSEA                               
069900            MOVE NEJ         TO INDATA-SW                                 
070000            MOVE MFS-ADD-LYS-UPP-FAELT                                    
070100                             TO MOD-DASPSEA-ATTR                          
070200            MOVE ERR-CORR-HILITE-FLDS                                     
070300                             TO MED-IDMFSFEL                              
070400            CALL WMEDKONV USING MED-WMEDAREA                              
070500            MOVE MED-TEMFSFEL                                             
070600                             TO MOD-TEMFSFEL                              
070700            MOVE MED-3 (SPRAK-IX)                                         
070800                             TO MOD-TEMFSINF                              
070900            PERFORM MFS-ROER-EJ-FAELT-UT                                  
071000         END-IF                                                           
071100       END-IF                                                             
071200     END-IF                                                               
071300                                                                          
071400     MOVE MFS-ADD-LAES-IN-FAELT                                           
071500                           TO MOD-RENSA-IX-ATTR                           
071600                                                                          
071700     IF MID-RENSA-IX = ALL '+'                                            
071800     OR MID-RENSA-IX = SPACE                                              
071900     OR MID-RENSA-IX = NEJ                                                
072000        MOVE MFS-RENSA-FAELT TO MID-RENSA-IX                              
072100                                MOD-RENSA-IX                              
072200     ELSE                                                                 
072300       IF MID-RENSA-IX = JA                                               
072400       OR MID-RENSA-IX = YES                                              
072500          MOVE 100           TO MID-SIMIX (1)                             
072600                                MID-SIMIX (2)                             
072700                                MID-SIMIX (3)                             
072800                                MID-SIMIX (4)                             
072900                                MID-SIMIX (5)                             
073000                                MID-SIMIX (6)                             
073100                                MID-SIMIX (7)                             
073200                                MID-SIMIX (8)                             
073300                                MID-SIMIX (9)                             
073400                                MID-SIMIX (10)                            
073500                                MID-SIMIX (11)                            
073600                                MID-SIMIX (12)                            
073610          MOVE CLAG-KVPB-SEP      TO MID-SIMANT(1)                        
073620                                     MID-SIMANT(2)                        
073630                                     MID-SIMANT(3)                        
073640                                     MID-SIMANT(4)                        
073650                                     MID-SIMANT(5)                        
073660                                     MID-SIMANT(6)                        
073670                                     MID-SIMANT(7)                        
073680                                     MID-SIMANT(8)                        
073690                                     MID-SIMANT(9)                        
073691                                     MID-SIMANT(10)                       
073692                                     MID-SIMANT(11)                       
073693                                     MID-SIMANT(12)                       
073700       ELSE                                                               
073800         MOVE MID-RENSA-IX   TO MOD-RENSA-IX                              
073900         MOVE NEJ TO INDATA-SW                                            
074000         MOVE MFS-ADD-LAES-IN-FAELT-HI                                    
074100                             TO MOD-RENSA-IX-ATTR                         
074200         MOVE ERR-CORR-HILITE-FLDS                                        
074300                             TO MED-IDMFSFEL                              
074400         CALL WMEDKONV USING MED-WMEDAREA                                 
074500         MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                              
074600         PERFORM MFS-ROER-EJ-FAELT-UT                                     
074700       END-IF                                                             
074800     END-IF                                                               
074900                                                                          
075000     MOVE 1                  TO IX                                        
075100     PERFORM UNTIL IX > 12                                                
075200       IF MID-SIMIX (IX) NOT = ALL '+'                                    
075300         MOVE JA             TO SIM-INDEX-SW                              
075400         IF MID-SIMIX (IX) NUMERIC                                        
075500           MOVE MID-SIMIX (IX)                                            
075600                         TO WS-MSGI-SIMIX (IX)                            
075700         ELSE                                                             
075800           MOVE ERR-CORR-HILITE-FLDS                                      
075900                             TO MED-IDMFSFEL                              
076000           CALL WMEDKONV USING MED-WMEDAREA                               
076100           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
076200           MOVE MFS-ALFA-FAELT-FEL                                        
076300                         TO MOD-SIMIX-ATTR (IX)                           
076400           MOVE NEJ          TO INDATA-SW                                 
076500           PERFORM MFS-ROER-EJ-FAELT-IN                                   
076600           PERFORM MFS-ROER-EJ-FAELT-UT                                   
076700         END-IF                                                           
076800       END-IF                                                             
076900       IF MID-SIMANT (IX) NOT = ALL '+'                                   
077000         MOVE JA             TO SIM-ANTAL-SW                              
077100         IF MID-SIMANT (IX) NUMERIC                                       
077200           MOVE MID-SIMANT (IX)                                           
077300                         TO WS-MSGI-SIMANT (IX)                           
077400         ELSE                                                             
077500           MOVE ERR-CORR-HILITE-FLDS                                      
077600                             TO MED-IDMFSFEL                              
077700           CALL WMEDKONV USING MED-WMEDAREA                               
077800           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
077900           MOVE MFS-ALFA-FAELT-FEL                                        
078000                         TO MOD-SIMANT-ATTR (IX)                          
078100           MOVE NEJ          TO INDATA-SW                                 
078200           PERFORM MFS-ROER-EJ-FAELT-IN                                   
078300           PERFORM MFS-ROER-EJ-FAELT-UT                                   
078400         END-IF                                                           
078500       END-IF                                                             
078600       ADD 1                 TO IX                                        
078700     END-PERFORM                                                          
078800                                                                          
078900     IF INDATA-OK                                                         
079000*      IF SIM-ANTAL-JA                                                    
079100*      AND SIM-INDEX-JA                                                   
079200****   INTE MÖJLIGT ATT SIMULERA MED BÅDE ANTAL OCH INDEX                 
079300*****  SAMTIDIGT                                                          
079310*          CONTINUE                                                       
079400*          MOVE NEJ TO INDATA-SW                                          
079500*          MOVE CONFLICT  TO MED-IDMFSFEL                                 
079600*          CALL WMEDKONV USING MED-WMEDAREA                               
079700*          MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
079800*          MOVE 1                  TO IX                                  
079900*          PERFORM UNTIL IX > 12                                          
080000*            IF MID-SIMIX (IX) NOT = ALL '+'                              
080100*              MOVE MFS-ALFA-FAELT-FEL                                    
080200*                         TO MOD-SIMIX-ATTR (IX)                          
080300*            END-IF                                                       
080400*            IF MID-SIMANT (IX) NOT = ALL '+'                             
080500*              MOVE MFS-ALFA-FAELT-FEL                                    
080600*                         TO MOD-SIMANT-ATTR (IX)                         
080700*            END-IF                                                       
080800*            ADD 1                 TO IX                                  
080900*          END-PERFORM                                                    
081000*          PERFORM MFS-ROER-EJ-FAELT-IN                                   
081100*          PERFORM MFS-ROER-EJ-FAELT-UT                                   
081200                                                                          
081300*      ELSE                                                               
081400         IF SIM-INDEX-JA                                                  
081500******** KOLLA ATT SUMMAN BLIR 1200                                       
081600           MOVE ZERO           TO WS-SIMIX-SUM                            
081700           MOVE 1              TO IX                                      
081800           PERFORM UNTIL IX > 12                                          
081900                                                                          
082000             ADD WS-MSGI-SIMIX (IX)                                       
082100                             TO WS-SIMIX-SUM                              
082200             ADD 1           TO IX                                        
082300           END-PERFORM                                                    
082400                                                                          
082500           IF WS-SIMIX-SUM NOT = 1200                                     
082600               MOVE NEJ TO INDATA-SW                                      
082700               IF SPRAK-IX = 1                                            
082800                  MOVE WS-SIMIX-SUM                                       
082900                             TO MED-2-INDEX-SE                            
083000               ELSE                                                       
083100                  MOVE WS-SIMIX-SUM                                       
083200                             TO MED-2-INDEX-GB                            
083300               END-IF                                                     
083400               MOVE MED-2 (SPRAK-IX)                                      
083500                             TO MOD-TEMFSINF                              
083600               MOVE 1                TO IX                                
083700               PERFORM UNTIL IX > 12                                      
083800                 IF MID-SIMIX (IX) NOT = ALL '+'                          
083900                   MOVE MFS-ALFA-FAELT-FEL                                
084000                              TO MOD-SIMIX-ATTR (IX)                      
084100                 END-IF                                                   
084200                 ADD 1               TO IX                                
084300               END-PERFORM                                                
084400               PERFORM MFS-ROER-EJ-FAELT-IN                               
084500               PERFORM MFS-ROER-EJ-FAELT-UT                               
084600           END-IF                                                         
084700         END-IF                                                           
084800                                                                          
084900*      END-IF                                                             
085000     END-IF                                                               
085100                                                                          
085200     .                                                                    
085300     EJECT                                                                
085400 EAB-SIMULERA-INDEX SECTION.                                              
085500                                                                          
085600     MOVE ZERO               TO WS-TOT-SIMANT                             
085700     MOVE 1                  TO IX                                        
085800     PERFORM UNTIL IX > 12                                                
085900       MOVE WS-MSGI-SIMANT (IX)                                           
086000                             TO WS-ANTAL                                  
086100       ADD WS-ANTAL          TO WS-TOT-SIMANT                             
086200       ADD 1                 TO IX                                        
086300     END-PERFORM                                                          
086400                                                                          
086500     IF SIM-ANTAL-JA                                                      
086600                                                                          
086700       MOVE 1                TO IX                                        
086800                                                                          
086900       PERFORM UNTIL IX > 12                                              
087000         MOVE WS-MSGI-SIMANT (IX)                                         
087100                             TO WS-ANTAL                                  
087200         COMPUTE WS-INDEX-DEC ROUNDED =                                   
087300                 WS-ANTAL / (WS-TOT-SIMANT / 12)                          
087400                  ON SIZE ERROR                                           
087500                      MOVE ZERO   TO WS-INDEX-DEC                         
087600         END-COMPUTE                                                      
087700         COMPUTE WS-MSGI-SIMIX (IX) ROUNDED =                             
087800                 WS-INDEX-DEC * 100                                       
087900         ADD 1               TO IX                                        
088000       END-PERFORM                                                        
088100                                                                          
088200       MOVE ZERO             TO WS-ANTAL                                  
088300       MOVE 1                TO IX                                        
088400       PERFORM UNTIL IX > 12                                              
088500         ADD WS-MSGI-SIMIX (IX)                                           
088600                               TO WS-ANTAL                                
088700         ADD 1               TO IX                                        
088800       END-PERFORM                                                        
088900                                                                          
089000****                                                                      
089100****   NORMERA SÄSONGSINDEXEN SÅ ATT TOTALEN BLIR 12 * 100                
089200****                                                                      
089300                                                                          
089400       IF WS-ANTAL > +1200                                                
089500         MOVE -1             TO WS-JUSTERA                                
089600       ELSE                                                               
089700         MOVE +1             TO WS-JUSTERA                                
089800       END-IF                                                             
089900                                                                          
090000       PERFORM UNTIL WS-ANTAL = +1200                                     
090100                                                                          
090200         MOVE 1              TO IX                                        
090300         PERFORM UNTIL WS-ANTAL = +1200                                   
090400         OR IX > 12                                                       
090500           ADD WS-JUSTERA    TO WS-MSGI-SIMIX (IX)                        
090600                                WS-ANTAL                                  
090700           ADD 1             TO IX                                        
090800         END-PERFORM                                                      
090900       END-PERFORM                                                        
091000                                                                          
091100                                                                          
091200     ELSE                                                                 
091300*      (SIM-INDEX-JA)                                                     
091400                                                                          
091500       MOVE 1                TO IX                                        
091600       MOVE ZERO             TO WS-TOT-SIMANT                             
091700                                                                          
091800       PERFORM UNTIL IX > 12                                              
091900                                                                          
092000         COMPUTE WS-MSGI-SIMANT (IX) ROUNDED =                            
092100*   MULTIPLICERA FÖRST FÖR ATT INTE TAPPA DECIMALER                       
092200*   WS-INDEX ÄR I %, DÄRFÖR / 100                                         
092300                 (WS-MSGI-SIMIX (IX) * CLAG-KVPB-SEP) / 100               
092400         ADD WS-MSGI-SIMANT (IX)                                          
092500                             TO WS-TOT-SIMANT                             
092600                                                                          
092700         ADD 1               TO IX                                        
092800                                                                          
092900       END-PERFORM                                                        
093000                                                                          
093100     END-IF                                                               
093200     .                                                                    
093300     EJECT                                                                
093400 F-HAEMTA-INFO SECTION.                                                   
093500                                                                          
093600     IF JUST-DASPSEA > ZERO                                               
093700       MOVE JUST-DASPSEA (3:6)                                            
093800                             TO MOD-DASPSEA                               
093900     ELSE                                                                 
094000       MOVE MFS-RENSA-FAELT                                               
094100                             TO MOD-DASPSEA                               
094200     END-IF                                                               
094300     IF JUST-DAMANSEA > ZERO                                              
094400       MOVE JUST-DAMANSEA (3:6)                                           
094500                             TO MOD-DAMANSEA                              
094600     ELSE                                                                 
094700       MOVE MFS-RENSA-FAELT                                               
094800                             TO MOD-DAMANSEA                              
094900     END-IF                                                               
095000                                                                          
095100     PERFORM IMS-GU-D301-BSEQ                                             
095200     IF SEGMENT-FINNS                                                     
095300       PERFORM IMS-GNP-D311                                               
095400       IF SEGMENT-FINNS                                                   
095500         MOVE TEXT-BEART                                                  
095600                             TO MOD-BEART-ENG                             
095700       ELSE                                                               
095800         MOVE MFS-RENSA-FAELT                                             
095900                             TO MOD-BEART-ENG                             
096000       END-IF                                                             
096100     END-IF                                                               
096200                                                                          
096300     MOVE W-IDARTNR          TO SEAS-IDARTNR                              
096400     MOVE NEJ                TO SEAS-FLKVARTAL                            
096500     MOVE ART-IDFKNGRP       TO SEAS-IDFKNGRP                             
096600                                                                          
096700     CALL W222SEAS USING SEAS-W222SEAS WDL8-PCB                           
096800                                                                          
096900     IF SEAS-KDSVAR = SPACE                                               
097000       MOVE SEAS-ANT-HIST-AR     TO MOD-ANT-HIST-AR                       
097100       MOVE SEAS-OSAKERHET       TO WS-OSAKERHET                          
097200       MOVE WS-OSAKERHET         TO MOD-OSAKERHET                         
097300       IF SEAS-SEASON-ARTIKEL = JA                                        
097400          IF MSGI-IDLAND-SPR = 'SE'                                       
097500             MOVE 'J'            TO MOD-SEASON-ARTIKEL                    
097600          ELSE                                                            
097700             MOVE 'Y'            TO MOD-SEASON-ARTIKEL                    
097800          END-IF                                                          
097900       ELSE                                                               
098000         MOVE 'N'                TO MOD-SEASON-ARTIKEL                    
098100       END-IF                                                             
098200                                                                          
098300       MOVE ZERO                 TO WS-TOT-VALANT                         
098400       MOVE 1                    TO IX                                    
098500       PERFORM UNTIL IX > 12                                              
098600                                                                          
098700         COMPUTE WS-INDEX = 100 * JUST-RESEASON (IX)                      
098800         MOVE WS-INDEX           TO MOD-VALIX (IX)                        
098900         COMPUTE WS-ANTAL ROUNDED = JUST-RESEASON (IX) *                  
099000                                  CLAG-KVPB-SEP                           
099100                                                                          
099200         MOVE WS-ANTAL           TO MOD-VALANT (IX)                       
099300         ADD WS-ANTAL            TO WS-TOT-VALANT                         
099400         ADD 1                   TO IX                                    
099500       END-PERFORM                                                        
099600                                                                          
099700       MOVE WS-TOT-VALANT        TO MOD-TOT-VALANT                        
099800                                                                          
099900       MOVE ZERO                 TO WS-TOT-SIMANT                         
100000                                    WS-TOT-HISANT                         
100100       MOVE 1                    TO IX                                    
100200       PERFORM UNTIL IX > 12                                              
100300                                                                          
100400         MOVE SEAS-RESEASON (IX) TO MOD-HISIX (IX)                        
100500                                                                          
100600         MOVE SEAS-KVOI (IX)     TO WS-KVOI                               
100700         MOVE WS-KVOI            TO MOD-HISANT (IX)                       
100800         ADD WS-KVOI             TO WS-TOT-HISANT                         
100900         ADD 1                   TO IX                                    
101000       END-PERFORM                                                        
101100                                                                          
101200       MOVE WS-TOT-HISANT        TO MOD-TOT-HISANT                        
101300                                                                          
101400     ELSE                                                                 
101500       MOVE NEJ              TO NYCKLAR-SW                                
101600       MOVE ARTIKEL-SAKNAS-SDC TO MED-IDMFSFEL                            
101700       CALL WMEDKONV USING MED-WMEDAREA                                   
101800       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
101900       PERFORM MFS-RENSA-FAELT-IN                                         
102000       PERFORM MFS-RENSA-FAELT-UT                                         
102100     END-IF                                                               
102200     .                                                                    
102300     EJECT                                                                
102400 G-KOLLA-INPUT SECTION.                                                   
102500                                                                          
102600     MOVE JA  TO INDATA-SW                                                
102700     MOVE MFS-ADD-LAES-IN-FAELT                                           
102800                             TO MOD-DASPSEA-ATTR                          
102900                                                                          
103000     IF MID-RENSA-IX = JA                                                 
103100     OR MID-RENSA-IX = YES                                                
103200        MOVE MID-RENSA-IX    TO MOD-RENSA-IX                              
103300        MOVE NEJ             TO INDATA-SW                                 
103400        MOVE MFS-ADD-LYS-UPP-FAELT                                        
103500                             TO MOD-RENSA-IX-ATTR                         
103600        MOVE ERR-CORR-HILITE-FLDS                                         
103700                             TO MED-IDMFSFEL                              
103800        CALL WMEDKONV USING MED-WMEDAREA                                  
103900        MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                              
104000        MOVE MED-1 (SPRAK-IX)                                             
104100                             TO MOD-TEMFSINF                              
104200        PERFORM MFS-ROER-EJ-FAELT-IN                                      
104300        PERFORM MFS-ROER-EJ-FAELT-UT                                      
104400     END-IF                                                               
104500                                                                          
104600     PERFORM IMS-GU-K601                                                  
104700                                                                          
104800     PERFORM IMS-GU-K611                                                  
104900     IF SEGMENT-SAKNAS                                                    
105000       MOVE NEJ              TO NYCKLAR-SW                                
105100       MOVE NEJ              TO INDATA-SW                                 
105200       MOVE ARTIKEL-SAKNAS   TO MED-IDMFSFEL                              
105300       CALL WMEDKONV USING MED-WMEDAREA                                   
105400       MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                              
105500       PERFORM MFS-RENSA-FAELT-IN                                         
105600       PERFORM MFS-RENSA-FAELT-UT                                         
105700                                                                          
105800     ELSE                                                                 
105810       MOVE CLAG-IDDC-REF      TO WS-IDDC-REF                             
105811       IF CLAG-IDDC-REF NOT = SPACE                                       
105812          MOVE INF-REFILL-PART TO MED-IDMFSFEL                            
105813          CALL WMEDKONV     USING MED-WMEDAREA                            
105814          MOVE MED-MFSFEL      TO MOD-TEMFSFEL                            
105816       END-IF                                                             
105820                                                                          
105900       IF MID-DASPSEA = ALL '+'                                           
106000         MOVE MFS-RENSA-FAELT  TO MOD-DASPSEA                             
106100       ELSE                                                               
106200         IF MID-DASPSEA(1:1) = SPACE                                      
106300         OR MID-DASPSEA(2:1) = SPACE                                      
106400         OR MID-DASPSEA(3:1) = SPACE                                      
106500         OR MID-DASPSEA(4:1) = SPACE                                      
106600         OR MID-DASPSEA(5:1) = SPACE                                      
106700         OR MID-DASPSEA(6:1) = SPACE                                      
106800           MOVE MID-DASPSEA    TO MOD-DASPSEA                             
106900           MOVE NEJ            TO INDATA-SW                               
107000           MOVE MFS-ADD-LYS-UPP-FAELT                                     
107100                               TO MOD-DASPSEA-ATTR                        
107200           MOVE ERR-CORR-HILITE-FLDS                                      
107300                               TO MED-IDMFSFEL                            
107400           CALL WMEDKONV USING MED-WMEDAREA                               
107500           MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                            
107600           MOVE MED-3 (SPRAK-IX)                                          
107700                               TO MOD-TEMFSINF                            
107800           PERFORM MFS-ROER-EJ-FAELT-UT                                   
107900                                                                          
108000         ELSE                                                             
108100           MOVE 'AAMMDD'       TO DAT-KDDATFORM                           
108200           MOVE MID-DASPSEA    TO DAT-I-TIDATUM                           
108300                                  MOD-DASPSEA                             
108400                                                                          
108500                                                                          
108600           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
108700                               DAT-O-TIDATUM DAT-KDSVAR                   
108800                                                                          
108900           IF DAT-KDSVAR-OK                                               
109000              MOVE MID-DASPSEA                                            
109100                             TO TMP1-YYMMDD                               
109200              MOVE DAGENS-DATUM                                           
109300                             TO TMP2-YYMMDD                               
109400              PERFORM WY2000P1                                            
109500              IF TMP1-YYMMDD < TMP2-YYMMDD                                
109600                 MOVE MID-DASPSEA                                         
109700                             TO MOD-DASPSEA                               
109800                 MOVE NEJ    TO INDATA-SW                                 
109900                 MOVE MFS-ADD-LYS-UPP-FAELT                               
110000                             TO MOD-DASPSEA-ATTR                          
110100                 MOVE ERR-CORR-HILITE-FLDS                                
110200                             TO MED-IDMFSFEL                              
110300                 CALL WMEDKONV USING MED-WMEDAREA                         
110400                 MOVE MED-TEMFSFEL                                        
110500                             TO MOD-TEMFSFEL                              
110600                 MOVE MED-3 (SPRAK-IX)                                    
110700                             TO MOD-TEMFSINF                              
110800                 PERFORM MFS-ROER-EJ-FAELT-UT                             
110900              END-IF                                                      
111000           ELSE                                                           
111100               MOVE MID-DASPSEA                                           
111200                             TO MOD-DASPSEA                               
111300               MOVE NEJ      TO INDATA-SW                                 
111400               MOVE MFS-ADD-LYS-UPP-FAELT                                 
111500                             TO MOD-DASPSEA-ATTR                          
111600               MOVE ERR-CORR-HILITE-FLDS                                  
111700                             TO MED-IDMFSFEL                              
111800               CALL WMEDKONV USING MED-WMEDAREA                           
111900               MOVE MED-TEMFSFEL                                          
112000                             TO MOD-TEMFSFEL                              
112100               MOVE MED-3 (SPRAK-IX)                                      
112200                             TO MOD-TEMFSINF                              
112300               PERFORM MFS-ROER-EJ-FAELT-UT                               
112400           END-IF                                                         
112500         END-IF                                                           
112600       END-IF                                                             
112900                                                                          
113000       PERFORM S01-KOLLA-MID-RADER                                        
113100       IF INDATA-OK                                                       
113200***********   KOLLA ATT SUMMAN BLIR 1200                                  
113300          MOVE ZERO             TO WS-SIMIX-SUM                           
113400          MOVE 1                TO IX                                     
113500          PERFORM UNTIL IX > 12                                           
113600            ADD WS-MSGI-SIMIX (IX)                                        
113700                                TO WS-SIMIX-SUM                           
113800            ADD 1               TO IX                                     
113900          END-PERFORM                                                     
114000                                                                          
114100          IF WS-SIMIX-SUM NOT = 1200                                      
114200              MOVE NEJ TO INDATA-SW                                       
114300              IF SPRAK-IX = 1                                             
114400                 MOVE WS-SIMIX-SUM                                        
114500                                TO MED-2-INDEX-SE                         
114600              ELSE                                                        
114700                 MOVE WS-SIMIX-SUM                                        
114800                                TO MED-2-INDEX-GB                         
114900              END-IF                                                      
115000              MOVE MED-2 (SPRAK-IX)                                       
115100                                TO MOD-TEMFSINF                           
115200              MOVE 1            TO IX                                     
115300              PERFORM UNTIL IX > 12                                       
115400                MOVE MFS-ALFA-FAELT-FEL                                   
115500                                TO MOD-SIMIX-ATTR (IX)                    
115600                ADD 1           TO IX                                     
115700              END-PERFORM                                                 
115800              PERFORM MFS-ROER-EJ-FAELT-IN                                
115900              PERFORM MFS-ROER-EJ-FAELT-UT                                
116000          END-IF                                                          
116001        END-IF                                                            
116002     END-IF                                                               
116003     .                                                                    
116100     EJECT                                                                
116200 H-UPPDATERA SECTION.                                                     
116300                                                                          
116400     PERFORM IMS-GHU-K626                                                 
116500     IF SEGMENT-FINNS                                                     
116600        IF NOT (WS-MSGI-SIMIX (1) = 100                                   
116700        AND  WS-MSGI-SIMIX (2) = 100                                      
116800        AND  WS-MSGI-SIMIX (3) = 100                                      
116900        AND  WS-MSGI-SIMIX (4) = 100                                      
117000        AND  WS-MSGI-SIMIX (5) = 100                                      
117100        AND  WS-MSGI-SIMIX (6) = 100                                      
117200        AND  WS-MSGI-SIMIX (7) = 100                                      
117300        AND  WS-MSGI-SIMIX (8) = 100                                      
117400        AND  WS-MSGI-SIMIX (9) = 100                                      
117500        AND  WS-MSGI-SIMIX (10) = 100                                     
117600        AND  WS-MSGI-SIMIX (11) = 100                                     
117700        AND  WS-MSGI-SIMIX (12) = 100)                                    
117800        AND (JUST-RESEASON (1) = 1.00                                     
117900        AND JUST-RESEASON (2) = 1.00                                      
118000        AND JUST-RESEASON (3) = 1.00                                      
118100        AND JUST-RESEASON (4) = 1.00                                      
118200        AND JUST-RESEASON (5) = 1.00                                      
118300        AND JUST-RESEASON (6) = 1.00                                      
118400        AND JUST-RESEASON (7) = 1.00                                      
118500        AND JUST-RESEASON (8) = 1.00                                      
118600        AND JUST-RESEASON (9) = 1.00                                      
118700        AND JUST-RESEASON (10) = 1.00                                     
118800        AND JUST-RESEASON (11) = 1.00                                     
118900        AND JUST-RESEASON (12) = 1.00)                                    
119000*                                                                         
119100*     ARTIKELN ÄR PÅ VÄG ATT BLI SÄSONG                                   
119200*     AUTOMATPLANER SKA STOPPAS                                           
119300*                                                                         
119400            MOVE WC-CDC-SE   TO W-IDDC-2203                               
119410            MOVE W-IDARTNR   TO 2204-IDARTNR                              
119500            MOVE 14          TO 2204-KDLPORS                              
119600            PERFORM IMS-ISRT-R2202                                        
119700        END-IF                                                            
119800                                                                          
119900     ELSE                                                                 
120000        IF NOT (WS-MSGI-SIMIX (1) = 100                                   
120100        AND  WS-MSGI-SIMIX (2) = 100                                      
120200        AND  WS-MSGI-SIMIX (3) = 100                                      
120300        AND  WS-MSGI-SIMIX (4) = 100                                      
120400        AND  WS-MSGI-SIMIX (5) = 100                                      
120500        AND  WS-MSGI-SIMIX (6) = 100                                      
120600        AND  WS-MSGI-SIMIX (7) = 100                                      
120700        AND  WS-MSGI-SIMIX (8) = 100                                      
120800        AND  WS-MSGI-SIMIX (9) = 100                                      
120900        AND  WS-MSGI-SIMIX (10) = 100                                     
121000        AND  WS-MSGI-SIMIX (11) = 100                                     
121100        AND  WS-MSGI-SIMIX (12) = 100)                                    
121200*                                                                         
121300*     ARTIKELN ÄR PÅ VÄG ATT BLI SÄSONG                                   
121400*     AUTOMATPLANER SKA STOPPAS                                           
121500*                                                                         
121510            MOVE WC-CDC-SE   TO W-IDDC-2203                               
121600            MOVE W-IDARTNR   TO 2204-IDARTNR                              
121700            MOVE 14          TO 2204-KDLPORS                              
121800            PERFORM IMS-ISRT-R2202                                        
121900        END-IF                                                            
122000     END-IF                                                               
122100                                                                          
122200     PERFORM IMS-GHU-K626                                                 
122300                                                                          
122400     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
122500     MOVE MOD-DASPSEA        TO DAT-I-TIDATUM                             
122600                                                                          
122700     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
122800                     DAT-O-TIDATUM DAT-KDSVAR                             
122900                                                                          
123000     IF DAT-KDSVAR-OK                                                     
123100       MOVE DAT-TISEKEL      TO JUST-DASPSEA (1:2)                        
123200       MOVE MOD-DASPSEA      TO JUST-DASPSEA (3:6)                        
123300     ELSE                                                                 
123400       MOVE ZERO             TO JUST-DASPSEA                              
123500     END-IF                                                               
123600     MOVE 1                  TO IX                                        
123700     MOVE ZERO               TO WS-TOT-VALANT                             
123800                                                                          
123900     PERFORM UNTIL IX > 12                                                
124000                                                                          
124100       MOVE WS-MSGI-SIMIX (IX)                                            
124200                             TO WS-INDEX                                  
124300                                MOD-VALIX (IX)                            
124400       COMPUTE JUST-RESEASON (IX) = WS-INDEX / 100                        
124500       COMPUTE WS-ANTAL ROUNDED = JUST-RESEASON (IX) *                    
124600                                  CLAG-KVPB-SEP                           
124700       MOVE WS-ANTAL         TO MOD-VALANT (IX)                           
124800       ADD WS-ANTAL          TO WS-TOT-VALANT                             
124900       ADD 1                 TO IX                                        
125000     END-PERFORM                                                          
125100                                                                          
125200     MOVE WS-TOT-VALANT      TO MOD-TOT-VALANT                            
125300                                                                          
125400     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
125500     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
125600                                                                          
125700     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
125800                     DAT-O-TIDATUM DAT-KDSVAR                             
125900                                                                          
126000     IF DAT-KDSVAR-OK                                                     
126100       MOVE DAT-TISEKEL      TO JUST-DAMANSEA (1:2)                       
126200       MOVE DAGENS-DATUM     TO JUST-DAMANSEA (3:6)                       
126300     ELSE                                                                 
126400       MOVE ZERO             TO JUST-DAMANSEA                             
126500     END-IF                                                               
126600                                                                          
126700     IF SEGMENT-FINNS                                                     
126800        PERFORM IMS-REPL-K626                                             
126900     ELSE                                                                 
127000        MOVE ZERO            TO JUST-REPBJUST                             
127100                                JUST-TIPBJUST-CENTR                       
127200                                JUST-KVPB-JUST (1)                        
127300                                JUST-TIPBJUST (1)                         
127400                                JUST-KVPB-JUST (2)                        
127500                                JUST-TIPBJUST (2)                         
127600        PERFORM IMS-ISRT-K626                                             
127700     END-IF                                                               
127800                                                                          
127810     IF WS-IDDC-REF NOT = SPACE                                           
127820       PERFORM IMS-GHU-WDK629                                             
127830       IF SEGMENT-FINNS                                                   
127831         IF CREF-FLREFNYO = JA                                            
127832           MOVE NEJ       TO CREF-FLREFNYO                                
127834           PERFORM IMS-REPL-WDK629                                        
127835         END-IF                                                           
127836       END-IF                                                             
127860     END-IF                                                               
127870                                                                          
127900     MOVE INF-UPDATE-DONE    TO MED-IDMFSINF                              
128000     CALL WMEDKONV USING MED-WMEDAREA                                     
128100     MOVE MED-TEMFSINF       TO MOD-TEMFSINF                              
128200     PERFORM MFS-ROER-EJ-FAELT-IN                                         
128600     .                                                                    
128700     EJECT                                                                
128800 S01-KOLLA-MID-RADER SECTION.                                             
128900                                                                          
129000     MOVE +1 TO IX                                                        
129100     PERFORM UNTIL IX > 12                                                
129200       IF MID-SIMIX(IX) = ALL '+'                                         
129300          MOVE NEJ TO INDATA-SW                                           
129400          MOVE MFS-ALFA-FAELT-FEL TO MOD-SIMIX-ATTR(IX)                   
129500       ELSE                                                               
129600          INSPECT MID-SIMIX(IX) REPLACING LEADING SPACE BY ZERO           
129700          IF MID-SIMIX(IX) NUMERIC                                        
129800             MOVE MID-SIMIX(IX) TO WS-MSGI-SIMIX(IX)                      
129900          ELSE                                                            
130000             MOVE NEJ TO INDATA-SW                                        
130100             MOVE MFS-ALFA-FAELT-FEL TO MOD-SIMIX-ATTR(IX)                
130200          END-IF                                                          
130300       END-IF                                                             
130400                                                                          
130500       IF MID-SIMANT(IX) = ALL '+'                                        
130600          MOVE NEJ TO INDATA-SW                                           
130700          MOVE MFS-ALFA-FAELT-FEL TO MOD-SIMANT-ATTR(IX)                  
130800       ELSE                                                               
130900          INSPECT MID-SIMANT(IX) REPLACING LEADING SPACE BY ZERO          
131000          IF MID-SIMANT(IX) NUMERIC                                       
131100             MOVE MID-SIMANT(IX) TO WS-MSGI-SIMANT(IX)                    
131200          ELSE                                                            
131300             MOVE NEJ TO INDATA-SW                                        
131400             MOVE MFS-ALFA-FAELT-FEL TO MOD-SIMANT-ATTR(IX)               
131500          END-IF                                                          
131600        END-IF                                                            
131700        ADD +1 TO IX                                                      
131800     END-PERFORM                                                          
131900                                                                          
132000     IF INDATA-FEL                                                        
132100        MOVE ERR-CORR-HILITE-FLDS                                         
132200                          TO MED-IDMFSFEL                                 
132300        CALL WMEDKONV USING MED-WMEDAREA                                  
132400        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
132500        PERFORM MFS-ROER-EJ-FAELT-IN                                      
132600        PERFORM MFS-ROER-EJ-FAELT-UT                                      
132601     END-IF                                                               
132602     .                                                                    
132603     EJECT                                                                
132604 MFS-RENSA-FAELT-UT SECTION.                                              
132605                                                                          
132606*    --- ALLA UTDATA-FÄLT                                                 
132607     MOVE MFS-RENSA-FAELT TO MOD-DAMANSEA                                 
132608                             MOD-BEART-ENG                                
132609                             MOD-OSAKERHET                                
132610                             MOD-ANT-HIST-AR                              
132611                             MOD-TEMFSINF                                 
132612                                                                          
132613     MOVE 1                  TO IX2                                       
132614     PERFORM UNTIL IX2 > 12                                               
132615       MOVE MFS-RENSA-FAELT TO MOD-VALIX (IX2)                            
132616                               MOD-VALANT (IX2)                           
132617       ADD 1                 TO IX2                                       
132618     END-PERFORM                                                          
132619                                                                          
132620     MOVE 1                  TO IX2                                       
132621     PERFORM UNTIL IX2 > 12                                               
132622       MOVE MFS-RENSA-FAELT TO MOD-HISIX (IX2)                            
132623                               MOD-HISANT (IX2)                           
132624       ADD 1                 TO IX2                                       
132625     END-PERFORM                                                          
132626     .                                                                    
132627     SKIP3                                                                
132628 MFS-RENSA-FAELT-IN SECTION.                                              
132629                                                                          
132630*    --- ALLA INDATA-FÄLT                                                 
132631     MOVE MFS-RENSA-FAELT    TO MOD-DASPSEA                               
132632                                MOD-RENSA-IX                              
132633                                                                          
132634     MOVE 1                  TO IX2                                       
132635     PERFORM UNTIL IX2 > 12                                               
132636       MOVE MFS-RENSA-FAELT TO MOD-SIMIX (IX2)                            
132637                               MOD-SIMANT (IX2)                           
132638       ADD 1                 TO IX2                                       
132639     END-PERFORM                                                          
132640     .                                                                    
132641     EJECT                                                                
132642 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
132700                                                                          
132800*    --- ALLA UTDATA-FÄLT                                                 
132900                                                                          
133000     MOVE MFS-ROER-EJ-FAELT  TO MOD-BEART-ENG                             
133100                                MOD-DAMANSEA                              
133200                                MOD-SEASON-ARTIKEL                        
133300                                MOD-OSAKERHET                             
133400                                MOD-ANT-HIST-AR                           
133500                                MOD-TOT-VALANT                            
133600                                MOD-TOT-SIMANT                            
133700                                MOD-TOT-HISANT                            
133800                                                                          
133900     MOVE 1                  TO IX2                                       
134000     PERFORM UNTIL IX2 > 12                                               
134100       MOVE MFS-ROER-EJ-FAELT  TO MOD-VALIX (IX2)                         
134200                                  MOD-VALANT (IX2)                        
134300       ADD 1                 TO IX2                                       
134400     END-PERFORM                                                          
134500                                                                          
134600     MOVE 1                  TO IX2                                       
134700     PERFORM UNTIL IX2 > 12                                               
134800       MOVE MFS-ROER-EJ-FAELT  TO MOD-HISIX (IX2)                         
134900                                  MOD-HISANT (IX2)                        
135000       ADD 1                 TO IX2                                       
135100     END-PERFORM                                                          
135200     .                                                                    
135300     SKIP3                                                                
135400 MFS-ROER-EJ-FAELT-UT-2  SECTION.                                         
135500                                                                          
135600*    --- ALLA UTDATA-FÄLT (EXKL. VALID)                                   
135700                                                                          
135800     MOVE MFS-ROER-EJ-FAELT  TO MOD-BEART-ENG                             
135900                                MOD-DAMANSEA                              
136000                                MOD-OSAKERHET                             
136100                                MOD-ANT-HIST-AR                           
136200                                MOD-TOT-SIMANT                            
136300                                MOD-TOT-HISANT                            
136400                                                                          
136500     MOVE 1                  TO IX2                                       
136600     PERFORM UNTIL IX2 > 12                                               
136700       MOVE MFS-ROER-EJ-FAELT  TO MOD-HISIX (IX2)                         
136800                                  MOD-HISANT (IX2)                        
136900       ADD 1                 TO IX2                                       
137000     END-PERFORM                                                          
137100     .                                                                    
137200     SKIP3                                                                
137300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
137400                                                                          
137500*    --- ALLA INDATA-FÄLT                                                 
137600     MOVE MFS-ROER-EJ-FAELT  TO MOD-DASPSEA                               
137700                                MOD-RENSA-IX                              
137800                                                                          
137900     MOVE 1                  TO IX2                                       
138000     PERFORM UNTIL IX2 > 12                                               
138100       MOVE MFS-ROER-EJ-FAELT                                             
138200                             TO MOD-SIMIX (IX2)                           
138300                                MOD-SIMANT (IX2)                          
138400       ADD 1                 TO IX2                                       
138500     END-PERFORM                                                          
138600     .                                                                    
138700     EJECT                                                                
138800 MFS-FORM-ATTR SECTION.                                                   
138900                                                                          
139000*    --- ALLA INDATA-FÄLT                                                 
139100     MOVE MFS-FORMATETS-ATTR TO MOD-DASPSEA-ATTR                          
139200                                MOD-RENSA-IX-ATTR                         
139300                                                                          
139400     MOVE 1                  TO IX2                                       
139500     PERFORM UNTIL IX2 > 12                                               
139600       MOVE MFS-FORMATETS-ATTR  TO MOD-SIMIX-ATTR (IX2)                   
139700                                   MOD-SIMANT-ATTR (IX2)                  
139800       ADD 1                 TO IX2                                       
139900     END-PERFORM                                                          
140000     .                                                                    
140100     SKIP2                                                                
140200 MFS-LAES-IN-IGEN SECTION.                                                
140300                                                                          
140400*    --- ALLA INDATA-FÄLT                                                 
140500     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-DASPSEA-ATTR                       
140600                                   MOD-RENSA-IX-ATTR                      
140700                                                                          
140800                                                                          
140900     MOVE 1                  TO IX2                                       
141000     PERFORM UNTIL IX2 > 12                                               
141100       MOVE MFS-ADD-LAES-IN-FAELT                                         
141200                                TO MOD-SIMIX-ATTR (IX2)                   
141300                                   MOD-SIMANT-ATTR (IX2)                  
141400       ADD 1                 TO IX2                                       
141500     END-PERFORM                                                          
141600     .                                                                    
141700     EJECT                                                                
141800                                                                          
141900 SEC-URITY   SECTION.                                                     
142000     SKIP2                                                                
142100*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
142200     PERFORM IMS-GU-K601                                                  
142300     IF  SEGMENT-FINNS                                                    
142400       MOVE ART-IDLEVNR          TO WS-IDLEVNR-8                          
142500       IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                          
142600       OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                    
142700*        --- BEHÖRIG USER                                                 
142800         SET PASSED-SECURITY-CHECK TO TRUE                                
142900       ELSE                                                               
143000*        --- OBEHÖRIG USER / USER NOT AUTHORIZED                          
143100           MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                        
143200           CALL WMEDKONV USING MED-WMEDAREA                               
143300           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
143400       END-IF                                                             
143500     ELSE                                                                 
143600       MOVE NEJ              TO NYCKLAR-SW                                
143700       MOVE ARTIKEL-SAKNAS   TO MED-IDMFSFEL                              
143800       CALL WMEDKONV USING MED-WMEDAREA                                   
143900       MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                              
144000       PERFORM MFS-RENSA-FAELT-IN                                         
144100       PERFORM MFS-RENSA-FAELT-UT                                         
144200     END-IF                                                               
144300     .                                                                    
144400     EJECT                                                                
144500                                                                          
144600* --- IMS SEKTIONER ---                                                   
144700     SKIP3                                                                
144800 IMS-GET-MSG SECTION.                                                     
144900                                                                          
145000     MOVE '  QC' TO GODK-STATUSKODER                                      
145100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
145200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
145300     PERFORM IMS-STATUSKONTROLL                                           
145400     .                                                                    
145500     SKIP3                                                                
145600 IMS-INSERT-MSG SECTION.                                                  
145700                                                                          
145800     IF ENGLISH-TEXT                                                      
145900       MOVE 'N' TO MFS-KDHUVOMR                                           
146000     END-IF                                                               
146100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
146200     MOVE SPACE TO GODK-STATUSKODER                                       
146300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
146400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
146500     PERFORM IMS-STATUSKONTROLL                                           
146600     .                                                                    
146700     EJECT                                                                
146800 IMS-GU-K601 SECTION.                                                     
146900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
147000          DELIMITED BY SIZE INTO SSA1                                     
147100     MOVE '  GE' TO GODK-STATUSKODER                                      
147200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
147300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
147400     PERFORM IMS-STATUSKONTROLL                                           
147500     .                                                                    
147600     EJECT                                                                
147700 IMS-GU-K611 SECTION.                                                     
147800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
147900          DELIMITED BY SIZE INTO SSA1                                     
148000     MOVE 'WDK611  '       TO SSA2                                        
148100     MOVE '  GE' TO GODK-STATUSKODER                                      
148200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
148300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
148400     PERFORM IMS-STATUSKONTROLL                                           
148500     .                                                                    
148600     EJECT                                                                
148700 IMS-GU-K626 SECTION.                                                     
148800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
148900          DELIMITED BY SIZE INTO SSA1                                     
149000     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
149100     MOVE 'WDK626   ' TO SSA3                                             
149200     MOVE '  GE' TO GODK-STATUSKODER                                      
149300     CALL CBLTDLI USING GU                                                
149400                      WDK6-PCB DLI-IO-WDK626 SSA1 SSA2 SSA3               
149500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
149600     PERFORM IMS-STATUSKONTROLL                                           
149700     .                                                                    
149800     EJECT                                                                
149900 IMS-GHU-K626 SECTION.                                                    
150000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
150100          DELIMITED BY SIZE INTO SSA1                                     
150200     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
150300     MOVE 'WDK626   ' TO SSA3                                             
150400     MOVE '  GE' TO GODK-STATUSKODER                                      
150500     CALL CBLTDLI USING GHU                                               
150600                      WDK6-PCB DLI-IO-WDK626 SSA1 SSA2 SSA3               
150700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
150800     PERFORM IMS-STATUSKONTROLL                                           
150900     .                                                                    
151000     EJECT                                                                
151100 IMS-ISRT-K626 SECTION.                                                   
151200                                                                          
151300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
151400          DELIMITED BY SIZE INTO SSA1                                     
151500     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
151600     MOVE 'WDK626   ' TO SSA3                                             
151700     MOVE '  ' TO GODK-STATUSKODER                                        
151800     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK626                       
151900                             SSA1 SSA2 SSA3                               
152000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
152100     PERFORM IMS-STATUSKONTROLL                                           
152200     .                                                                    
152300     EJECT                                                                
152400 IMS-REPL-K626 SECTION.                                                   
152500                                                                          
152600     MOVE '  ' TO GODK-STATUSKODER                                        
152700     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK626                       
152800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
152900     PERFORM IMS-STATUSKONTROLL                                           
153000     .                                                                    
153100     EJECT                                                                
153110 IMS-GHU-WDK629 SECTION.                                                  
153111                                                                          
153120     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
153130          DELIMITED BY SIZE INTO SSA1                                     
153131     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
153132           DELIMITED BY SIZE INTO SSA2                                    
153150     MOVE 'WDK629   ' TO SSA3                                             
153160     MOVE '  GE' TO GODK-STATUSKODER                                      
153170     CALL CBLTDLI USING GHU                                               
153180                      WDK6-PCB DLI-IO-WDK629 SSA1 SSA2 SSA3               
153190     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
153191     PERFORM IMS-STATUSKONTROLL                                           
153192     .                                                                    
153193     EJECT                                                                
153194 IMS-REPL-WDK629 SECTION.                                                 
153195                                                                          
153196     MOVE '  ' TO GODK-STATUSKODER                                        
153197     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK629                       
153198     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
153199     PERFORM IMS-STATUSKONTROLL                                           
153200     .                                                                    
153201     EJECT                                                                
153210 IMS-GU-D301-BSEQ SECTION.                                                
153300                                                                          
153400     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
153500          DELIMITED BY SIZE INTO SSA1                                     
153600     MOVE '  GE' TO GODK-STATUSKODER                                      
153700     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD301 SSA1                    
153800     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
153900     PERFORM IMS-STATUSKONTROLL                                           
154000     .                                                                    
154100     SKIP3                                                                
154200 IMS-GNP-D311 SECTION.                                                    
154300                                                                          
154400     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
154500          DELIMITED BY SIZE INTO SSA1                                     
154600     MOVE '  GE' TO GODK-STATUSKODER                                      
154700     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-WDD311 SSA1                   
154800     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
154900     PERFORM IMS-STATUSKONTROLL                                           
155000     .                                                                    
155100     EJECT                                                                
155200 IMS-ISRT-R2202 SECTION.                                                  
155300                                                                          
155400     STRING 'WDG301  (WDG3KEY  =' W-WDG3KEY ')'                           
155500             DELIMITED BY SIZE INTO SSA1                                  
155600     MOVE 'WDG302  ' TO SSA2                                              
155700     MOVE '  ' TO GODK-STATUSKODER                                        
155800     CALL CBLTDLI USING ISRT WDG3-PCB                                     
155900                                 DLI-IO-WDG302 SSA1 SSA2                  
156000     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
156100     PERFORM IMS-STATUSKONTROLL                                           
156200     .                                                                    
156300     EJECT                                                                
156400 IMS-STATUSKONTROLL SECTION.                                              
156500                                                                          
156600     SET STATUS-IX TO 1                                                   
156700     SEARCH GODK-STATUS                                                   
156800       AT END                                                             
156900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
157000         DELIMITED BY SIZE INTO FELTEXT                                   
157100         CALL FELLOG                                                      
157200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
157300         CONTINUE                                                         
157400     END-SEARCH                                                           
157500     .                                                                    
157600     EJECT                                                                
157700*    -COPY WY2000P1                                                       
