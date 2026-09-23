000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4010400.                                                
000300 AUTHOR.         GÖRAN KJELLSON.                                          
000400 DATE-WRITTEN.   APRIL 1979.                                              
000500     REMARKS.                                                             
000600*    FUNKTION.   TP-FRÅGE-PROGRAM ARTIKELINFO DISTRIBUTION - CDC          
000700*                                                                         
000800*                PROGRAMMET LÄSER:                                        
000900*                                                                         
001000*                  WLINLB LEVERANSPLANEREGISTER WDD9                      
001100*                  WLARTC ARTIKELREGISTER       WDK6                      
001200*                  WLARTM ART.REG SALDO         WDK9                      
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W4T104                                              
001600*        MID:         W4I10401                                            
001700*    UTDATA.                                                              
001800*        MOD:         W4O10401                                            
001900*    SUBPROGRAM.                                                          
002000*        FELLOG                                                           
002100*        WDATKONV                                                         
002200*                                                                         
002300*   ÄNDRINGAR:                                                            
002400*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
002500*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
002600*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
002700*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
002800*                                                                         
002900     SKIP2                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 DATA DIVISION.                                                           
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500*    -COPY WY2000W1                                                       
003600     SKIP3                                                                
003700 77  MAX-MOD-LAENGD  PIC S9(9) COMP SYNC  VALUE ZERO.                     
003800 77  CL-INDEX        PIC S9(9) COMP SYNC.                                 
003900 77  IX              PIC S9(9) COMP SYNC.                                 
004000 77  IND             PIC S9(9) COMP SYNC.                                 
004100 77  WS-IDTRANS                  PIC X(4).                                
004200     88  WS-GODKAEND-BILD      VALUE '4101' '4102' '4103' '4104'          
004300                                     '4105' '4106' '4107' '4108'.         
004400     88  EGEN-MID              VALUE '4104'.                              
004500                                                                          
004600 77  WS-IDLEVNR-8        PIC X(8)               VALUE SPACE.              
004700                                                                          
004800 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
004900     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
005000     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
005100                                                                          
005200 01  KONSTANTER.                                                          
005300     03  JA                  PIC X       VALUE 'J'.                       
005400     03  NEJ                 PIC X       VALUE 'N'.                       
005500                                                                          
005600 01  WS-TIAAVVD.                                                          
005700     03  WS-TIAAVV           PIC S9(4)   VALUE ZERO.                      
005800     03  FILLER              PIC S9(1)   VALUE ZERO.                      
005900                                                                          
006000 01  DATUM-FAELT.                                                         
006100     03  DAGENS-DATUM        PIC S9(7)   COMP-3 VALUE ZERO.               
006200                                                                          
006300 01  NYCKEL-SW               PIC X.                                       
006400     88  NYCKLAR-OK          VALUE 'J'.                                   
006500                                                                          
006600 01  ARTIKEL-SW              PIC X.                                       
006700     88  NY-ARTIKEL          VALUE 'J'.                                   
006800                                                                          
006900 01  LEVBESK-SW              PIC X.                                       
007000     88  FLER-LEVERANSBESKED VALUE 'J'.                                   
007100                                                                          
007200 01  TEXT-LEVBESK-SW         PIC X.                                       
007300     88  FLER-TEXTLEVBESKED  VALUE 'J'.                                   
007400                                                                          
007500 01  DYNAMISKA-SUBPROGRAM.                                                
007600     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
007700     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI'.                 
007800     03  FELLOG              PIC X(8)    VALUE 'FELLOG'.                  
007900     03  W005INIT            PIC X(8)    VALUE 'W005INIT'.                
008000     03  WMEDKONV            PIC X(8)    VALUE 'WMEDKONV'.                
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008300*01 -COPY WMSGINIT                                                        
008400     EJECT                                                                
008500                                                                          
008600     EJECT                                                                
008700 01      W-IDARTNR-X.                                                     
008800   03    W-IDARTNR       PIC S9(9)   VALUE ZERO  COMP-3.                  
008900 01      W-WDD901KY-X.                                                    
009000   03    W-IDARTNR-D9    PIC S9(9)   VALUE ZERO  COMP-3.                  
009100   03    W-IDDC-D9       PIC X(2)    VALUE SPACE.                         
009200 01      W-IDLEVNR-X.                                                     
009300   03    W-IDLEVNR       PIC  X(5)   VALUE SPACE.                         
009400                                                                          
009500 01      W-IDLEVBSK-2-X.                                                  
009600   03    W-IDLEVBSK-2    PIC S9      VALUE +2    COMP-3.                  
009700 01      W-IDLEVBSK-4-X.                                                  
009800   03    W-IDLEVBSK-4    PIC S9      VALUE +4    COMP-3.                  
009900     EJECT                                                                
010000 01      MEDDELANDEN.                                                     
010100   03  FILLER-1.                                                          
010200     05  FILLER          PIC X(36)                                        
010300         VALUE 'ARTIKELN FINNS EJ I ARTIKELREGISTRET'.                    
010400     05  FILLER          PIC 9  COMP-3 VALUE 3.                           
010500     05  FILLER          PIC X(3).                                        
010600     05  FILLER          PIC X(35)                                        
010700      VALUE 'THIS PARTNO IS NOT IN THE DATABASE'.                         
010800     05  FILLER          PIC 9  COMP-3 VALUE 3.                           
010900     05  FILLER          PIC X(4).                                        
011000   03    FILLER REDEFINES FILLER-1.                                       
011100     05  MEDDELANDE-1   PIC X(40)  OCCURS 2.                              
011200                                                                          
011300   03  FILLER-15.                                                         
011400     05  FILLER          PIC X(20)                                        
011500         VALUE 'ARTIKELN ÄR UTGÅNGEN'.                                    
011600     05  FILLER          PIC 9 COMP-3 VALUE 3.                            
011700     05  FILLER          PIC X(19).                                       
011800     05  FILLER          PIC X(23)                                        
011900         VALUE 'THIS PARTNO IS DELETED'.                                  
012000     05  FILLER          PIC 9 COMP-3 VALUE 3.                            
012100     05  FILLER          PIC X(16).                                       
012200   03    FILLER REDEFINES FILLER-15.                                      
012300     05  MEDDELANDE-2    PIC X(40)  OCCURS 2.                             
012400                                                                          
012500   03  FILLER-2.                                                          
012600     05  FILLER          PIC X(26)                                        
012700         VALUE 'ARTIKELNUMRET EJ NUMERISKT'.                              
012800     05  FILLER          PIC 9  COMP-3 VALUE 3.                           
012900     05  FILLER          PIC X(13).                                       
013000     05  FILLER          PIC X(22)                                        
013100      VALUE 'PARTNUMBER NOT NUMERIC'.                                     
013200     05  FILLER          PIC 9  COMP-3 VALUE 3.                           
013300     05  FILLER          PIC X(17).                                       
013400   03    FILLER REDEFINES FILLER-2.                                       
013500     05  EJ-NUMERISK    PIC X(40)  OCCURS 2.                              
013600                                                                          
013700   03  FILLER-3.                                                          
013800     05  FILLER          PIC X(27)                                        
013900         VALUE 'FLER LEVERANSBESKED FINNS  '.                             
014000     05  FILLER          PIC X(27)                                        
014100         VALUE 'MORE DELIVERY NOTICE EXISTS'.                             
014200   03  FILLER REDEFINES FILLER-3.                                         
014300     05  FLER-LEVBESK    PIC X(27)  OCCURS 2.                             
014400                                                                          
014500     EJECT                                                                
014600 01  MESSAGE-CODES.                                                       
014700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
014800     03  CONFLICT                PIC X(3)    VALUE '002'.                 
014900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
015000     03  URVAL-SAKNAS            PIC X(3)    VALUE '005'.                 
015100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
015200     03  ERR-NOT-REGISTERED      PIC X(3)    VALUE '010'.                 
015300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
015400     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
015500     03  ARTIKEL-UTGANGEN        PIC X(3)    VALUE '018'.                 
015600     03  INF-UPDATE-NOT-DONE     PIC X(3)    VALUE '034'.                 
015700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
015800     03  ARTIKEL-ERSATT          PIC X(3)    VALUE '220'.                 
015900     03  ARTIKEL-SAKNAS-SDC      PIC X(3)    VALUE '305'.                 
016000     03  EJ-GODK-REFILL          PIC X(3)    VALUE '307'.                 
016100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
016200     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
016300     EJECT                                                                
016400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
016500*01 -COPY WMEDAREA                                                        
016600     EJECT                                                                
016700*                        ****    TP-AREOR                                 
016800 01  FILLER  PIC X(16)   VALUE '    TP-AREAOR   '.                        
016900     EJECT                                                                
017000*01      MID -COPY W4I10401 -PRE MID-.                                    
017100     EJECT                                                                
017200*01      -COPY WMSGAREA                                                   
017300     EJECT                                                                
017400*  03    MOD -COPY W4O10401 -PRE MOD- -RED MSG-AREA.                      
017500     EJECT                                                                
017600*01  -COPY WMFSAREA.                                                      
017700     EJECT                                                                
017800*                        ****    PARAMETRAR TILL WDATKONV                 
017900 01  FILLER  PIC X(16)   VALUE '   WDATAREAC0   '.                        
018000*01   -COPY WDATAREA                                                      
018100     EJECT                                                                
018200******************************************************************        
018300*****                                                                     
018400*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018500*****                                                                     
018600 01  IMS-WS.                                                              
018700   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
018800     SKIP3                                                                
018900*****                    **** STATUS-KOD FRÅN IMS                         
019000   03    STATUS-WS       PIC XX.                                          
019100         88  SEGMENT-FINNS       VALUE '  '.                              
019200         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
019300                                                                          
019400   03    GODK-STATUSKODER.                                                
019500     05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.                
019600                                                                          
019700 01      SSA1            PIC X(50).                                       
019800 01      SSA2            PIC X(50).                                       
019900     SKIP3                                                                
020000*                            IMS FUNKTIONSKODER                           
020100*01      -COPY W0003                                                      
020200     EJECT                                                                
020300*                            DLI INPUT-OUTPUT AREA                        
020400 01      DLI-IO-AREA     PIC X(900)  VALUE SPACE.                         
020500                                                                          
020600*01  WLINLB01     -COPY WDD901 -PRE INLB01- -RED DLI-IO-AREA.             
020700     EJECT                                                                
020800*01  WLINLB11     -COPY WDD902 -PRE INLB11- -RED DLI-IO-AREA.             
020900     EJECT                                                                
021000*01  WLINLB23     -COPY WDD905 -PRE INLB23- -RED DLI-IO-AREA.             
021100     EJECT                                                                
021200*01  WLINLB24     -COPY WDD924 -PRE INLB24- -RED DLI-IO-AREA.             
021300     EJECT                                                                
021400*01  WLINLB25     -COPY WDD925 -PRE INLB25- -RED DLI-IO-AREA.             
021500     EJECT                                                                
021600*01  WLARTC01     -COPY WDK601              -RED DLI-IO-AREA.             
021700     EJECT                                                                
021800*01  WLARTC11     -COPY WDK611              -RED DLI-IO-AREA.             
021900     EJECT                                                                
022000*                            DLI INPUT-OUTPUT AREA2                       
022100 01      DLI-IO-AREA2    PIC X(100)  VALUE SPACE.                         
022200     SKIP3                                                                
022300*01  WLARTM01     -COPY WDK901 -PRE ARTM-   -RED DLI-IO-AREA2.            
022400     EJECT                                                                
022500 LINKAGE SECTION.                                                         
022600     SKIP2                                                                
022700*01  -COPY W0009     -PRE MSG-                                            
022800     EJECT                                                                
022900*01  -COPY W0008     -PRE USEA-.                                          
023000         05  FILLER           PIC X.                                      
023100     EJECT                                                                
023200*01  -COPY W0008     -PRE INLB-.                                          
023300         05  INLB-KEY-IDARTNR     PIC S9(9) COMP-3.                       
023400         05  INLB-KEY-IDLEVNR     PIC  X(5).                              
023500     EJECT                                                                
023600*01  -COPY W0008     -PRE ARTC-.                                          
023700         05  FILLER           PIC X.                                      
023800     EJECT                                                                
023900*01  -COPY W0008     -PRE ARTM-.                                          
024000         05  FILLER           PIC X.                                      
024100     EJECT                                                                
024200 PROCEDURE DIVISION USING MSG-PCB USEA-PCB INLB-PCB ARTC-PCB              
024300                                  ARTM-PCB.                               
024400     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB INLB-PCB ARTC-PCB             
024500                                   ARTM-PCB.                              
024600                                                                          
024700     PERFORM IMS-GET-MSG                                                  
024800     IF SEGMENT-FINNS                                                     
024900       PERFORM A-KOLLA-NYCKLAR                                            
025000       IF NYCKLAR-OK                                                      
025100         PERFORM S1-SECURITY-CHECK-PARTNO                                 
025200         IF PASSED-SECURITY-CHECK                                         
025300                                                                          
025400           PERFORM IMS-GET-WLARTC01-ARTIKEL                               
025500           IF SEGMENT-FINNS                                               
025600             IF NY-ARTIKEL                                                
025700               PERFORM B-REDIGERA-WLARTC-INFO                             
025800               PERFORM C-REDIGERA-WLARTM-INFO                             
025900               PERFORM D-REDIGERA-WLINLB-INFO                             
026000             ELSE                                                         
026100               PERFORM E-SPARA-GRUNDBILD                                  
026200               IF FLER-LEVERANSBESKED                                     
026300                 PERFORM DA-REDIGERA-LEVERANSBESKED                       
026400               ELSE                                                       
026500                 PERFORM F-SPARA-LEVERANSBESKED                           
026600               END-IF                                                     
026700               IF FLER-TEXTLEVBESKED                                      
026800                 PERFORM DB-REDIGERA-TEXTLEVBESKED                        
026900               ELSE                                                       
027000                 MOVE MFS-ROER-EJ-FAELT TO MOD-TELEVBSK-EXT               
027100                                           MOD-TELEVBSK-EXT2              
027200               END-IF                                                     
027300             END-IF                                                       
027400           ELSE                                                           
027500             MOVE MEDDELANDE-1 (CL-INDEX) TO MOD-TEMFSFEL                 
027600           END-IF                                                         
027700                                                                          
027800         END-IF                                                           
027900       ELSE                                                               
028000           PERFORM G-RENSA-NYCKLAR                                        
028100       END-IF                                                             
028200       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
028300       PERFORM IMS-INSERT-MSG                                             
028400     END-IF                                                               
028500     MOVE ZERO TO RETURN-CODE                                             
028600     GOBACK.                                                              
028700     EJECT                                                                
028800 A-KOLLA-NYCKLAR SECTION.                                                 
028900                                                                          
029000     MOVE JA TO NYCKEL-SW                                                 
029100     MOVE NEJ TO ARTIKEL-SW                                               
029200                 LEVBESK-SW                                               
029300                 TEXT-LEVBESK-SW                                          
029400     IF MSG-DUBBLA-TRANSKODER                                             
029500         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I10401               
029600         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                
029700         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR CL-INDEX                     
029800         MOVE JA TO ARTIKEL-SW                                            
029900     ELSE                                                                 
030000         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I10401                
030100         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                
030200         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR CL-INDEX                     
030300     END-IF                                                               
030400                                                                          
030500     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W4O10401 + 4                  
030600     MOVE MFS-IDTRANS        TO WS-IDTRANS                                
030700                                                                          
030800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
030900     MOVE '001'             TO MSGI-KDCALL                                
031000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
031100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
031200     MOVE '4104'            TO MSGI-IDTRANS                               
031300                                                                          
031400     IF MFS-IDTRANS = '4104'                                              
031500     OR (MID-IDARTNR-IN NUMERIC                                           
031600     AND MID-IDARTNR-IN > ZERO)                                           
031700         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
031800     END-IF                                                               
031900                                                                          
032000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
032100                                                                          
032200     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
032300                                                                          
032400     IF  MSGI-IDLAND-SPR = 'GB'                                           
032500         MOVE 2 TO CL-INDEX                                               
032600     ELSE                                                                 
032700         MOVE 1 TO CL-INDEX                                               
032800     END-IF                                                               
032900                                                                          
033000     IF MID-IDARTNR-IN     NOT = ALL '+'                                  
033100       MOVE JA TO ARTIKEL-SW                                              
033200       MOVE ZERO TO MID-KVLEVBSK                                          
033300                    MID-KVLEVBSK-TEXT                                     
033400     END-IF                                                               
033500                                                                          
033600     IF MID-KVLEVBSK NOT NUMERIC                                          
033700       MOVE ZERO TO MID-KVLEVBSK                                          
033800     END-IF                                                               
033900     IF MID-KVLEVBSK-TEXT NOT NUMERIC                                     
034000       MOVE ZERO TO MID-KVLEVBSK-TEXT                                     
034100     END-IF                                                               
034200     IF MID-KVLEVBSK NOT = ZERO                                           
034300       MOVE JA TO LEVBESK-SW                                              
034400     END-IF                                                               
034500     IF MID-KVLEVBSK-TEXT NOT = ZERO                                      
034600       MOVE JA TO TEXT-LEVBESK-SW                                         
034700     END-IF                                                               
034800     IF MFS-IDTRANS NOT = '4104'                                          
034900         MOVE JA TO ARTIKEL-SW                                            
035000     END-IF                                                               
035100     MOVE LOW-VALUE TO MOD-W4O10401                                       
035200     MOVE 'W4O104N1' TO MFS-IDMOD                                         
035300     MOVE '4104' TO MOD-IDTRANS                                           
035400                                                                          
035500     MOVE MSGI-IDARTNR      TO MOD-IDARTNR-UT                             
035600     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
035700                                                                          
035800     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
035900     IF MSGI-IDARTNR    NOT NUMERIC                                       
036000         MOVE NEJ TO NYCKEL-SW                                            
036100         MOVE EJ-NUMERISK (CL-INDEX) TO MOD-TEMFSFEL                      
036200     ELSE                                                                 
036300         MOVE MSGI-IDARTNR TO W-IDARTNR                                   
036400     END-IF                                                               
036500                                                                          
036600     MOVE MAX-MOD-LAENGD TO MSG-KVLL                                      
036700     ACCEPT DAGENS-DATUM FROM DATE                                        
036800     .                                                                    
036900     EJECT                                                                
037000 B-REDIGERA-WLARTC-INFO SECTION.                                          
037100                                                                          
037200     IF SEGMENT-FINNS                                                     
037300       MOVE '-'          TO MOD-STRECK                                    
037400       MOVE ART-REKSIFFR TO MOD-REKSIFFR                                  
037500       MOVE ART-KDPRODSL TO MOD-KDPRODSL MOD-KDPRORED                     
037600       MOVE ART-IDFKNGRP TO MOD-IDFKNGRP                                  
037700       MOVE ART-KDSORT   TO MOD-KDSORT                                    
037800                                                                          
037900       IF ART-KDERS-UTG > ZERO                                            
038000           MOVE MEDDELANDE-2 (CL-INDEX) TO MOD-TEMFSFEL                   
038100       ELSE                                                               
038200         PERFORM BA-REDIGERA-ARTC11-CLAG                                  
038300       END-IF                                                             
038400                                                                          
038500     END-IF                                                               
038600     .                                                                    
038700     EJECT                                                                
038800 BA-REDIGERA-ARTC11-CLAG SECTION.                                         
038900                                                                          
039000     PERFORM IMS-GET-WLARTC11-CLAG                                        
039100                                                                          
039200     PERFORM BAA-REDIGERA-IDVAGN                                          
039300                                                                          
039400     MOVE CLAG-KDGK     TO MOD-KDGK                                       
039500     MOVE CLAG-KDLTK    TO MOD-KDLTK                                      
039600     MOVE CLAG-VKART    TO MOD-VKART                                      
039700     MOVE CLAG-VLARTNTO TO MOD-VLARTNTO                                   
039800                                                                          
039900     MOVE +1 TO IX                                                        
040000     PERFORM UNTIL IX > 6                                                 
040100       MOVE CLAG-IDSTATNR (IX) TO MOD-IDSTATNR (IX)                       
040200       ADD +1 TO IX                                                       
040300     END-PERFORM                                                          
040400                                                                          
040500     MOVE CLAG-KVEFRS TO MOD-KVEFRS                                       
040600     .                                                                    
040700     EJECT                                                                
040800 BAA-REDIGERA-IDVAGN SECTION.                                             
040900                                                                          
041000     MOVE +1 TO IND                                                       
041100     PERFORM UNTIL IND > 3                                                
041200       IF CLAG-IDKAT (IND) NOT = SPACE                                    
041300         MOVE CLAG-IDKAT (IND) TO MOD-IDVAGN (IND)                        
041400       END-IF                                                             
041500                                                                          
041600       ADD +1 TO IND                                                      
041700     END-PERFORM                                                          
041800     .                                                                    
041900     EJECT                                                                
042000 C-REDIGERA-WLARTM-INFO SECTION.                                          
042100                                                                          
042200     PERFORM IMS-GET-ARTM01                                               
042300     IF SEGMENT-FINNS                                                     
042400        MOVE ARTM-ART-SUTPO-TOT  TO MOD-SUTPO-TOT                         
042500                                                                          
042600        COMPUTE MOD-KVOKS-DAG = ARTM-ART-KVOKS-DAG +                      
042700                                ARTM-ART-KVOKS-VOR                        
042800                                                                          
042900        MOVE ARTM-ART-KVOKS-BULK TO MOD-KVOKS-BULK                        
043000     ELSE                                                                 
043100        MOVE ZERO                TO MOD-SUTPO-TOT                         
043200        MOVE ZERO                TO MOD-KVOKS-DAG                         
043300        MOVE ZERO                TO MOD-KVOKS-BULK                        
043400     END-IF                                                               
043500     .                                                                    
043600     EJECT                                                                
043700 D-REDIGERA-WLINLB-INFO SECTION.                                          
043800                                                                          
043900     PERFORM DA-REDIGERA-LEVERANSBESKED                                   
044000     PERFORM DB-REDIGERA-TEXTLEVBESKED                                    
044100     .                                                                    
044200     EJECT                                                                
044300 DA-REDIGERA-LEVERANSBESKED SECTION.                                      
044400                                                                          
044500     MOVE MSGI-IDARTNR  TO W-IDARTNR-D9                                   
044510     MOVE MSGI-IDDC     TO W-IDDC-D9                                      
044600     PERFORM IMS-GET-WLINLB-ARTIKEL                                       
044700     IF SEGMENT-FINNS                                                     
044800        PERFORM IMS-GET-WLINLB-LEVERANSBESKED                             
044900        IF SEGMENT-FINNS                                                  
045000           MOVE MID-KVLEVBSK TO IX                                        
045100           PERFORM UNTIL SEGMENT-SAKNAS OR IX = 0                         
045200                PERFORM IMS-GET-WLINLB-LEVERANSBESKED                     
045300                SUBTRACT 1 FROM IX                                        
045400           END-PERFORM                                                    
045500           MOVE +1 TO IX                                                  
045600           PERFORM UNTIL SEGMENT-SAKNAS OR IX > 4                         
045700                 MOVE INLB24-LEV-KVAVIS-BSKKVAR           TO              
045800                                                MOD-KVAVIS(IX)            
045900              IF INLB24-LEV-TILEVBSK-DISP           > 0                   
046000                 PERFORM S01-KONVERTERA-TILEVBSK                          
046100                 MOVE WS-TIAAVV       TO MOD-TILEVBSK(IX)                 
046200              ELSE                                                        
046300                 MOVE MFS-RENSA-FAELT TO MOD-TILEVBSK(IX)                 
046400              END-IF                                                      
046500              ADD +1 TO IX                                                
046600              PERFORM IMS-GET-WLINLB-LEVERANSBESKED                       
046700           END-PERFORM                                                    
046800           IF IX = 5 AND SEGMENT-FINNS                                    
046900              MOVE FLER-LEVBESK (CL-INDEX) TO MOD-TILEVBSK-TEXT           
047000              COMPUTE MOD-KVLEVBSK = MID-KVLEVBSK + 4                     
047100           ELSE                                                           
047200              MOVE ZERO TO MOD-KVLEVBSK                                   
047300           END-IF                                                         
047400        END-IF                                                            
047500     END-IF                                                               
047600     .                                                                    
047700     EJECT                                                                
047800 DB-REDIGERA-TEXTLEVBESKED SECTION.                                       
047900                                                                          
048000     PERFORM IMS-GET-WLINLB-ARTIKEL                                       
048100     IF SEGMENT-FINNS                                                     
048200        PERFORM IMS-GET-WLINLB-2-TEXTLEVBESKED                            
048300        IF SEGMENT-FINNS                                                  
048400           MOVE MID-KVLEVBSK-TEXT TO IX                                   
048500           PERFORM UNTIL SEGMENT-SAKNAS OR IX = 0                         
048600              PERFORM IMS-GET-WLINLB-2-TEXTLEVBESKED                      
048700              SUBTRACT 1 FROM IX                                          
048800           END-PERFORM                                                    
048900           MOVE DAGENS-DATUM         TO TMP1-YYMMDD                       
049000           MOVE INLB25-INFO-TIBORT   TO TMP2-YYMMDD                       
049100           PERFORM WY2000P1                                               
049200           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
049300              MOVE MFS-RENSA-FAELT      TO MOD-TELEVBSK-EXT               
049400                                           MOD-TELEVBSK-EXT2              
049500           ELSE                                                           
049600              MOVE INLB25-INFO-TELEVBSK TO MOD-TELEVBSK-EXT               
049700              MOVE INLB-KEY-IDLEVNR     TO W-IDLEVNR                      
049800              PERFORM IMS-GET-WLINLB-4-TEXTLEVBESKED                      
049900              IF SEGMENT-FINNS                                            
050000                MOVE INLB25-INFO-TELEVBSK TO MOD-TELEVBSK-EXT2            
050100              ELSE                                                        
050200                MOVE MFS-RENSA-FAELT      TO MOD-TELEVBSK-EXT2            
050300              END-IF                                                      
050400           END-IF                                                         
050500           PERFORM IMS-GET-WLINLB-2-TEXTLEVBESKED                         
050600           IF SEGMENT-FINNS                                               
050700              MOVE FLER-LEVBESK(CL-INDEX) TO MOD-TILEVBSK-TEXT            
050800              COMPUTE MOD-KVLEVBSK-TEXT = MID-KVLEVBSK-TEXT + 1           
050900           ELSE                                                           
051000              MOVE ZERO TO MOD-KVLEVBSK-TEXT                              
051100           END-IF                                                         
051200        ELSE                                                              
051300           MOVE MFS-RENSA-FAELT         TO MOD-TELEVBSK-EXT               
051400                                           MOD-TELEVBSK-EXT2              
051500        END-IF                                                            
051600     END-IF                                                               
051700     .                                                                    
051800     EJECT                                                                
051900 E-SPARA-GRUNDBILD SECTION.                                               
052000                                                                          
052100     MOVE MFS-ROER-EJ-FAELT TO MOD-IDFKNGRP                               
052200                               MOD-KDSORT                                 
052300                               MOD-VKART                                  
052400                               MOD-VLARTNTO                               
052500                               MOD-KVOKS-DAG                              
052600                               MOD-KVOKS-BULK                             
052700                               MOD-KDPRODSL                               
052800                               MOD-KDPRORED                               
052900                               MOD-KDLTK                                  
053000                               MOD-KDGK                                   
053100                               MOD-SUTPO-TOT                              
053200                               MOD-KVEFRS                                 
053300     MOVE +1 TO IX                                                        
053400     PERFORM UNTIL IX > 6                                                 
053500       MOVE MFS-ROER-EJ-FAELT TO MOD-IDSTATNR (IX)                        
053600       ADD +1 TO IX                                                       
053700     END-PERFORM                                                          
053800                                                                          
053900     MOVE +1 TO IX                                                        
054000     PERFORM UNTIL IX > 3                                                 
054100       MOVE MFS-ROER-EJ-FAELT TO MOD-IDVAGN (IX)                          
054200       ADD +1 TO IX                                                       
054300     END-PERFORM                                                          
054400     .                                                                    
054500     EJECT                                                                
054600 F-SPARA-LEVERANSBESKED SECTION.                                          
054700                                                                          
054800     MOVE MFS-ROER-EJ-FAELT TO MOD-TILEVBSK-TEXT                          
054900                               MOD-TELEVBSK-EXT                           
055000                               MOD-TELEVBSK-EXT2                          
055100     MOVE +1 TO IX                                                        
055200     PERFORM UNTIL IX > 4                                                 
055300       MOVE MFS-ROER-EJ-FAELT TO MOD-KVAVIS (IX)                          
055400                                 MOD-TILEVBSK (IX)                        
055500       ADD +1 TO IX                                                       
055600     END-PERFORM                                                          
055700     .                                                                    
055800     EJECT                                                                
055900 G-RENSA-NYCKLAR SECTION.                                                 
056000     MOVE MFS-RENSA-FAELT           TO  MOD-IDARTNR-UT                    
056100     .                                                                    
056200     EJECT                                                                
056300 S01-KONVERTERA-TILEVBSK SECTION.                                         
056400                                                                          
056500     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
056600     MOVE INLB24-LEV-TILEVBSK-DISP           TO DAT-I-TIDATUM             
056700     CALL WDATKONV USING DAT-KDDATFORM                                    
056800                         DAT-I-TIDATUM                                    
056900                         DAT-O-TIDATUM                                    
057000                         DAT-KDSVAR                                       
057100     IF DAT-KDSVAR-OK                                                     
057200        MOVE DAT-TIAAVVD TO WS-TIAAVVD                                    
057300     ELSE                                                                 
057400        MOVE ZERO        TO WS-TIAAVVD                                    
057500     END-IF                                                               
057600     .                                                                    
057700     EJECT                                                                
057800                                                                          
057900 S1-SECURITY-CHECK-PARTNO SECTION.                                        
058000     SKIP2                                                                
058100*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
058200     PERFORM IMS-GET-WLARTC01-ARTIKEL                                     
058300     IF  SEGMENT-FINNS                                                    
058400       MOVE ART-IDLEVNR          TO WS-IDLEVNR-8                          
058500       IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                          
058600       OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                    
058700*        --- USER AUTHORIZED                                              
058800         SET PASSED-SECURITY-CHECK TO TRUE                                
058900       ELSE                                                               
059000*        --- OBEHÖRIG USER / USER NOT AUTHORIZED                          
059100         MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                          
059200         CALL WMEDKONV USING MED-WMEDAREA                                 
059300         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
059400                                                                          
059500         SET BLOCKED-SECURITY-CHECK TO TRUE                               
059600       END-IF                                                             
059700     ELSE                                                                 
059800       MOVE MEDDELANDE-1 (CL-INDEX) TO MOD-TEMFSFEL                       
059900                                                                          
060000     END-IF                                                               
060100     .                                                                    
060200     EJECT                                                                
060300                                                                          
060400 IMS-GET-MSG SECTION.                                                     
060500                                                                          
060600     MOVE '  QC' TO GODK-STATUSKODER                                      
060700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
060800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
060900     PERFORM IMS-STATUSKONTROLL                                           
061000     .                                                                    
061100     SKIP3                                                                
061200 IMS-INSERT-MSG SECTION.                                                  
061300                                                                          
061400     IF  MSGI-IDLAND-SPR NOT = 'GB'                                       
061500         MOVE '0' TO MFS-KDHUVOMR                                         
061600     END-IF                                                               
061700                                                                          
061800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
061900     MOVE SPACE TO GODK-STATUSKODER                                       
062000     CALL CBLTDLI USING ISRT MSG-PCB                                      
062100                          MSG-IO-AREA MFS-IDMOD                           
062200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
062300     PERFORM IMS-STATUSKONTROLL                                           
062400     .                                                                    
062500     EJECT                                                                
062600 IMS-GET-WLARTC01-ARTIKEL  SECTION.                                       
062700                                                                          
062800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
062900     DELIMITED BY SIZE INTO SSA1                                          
063000     MOVE '  GE' TO GODK-STATUSKODER                                      
063100     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
063200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
063300     PERFORM IMS-STATUSKONTROLL                                           
063400     .                                                                    
063500     SKIP3                                                                
063600 IMS-GET-WLARTC11-CLAG SECTION.                                           
063700                                                                          
063800     MOVE 'WLARTC11 ' TO SSA1                                             
063900     MOVE '  ' TO GODK-STATUSKODER                                        
064000     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
064100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
064200     PERFORM IMS-STATUSKONTROLL                                           
064300     .                                                                    
064400     EJECT                                                                
064500 IMS-GET-WLINLB-ARTIKEL SECTION.                                          
064600                                                                          
064700     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
064800            DELIMITED BY SIZE INTO SSA1                                   
064900     MOVE '  GE' TO GODK-STATUSKODER                                      
065000     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA SSA1                      
065100     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
065200     PERFORM IMS-STATUSKONTROLL                                           
065300     .                                                                    
065400     SKIP3                                                                
065500 IMS-GET-WLINLB-LEVERANSBESKED SECTION.                                   
065600                                                                          
065700     MOVE 'WLINLB24 ' TO SSA1                                             
065800     MOVE '  GE' TO GODK-STATUSKODER                                      
065900     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1                     
066000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
066100     PERFORM IMS-STATUSKONTROLL                                           
066200     .                                                                    
066300     EJECT                                                                
066400 IMS-GET-WLINLB-2-TEXTLEVBESKED   SECTION.                                
066500                                                                          
066600     STRING 'WLINLB25(IDLEVBSK =' W-IDLEVBSK-2-X ')'                      
066700            DELIMITED BY SIZE INTO SSA1                                   
066800     MOVE '  GE' TO GODK-STATUSKODER                                      
066900     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1                     
067000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
067100     PERFORM IMS-STATUSKONTROLL                                           
067200     .                                                                    
067300     SKIP3                                                                
067400 IMS-GET-WLINLB-4-TEXTLEVBESKED   SECTION.                                
067500                                                                          
067600     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
067700            DELIMITED BY SIZE INTO SSA1                                   
067800     STRING 'WLINLB25(IDLEVBSK =' W-IDLEVBSK-4-X ')'                      
067900            DELIMITED BY SIZE INTO SSA2                                   
068000     MOVE '  GE' TO GODK-STATUSKODER                                      
068100     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1 SSA2                
068200     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
068300     PERFORM IMS-STATUSKONTROLL                                           
068400     .                                                                    
068500     SKIP3                                                                
068600 IMS-GET-ARTM01 SECTION.                                                  
068700                                                                          
068800     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
068900            DELIMITED BY SIZE INTO SSA1                                   
069000     MOVE '  GE' TO GODK-STATUSKODER                                      
069100     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA2 SSA1                     
069200     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
069300     PERFORM IMS-STATUSKONTROLL                                           
069400     .                                                                    
069500     EJECT                                                                
069600 IMS-STATUSKONTROLL SECTION.                                              
069700                                                                          
069800     SET STATUS-IX TO 1                                                   
069900     SEARCH GODK-STATUS AT END CALL FELLOG                                
070000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
070100     END-SEARCH                                                           
070200     .                                                                    
070300     EJECT                                                                
070400*    -COPY WY2000P1                                                       
