000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4010100.                                                
000300 AUTHOR.         INGRID DANIELSSON.                                       
000400     DATE-WRITTEN.   OKT 1977.                                            
000500     REMARKS.                                                             
000600*    FUNKTION.                                                            
000700*        TP-PROGRAM FÖR LAGRET (LAGERINFORMATION).                        
000800*        PROGRAMMET LÄSER :                                               
000900*        LOGISKA DATABASEN WLARTC (ARTREG WDK6)                           
001000*        LOGISKA DATABASEN WLARTS (ARTREG WDK7) SAMT                      
001100*        LOGISKA DATABASEN W6KVAH (KVALREG W6D2).                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W4T101                                              
001400*        MID:         W4I10101                                            
001500*    UTDATA.                                                              
001600*        MOD:         W4O10101                                            
001700*    SUBPROGRAM                                                           
001800*        FELLOG                                                           
001900*                                                                         
002000*    E-TRACKER: 7450319  2008-HÖST  VOHF                                  
002100*    E-TRACKER: 10254592 2015 DECOMISSION VOHF                            
002200*    SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900*                                                                         
003000 77  WS-KVLS                  PIC S9(7)             VALUE ZERO.           
003100 77  WS-KVAKS                 PIC S9(7)             VALUE ZERO.           
003200 77  WS-KVAKS-PAV             PIC S9(7)             VALUE ZERO.           
003300 77  WS-KVEFRS                PIC S9(7)             VALUE ZERO.           
003400 77  WS-KVPB-TOT              PIC S9(6)V9(1)        VALUE ZERO.           
003500 77  JA                       PIC X                 VALUE 'J'.            
003600 77  YES                      PIC X                 VALUE 'Y'.            
003700 77  NEJ                      PIC X                 VALUE 'N'.            
003800 77  IX-CDC                   PIC S9(9)  COMP SYNC  VALUE +1.             
003900 77  IX-SDC                   PIC S9(9)  COMP SYNC  VALUE +2.             
004000 77  MAX-MOD-LENGD            PIC S9(4)  COMP SYNC  VALUE ZERO.           
004100 77  WS-IDTRANS               PIC X(4).                                   
004200     88  WS-GODKAEND-BILD      VALUE '4101' '4102' '4103' '4104'          
004300                                     '4105' '4106' '4107' '4108'.         
004400     88  EGEN-MID              VALUE '4101'.                              
004500                                                                          
004510 01  DAGENS-DATUM             PIC 9(6)    VALUE ZERO.                     
004520                                                                          
004600 01  NYCKEL-SW                PIC X.                                      
004700     88  NYCKLAR-OK                                 VALUE 'J'.            
004800                                                                          
004900 01  INDEX-ORD.                                                           
005000     03  IX                   PIC S9(9) COMP  SYNC  VALUE ZERO.           
005100     03  IND                  PIC S9(9) COMP  SYNC  VALUE ZERO.           
005200     EJECT                                                                
005300 01  DYNAMISKA-SUBPROGRAM.                                                
005400     03  CBLTDLI              PIC X(8)          VALUE 'CBLTDLI'.          
005500     03  FELLOG               PIC X(8)          VALUE 'FELLOG'.           
005600     03  W005INIT             PIC X(8)          VALUE 'W005INIT'.         
005700     EJECT                                                                
005800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
005900*01 -COPY WMSGINIT                                                        
006000     EJECT                                                                
006100                                                                          
006200 01  FILLER                   PIC X(16)          VALUE 'IMS-WS'.          
006300                                                                          
006400 01  NYCKLAR-TILL-DLI.                                                    
006500                                                                          
006600   03   W-IDARTNR-X.                                                      
006700     05 W-IDARTNR             PIC S9(9)  COMP-3  VALUE ZERO.              
006800                                                                          
006900   03   W-IDSKYLT-X.                                                      
007000     05 W-IDSKYLT             PIC X(3).                                   
007100                                                                          
007200   03  W-W6D211KY-MIN-X.                                                  
007300     05  W-W6D2-DAREGDAT-MIN  PIC 9(8)    VALUE ZERO.                     
007400     05  W-W6D2-TIKLOCK-MIN   PIC S9(9)   COMP-3 VALUE +0.                
007500                                                                          
007600   03  W-W6D211KY-MAX-X.                                                  
007700     05  W-W6D2-DAREGDAT-MAX  PIC 9(8)    VALUE ZERO.                     
007800     05  W-W6D2-TIKLOCK-MAX   PIC S9(9)   COMP-3 VALUE +0.                
007900                                                                          
008000     SKIP3                                                                
008100 01  FELMEDDELANDE.                                                       
008200     03  FEL-1   PIC X(26)   VALUE 'PART NUMBER IS NOT NUMERIC'.          
008300     03  FEL-2   PIC X(35)   VALUE 'THIS ARTICLE IS NOT IN THE DAT        
008400-                                  'ABASE'.                               
008500     03  FEL-3   PIC X(29)  VALUE 'THIS ARTICLE HAS BEEN DELETED'.        
008600     03  FEL-5   PIC X(36)  VALUE  'MORE SUPERSESSION INF. ARE AVA        
008700-                                  'ILABLE'.                              
008800     EJECT                                                                
008900 01  KONSTANT-AREA.                                                       
009000                                                                          
009100     03  K-KDKVAINF-R    PIC X(1)  VALUE 'R'.                             
009200     EJECT                                                                
009300 01  FILLER              PIC X(16)       VALUE 'SPAR-WDK611'.             
009400*01  WLARTC11   -COPY WDK611 -PRE SPAR-.                                  
009500     EJECT                                                                
009600*                    *** TP-AREOR                                         
009700*01  MID  -COPY W4I10101 -PRE MID-.                                       
009800     EJECT                                                                
009900*01      -COPY  WMSGAREA                                                  
010000     EJECT                                                                
010100*    03  MOD    -COPY W4O10101 -PRE MOD- -RED MSG-AREA.                   
010200     EJECT                                                                
010300*01  -COPY WMFSAREA.                                                      
010400     EJECT                                                                
010500*            ARBETSAREOR TILL IMS-SEKTIONERNA                             
010600*                                                                         
010700 01  IMS-WS.                                                              
010800     03  FILLER          PIC X(8)    VALUE 'IMS-WS  '.                    
010900     SKIP3                                                                
011000*            *** STATUSKOD FRÅN IMS ***                                   
011100     03  STATUS-WS       PIC XX.                                          
011200         88  SEGMENT-FINNS           VALUE '  '.                          
011300         88  SEGMENT-SAKNAS          VALUE 'GE'.                          
011400         88  BASEN-SLUT              VALUE 'GB'.                          
011500     SKIP3                                                                
011600     03  GODK-STATUSKODER.                                                
011700         05  GODK-STATUS   PIC XX  OCCURS 5  INDEXED BY STATUS-IX.        
011800     SKIP3                                                                
011900     03  SSA1              PIC X(64).                                     
012000     03  SSA2              PIC X(96).                                     
012100     EJECT                                                                
012200*            *** IMS-FUNKTIONSKODER ***                                   
012300*    03      -COPY   W0003                                                
012400     EJECT                                                                
012500*            *** DLI INPUT-OUTPUT AREA ***                                
012600 01  FILLER              PIC X(16)       VALUE 'DLI-IO-AREA'.             
012700     SKIP3                                                                
012800 01  DLI-IO-AREA         PIC X(900)      VALUE SPACE.                     
012900     SKIP3                                                                
013000*01  WLARTC01   -COPY WDK601              -RED DLI-IO-AREA.               
013100     EJECT                                                                
013200*01  WLARTC11   -COPY WDK611              -RED DLI-IO-AREA.               
013300     EJECT                                                                
013400*01  WLARTS01   -COPY WDK701              -RED DLI-IO-AREA.               
013500     EJECT                                                                
013600*01  WLARTS11   -COPY WDK711              -RED DLI-IO-AREA.               
013700     EJECT                                                                
013800*01  W6KVAH01   -COPY W6D201              -RED DLI-IO-AREA.               
013900     EJECT                                                                
014000*01  W6KVAH11   -COPY W6D211              -RED DLI-IO-AREA.               
014100     EJECT                                                                
014200*01  TEXT-AREA  -COPY WDD311              -RED DLI-IO-AREA.               
014300     EJECT                                                                
014400 LINKAGE SECTION.                                                         
014500*01         -COPY W0009     -PRE MSG-                                     
014600     EJECT                                                                
014700*01         -COPY W0008     -PRE USEA-                                    
014800         05  FILLER      PIC X.                                           
014900     EJECT                                                                
015000*01         -COPY W0008     -PRE ARTC-                                    
015100         05  FILLER      PIC X.                                           
015200     EJECT                                                                
015300*01         -COPY W0008     -PRE ARTS-                                    
015400         05  FILLER      PIC X.                                           
015500     EJECT                                                                
015600*01         -COPY W0008     -PRE BENA-                                    
015700         05  FILLER      PIC X.                                           
015800     EJECT                                                                
015900*01         -COPY W0008     -PRE KVAH-                                    
016000         05  FILLER      PIC X.                                           
016100     EJECT                                                                
016200 PROCEDURE DIVISION USING  MSG-PCB   USEA-PCB ARTC-PCB   ARTS-PCB         
016300                           BENA-PCB  KVAH-PCB.                            
016400     ENTRY 'DLITCBL' USING MSG-PCB   USEA-PCB ARTC-PCB   ARTS-PCB         
016500                           BENA-PCB  KVAH-PCB.                            
016600                                                                          
016700     PERFORM IMS-GET-MSG                                                  
016800     IF SEGMENT-FINNS                                                     
016900       PERFORM A-KOLLA-NYCKLAR                                            
017000       IF NYCKLAR-OK                                                      
017100         PERFORM IMS-GET-WLARTC01-ARTIKEL                                 
017200         IF SEGMENT-FINNS                                                 
017300           PERFORM B-LAES-BEARB-REDIGERA-ARTREG                           
017400           PERFORM C-KOLLA-OM-KVAL-INFO-FINNS                             
017500           PERFORM D-LAES-BENA-TEXT                                       
017600         ELSE                                                             
017700           MOVE FEL-2 TO MOD-TEMFSFEL                                     
017800         END-IF                                                           
017900       ELSE                                                               
018000         PERFORM E-RENSA-NYCKLAR                                          
018100       END-IF                                                             
018200       MOVE MAX-MOD-LENGD TO MSG-KVLL                                     
018300       PERFORM IMS-ISRT-MSG                                               
018400     END-IF                                                               
018500     MOVE ZERO TO RETURN-CODE                                             
018600     GOBACK                                                               
018700     .                                                                    
018800     EJECT                                                                
018900 A-KOLLA-NYCKLAR SECTION.                                                 
019000                                                                          
019100     MOVE JA TO NYCKEL-SW                                                 
019200     IF MSG-DUBBLA-TRANSKODER                                             
019300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I10101                 
019400       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
019500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
019600       MOVE ZERO TO MID-VAGNSKIP                                          
019700     ELSE                                                                 
019800       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I10101                   
019900       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
020000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
020100     END-IF                                                               
020200                                                                          
020300     COMPUTE MAX-MOD-LENGD = LENGTH OF MOD-W4O10101 + 4                   
020400                                                                          
020500     MOVE MFS-IDTRANS      TO WS-IDTRANS                                  
020600     IF MFS-IDTRANS NOT = '4101'                                          
020700         MOVE ZERO TO MID-VAGNSKIP                                        
020800     END-IF                                                               
020900                                                                          
021000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
021100     MOVE '001'             TO MSGI-KDCALL                                
021200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
021300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
021400     MOVE '4101'            TO MSGI-IDTRANS                               
021500                                                                          
021600     IF MFS-IDTRANS = '4101'                                              
021700     OR (MID-IDARTNR1 NUMERIC                                             
021800     AND MID-IDARTNR1 > ZERO)                                             
021900         MOVE MID-IDARTNR1   TO MSGI-IDARTNR                              
022000     END-IF                                                               
022100                                                                          
022200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
022300                                                                          
022400     IF MSGI-IDLAND-SPR = 'GB'                                            
022500       MOVE 'GB ' TO W-IDSKYLT                                            
022600     ELSE                                                                 
022700       MOVE 'S  ' TO W-IDSKYLT                                            
022800     END-IF                                                               
022900                                                                          
023000     MOVE LOW-VALUE TO MOD-W4O10101                                       
023100     MOVE 'W4O101N1' TO MFS-IDMOD                                         
023200     MOVE '4101' TO MOD-IDTRANS                                           
023300     MOVE ZERO TO MOD-VAGNSKIP                                            
023400       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                             
023500     MOVE MSGI-IDARTNR TO MOD-IDARTNR-UT                                  
023600     MOVE '-' TO MOD-STRECK                                               
023700     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
023800     IF MSGI-IDARTNR NOT NUMERIC                                          
023900       MOVE FEL-1 TO MOD-TEMFSFEL                                         
024000       MOVE NEJ TO NYCKEL-SW                                              
024100     ELSE                                                                 
024200       MOVE MFS-BLANKA-UT-FAELT TO MOD-TEMFSFEL                           
024300       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
024400     END-IF                                                               
024401                                                                          
024410     ACCEPT DAGENS-DATUM    FROM DATE                                     
024500     .                                                                    
024600     EJECT                                                                
024700 B-LAES-BEARB-REDIGERA-ARTREG SECTION.                                    
024800                                                                          
024900*ARTREG CDC-WDK6                                                          
025000*                                                                         
025100     PERFORM BA-BEARB-REDIGERA-WLARTC01                                   
025200     IF ART-KDERS-UTG > ZERO                                              
025300       MOVE FEL-3      TO MOD-TEMFSFEL                                    
025400     ELSE                                                                 
025500       PERFORM IMS-GNP-WLARTC11-CLAG                                      
025600       PERFORM BB-BEARB-REDIGERA-WLARTC11                                 
025700                                                                          
025800*ARTREG   SCD-WDK7                                                        
025900*                                                                         
026000       PERFORM IMS-GET-WLARTS01-ARTIKEL                                   
026100                                                                          
026200       IF SEGMENT-FINNS                                                   
026300         PERFORM IMS-GNP-WLARTS11-SLAG                                    
026400         PERFORM BC-FLYTTA-SPAR-AREA                                      
026500                                                                          
026600         PERFORM UNTIL SEGMENT-SAKNAS                                     
026700           PERFORM BD-BEARB-REDIGERA-WLARTS11                             
026800           PERFORM IMS-GNP-WLARTS11-SLAG                                  
026900         END-PERFORM                                                      
027000                                                                          
027100         PERFORM BE-FLYTTA-TILL-MOD                                       
027200       END-IF                                                             
027300     END-IF                                                               
027400     .                                                                    
027500     EJECT                                                                
027600 BA-BEARB-REDIGERA-WLARTC01 SECTION.                                      
027700                                                                          
027800     MOVE ART-REKSIFFR TO MOD-REKSIFFR                                    
027900     MOVE ART-IDLEVNR  TO MOD-IDLEVNR                                     
028000     .                                                                    
028100     EJECT                                                                
028200 BB-BEARB-REDIGERA-WLARTC11 SECTION.                                      
028300                                                                          
028400     IF SEGMENT-FINNS                                                     
028500       MOVE CLAG-ADLAGOMR      TO MOD-ADLAGOMR                            
028600       MOVE CLAG-ADGANG        TO MOD-ADGANG                              
028700       MOVE CLAG-ADPLATS       TO MOD-ADPLATS                             
028800       IF CLAG-ADLAGOMR-SVS   > ZERO OR                                   
028900          CLAG-ADLAGOMR-CD(1) > ZERO                                      
029000         MOVE JA               TO MOD-FLERPL                              
029100       ELSE                                                               
029200         MOVE NEJ              TO MOD-FLERPL                              
029300       END-IF                                                             
029400       MOVE CLAG-KVLS          TO MOD-KVLS (IX-CDC)                       
029500                                                                          
029600       COMPUTE MOD-KVAKS(IX-CDC)  = CLAG-KVAKS-CDC +                      
029700                                    CLAG-KVAKS-PAV +                      
029800                                    CLAG-KVAKS-T                          
029900                                                                          
030000       MOVE CLAG-KVAKS-PAV     TO MOD-KVAKS-PAV(IX-CDC)                   
030100       MOVE CLAG-KVEFRS        TO MOD-KVEFRS(IX-CDC)                      
030200       MOVE CLAG-VKART         TO MOD-VKART                               
030300       MOVE CLAG-VLARTNTO      TO MOD-VLARTNTO                            
030400       MOVE CLAG-KDVSOP        TO MOD-KDVSOP                              
030500       MOVE CLAG-IDANSK        TO MOD-IDANSK                              
030600       MOVE CLAG-KDARTURS      TO MOD-KDARTURS (IX-CDC)                   
030700                                  SPAR-CLAG-KDARTURS                      
030710       PERFORM S20-HAMTA-FLPCOO                                           
030900       MOVE CLAG-KDARTHNT      TO MOD-KDARTHNT                            
031000       MOVE CLAG-KDGK          TO MOD-KDGK                                
031100       MOVE CLAG-BEFT          TO MOD-BEFT (IX-CDC)                       
031200                                  SPAR-CLAG-BEFT                          
031300       MOVE CLAG-KDFORP        TO MOD-KDFORP (IX-CDC)                     
031400                                  SPAR-CLAG-KDFORP                        
031500       MOVE CLAG-KVMP          TO MOD-KVMP                                
031600       MOVE CLAG-KDERS         TO MOD-KDERS                               
031700                                                                          
031800       COMPUTE MOD-KVPB-TOT (IX-CDC) = CLAG-KVPB-SEP  +                   
031900                                       CLAG-KVPB-SATS +                   
032000                                       CLAG-KVPB-TPO                      
032100       MOVE CLAG-KVPB-SATS     TO MOD-KVPB-SATS                           
032200       MOVE CLAG-IDARTNR-EMBQ0 TO MOD-IDARTNR-EMBQ0 (IX-CDC)              
032300                                  SPAR-CLAG-IDARTNR-EMBQ0                 
032400       MOVE CLAG-KDEMBKOD-0    TO MOD-KDEMBKOD-0 (IX-CDC)                 
032500                                  SPAR-CLAG-KDEMBKOD-0                    
032600       MOVE CLAG-KVQPACK-0     TO MOD-KVQPACK-0                           
032700       MOVE CLAG-IDARTNR-EMBQ1 TO MOD-IDARTNR-EMBQ1 (IX-CDC)              
032800                                  SPAR-CLAG-IDARTNR-EMBQ1                 
032900       MOVE CLAG-KDEMBKOD-1    TO MOD-KDEMBKOD-1 (IX-CDC)                 
033000                                  SPAR-CLAG-KDEMBKOD-1                    
033100       MOVE CLAG-KVQPACK-1     TO MOD-KVQPACK-1                           
033200       MOVE CLAG-IDARTNR-EMBQ2 TO MOD-IDARTNR-EMBQ2 (IX-CDC)              
033300                                  SPAR-CLAG-IDARTNR-EMBQ2                 
033400       MOVE CLAG-KDEMBKOD-2    TO MOD-KDEMBKOD-2 (IX-CDC)                 
033500                                  SPAR-CLAG-KDEMBKOD-2                    
033600       MOVE CLAG-KVQPACK-2     TO MOD-KVQPACK-2                           
033700       MOVE CLAG-IDARTNR-EMBQ3 TO MOD-IDARTNR-EMBQ3 (IX-CDC)              
033800                                  SPAR-CLAG-IDARTNR-EMBQ3                 
033900       MOVE CLAG-KVQPACK-3     TO MOD-KVQPACK-3                           
034000       MOVE CLAG-IDARTNR-EMBQ4 TO MOD-IDARTNR-EMBQ4 (IX-CDC)              
034100                                  SPAR-CLAG-IDARTNR-EMBQ4                 
034200       MOVE CLAG-KVQPACK-4     TO MOD-KVQPACK-4                           
034300       MOVE +1                 TO  IX                                     
034400       MOVE +1                 TO  IND                                    
034500                                                                          
034600       PERFORM UNTIL IND > +3                                             
034700          MOVE CLAG-IDKAT (IND) TO MOD-IDKAT (IX)                         
034800          ADD +1               TO IX                                      
034900          ADD +1               TO IND                                     
035000       END-PERFORM                                                        
035100     ELSE                                                                 
035200       MOVE MFS-RENSA-FAELT    TO MOD-ADLAGOMR                            
035300                                  MOD-ADGANG                              
035400                                  MOD-ADPLATS                             
035500                                  MOD-FLERPL                              
035600                                  MOD-KVLS (IX-CDC)                       
035700                                  MOD-KVAKS(IX-CDC)                       
035800                                  MOD-KVAKS-PAV(IX-CDC)                   
035900                                  MOD-KVEFRS(IX-CDC)                      
036000                                  MOD-VKART                               
036100                                  MOD-VLARTNTO                            
036200                                  MOD-KDVSOP                              
036300                                  MOD-IDANSK                              
036400                                  MOD-KDARTURS (IX-CDC)                   
036410                                  MOD-FLPCOO                              
036500                                  MOD-KDARTHNT                            
036600                                  MOD-KDGK                                
036700                                  MOD-BEFT (IX-CDC)                       
036800                                  MOD-KDFORP (IX-CDC)                     
036900                                  MOD-KVMP                                
037000                                  MOD-KDERS                               
037100                                  MOD-KVPB-TOT(IX-CDC)                    
037200                                  MOD-KVPB-SATS                           
037300                                  MOD-IDARTNR-EMBQ0 (IX-CDC)              
037400                                  MOD-KDEMBKOD-0 (IX-CDC)                 
037500                                  MOD-KVQPACK-0                           
037600                                  MOD-IDARTNR-EMBQ1 (IX-CDC)              
037700                                  MOD-KDEMBKOD-1 (IX-CDC)                 
037800                                  MOD-KVQPACK-1                           
037900                                  MOD-IDARTNR-EMBQ2 (IX-CDC)              
038000                                  MOD-KDEMBKOD-2 (IX-CDC)                 
038100                                  MOD-KVQPACK-2                           
038200                                  MOD-IDARTNR-EMBQ3 (IX-CDC)              
038300                                  MOD-KVQPACK-3                           
038400                                  MOD-IDARTNR-EMBQ4 (IX-CDC)              
038500                                  MOD-KVQPACK-4                           
038600       MOVE +0                 TO SPAR-CLAG-IDARTNR-EMBQ4                 
038700                                  SPAR-CLAG-IDARTNR-EMBQ3                 
038800                                  SPAR-CLAG-KDEMBKOD-2                    
038900                                  SPAR-CLAG-IDARTNR-EMBQ2                 
039000                                  SPAR-CLAG-KDEMBKOD-1                    
039100                                  SPAR-CLAG-IDARTNR-EMBQ1                 
039200                                  SPAR-CLAG-KDEMBKOD-0                    
039300                                  SPAR-CLAG-IDARTNR-EMBQ0                 
039400                                  SPAR-CLAG-KDFORP                        
039500                                  SPAR-CLAG-BEFT                          
039600                                  SPAR-CLAG-KDARTURS                      
039700                                                                          
039800       MOVE   +1               TO IX                                      
039900       MOVE   +1               TO IND                                     
040000       PERFORM UNTIL IND > +3                                             
040100          MOVE MFS-RENSA-FAELT TO MOD-IDKAT (IX)                          
040200          ADD +1               TO IX                                      
040300          ADD +1               TO IND                                     
040400       END-PERFORM                                                        
040500     END-IF                                                               
040600     .                                                                    
040700     EJECT                                                                
040800 BC-FLYTTA-SPAR-AREA SECTION.                                             
040900                                                                          
041000     MOVE SPAR-CLAG-KDARTURS      TO MOD-KDARTURS (IX-SDC)                
041100     MOVE SPAR-CLAG-BEFT          TO MOD-BEFT (IX-SDC)                    
041200     MOVE SPAR-CLAG-KDFORP        TO MOD-KDFORP (IX-SDC)                  
041300     MOVE SPAR-CLAG-IDARTNR-EMBQ0 TO MOD-IDARTNR-EMBQ0 (IX-SDC)           
041400     MOVE SPAR-CLAG-KDEMBKOD-0    TO MOD-KDEMBKOD-0 (IX-SDC)              
041500     MOVE SPAR-CLAG-IDARTNR-EMBQ1 TO MOD-IDARTNR-EMBQ1 (IX-SDC)           
041600     MOVE SPAR-CLAG-KDEMBKOD-1    TO MOD-KDEMBKOD-1 (IX-SDC)              
041700     MOVE SPAR-CLAG-IDARTNR-EMBQ2 TO MOD-IDARTNR-EMBQ2 (IX-SDC)           
041800     MOVE SPAR-CLAG-KDEMBKOD-2    TO MOD-KDEMBKOD-2 (IX-SDC)              
041900     MOVE SPAR-CLAG-IDARTNR-EMBQ3 TO MOD-IDARTNR-EMBQ3 (IX-SDC)           
042000     MOVE SPAR-CLAG-IDARTNR-EMBQ4 TO MOD-IDARTNR-EMBQ4 (IX-SDC)           
042100     .                                                                    
042200     EJECT                                                                
042300 BD-BEARB-REDIGERA-WLARTS11 SECTION.                                      
042400                                                                          
042500     COMPUTE WS-KVLS      = WS-KVLS         +                             
042600                            SLAG-KVLS                                     
042700                                                                          
042800     COMPUTE WS-KVAKS     = WS-KVAKS        +                             
042900                            SLAG-KVAKS-SDC  +                             
043000                            SLAG-KVAKS-PAV                                
043100                                                                          
043200     COMPUTE WS-KVAKS-PAV = WS-KVAKS-PAV    +                             
043300                            SLAG-KVAKS-PAV                                
043400                                                                          
043500     COMPUTE WS-KVEFRS    = WS-KVEFRS       +                             
043600                            SLAG-KVEFRS                                   
043700                                                                          
043800     COMPUTE WS-KVPB-TOT  = WS-KVPB-TOT     +                             
043900                            SLAG-KVPB-REF                                 
044000     .                                                                    
044100     EJECT                                                                
044200 BE-FLYTTA-TILL-MOD SECTION.                                              
044300                                                                          
044400     MOVE WS-KVLS            TO MOD-KVLS(IX-SDC)                          
044500                                                                          
044600     MOVE WS-KVAKS           TO MOD-KVAKS(IX-SDC)                         
044700                                                                          
044800     MOVE WS-KVAKS-PAV       TO MOD-KVAKS-PAV(IX-SDC)                     
044900                                                                          
045000     MOVE WS-KVEFRS          TO MOD-KVEFRS(IX-SDC)                        
045100                                                                          
045200     MOVE WS-KVPB-TOT        TO MOD-KVPB-TOT(IX-SDC)                      
045300     .                                                                    
045400     EJECT                                                                
045500 C-KOLLA-OM-KVAL-INFO-FINNS SECTION.                                      
045600                                                                          
045700     MOVE LOW-VALUE  TO W-W6D211KY-MIN-X                                  
045800     MOVE HIGH-VALUE TO W-W6D211KY-MAX-X                                  
045900                                                                          
046000     PERFORM IMS-GET-W6KVAH11                                             
046100                                                                          
046200     IF SEGMENT-FINNS                                                     
046300       IF ENGLISH-TEXT                                                    
046400         MOVE YES TO MOD-FLAGGA-KVAL-INFO                                 
046500       ELSE                                                               
046600         MOVE JA  TO MOD-FLAGGA-KVAL-INFO                                 
046700       END-IF                                                             
046800     ELSE                                                                 
046900       MOVE NEJ   TO MOD-FLAGGA-KVAL-INFO                                 
047000     END-IF                                                               
047100     .                                                                    
047200     EJECT                                                                
047300 D-LAES-BENA-TEXT SECTION.                                                
047400                                                                          
047500     PERFORM IMS-GET-BENA-TEXT                                            
047600                                                                          
047700     IF SEGMENT-FINNS                                                     
047800       MOVE TEXT-BEART  TO MOD-BEART-SVE                                  
047900     END-IF                                                               
048000     .                                                                    
048100     EJECT                                                                
048200 E-RENSA-NYCKLAR SECTION.                                                 
048300                                                                          
048400     MOVE MFS-RENSA-FAELT           TO  MOD-IDARTNR-UT                    
048500     .                                                                    
048600     EJECT                                                                
048601                                                                          
048602 S20-HAMTA-FLPCOO          SECTION.                                       
048607     IF CLAG-KDPCOO > ' '                                                 
048608       IF DAGENS-DATUM > CLAG-TIGILTIG-PCOO                               
048610         MOVE JA  TO MOD-FLPCOO                                           
048614       ELSE                                                               
048615         MOVE NEJ TO MOD-FLPCOO                                           
048616       END-IF                                                             
048617     ELSE                                                                 
048618       MOVE JA    TO MOD-FLPCOO                                           
048659     END-IF                                                               
048660     IF MOD-FLPCOO = 'J'                                                  
048661       MOVE '*' TO MOD-FLPCOO                                             
048662     ELSE                                                                 
048663       MOVE ' ' TO MOD-FLPCOO                                             
048664     END-IF                                                               
048667     .                                                                    
048668     EJECT                                                                
048670                                                                          
048700 IMS-GET-MSG SECTION.                                                     
048900     MOVE '  QC' TO GODK-STATUSKODER                                      
049000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
049100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
049200     PERFORM IMS-STATUS-KONTROLL                                          
049300     .                                                                    
049400     SKIP3                                                                
049500 IMS-ISRT-MSG SECTION.                                                    
049600                                                                          
049700     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
049800       MOVE '0' TO MFS-KDHUVOMR                                           
049900     END-IF                                                               
050000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
050100     MOVE SPACE TO GODK-STATUSKODER                                       
050200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
050300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
050400     PERFORM IMS-STATUS-KONTROLL                                          
050500     .                                                                    
050600     EJECT                                                                
050700 IMS-GET-WLARTC01-ARTIKEL SECTION.                                        
050800                                                                          
050900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
051000            DELIMITED BY SIZE INTO SSA1                                   
051100     MOVE '  GE' TO GODK-STATUSKODER                                      
051200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
051300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
051400     PERFORM IMS-STATUS-KONTROLL                                          
051500     .                                                                    
051600     SKIP3                                                                
051700 IMS-GNP-WLARTC11-CLAG SECTION.                                           
051800                                                                          
051900     MOVE 'WLARTC11' TO SSA1                                              
052000     MOVE '  GE' TO GODK-STATUSKODER                                      
052100     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
052200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
052300     PERFORM IMS-STATUS-KONTROLL                                          
052400     .                                                                    
052500     EJECT                                                                
052600 IMS-GET-WLARTS01-ARTIKEL SECTION.                                        
052700                                                                          
052800     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
052900            DELIMITED BY SIZE INTO SSA1                                   
053000     MOVE '  GE' TO GODK-STATUSKODER                                      
053100     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA SSA1                      
053200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
053300     PERFORM IMS-STATUS-KONTROLL                                          
053400     .                                                                    
053500     SKIP3                                                                
053600 IMS-GNP-WLARTS11-SLAG SECTION.                                           
053700                                                                          
053800     MOVE 'WLARTS11' TO SSA1                                              
053900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
054000     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-AREA SSA1                     
054100     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
054200     PERFORM IMS-STATUS-KONTROLL                                          
054300     .                                                                    
054400     EJECT                                                                
054500 IMS-GET-BENA-TEXT SECTION.                                               
054600                                                                          
054700     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
054800            DELIMITED BY SIZE INTO SSA1                                   
054900     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
055000            DELIMITED BY SIZE INTO SSA2                                   
055100     MOVE '  GE' TO GODK-STATUSKODER                                      
055200     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
055300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
055400     PERFORM IMS-STATUS-KONTROLL                                          
055500     .                                                                    
055600     SKIP3                                                                
055700 IMS-GET-W6KVAH11 SECTION.                                                
055800                                                                          
055900     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X      ')'                    
056000             DELIMITED BY SIZE INTO SSA1                                  
056100     STRING 'W6KVAH11(W6D211KY>=' W-W6D211KY-MIN-X                        
056200                     '&W6D211KY<=' W-W6D211KY-MAX-X                       
056300                     '&KDKVAINF =' K-KDKVAINF-R     ')'                   
056400             DELIMITED BY SIZE INTO SSA2                                  
056500     MOVE '  GEGB' TO    GODK-STATUSKODER                                 
056600     CALL CBLTDLI USING GU KVAH-PCB DLI-IO-AREA SSA1 SSA2                 
056700     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
056800     PERFORM IMS-STATUS-KONTROLL                                          
056900     .                                                                    
057000     EJECT                                                                
057100 IMS-STATUS-KONTROLL SECTION.                                             
057200                                                                          
057300     SET STATUS-IX TO 1                                                   
057400     SEARCH GODK-STATUS                                                   
057500         AT END  CALL FELLOG                                              
057600         WHEN GODK-STATUS (STATUS-IX) = STATUS-WS   CONTINUE              
057700     END-SEARCH                                                           
057800     .                                                                    
