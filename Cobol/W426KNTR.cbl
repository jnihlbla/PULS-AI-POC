000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W426KNTR.                                                
000500*AUTHOR.         INGER NILSSON.                                           
000600*DATE-WRITTEN.   92/08/24.                                                
000700*                                                                         
000800*    REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        URVAL FÖR ANTAL- OCH KVALITETSKONTROLL                           
001200*        I MOTTAGNINGSKONTROLLEN.                                         
001300*                                                                         
001400*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001500*        PROGRAMMET UPPDATERA  W6KVAH (W6D2)                              
001600*        PROGRAMMET UPPDATERA  W6KVAE (W6H7)                              
001700*        PROGRAMMET UPPDATERA  W6LEVA (W6L1)                              
001800*        PROGRAMMET LÄSER      W6UPFA (W6F1)                              
001900*        PROGRAMMET LÄSER      W6PROA (W6F1)                              
002000*        PROGRAMMET LÄSER      WLXXLA (WDR1)                              
002100*        PROGRAMMET LÄSER      W6KODA (W6G2)                              
002200*                                                                         
002300*    ABENDKODER:                                                          
002400*        U0016 -  . . . .                                                 
002500*        U1000 -  . . . .                                                 
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700*    -- CHECKED BY WY2000                                                 
003800*                                                                         
003900     SKIP2                                                                
004000 77  IDPGM                       PIC X(8)    VALUE 'W426KNTR'.            
004100 77  W-SKIPLOT-KONTROLL-PRI      PIC X.                                   
004200 77  W-SKIPLOT-KONTROLL-SEK      PIC X.                                   
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  W-KVPROVPL-PRI-FINNS        PIC X       VALUE 'J'.                   
004600 77  W-KVPROVPL-SEK-FINNS        PIC X       VALUE 'J'.                   
004700 77  W-KVANT-SAKNAS              PIC X.                                   
004800 77  W-KVKVAPRIM                 PIC S9(07)  COMP-3.                      
004900 77  W-KVKVASEK                  PIC S9(07)  COMP-3.                      
005000 77  W-SPAR-KVKVAPRIM            PIC S9(07)  COMP-3.                      
005100 77  W-SPAR-KVKVASEK             PIC S9(07)  COMP-3.                      
005200 77  W-KDKVAANT                  PIC X.                                   
005300 77  W-GODK-FLKVARED             PIC X.                                   
005400 77  W-ARTIKEL-KONTROLL          PIC X.                                   
005500 77  W-SKIPLOT-KONTROLL          PIC X.                                   
005600 77  W-ANTALS-KONTROLL           PIC X.                                   
005700 77  W-KDKVASTA-ANT              PIC X.                                   
005800 77  W-KDKVASTA-PRI              PIC X.                                   
005900 77  W-KDKVASTA-SEK              PIC X.                                   
006000 77  W-FLKVAKAR                  PIC X.                                   
006100 77  W-ANTAL-DAGAR               PIC S9(3)   COMP-3.                      
006200 77  W-KDERS                     PIC S9(3)   COMP-3.                      
006300 77  W-SENASTE-INLEV-ETT-AAR     PIC X(1).                                
006400 77  W-FLSKPSAK                  PIC X(1).                                
006500 77  IX1                         PIC S9(3)   COMP SYNC.                   
006600 01  W-TIUPPDAT                  PIC 9(6).                                
006700 01  FILLER REDEFINES W-TIUPPDAT.                                         
006800     03  W-TIUPPDAT-AAR          PIC 9(2).                                
006900     03  W-TIUPPDAT-MAANAD-DAG   PIC 9(4).                                
007000     EJECT                                                                
007100 01  MAX-TAL                     PIC S9(09) COMP VALUE +99.               
007200 01  SLUMP-TAL                   PIC S9(09) COMP.                         
007300*01  -COPY W6D201 -PRE WS-                                                
007400     EJECT                                                                
007500 01  DAGENS-DATUM                PIC 9(6).                                
007600 01  FILLER REDEFINES DAGENS-DATUM.                                       
007700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007800     03  DAGENS-DATUM-MAANAD-DAG PIC 9(4).                                
007900     EJECT                                                                
008000*      --- VALID IDDC CODES                                               
008100*                                                                         
008200*01    -COPY WWDC99                                                       
008300       EJECT                                                              
008400*                                                                         
008500*01    -COPY WWPRODSL                                                     
008600       EJECT                                                              
008700 01  DYNAMISKA-SUBPROGRAM.                                                
008800*                                                                         
008900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
009300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009400     03  WRANDOM                 PIC X(8)    VALUE 'WRANDOM '.            
009500     SKIP2                                                                
009600*    --- PARAMETRAR TILL ABEND                                            
009700                                                                          
009800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010000     SKIP2                                                                
010100 01  FELTEXT.                                                             
010200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010400     EJECT                                                                
010500*    --- PARAMETRAR TILL IDARTNR                                          
010600*                                                                         
010700 01  TEST-IDARTNR                PIC 9(9) COMP-3.                         
010800                                                                          
010900*01  FILLER -COPY WWBYT03 -RED TEST-IDARTNR                               
011000                                                                          
011100     EJECT                                                                
011200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011300*                                                                         
011400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011500     SKIP3                                                                
011600 01  NYCKLAR-TILL-DLI.                                                    
011700     03  W-IDARTNR-X.                                                     
011800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011900     03  W-W6GXKEY-6103-X.                                                
012000         05  FILLER              PIC  X(04)  VALUE '6103'.                
012100         05  FILLER              PIC  X(26)  VALUE LOW-VALUE.             
012200     03  W-W6GXKEY-6101-X.                                                
012300         05  FILLER              PIC  X(04)  VALUE '6101'.                
012400         05  FILLER              PIC  X(26)  VALUE LOW-VALUE.             
012500     03  W-W6GXKEY-6102-X.                                                
012600         05  W-IDPROVPL          PIC  X(01)  VALUE SPACE.                 
012700         05  W-KDPROVPL          PIC  X(01)  VALUE SPACE.                 
012800     03  W-WDGX-4825-KEY-X.                                               
012900         05  W-IDHTYP-4825       PIC  X(04)  VALUE '4825'.                
013000         05  W-IDSKYLT-4825      PIC  X(03)  VALUE 'S  '.                 
013100         05  W-FILLER            PIC  X(23)  VALUE LOW-VALUE.             
013200     03  W-WDGX-4826-KEY-X.                                               
013300         05  W-IDKRFEL           PIC  X(02)  VALUE SPACE.                 
013400         05  W-FILLER            PIC  X(08)  VALUE LOW-VALUE.             
013500     03  W-IDKVAINF-X.                                                    
013600         05  W-IDKVAINF          PIC  9(2)   VALUE ZERO.                  
013700     03  W-IDLOPNRM-X.                                                    
013800         05  W-IDLOPNRM          PIC S9(9)   VALUE ZERO COMP-3.           
013900     03  W-IDLEVNR-X.                                                     
014000         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
014100     03  W1-W6H7BSEQ-X.                                                   
014200         05  W1-IDARTNR          PIC S9(9)   VALUE ZERO COMP-3.           
014300         05  W1-DAREGDAT-9KOMPL                                           
014400                                 PIC  9(8)   VALUE ZERO.                  
014500         05  W1-IDLEVNR-X.                                                
014600           07 W1-IDLEVNR         PIC X(5)    VALUE SPACE.                 
014700         05  W1-KVKRKNTR-X.                                               
014800           07 W1-KVKRKNTR        PIC S9      VALUE ZERO COMP-3.           
014900     03  W2-W6H7BSEQ-X.                                                   
015000         05  W2-IDARTNR          PIC S9(9)   VALUE ZERO COMP-3.           
015100         05  W2-DAREGDAT-9KOMPL                                           
015200                                 PIC  9(8)   VALUE ZERO.                  
015300         05  W2-IDLEVNR-X.                                                
015400           07 W2-IDLEVNR         PIC X(5)    VALUE SPACE.                 
015500         05  W2-KVKRKNTR-X.                                               
015600           07 W2-KVKRKNTR        PIC S9      VALUE ZERO COMP-3.           
015700     SKIP2                                                                
015800*    --- STATUS-KOD FRÅN IMS                                              
015900 01  STATUS-WS                   PIC XX.                                  
016000     88  SEGMENT-FINNS                       VALUE '  '.                  
016100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
016400     SKIP2                                                                
016500 01  GODK-STATUSKODER.                                                    
016600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016700     SKIP3                                                                
016800 01  SSA1                        PIC X(128).                              
016900 01  SSA2                        PIC X(64).                               
017000 01  SSA3                        PIC X(64).                               
017100     EJECT                                                                
017200*    --- IMS FUNKTIONSKODER                                               
017300*01  -COPY W0003                                                          
017400     EJECT                                                                
017500*    ---  DLI INPUT-OUTPUT AREA                                           
017600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
017700     SKIP3                                                                
017800 01  DLI-IO-AREA1.                                                        
017900     03  IO-AREA1                PIC X(900)  VALUE SPACE.                 
018000     SKIP3                                                                
018100     03  WLARTC01 REDEFINES IO-AREA1.                                     
018200*        05  -COPY WDK601  -PRE ARTC-                                     
018300     EJECT                                                                
018400     03  WLARTC11 REDEFINES IO-AREA1.                                     
018500*        05  -COPY WDK611  -PRE ARTC-                                     
018600     EJECT                                                                
018700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
018800     SKIP3                                                                
018900 01  DLI-IO-AREA2.                                                        
019000     03  IO-AREA2                PIC X(1154)  VALUE SPACE.                
019100     SKIP3                                                                
019200     03  W6KVAH01 REDEFINES IO-AREA2.                                     
019300*        05  -COPY W6D201                                                 
019400     EJECT                                                                
019500     03  W6KVAH11 REDEFINES IO-AREA2.                                     
019600*        05  -COPY W6D211                                                 
019700     EJECT                                                                
019800     03  W6KVAH12 REDEFINES IO-AREA2.                                     
019900*        05  -COPY W6D212                                                 
020000     EJECT                                                                
020100     03  W6KVAH22 REDEFINES IO-AREA2.                                     
020200*        05  -COPY W6D222                                                 
020300     EJECT                                                                
020400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
020500     SKIP3                                                                
020600 01  DLI-IO-AREA3.                                                        
020700     03  IO-AREA3                PIC X(516)  VALUE SPACE.                 
020800     SKIP3                                                                
020900     03  W6KVAE01 REDEFINES IO-AREA3.                                     
021000*        05  -COPY W6H701                                                 
021100     EJECT                                                                
021200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA4'.        
021300     SKIP3                                                                
021400 01  DLI-IO-AREA4.                                                        
021500     03  IO-AREA4                PIC X(286) VALUE SPACE.                  
021600     SKIP3                                                                
021700     03  W6UPFA01 REDEFINES IO-AREA4.                                     
021800*        05  -COPY W6L101                                                 
021900     EJECT                                                                
022000     03  W6UPFA11 REDEFINES IO-AREA4.                                     
022100*        05  -COPY W6L111                                                 
022200     EJECT                                                                
022300     03  W6UPFA12 REDEFINES IO-AREA4.                                     
022400*        05  -COPY W6L112  -PRE L112-                                     
022500     EJECT                                                                
022600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA5'.        
022700     SKIP3                                                                
022800 01  DLI-IO-AREA5.                                                        
022900     03  IO-AREA5                PIC X(64)  VALUE SPACE.                  
023000     SKIP3                                                                
023100     03  W6GX140  REDEFINES IO-AREA5.                                     
023200*        05  -COPY W6GX6102                                               
023300     EJECT                                                                
023400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA6'.        
023500     SKIP3                                                                
023600 01  DLI-IO-AREA6.                                                        
023700     03  IO-AREA6                PIC X(300)  VALUE SPACE.                 
023800     SKIP3                                                                
023900     03  W6LEVA   REDEFINES IO-AREA6.                                     
024000*        05  -COPY W6F101  -PRE LEVA01-                                   
024100     EJECT                                                                
024200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA7'.        
024300     SKIP3                                                                
024400 01  DLI-IO-AREA7.                                                        
024500     03  IO-AREA7                PIC X(20)  VALUE SPACE.                  
024600     SKIP3                                                                
024700     03  W6G230   REDEFINES IO-AREA7.                                     
024800*        05  -COPY W6GX6104                                               
024900     EJECT                                                                
025000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA8'.        
025100     SKIP3                                                                
025200 01  DLI-IO-AREA8.                                                        
025300     03  IO-AREA8                PIC X(100) VALUE SPACE.                  
025400     SKIP3                                                                
025500     03  WLXXLA11 REDEFINES IO-AREA8.                                     
025600*        05  -COPY WDGX4826                                               
025700     EJECT                                                                
025800 LINKAGE SECTION.                                                         
025900                                                                          
026000     EJECT                                                                
026100*01  -COPY W426KNTR                                                       
026200     EJECT                                                                
026300*01  -COPY W0008  -PRE ARTC-                                              
026400     05  FILLER                  PIC X.                                   
026500     EJECT                                                                
026600*01  -COPY W0008  -PRE KVAH1-                                             
026700     05  FILLER                  PIC X.                                   
026800     EJECT                                                                
026900*01  -COPY W0008  -PRE KVAH2-                                             
027000     05  FILLER                  PIC X.                                   
027100     EJECT                                                                
027200*01  -COPY W0008  -PRE KVAE-                                              
027300     05  FILLER                  PIC X.                                   
027400     EJECT                                                                
027500*01  -COPY W0008  -PRE LEVA-                                              
027600     05  FILLER                  PIC X.                                   
027700     EJECT                                                                
027800*01  -COPY W0008  -PRE UPFA-                                              
027900     05  FILLER                  PIC X.                                   
028000     EJECT                                                                
028100*01  -COPY W0008  -PRE PROA-                                              
028200     05  FILLER                  PIC X.                                   
028300     EJECT                                                                
028400*01  -COPY W0008  -PRE XXLA-                                              
028500     05  FILLER                  PIC X.                                   
028600     EJECT                                                                
028700*01  -COPY W0008  -PRE KODA-                                              
028800     05  FILLER                  PIC X.                                   
028900     EJECT                                                                
029000 PROCEDURE DIVISION  USING KVAL-W426KNTR                                  
029100                           ARTC-PCB KVAH1-PCB KVAH2-PCB                   
029200                           KVAE-PCB LEVA-PCB  UPFA-PCB                    
029300                           PROA-PCB XXLA-PCB  KODA-PCB.                   
029400                                                                          
029500     PERFORM A-INIT                                                       
029600                                                                          
029700     MOVE KVAL-IDARTNR          TO W-IDARTNR                              
029800     MOVE KVAL-IDLEVNR          TO W-IDLEVNR                              
029900     MOVE KVAL-IDLOPNRM         TO W-IDLOPNRM                             
030000     MOVE KVAL-IDDC             TO WS-IDDC                                
030100                                                                          
030200     PERFORM S02-UPPDATERA-EV-W6KVAH01                                    
030300                                                                          
030400     PERFORM B-ARTIKEL-KONTROLL                                           
030500                                                                          
030600     IF W-ARTIKEL-KONTROLL = JA                                           
030700        PERFORM C-KONTROLLRAPPORT                                         
030800                                                                          
030900        PERFORM D-SPECIALKONTRLL                                          
031000                                                                          
031100        PERFORM IMS-GU-LEVA-01                                            
031200** FROM 981209 TAS SKIPLOT-SÄKRADE LEVERANTÖRER EJ HELLER UT TILL         
031300** ANTALSKONTROLL                                                         
031400        IF SEGMENT-FINNS                                                  
031500           IF LEVA01-LEV-FLSKPLOT = NEJ                                   
031600              MOVE NEJ          TO W-SKIPLOT-KONTROLL                     
031700              MOVE NEJ          TO W-ANTALS-KONTROLL                      
031800           END-IF                                                         
031900        END-IF                                                            
031901                                                                          
032100        IF W-SKIPLOT-KONTROLL = JA                                        
032200           PERFORM IMS-GU-KVAH-01                                         
032300           MOVE ART-W6D201   TO WS-ART-W6D201                             
032400           IF (WS-ART-IDPROVPL-PRI = '1' OR '2')                          
032500           OR (WS-ART-IDPROVPL-SEK = '1' OR '2')                          
032600              MOVE NEJ       TO W-GODK-FLKVARED                           
032700           END-IF                                                         
032800           IF WS-ART-IDPROVPL-PRI = '0'                                   
032900              IF WS-ART-IDPROVPL-SEK NOT = '0'                            
033000                 MOVE NEJ    TO W-GODK-FLKVARED                           
033100              END-IF                                                      
033200           END-IF                                                         
033300           PERFORM IMS-GHNP-KVAH-12                                       
033400           IF SEGMENT-SAKNAS                                              
033500              MOVE NEJ       TO W-GODK-FLKVARED                           
033600              PERFORM F-SKIPLOT-KONTROLL-LEV-SAKNAS                       
033700           ELSE                                                           
033800              IF LEV-FLUPG = NEJ                                          
033900                 MOVE JA     TO W-FLKVAKAR                                
034000              END-IF                                                      
034100                                                                          
034200              IF LEV-FLKVARED = JA                                        
034300              OR LEV-FLUPG    = NEJ                                       
034400                 MOVE NEJ    TO W-GODK-FLKVARED                           
034500              END-IF                                                      
034600                                                                          
034700              IF LEV-FLSKPSAK = NEJ                                       
034800                 IF LEV-KVSKPLOT-PRI > 1                                  
034900                 OR LEV-KVSKPLOT-SEK > 1                                  
035000                    MOVE NEJ TO W-GODK-FLKVARED                           
035100                 END-IF                                                   
035200              END-IF                                                      
035300              PERFORM G-SKIPLOT-KONTROLL-LEV-FINNS                        
035400           END-IF                                                         
035500        END-IF                                                            
035600        PERFORM H-SLUMP-ANTALSKONTROLL                                    
035700     END-IF                                                               
035800                                                                          
035900     PERFORM I-KVALITETSINFO-TILL-INLEV                                   
036000                                                                          
036100     GOBACK                                                               
036200     .                                                                    
036300     EJECT                                                                
036400 A-INIT SECTION.                                                          
036500     SKIP2                                                                
036600     ACCEPT DAGENS-DATUM FROM DATE                                        
036700     MOVE SPACE  TO WS-ART-W6D201                                         
036800     MOVE 'N'    TO W-KVANT-SAKNAS                                        
036900     MOVE +0     TO W-KVKVAPRIM                                           
037000     MOVE +0     TO W-KVKVASEK                                            
037100     MOVE +0     TO W-SPAR-KVKVAPRIM                                      
037200     MOVE +0     TO W-SPAR-KVKVASEK                                       
037300     MOVE '0'    TO W-KDKVAANT                                            
037400     MOVE 'J'    TO W-GODK-FLKVARED                                       
037500     MOVE 'J'    TO W-ARTIKEL-KONTROLL                                    
037600     MOVE 'J'    TO W-SKIPLOT-KONTROLL                                    
037700     MOVE 'J'    TO W-ANTALS-KONTROLL                                     
037800     MOVE 'N'    TO W-SKIPLOT-KONTROLL-PRI                                
037900     MOVE 'N'    TO W-SKIPLOT-KONTROLL-SEK                                
038000     MOVE '0'    TO W-KDKVASTA-ANT                                        
038100     MOVE '0'    TO W-KDKVASTA-PRI                                        
038200     MOVE '0'    TO W-KDKVASTA-SEK                                        
038300     MOVE 'N'    TO W-FLKVAKAR                                            
038400     MOVE +0     TO W-ANTAL-DAGAR                                         
038500     MOVE +0     TO W-KDERS                                               
038600     MOVE 'J'    TO W-SENASTE-INLEV-ETT-AAR                               
038700     .                                                                    
038800     EJECT                                                                
038900 B-ARTIKEL-KONTROLL  SECTION.                                             
039000     SKIP2                                                                
039100     PERFORM IMS-GET-ARTC-01                                              
039200     IF SEGMENT-FINNS                                                     
039300        MOVE KVAL-IDARTNR   TO TEST-IDARTNR                               
039400        IF BYT03-OBJEKT                                                   
039500           MOVE NEJ         TO W-ARTIKEL-KONTROLL                         
039600        END-IF                                                            
039700                                                                          
039800** UNDATAR BIMA FRÅN KONTROLLER                                           
039900        MOVE ARTC-ART-KDPRODSL TO TEST-KDPRODSL                           
040000        IF KDPRODSL-BIMA                                                  
040100           MOVE NEJ            TO W-ARTIKEL-KONTROLL                      
040200        END-IF                                                            
040300                                                                          
040400                                                                          
040500        IF ARTC-ART-IDFKNGRP > +1919 AND < 2000                           
040600** EMBALLAGE                                                              
040700           MOVE NEJ            TO W-SKIPLOT-KONTROLL                      
040800        END-IF                                                            
040900                                                                          
041000        PERFORM IMS-GNP-ARTC-11                                           
041100        IF SEGMENT-FINNS                                                  
041200           MOVE ARTC-CLAG-KDERS   TO W-KDERS                              
041300        END-IF                                                            
041400     END-IF                                                               
041500     .                                                                    
041600     EJECT                                                                
041700 C-KONTROLLRAPPORT SECTION.                                               
041800     SKIP2                                                                
041900     MOVE LOW-VALUE             TO W1-W6H7BSEQ-X                          
042000     MOVE HIGH-VALUE            TO W2-W6H7BSEQ-X                          
042100     MOVE KVAL-IDARTNR          TO W1-IDARTNR W2-IDARTNR                  
042200     MOVE KVAL-IDLEVNR          TO W1-IDLEVNR W2-IDLEVNR                  
042300     MOVE +1                    TO W1-KVKRKNTR                            
042400     MOVE +9                    TO W2-KVKRKNTR                            
042500     PERFORM IMS-GU-KVAE-01                                               
042600     PERFORM UNTIL SEGMENT-SAKNAS                                         
042700                                                                          
042800       MOVE NEJ                  TO W-GODK-FLKVARED                       
042900       IF KR-IDKRFEL = 'PA' OR 'PB'                                       
043000          MOVE '1'               TO W-KDKVAANT                            
043100          MOVE '1'               TO W-KDKVASTA-ANT                        
043200       ELSE                                                               
043300          MOVE JA                TO W-FLKVAKAR                            
043400          MOVE 3                 TO W-IDPROVPL                            
043500          MOVE 'R'               TO W-KDPROVPL                            
043600          PERFORM IMS-GU-PROA-6102                                        
043700          IF SEGMENT-FINNS                                                
043800             MOVE +0             TO IX1                                   
043900             PERFORM UNTIL IX1 = +5                                       
044000               ADD +1            TO IX1                                   
044100               IF KVAL-KVAVIS < 6102-KVAVIS-FOM (IX1) OR                  
044200                              > 6102-KVAVIS-TOM (IX1)                     
044300                  CONTINUE                                                
044400               ELSE                                                       
044500                  IF 6102-KVPROVPL (IX1) > W-KVKVAPRIM                    
044600                     MOVE 6102-KVPROVPL (IX1) TO W-KVKVAPRIM              
044700                  END-IF                                                  
044800                  MOVE +5        TO IX1                                   
044900               END-IF                                                     
045000             END-PERFORM                                                  
045100          END-IF                                                          
045200          IF KVAL-KVAVIS < W-KVKVAPRIM                                    
045300             MOVE KVAL-KVAVIS    TO W-KVKVAPRIM                           
045400          END-IF                                                          
045500          IF W-KVKVAPRIM > W-SPAR-KVKVAPRIM                               
045600             MOVE JA TO W-FLSKPSAK                                        
045700             PERFORM S01-UPPDATERA-W6L101                                 
045800          END-IF                                                          
045900          PERFORM CA-UPPDATERA-W6L111                                     
046000       END-IF                                                             
046100                                                                          
046200       PERFORM IMS-GN-KVAE-01                                             
046300     END-PERFORM                                                          
046400     .                                                                    
046500     EJECT                                                                
046600 CA-UPPDATERA-W6L111 SECTION.                                             
046700     SKIP2                                                                
046800     MOVE KR-IDKR                    TO RAPP-IDKR                         
046900     MOVE '1'                        TO RAPP-KDKVASTA-PRI                 
047000     MOVE KR-IDKRFEL                 TO W-IDKRFEL                         
047100     PERFORM IMS-GU-WLXXLA11                                              
047200     IF SEGMENT-FINNS                                                     
047300        MOVE 4826-BEKRFEL            TO RAPP-BEKRFEL (01)                 
047400     ELSE                                                                 
047500        MOVE SPACE                   TO RAPP-BEKRFEL (01)                 
047600     END-IF                                                               
047700                                                                          
047800     MOVE SPACE                      TO RAPP-BEKRFEL (02)                 
047900                                        RAPP-BEKRFEL (03)                 
048000                                        RAPP-TEKRFEL                      
048010                                                                          
048020     MOVE SPACE                      TO RAPP-IDUSER                       
048100                                                                          
048200     PERFORM IMS-ISRT-UPFA-11                                             
048300     .                                                                    
048400     EJECT                                                                
048500 D-SPECIALKONTRLL SECTION.                                                
048600     SKIP2                                                                
048700     PERFORM IMS-GU-KVAH-01                                               
048800     PERFORM IMS-GNP-KVAH-22                                              
048900     PERFORM UNTIL SEGMENT-SAKNAS                                         
049000        MOVE JA                TO W-FLKVAKAR                              
049100        MOVE NEJ               TO W-GODK-FLKVARED                         
049200        MOVE SPEC-IDKVAINF     TO W-IDKVAINF                              
049300        MOVE SPEC-IDPROVPL-PRI TO W-IDPROVPL                              
049400        MOVE 'R'               TO W-KDPROVPL                              
049500        PERFORM IMS-GU-PROA-6102                                          
049600        IF SEGMENT-FINNS                                                  
049700           MOVE +0             TO IX1                                     
049800           PERFORM UNTIL IX1 = +5                                         
049900             ADD +1            TO IX1                                     
050000             IF KVAL-KVAVIS < 6102-KVAVIS-FOM (IX1) OR                    
050100                            > 6102-KVAVIS-TOM (IX1)                       
050200                CONTINUE                                                  
050300             ELSE                                                         
050400                IF 6102-KVPROVPL (IX1) > W-KVKVAPRIM                      
050500                   MOVE 6102-KVPROVPL (IX1) TO W-KVKVAPRIM                
050600                END-IF                                                    
050700                MOVE +5        TO IX1                                     
050800             END-IF                                                       
050900           END-PERFORM                                                    
051000        END-IF                                                            
051100        IF KVAL-KVAVIS < W-KVKVAPRIM                                      
051200           MOVE KVAL-KVAVIS    TO W-KVKVAPRIM                             
051300        END-IF                                                            
051400        IF W-KVKVAPRIM > W-SPAR-KVKVAPRIM                                 
051500           MOVE JA TO W-FLSKPSAK                                          
051600           PERFORM S01-UPPDATERA-W6L101                                   
051700        END-IF                                                            
051800        PERFORM DA-UPPDATERA-W6L112                                       
051900        PERFORM IMS-GNP-KVAH-22                                           
052000     END-PERFORM                                                          
052100     .                                                                    
052200     EJECT                                                                
052300 DA-UPPDATERA-W6L112 SECTION.                                             
052400     SKIP2                                                                
052500     MOVE '1'                        TO L112-SPEC-KDKVASTA-PRI            
052600     MOVE W-IDKVAINF                 TO L112-SPEC-IDKVAINF                
052700     PERFORM IMS-GU-KVAH-11                                               
052800                                                                          
052900     MOVE INFO-TEKVAINF-EXT(1)(1:60) TO L112-SPEC-TEKVAINF(1)             
053000     MOVE INFO-TEKVAINF-EXT(1)(61:6) TO L112-SPEC-TEKVAINF(2)(1:6)        
053100     MOVE INFO-TEKVAINF-EXT(2)(1:54) TO                                   
053200                                     L112-SPEC-TEKVAINF(2)(7:54)          
053300     MOVE INFO-TEKVAINF-EXT(2)(55:25) TO                                  
053400                                     L112-SPEC-TEKVAINF(3)(1:25)          
053500     MOVE INFO-TEKVAINF-EXT(3)(1:35) TO                                   
053600                                     L112-SPEC-TEKVAINF(3)(26:35)         
053610     MOVE SPACE                      TO  L112-SPEC-IDUSER                 
053700                                                                          
053800     PERFORM IMS-ISRT-UPFA-12                                             
053900     .                                                                    
054000     EJECT                                                                
054100 F-SKIPLOT-KONTROLL-LEV-SAKNAS SECTION.                                   
054200     SKIP2                                                                
054300     IF  WS-ART-IDPROVPL-PRI = '0'                                        
054400     AND WS-ART-IDPROVPL-SEK = '0'                                        
054500        PERFORM IMS-GU-W6G2-6103                                          
054600        MOVE '1'                 TO 6104-KDSEGKEY                         
054700        MOVE KVAL-IDARTNR        TO 6104-IDARTNR                          
054800        PERFORM IMS-ISRT-W6G2-6104                                        
054900        MOVE 1                   TO WS-ART-KDKVATYP                       
055000        MOVE 3                   TO WS-ART-IDPROVPL-PRI                   
055100     END-IF                                                               
055200                                                                          
055300     IF WS-ART-IDPROVPL-PRI NOT = '0'                                     
055400        MOVE WS-ART-IDPROVPL-PRI TO W-IDPROVPL                            
055500     ELSE                                                                 
055600        MOVE '4' TO W-IDPROVPL                                            
055700     END-IF                                                               
055800     MOVE 'N'                 TO W-KDPROVPL                               
055900     PERFORM IMS-GU-PROA-6102                                             
056000     IF SEGMENT-FINNS                                                     
056100       MOVE +0                TO IX1                                      
056200       PERFORM UNTIL IX1 = +5                                             
056300          ADD +1              TO IX1                                      
056400          IF KVAL-KVAVIS < 6102-KVAVIS-FOM (IX1) OR                       
056500                         > 6102-KVAVIS-TOM (IX1)                          
056600             CONTINUE                                                     
056700          ELSE                                                            
056800             MOVE 6102-KVPROVPL (IX1) TO W-KVKVAPRIM                      
056900             MOVE JA          TO W-SKIPLOT-KONTROLL-PRI                   
057000             MOVE +5          TO IX1                                      
057100          END-IF                                                          
057200       END-PERFORM                                                        
057300     END-IF                                                               
057400     IF KVAL-KVAVIS < W-KVKVAPRIM                                         
057500        MOVE KVAL-KVAVIS         TO W-KVKVAPRIM                           
057600     END-IF                                                               
057700                                                                          
057800     IF CDC-SE                                                            
057900       IF WS-ART-IDPROVPL-SEK NOT = '0'                                   
058000          MOVE WS-ART-IDPROVPL-SEK TO W-IDPROVPL                          
058100          MOVE 'N'                 TO W-KDPROVPL                          
058200          PERFORM IMS-GU-PROA-6102                                        
058300          IF SEGMENT-FINNS                                                
058400            MOVE +0                TO IX1                                 
058500            PERFORM UNTIL IX1 = +5                                        
058600               ADD +1              TO IX1                                 
058700               IF KVAL-KVAVIS < 6102-KVAVIS-FOM (IX1) OR                  
058800                              > 6102-KVAVIS-TOM (IX1)                     
058900                  CONTINUE                                                
059000               ELSE                                                       
059100                  MOVE 6102-KVPROVPL (IX1) TO W-KVKVASEK                  
059200                  MOVE +5          TO IX1                                 
059300                  MOVE JA          TO W-SKIPLOT-KONTROLL-SEK              
059400               END-IF                                                     
059500            END-PERFORM                                                   
059600          END-IF                                                          
059700       END-IF                                                             
059800       IF KVAL-KVAVIS < W-KVKVASEK                                        
059900          MOVE KVAL-KVAVIS         TO W-KVKVASEK                          
060000       END-IF                                                             
060100     END-IF                                                               
060200                                                                          
060300     IF W-KVKVASEK  > +0                                                  
060400     OR W-KVKVAPRIM > +0                                                  
060500        MOVE NEJ TO W-FLSKPSAK                                            
060600        PERFORM S01-UPPDATERA-W6L101                                      
060700     END-IF                                                               
060800                                                                          
060900     MOVE KVAL-IDLEVNR           TO LEV-IDLEVNR                           
061000     MOVE NEJ                    TO LEV-FLKVARED                          
061100     MOVE NEJ                    TO LEV-FLKVASAK                          
061200     MOVE NEJ                    TO LEV-FLSKPSAK                          
061300     MOVE JA                     TO LEV-FLUPG                             
061400     MOVE '1'                    TO LEV-KDKVASAK                          
061500     MOVE '1'                    TO LEV-KDKVAUP                           
061600     MOVE +0                     TO LEV-KVSKPLOT-PRI                      
061700     IF WS-ART-IDPROVPL-PRI > ZERO                                        
061800        MOVE WS-ART-IDPROVPL-PRI TO W-IDPROVPL                            
061900        MOVE 'N'                 TO W-KDPROVPL                            
062000        PERFORM IMS-GU-PROA-6102                                          
062100        IF SEGMENT-FINNS                                                  
062200           MOVE 6102-KVSKPLOT    TO LEV-KVSKPLOT-PRI                      
062300        END-IF                                                            
062400     END-IF                                                               
062500     MOVE +0                     TO LEV-KVSKPLOT-SEK                      
062600     IF WS-ART-IDPROVPL-SEK > ZERO                                        
062700        MOVE WS-ART-IDPROVPL-SEK TO W-IDPROVPL                            
062800        MOVE 'N'                 TO W-KDPROVPL                            
062900        PERFORM IMS-GU-PROA-6102                                          
063000        IF SEGMENT-FINNS                                                  
063100           MOVE 6102-KVSKPLOT    TO LEV-KVSKPLOT-SEK                      
063200        END-IF                                                            
063300     END-IF                                                               
063400     MOVE DAGENS-DATUM           TO LEV-TIKVASAK                          
063500     MOVE DAGENS-DATUM           TO LEV-TIUPG                             
063600     MOVE DAGENS-DATUM           TO LEV-TIUPPDAT                          
063700     PERFORM IMS-ISRT-KVAH-12                                             
063800     .                                                                    
063900     EJECT                                                                
064000 G-SKIPLOT-KONTROLL-LEV-FINNS SECTION.                                    
064100     SKIP2                                                                
064200     MOVE LEV-TIUPPDAT           TO W-TIUPPDAT                            
064300     IF W-TIUPPDAT-AAR NOT = DAGENS-DATUM-AAR                             
064400        IF DAGENS-DATUM-MAANAD-DAG > W-TIUPPDAT-MAANAD-DAG                
064500           MOVE NEJ              TO W-SENASTE-INLEV-ETT-AAR               
064600           MOVE NEJ              TO W-GODK-FLKVARED                       
064700        END-IF                                                            
064800     END-IF                                                               
064900                                                                          
065000     IF ((WS-ART-KDKVATYP = '2' AND LEV-FLSKPSAK    = JA) OR              
065100         (LEV-FLKVASAK    = JA)                           OR              
065200         (KVAL-FLKVARED   = JA  AND W-GODK-FLKVARED = JA))                
065300*    AND W-SENASTE-INLEV-ETT-AAR = JA                                     
065400*                                                                         
065500*    ÄNDRAT MAJ-96 KVALITETSSÄKRADE ARTIKLAR MED INLEVERANS               
065600*    ÄLDRE ÄN ETT ÅR SKA INTE KOMMA IN PÅ KONTROLL                        
065700*                                                                         
065800         CONTINUE                                                         
065900     ELSE                                                                 
066000                                                                          
066100        IF LEV-FLUPG = NEJ                                                
066200        OR W-SENASTE-INLEV-ETT-AAR = NEJ                                  
066300           MOVE JA               TO W-SKIPLOT-KONTROLL-PRI                
066400           IF WS-ART-IDPROVPL-SEK NOT = '0'                               
066500              MOVE JA            TO W-SKIPLOT-KONTROLL-SEK                
066600           END-IF                                                         
066700        ELSE                                                              
066800           IF (LEV-FLSKPSAK = NEJ AND LEV-KVSKPLOT-PRI > +1)              
066900              MOVE JA            TO W-SKIPLOT-KONTROLL-PRI                
067000           END-IF                                                         
067100           IF (LEV-FLSKPSAK = NEJ AND LEV-KVSKPLOT-SEK > +1)              
067200           OR (LEV-FLSKPSAK = JA  AND LEV-KVSKPLOT-SEK = +1)              
067300              MOVE JA            TO W-SKIPLOT-KONTROLL-SEK                
067400           END-IF                                                         
067500        END-IF                                                            
067600                                                                          
067700        IF W-SKIPLOT-KONTROLL-PRI = JA                                    
067800                                                                          
067900          IF WS-ART-IDPROVPL-PRI > 0                                      
068000            MOVE WS-ART-IDPROVPL-PRI TO W-IDPROVPL                        
068100          ELSE                                                            
068200            MOVE '4' TO W-IDPROVPL                                        
068300          END-IF                                                          
068400          MOVE 'N'               TO W-KDPROVPL                            
068500          IF (LEV-FLSKPSAK     = JA)                                      
068600          OR (LEV-FLSKPSAK     = NEJ AND                                  
068700              LEV-KVSKPLOT-PRI = +2 OR +3 OR +4)                          
068800              MOVE 'R'           TO W-KDPROVPL                            
068900          END-IF                                                          
069000          MOVE NEJ               TO W-KVPROVPL-PRI-FINNS                  
069100          PERFORM IMS-GU-PROA-6102                                        
069200          IF SEGMENT-FINNS                                                
069300             MOVE +0             TO IX1                                   
069400             PERFORM UNTIL IX1 = +5                                       
069500               ADD +1            TO IX1                                   
069600               IF KVAL-KVAVIS < 6102-KVAVIS-FOM (IX1) OR                  
069700                              > 6102-KVAVIS-TOM (IX1)                     
069800                  CONTINUE                                                
069900               ELSE                                                       
070000                  MOVE JA        TO W-KVPROVPL-PRI-FINNS                  
070100                  MOVE 6102-KVPROVPL (IX1) TO W-KVKVAPRIM                 
070200                  MOVE +5        TO IX1                                   
070300               END-IF                                                     
070400             END-PERFORM                                                  
070500          END-IF                                                          
070600        END-IF                                                            
070700        IF KVAL-KVAVIS < W-KVKVAPRIM                                      
070800           MOVE KVAL-KVAVIS      TO W-KVKVAPRIM                           
070900        END-IF                                                            
071000                                                                          
071100        IF W-SKIPLOT-KONTROLL-SEK = JA AND CDC-SE                         
071200          MOVE WS-ART-IDPROVPL-SEK TO W-IDPROVPL                          
071300          MOVE 'N'                 TO W-KDPROVPL                          
071400          IF (LEV-FLSKPSAK     = JA)                                      
071500          OR (LEV-FLSKPSAK     = NEJ AND                                  
071600              LEV-KVSKPLOT-SEK = +2 OR +3 OR +4)                          
071700              MOVE 'R'           TO W-KDPROVPL                            
071800          END-IF                                                          
071900          MOVE NEJ               TO W-KVPROVPL-SEK-FINNS                  
072000          PERFORM IMS-GU-PROA-6102                                        
072100          IF SEGMENT-FINNS                                                
072200             MOVE +0             TO IX1                                   
072300             PERFORM UNTIL IX1 = +5                                       
072400               ADD +1            TO IX1                                   
072500               IF KVAL-KVAVIS < 6102-KVAVIS-FOM (IX1) OR                  
072600                              > 6102-KVAVIS-TOM (IX1)                     
072700                  CONTINUE                                                
072800               ELSE                                                       
072900                  MOVE JA        TO W-KVPROVPL-SEK-FINNS                  
073000                  MOVE 6102-KVPROVPL (IX1) TO W-KVKVASEK                  
073100                  MOVE +5        TO IX1                                   
073200               END-IF                                                     
073300             END-PERFORM                                                  
073400          END-IF                                                          
073500        END-IF                                                            
073600        IF KVAL-KVAVIS < W-KVKVASEK                                       
073700           MOVE KVAL-KVAVIS      TO W-KVKVASEK                            
073800        END-IF                                                            
073900     END-IF                                                               
074000                                                                          
074100     IF W-SKIPLOT-KONTROLL-PRI = JA                                       
074200     OR W-SKIPLOT-KONTROLL-SEK = JA                                       
074300        IF W-KVPROVPL-PRI-FINNS = NEJ                                     
074400        OR W-KVPROVPL-SEK-FINNS = NEJ                                     
074500           PERFORM IMS-GU-W6G2-6103                                       
074600           MOVE '1'              TO 6104-KDSEGKEY                         
074700           MOVE KVAL-IDARTNR     TO 6104-IDARTNR                          
074800           PERFORM IMS-ISRT-W6G2-6104                                     
074900         ELSE                                                             
075000           MOVE LEV-FLSKPSAK TO W-FLSKPSAK                                
075100           PERFORM S01-UPPDATERA-W6L101                                   
075200        END-IF                                                            
075300     END-IF                                                               
075400                                                                          
075500     IF (WS-ART-KDKVATYP = '2' AND LEV-FLSKPSAK = JA)                     
075600     OR (LEV-FLKVASAK = JA)                                               
075700     OR (KVAL-FLKVARED = JA AND W-GODK-FLKVARED = JA AND                  
075800        (LEV-KVSKPLOT-PRI = +1 OR LEV-KVSKPLOT-SEK = +1))                 
075900        CONTINUE                                                          
076000     ELSE                                                                 
076100        IF LEV-KVSKPLOT-PRI > +0                                          
076200           SUBTRACT 1 FROM LEV-KVSKPLOT-PRI                               
076300        END-IF                                                            
076400        IF LEV-KVSKPLOT-SEK > +0                                          
076500           SUBTRACT 1 FROM LEV-KVSKPLOT-SEK                               
076600        END-IF                                                            
076700        IF LEV-FLSKPSAK = NEJ                                             
076800        AND (LEV-KVSKPLOT-PRI > ZERO OR LEV-KVSKPLOT-SEK > ZERO)          
076900           CONTINUE                                                       
077000        ELSE                                                              
077100          IF LEV-KVSKPLOT-PRI = +0                                        
077200             IF LEV-FLSKPSAK = NEJ                                        
077300                MOVE JA          TO LEV-FLSKPSAK                          
077400             END-IF                                                       
077500             IF  WS-ART-KDKVATYP = '2'                                    
077600             AND LEV-FLSKPSAK    = JA                                     
077700                MOVE +0          TO LEV-KVSKPLOT-PRI                      
077800             ELSE                                                         
077900                MOVE WS-ART-IDPROVPL-PRI TO W-IDPROVPL                    
078000                MOVE 'R'         TO W-KDPROVPL                            
078100                PERFORM IMS-GU-PROA-6102                                  
078200                IF SEGMENT-FINNS                                          
078300                   MOVE 6102-KVSKPLOT  TO LEV-KVSKPLOT-PRI                
078400                END-IF                                                    
078500             END-IF                                                       
078600          END-IF                                                          
078700                                                                          
078800          IF LEV-KVSKPLOT-SEK = +0 AND WS-ART-IDPROVPL-SEK > ZERO         
078900             IF LEV-FLSKPSAK = NEJ                                        
079000                MOVE JA          TO LEV-FLSKPSAK                          
079100             END-IF                                                       
079200             IF  WS-ART-KDKVATYP = '2'                                    
079300             AND LEV-FLSKPSAK    = JA                                     
079400                MOVE +0          TO LEV-KVSKPLOT-SEK                      
079500             ELSE                                                         
079600                MOVE WS-ART-IDPROVPL-SEK TO W-IDPROVPL                    
079700                MOVE 'R'         TO W-KDPROVPL                            
079800                PERFORM IMS-GU-PROA-6102                                  
079900                IF SEGMENT-FINNS                                          
080000                   MOVE 6102-KVSKPLOT TO LEV-KVSKPLOT-SEK                 
080100                END-IF                                                    
080200             END-IF                                                       
080300          END-IF                                                          
080400        END-IF                                                            
080500     END-IF                                                               
080600     MOVE DAGENS-DATUM           TO LEV-TIUPPDAT                          
080700     IF   KVAL-FLKVARED   = JA                                            
080800     AND  W-GODK-FLKVARED = JA                                            
080900     AND (LEV-KVSKPLOT-PRI = +1 OR LEV-KVSKPLOT-SEK = +1)                 
081000         MOVE JA                 TO LEV-FLKVARED                          
081100     ELSE                                                                 
081200         MOVE NEJ                TO LEV-FLKVARED                          
081300     END-IF                                                               
081400     PERFORM IMS-REPL-KVAH-12                                             
081500     .                                                                    
081600     EJECT                                                                
081700 H-SLUMP-ANTALSKONTROLL SECTION.                                          
081800                                                                          
081900     IF W-ANTALS-KONTROLL = JA                                            
082000       IF W-KDKVASTA-ANT = '0'                                            
082100          CALL WRANDOM USING MAX-TAL SLUMP-TAL                            
082200          IF SLUMP-TAL < +11                                              
082300             MOVE '1' TO W-KDKVASTA-ANT                                   
082400             MOVE '2' TO W-KDKVAANT                                       
082500          END-IF                                                          
082600       END-IF                                                             
082700     END-IF                                                               
082800     IF W-KDKVASTA-ANT = '1'                                              
082900        MOVE JA TO W-FLSKPSAK                                             
083000        PERFORM S01-UPPDATERA-W6L101                                      
083100     END-IF                                                               
083200     .                                                                    
083300     EJECT                                                                
083400 I-KVALITETSINFO-TILL-INLEV SECTION.                                      
083500     SKIP2                                                                
083600     MOVE W-SPAR-KVKVAPRIM      TO KVAL-KVKVAPRIM                         
083700     MOVE W-SPAR-KVKVASEK       TO KVAL-KVKVASEK                          
083800     MOVE W-KDKVAANT            TO KVAL-KDKVAANT                          
083900     MOVE W-FLKVAKAR            TO KVAL-FLKVAKAR                          
084000     .                                                                    
084100     EJECT                                                                
084200 S01-UPPDATERA-W6L101 SECTION.                                            
084300     SKIP2                                                                
084400     PERFORM IMS-GHU-UPFA-01                                              
084500     MOVE KVAL-IDDC                  TO UPPF-IDDC                         
084600     MOVE KVAL-IDLOPNRM              TO UPPF-IDLOPNRM                     
084700     MOVE WS-ART-KDKVAKTL            TO UPPF-KDKVAKTL                     
084800     MOVE WS-ART-ADKVAULG            TO UPPF-ADKVAULG                     
084900     MOVE SPACE                      TO UPPF-BEANST                       
085000     MOVE NEJ                        TO UPPF-FLANNULL                     
085100     MOVE W-GODK-FLKVARED            TO UPPF-FLKVARED                     
085200     MOVE W-FLSKPSAK                 TO UPPF-FLSKPSAK                     
085300     IF W-KDKVASTA-ANT = '1'                                              
085400        MOVE JA                      TO UPPF-FLKVAUTV-ANT                 
085500     ELSE                                                                 
085600        MOVE NEJ                     TO UPPF-FLKVAUTV-ANT                 
085700     END-IF                                                               
085800     IF  W-SKIPLOT-KONTROLL-PRI = JA                                      
085900     OR  W-SKIPLOT-KONTROLL-SEK = JA                                      
086000        MOVE JA                      TO UPPF-FLKVAUTV-KVAL                
086100     ELSE                                                                 
086200        MOVE NEJ                     TO UPPF-FLKVAUTV-KVAL                
086300     END-IF                                                               
086400     MOVE KVAL-IDARTNR               TO UPPF-IDARTNR                      
086500     MOVE KVAL-IDLEVNR               TO UPPF-IDLEVNR                      
086600     MOVE SPACE                      TO UPPF-IDUSER-PRI                   
086700     MOVE SPACE                      TO UPPF-IDUSER-SEK                   
086800     MOVE SPACE                      TO UPPF-IDUSER-ADM                   
086900     MOVE W-KDKVASTA-ANT             TO UPPF-KDKVASTA-ANT                 
087000     MOVE '0'                        TO UPPF-KDKVASTA-ADM                 
087100     IF  W-SKIPLOT-KONTROLL-PRI = JA                                      
087200     AND W-KVKVAPRIM > +0                                                 
087300        MOVE '1'                     TO UPPF-KDKVASTA-PRI                 
087400     ELSE                                                                 
087500        MOVE '0'                     TO UPPF-KDKVASTA-PRI                 
087600     END-IF                                                               
087700     IF  W-SKIPLOT-KONTROLL-SEK = JA                                      
087800     AND W-KVKVASEK > +0                                                  
087900        MOVE '1'                     TO UPPF-KDKVASTA-SEK                 
088000     ELSE                                                                 
088100        MOVE '0'                     TO UPPF-KDKVASTA-SEK                 
088200     END-IF                                                               
088300     IF W-KVKVAPRIM NOT < W-SPAR-KVKVAPRIM                                
088400        MOVE W-KVKVAPRIM             TO UPPF-KVKVAPRIM                    
088500                                        W-SPAR-KVKVAPRIM                  
088600     END-IF                                                               
088700     IF W-KVKVASEK  NOT < W-SPAR-KVKVASEK                                 
088800        MOVE W-KVKVASEK              TO UPPF-KVKVASEK                     
088900                                        W-SPAR-KVKVASEK                   
089000     END-IF                                                               
089100     MOVE DAGENS-DATUM               TO UPPF-TIREGDAT                     
089200     IF SEGMENT-FINNS                                                     
089300        PERFORM IMS-REPL-UPFA-01                                          
089400     ELSE                                                                 
089500        PERFORM IMS-ISRT-UPFA-01                                          
089600     END-IF                                                               
089700     .                                                                    
089800     EJECT                                                                
089900 S02-UPPDATERA-EV-W6KVAH01 SECTION.                                       
090000     PERFORM IMS-GHU-KVAH-W6KVAH01                                        
090100     IF SEGMENT-SAKNAS                                                    
090200        MOVE KVAL-IDARTNR TO ART-IDARTNR                                  
090300        MOVE 1003         TO ART-KDKVAKTL                                 
090400        MOVE SPACE        TO ART-ADKVAULG                                 
090500        PERFORM IMS-ISRT-KVAH                                             
090600     END-IF                                                               
090700     .                                                                    
090800     EJECT                                                                
090900* --- IMS-SEKTIONER ---                                                   
091000     SKIP3                                                                
091100 IMS-GET-ARTC-01 SECTION.                                                 
091200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
091300          DELIMITED BY SIZE INTO SSA1                                     
091400     MOVE '  GE' TO GODK-STATUSKODER                                      
091500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA1 SSA1                     
091600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
091700     PERFORM IMS-STATUSKONTROLL                                           
091800     .                                                                    
091900     SKIP3                                                                
092000 IMS-GNP-ARTC-11 SECTION.                                                 
092100     MOVE 'WLARTC11 '         TO SSA1                                     
092200     MOVE '  GE' TO GODK-STATUSKODER                                      
092300     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA1 SSA1                    
092400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
092500     PERFORM IMS-STATUSKONTROLL                                           
092600     .                                                                    
092700     EJECT                                                                
092800 IMS-GU-KVAH-01 SECTION.                                                  
092900     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
093000          DELIMITED BY SIZE INTO SSA1                                     
093100     MOVE '  ' TO GODK-STATUSKODER                                        
093200     CALL CBLTDLI USING GU KVAH1-PCB DLI-IO-AREA2 SSA1                    
093300     MOVE KVAH1-STATUS-CODE TO STATUS-WS                                  
093400     PERFORM IMS-STATUSKONTROLL                                           
093500     .                                                                    
093600     SKIP2                                                                
093700 IMS-GHU-KVAH-W6KVAH01 SECTION.                                           
093800     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
093900          DELIMITED BY SIZE INTO SSA1                                     
094000     MOVE '  GE' TO GODK-STATUSKODER                                      
094100     CALL CBLTDLI USING GHU KVAH1-PCB DLI-IO-AREA2 SSA1                   
094200     MOVE KVAH1-STATUS-CODE TO STATUS-WS                                  
094300     PERFORM IMS-STATUSKONTROLL                                           
094400     .                                                                    
094500     SKIP3                                                                
094600 IMS-ISRT-KVAH SECTION.                                                   
094700                                                                          
094800     MOVE 'W6KVAH01 ' TO SSA1                                             
094900     MOVE '  ' TO GODK-STATUSKODER                                        
095000     CALL CBLTDLI USING ISRT KVAH1-PCB DLI-IO-AREA2 SSA1                  
095100     MOVE KVAH1-STATUS-CODE TO STATUS-WS                                  
095200     PERFORM IMS-STATUSKONTROLL                                           
095300     .                                                                    
095400     SKIP3                                                                
095500 IMS-GNP-KVAH-22 SECTION.                                                 
095600     STRING 'W6KVAH12(IDLEVNR  =' W-IDLEVNR-X ')'                         
095700          DELIMITED BY SIZE INTO SSA1                                     
095800     MOVE 'W6KVAH22 '         TO SSA2                                     
095900     MOVE '  GE' TO GODK-STATUSKODER                                      
096000     CALL CBLTDLI USING GNP KVAH1-PCB DLI-IO-AREA2 SSA1 SSA2              
096100     MOVE KVAH1-STATUS-CODE TO STATUS-WS                                  
096200     PERFORM IMS-STATUSKONTROLL                                           
096300     .                                                                    
096400     SKIP2                                                                
096500 IMS-GU-KVAH-11 SECTION.                                                  
096600     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
096700          DELIMITED BY SIZE INTO SSA1                                     
096800     STRING 'W6KVAH11(IDKVAINF =' W-IDKVAINF-X ')'                        
096900          DELIMITED BY SIZE INTO SSA2                                     
097000     MOVE '  ' TO GODK-STATUSKODER                                        
097100     CALL CBLTDLI USING GU KVAH2-PCB DLI-IO-AREA2 SSA1 SSA2               
097200     MOVE KVAH2-STATUS-CODE TO STATUS-WS                                  
097300     PERFORM IMS-STATUSKONTROLL                                           
097400     .                                                                    
097500     EJECT                                                                
097600 IMS-GHNP-KVAH-12 SECTION.                                                
097700     STRING 'W6KVAH12(IDLEVNR  =' W-IDLEVNR-X ')'                         
097800          DELIMITED BY SIZE INTO SSA1                                     
097900     MOVE '  GE' TO GODK-STATUSKODER                                      
098000     CALL CBLTDLI USING GHNP KVAH1-PCB DLI-IO-AREA2 SSA1                  
098100     MOVE KVAH1-STATUS-CODE TO STATUS-WS                                  
098200     PERFORM IMS-STATUSKONTROLL                                           
098300     .                                                                    
098400     SKIP2                                                                
098500 IMS-ISRT-KVAH-12 SECTION.                                                
098600     MOVE 'W6KVAH12 '         TO SSA1                                     
098700     MOVE '  '                TO GODK-STATUSKODER                         
098800     CALL CBLTDLI USING ISRT KVAH1-PCB DLI-IO-AREA2 SSA1                  
098900     MOVE KVAH1-STATUS-CODE TO STATUS-WS                                  
099000     PERFORM IMS-STATUSKONTROLL                                           
099100     .                                                                    
099200     SKIP2                                                                
099300 IMS-REPL-KVAH-12 SECTION.                                                
099400     MOVE '  '   TO GODK-STATUSKODER                                      
099500     CALL CBLTDLI USING REPL KVAH1-PCB DLI-IO-AREA2                       
099600     MOVE KVAH1-STATUS-CODE TO STATUS-WS                                  
099700     PERFORM IMS-STATUSKONTROLL                                           
099800     .                                                                    
099900     EJECT                                                                
100000 IMS-GU-KVAE-01 SECTION.                                                  
100100     STRING 'W6KVAE01(W6H7BSEQ=>' W1-W6H7BSEQ-X                           
100200                    '&W6H7BSEQ=<' W2-W6H7BSEQ-X                           
100300                    '&IDLEVNR  =' W1-IDLEVNR-X                            
100400                    '&KVKRKNTR=>' W1-KVKRKNTR-X                           
100500                    '&KVKRKNTR=<' W2-KVKRKNTR-X ')'                       
100600          DELIMITED BY SIZE INTO SSA1                                     
100700     MOVE '  GE' TO GODK-STATUSKODER                                      
100800     CALL CBLTDLI USING GU KVAE-PCB DLI-IO-AREA3 SSA1                     
100900     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
101000     PERFORM IMS-STATUSKONTROLL                                           
101100     .                                                                    
101200     EJECT                                                                
101300 IMS-GN-KVAE-01 SECTION.                                                  
101400     STRING 'W6KVAE01(W6H7BSEQ=>' W1-W6H7BSEQ-X                           
101500                    '&W6H7BSEQ=<' W2-W6H7BSEQ-X                           
101600                    '&IDLEVNR  =' W1-IDLEVNR-X                            
101700                    '&KVKRKNTR=>' W1-KVKRKNTR-X                           
101800                    '&KVKRKNTR=<' W2-KVKRKNTR-X ')'                       
101900          DELIMITED BY SIZE INTO SSA1                                     
102000     MOVE '  GE' TO GODK-STATUSKODER                                      
102100     CALL CBLTDLI USING GN KVAE-PCB DLI-IO-AREA3 SSA1                     
102200     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
102300     PERFORM IMS-STATUSKONTROLL                                           
102400     .                                                                    
102500     EJECT                                                                
102600 IMS-ISRT-UPFA-01 SECTION.                                                
102700     MOVE 'W6UPFA01 '         TO SSA1                                     
102800     MOVE '  '                TO GODK-STATUSKODER                         
102900     CALL CBLTDLI USING ISRT UPFA-PCB DLI-IO-AREA4 SSA1                   
103000     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
103100     PERFORM IMS-STATUSKONTROLL                                           
103200     .                                                                    
103300     SKIP2                                                                
103400 IMS-ISRT-UPFA-11 SECTION.                                                
103500     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
103600          DELIMITED BY SIZE INTO SSA1                                     
103700     MOVE 'W6UPFA11 '         TO SSA2                                     
103800     MOVE '  '                TO GODK-STATUSKODER                         
103900     CALL CBLTDLI USING ISRT UPFA-PCB DLI-IO-AREA4 SSA1 SSA2              
104000     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
104100     PERFORM IMS-STATUSKONTROLL                                           
104200     .                                                                    
104300     SKIP2                                                                
104400 IMS-ISRT-UPFA-12 SECTION.                                                
104500     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
104600          DELIMITED BY SIZE INTO SSA1                                     
104700     MOVE 'W6UPFA12 '         TO SSA1                                     
104800     MOVE '  '                TO GODK-STATUSKODER                         
104900     CALL CBLTDLI USING ISRT UPFA-PCB DLI-IO-AREA4 SSA1                   
105000     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
105100     PERFORM IMS-STATUSKONTROLL                                           
105200     .                                                                    
105300     EJECT                                                                
105400 IMS-GHU-UPFA-01 SECTION.                                                 
105500     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
105600          DELIMITED BY SIZE INTO SSA1                                     
105700     MOVE '  GE' TO GODK-STATUSKODER                                      
105800     CALL CBLTDLI USING GHU UPFA-PCB DLI-IO-AREA4 SSA1                    
105900     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
106000     PERFORM IMS-STATUSKONTROLL                                           
106100     .                                                                    
106200     SKIP2                                                                
106300 IMS-REPL-UPFA-01 SECTION.                                                
106400     MOVE '  '                TO GODK-STATUSKODER                         
106500     CALL CBLTDLI USING REPL UPFA-PCB DLI-IO-AREA4                        
106600     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
106700     PERFORM IMS-STATUSKONTROLL                                           
106800     .                                                                    
106900     EJECT                                                                
107000 IMS-GU-PROA-6102 SECTION.                                                
107100     STRING 'W6PROA01(W6GXKEY  =' W-W6GXKEY-6101-X ')'                    
107200          DELIMITED BY SIZE INTO SSA1                                     
107300     STRING 'W6PROA11(W6GXKEY  =' W-W6GXKEY-6102-X ')'                    
107400          DELIMITED BY SIZE INTO SSA2                                     
107500     MOVE '  GE' TO GODK-STATUSKODER                                      
107600     CALL CBLTDLI USING GU PROA-PCB DLI-IO-AREA5 SSA1 SSA2                
107700     MOVE PROA-STATUS-CODE TO STATUS-WS                                   
107800     PERFORM IMS-STATUSKONTROLL                                           
107900     .                                                                    
108000     EJECT                                                                
108100 IMS-GU-LEVA-01 SECTION.                                                  
108200     STRING 'W6LEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
108300          DELIMITED BY SIZE INTO SSA1                                     
108400     MOVE '  GE' TO GODK-STATUSKODER                                      
108500     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA6 SSA1                     
108600     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
108700     PERFORM IMS-STATUSKONTROLL                                           
108800     .                                                                    
108900     EJECT                                                                
109000 IMS-GU-W6G2-6103 SECTION.                                                
109100     SKIP2                                                                
109200     STRING 'W6KODA01(W6GXKEY  =' W-W6GXKEY-6103-X ')'                    
109300          DELIMITED BY SIZE INTO SSA1                                     
109400     MOVE '  '                TO GODK-STATUSKODER                         
109500     CALL CBLTDLI USING GU KODA-PCB DLI-IO-AREA7 SSA1                     
109600     MOVE KODA-STATUS-CODE TO STATUS-WS                                   
109700     PERFORM IMS-STATUSKONTROLL                                           
109800     .                                                                    
109900     SKIP2                                                                
110000 IMS-ISRT-W6G2-6104 SECTION.                                              
110100     SKIP2                                                                
110200     MOVE 'W6KODA11 '         TO SSA1                                     
110300     MOVE '  '                TO GODK-STATUSKODER                         
110400     CALL CBLTDLI USING ISRT KODA-PCB DLI-IO-AREA7 SSA1                   
110500     MOVE KODA-STATUS-CODE TO STATUS-WS                                   
110600     PERFORM IMS-STATUSKONTROLL                                           
110700     .                                                                    
110800     EJECT                                                                
110900 IMS-GU-WLXXLA11 SECTION.                                                 
111000     STRING 'WLXXLA01(WDGXKEY  =' W-WDGX-4825-KEY-X ')'                   
111100          DELIMITED BY SIZE INTO SSA1                                     
111200     STRING 'WLXXLA11(WDGXKEY  =' W-WDGX-4826-KEY-X ')'                   
111300          DELIMITED BY SIZE INTO SSA2                                     
111400     MOVE '  GE' TO GODK-STATUSKODER                                      
111500     CALL CBLTDLI USING GU XXLA-PCB DLI-IO-AREA8 SSA1 SSA2                
111600     MOVE XXLA-STATUS-CODE TO STATUS-WS                                   
111700     PERFORM IMS-STATUSKONTROLL                                           
111800     .                                                                    
111900     EJECT                                                                
112000 IMS-STATUSKONTROLL SECTION.                                              
112100     SKIP2                                                                
112200     SET STATUS-IX TO 1                                                   
112300     SEARCH GODK-STATUS                                                   
112400       AT END                                                             
112500         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
112600         CALL FELLOG                                                      
112700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
112800         CONTINUE                                                         
112900     END-SEARCH                                                           
113000     .                                                                    
