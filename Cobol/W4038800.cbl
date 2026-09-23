000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4038800.                                                
000400 AUTHOR.         PER BERGH.                                               
000500 DATE-WRITTEN.   90/05/21.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*                                                                         
001100*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
001200*        PROGRAMMET UPPDATERAR WLXXKX (WDR4)                              
001300*                                                                         
001400*        LISTBESTÄLLNING WOPS; PER PRC-GRUPP KAN LISTOR BESTÄLLAS         
001500*        FÖR UTPRINTNING, LÄGGAS TILL, ÄNDRAS OCH TAS BORT.               
001600*        ÄVEN PRC-GRUPP KAN LÄGGAS TILL OCH TAS BORT.                     
001700*        GÄLLER EGET C-LAGER.                                             
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W4T388                                              
002100*        MID:         W4I38801                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W4O38801                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 WORKING-STORAGE SECTION.                                                 
003001                                                                          
003010*    -- CHECKED BY WY2000                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W4038800'.            
003200                                                                          
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  YES                         PIC X       VALUE 'Y'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600                                                                          
003700*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003800 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003900 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004000                                                                          
004100 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004200 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +708  COMP SYNC.        
004300                                                                          
004400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004500                                                                          
004600 77  WS-4473-KDPRCGRP            PIC X(5).                                
004700 77  WS-4474-IDLISTTYP           PIC X(6).                                
004800 77  WS-4474-IDLISTA             PIC X(3).                                
004900                                                                          
005000 77  UPDATE-SW                   PIC X       VALUE 'J'.                   
005100     88  UPDATE-OK                           VALUE 'J'.                   
005200                                                                          
005300 77  TEXTKOLL-SW                 PIC X       VALUE 'J'.                   
005400     88  AENDRAD-TEXT                        VALUE 'J'.                   
005500                                                                          
005600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005700     88  INDATA-OK                           VALUE 'J'.                   
005800     88  INDATA-FEL                          VALUE 'N'.                   
005900                                                                          
006000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006100     88  NYCKLAR-OK                          VALUE 'J'.                   
006200     88  NYCKLAR-FEL                         VALUE 'N'.                   
006300                                                                          
006400 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006500     88  ALLT-OK                             VALUE 'J'.                   
006600                                                                          
006700 77  FLNYRAD                     PIC X       VALUE 'N'.                   
006800                                                                          
006900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007000     88  EGEN-MID                            VALUE '4388'.                
007100     88  GODK-MID                            VALUE '4388'.                
007200*      --- VALID IDDC CODES                                               
007300*                                                                         
007400*01    -COPY WWDC99                                                       
007500       EJECT                                                              
007600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007700 01  GENERELLA-SUBPROGRAM.                                                
007800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008010     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008300*   -COPY WMEDAREA                                                        
008400     EJECT                                                                
008410*                   ****    PARAMETRAR TILL W005INIT                      
008420*01  -COPY WMSGINIT                                                       
008430     EJECT                                                                
008600 01  MESSAGE-CODES.                                                       
008700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008900     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
009000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
009100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
009200     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
009300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009400     03  ERR-INF-MISS            PIC X(3)    VALUE '413'.                 
009500     03  ERR-NO-CHANGE           PIC X(3)    VALUE '789'.                 
009600     EJECT                                                                
009700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009800*                                                                         
009900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010000     SKIP3                                                                
010100*01  MID -COPY W4I38801                                                   
010300     EJECT                                                                
010400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010500     SKIP3                                                                
010600*01  -COPY WMSGAREA                                                       
010800     EJECT                                                                
010900     03  MOD REDEFINES MSG-AREA.                                          
011000*      05  -COPY W4O38801                                                 
011200     EJECT                                                                
011300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011400     SKIP3                                                                
011500*01  -COPY WMFSAREA                                                       
011700     EJECT                                                                
011800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011900*                                                                         
012000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012100     SKIP3                                                                
012200 01  NYCKLAR-TILL-DLI.                                                    
012300     03  W-4473-X.                                                        
012400         05  W-IDHTYP            PIC X(04)    VALUE '4473'.               
012500         05  W-IDDC              PIC X(02).                               
012600         05  W-KDPRCGRP          PIC X(05)    VALUE SPACE.                
012700         05  W-4473-LOW-VALUE    PIC X(19)    VALUE LOW-VALUE.            
012800     03  W-4474-X.                                                        
012900         05  W-IDLISTTYP         PIC X(06)    VALUE SPACE.                
013000         05  W-IDLISTA           PIC S9(3)    VALUE ZERO COMP-3.          
013100         05  W-4474-LOW-VALUE    PIC X(02)    VALUE LOW-VALUE.            
013200*                                                                         
013300 01  W-UTDATA.                                                            
013400     03  W-IDLISTTYP-UT          PIC X(06)    VALUE SPACE.                
013500     03  W-STRECK-UT             PIC X        VALUE '-'.                  
013600     03  W-IDLISTA-UT            PIC X(03)    VALUE SPACE.                
013700                                                                          
013800*                                                                         
013900 01  W-INDATA.                                                            
014000     03  W-IDLISTTYP-IN          PIC X(06)    VALUE SPACE.                
014100     03  W-IDLISTA-IN            PIC S9(3)    VALUE ZERO COMP-3.          
014200     03  FILLER                  PIC X(02)    VALUE LOW-VALUE.            
014300     03  W-BELISTA-IN            PIC X(25)    VALUE SPACE.                
014400     03  W-FLBEST-IN             PIC X        VALUE 'N'.                  
014500     03  W-FLBORT-IN             PIC X        VALUE 'N'.                  
014600     EJECT                                                                
014700*    --- STATUS-KOD FRÅN IMS                                              
014800 01  STATUS-WS                   PIC XX.                                  
014900     88  SEGMENT-FINNS                       VALUE '  '.                  
015000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015200     SKIP2                                                                
015300 01  GODK-STATUSKODER.                                                    
015400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015500     SKIP3                                                                
015600 01  SSA1                        PIC X(64).                               
015700 01  SSA2                        PIC X(64).                               
015800     EJECT                                                                
015900*    --- IMS FUNKTIONSKODER                                               
016000*01  -COPY W0003                                                          
016200     EJECT                                                                
016300*    ---  DLI INPUT-OUTPUT AREA                                           
016400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
016500     SKIP3                                                                
016600 01  DLI-IO-AREA.                                                         
016700     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
016800     SKIP3                                                                
016900     03  WLXXKX01 REDEFINES IO-AREA.                                      
017000*        05  -COPY WDGX4473                                               
017200     EJECT                                                                
017300     03  WLXXKX11 REDEFINES IO-AREA.                                      
017400*        05  -COPY WDGX4474                                               
017600     EJECT                                                                
017700 LINKAGE SECTION.                                                         
017800                                                                          
017900*01  -COPY W0009      -PRE MSG-                                           
018000     EJECT                                                                
018010*01  -COPY W0008     -PRE USEA-                                           
018020     05  FILLER              PIC X.                                       
018100     EJECT                                                                
018200*01  -COPY W0008      -PRE XXKX-                                          
018400     05  FILLER                  PIC X.                                   
018500     EJECT                                                                
018600 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
018610                                   XXKX-PCB.                              
018700     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
018710                                   XXKX-PCB.                              
018800                                                                          
018900     PERFORM IMS-GET-MSG                                                  
019000     IF SEGMENT-FINNS                                                     
019100       PERFORM A-INIT                                                     
019200       PERFORM B-KOLLA-NYCKLAR                                            
019300       IF NYCKLAR-OK                                                      
019400         IF MFS-UPDATE                                                    
019500           PERFORM G-KOLLA-INPUT                                          
019600           IF INDATA-OK                                                   
019700             PERFORM H-UPPDATERA                                          
019800           END-IF                                                         
019900         ELSE                                                             
020000           IF MFS-FIRST                                                   
020100             PERFORM C-FOERSTA-SIDA                                       
020200           ELSE                                                           
020300             IF MFS-NEXT                                                  
020400               PERFORM D-NAESTA-SIDA                                      
020500             ELSE                                                         
020600               PERFORM E-SAMMA-SIDA                                       
020700             END-IF                                                       
020800           END-IF                                                         
020900           IF ALLT-OK                                                     
021000             PERFORM F-LAES-VISA-INFO                                     
021100           END-IF                                                         
021200         END-IF                                                           
021300       END-IF                                                             
021400       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
021500       PERFORM IMS-INSERT-MSG                                             
021600     END-IF                                                               
021700                                                                          
021800     MOVE ZERO TO RETURN-CODE                                             
021900     GOBACK                                                               
022000     .                                                                    
022100     EJECT                                                                
022200 A-INIT SECTION.                                                          
022300                                                                          
022400     IF MSG-DUBBLA-TRANSKODER                                             
022500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I38801                 
022600       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
022700       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
022800     ELSE                                                                 
022900       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I38801                 
023000       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
023100       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
023200     END-IF                                                               
023300                                                                          
023400     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
023500     MOVE MSG-IDPFK            TO MFS-IDPFK                               
023600     MOVE MFS-IDTRANS          TO W-IDTRANS                               
023700                                                                          
023800     MOVE LOW-VALUE            TO MSG-AREA                                
023900     MOVE 'W4O388N1'           TO MFS-IDMOD                               
024000     MOVE '4388'               TO MOD-IDTRANS                             
024100     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL MOD-TEMFSINF               
024200                                                                          
024300     IF NOT EGEN-MID                                                      
024400       MOVE SPACE                         TO MFS-KDTRTYP                  
024500       MOVE '7'                           TO MFS-IDPFK                    
024600     END-IF                                                               
024700                                                                          
024800     IF ENGLISH-TEXT                                                      
024900       MOVE +2                            TO SPRAK-IX                     
025000       MOVE 'GB '                         TO MED-IDSKYLT                  
025100     ELSE                                                                 
025200       MOVE +1                            TO SPRAK-IX                     
025300       MOVE 'S  '                         TO MED-IDSKYLT                  
025400     END-IF                                                               
025500     .                                                                    
025600     EJECT                                                                
025700 B-KOLLA-NYCKLAR SECTION.                                                 
025710                                                                          
025720     MOVE ALL '+'           TO MSGI-WMSGINIT                              
025730     MOVE '001'             TO MSGI-KDCALL                                
025740     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
025741     MOVE '4388'            TO MSGI-IDTRANS                               
025742     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
025750     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
025800                                                                          
025900     MOVE JA                   TO NYCKLAR-SW                              
026100*                                   -- FORMATETS C-LAGER                  
026200*    -- KONTROLL AV KDPRCGRP                                              
026300     MOVE MFS-RENSA-FAELT      TO MOD-KDPRCGRP-IN                         
026301                                  MOD-IDDC-IN                             
026310*                                                                         
026400     IF MID-KDPRCGRP-IN = ALL '+'                                         
026500        MOVE MID-KDPRCGRP-UT              TO WS-4473-KDPRCGRP             
026600        INSPECT MOD-KDPRCGRP-UT REPLACING LEADING SPACE BY ZERO           
026700     ELSE                                                                 
026800        MOVE MID-KDPRCGRP-IN              TO WS-4473-KDPRCGRP             
026900        MOVE '7'                          TO MFS-IDPFK                    
027000        MOVE SPACE                        TO MFS-KDTRTYP                  
027100     END-IF                                                               
027200                                                                          
027300     IF WS-4473-KDPRCGRP = ALL SPACE OR                                   
027400        WS-4473-KDPRCGRP = ALL ZEROS                                      
027500        MOVE NEJ                          TO NYCKLAR-SW                   
027600     ELSE                                                                 
027700        MOVE WS-4473-KDPRCGRP             TO W-KDPRCGRP                   
027800     END-IF                                                               
027900                                                                          
028000     IF GODK-MID                                                          
028100        IF MID-KDPRODKL = ALL '+'                                         
028200           MOVE ZEROS                     TO MOD-KDPRODKL                 
028300        ELSE                                                              
028400           MOVE MID-KDPRODKL              TO MOD-KDPRODKL                 
028500        END-IF                                                            
028600        IF MID-IDPRC = ALL '+'                                            
028700           MOVE ZEROS                     TO MOD-IDPRC                    
028800        ELSE                                                              
028900           MOVE MID-IDPRC                 TO MOD-IDPRC                    
029000        END-IF                                                            
029001     END-IF                                                               
029002                                                                          
029003     MOVE MSGI-IDDC               TO WS-IDDC                              
029097                                                                          
029098     IF WS-IDDC IS > SPACE                                                
029099       MOVE WS-IDDC                       TO W-IDDC                       
029100     ELSE                                                                 
029106       MOVE NEJ                           TO NYCKLAR-SW                   
029107     END-IF                                                               
029110                                                                          
029200     IF GODK-MID OR NYCKLAR-OK                                            
029300        MOVE WS-4473-KDPRCGRP             TO MOD-KDPRCGRP-UT              
029400        INSPECT MOD-KDPRCGRP-UT REPLACING LEADING ZERO BY SPACE           
029410        MOVE WS-IDDC                      TO MOD-IDDC-UT                  
029500     ELSE                                                                 
029600        MOVE MFS-RENSA-FAELT              TO MOD-KDPRCGRP-UT              
029610                                             MOD-IDDC-UT                  
029700     END-IF                                                               
029800                                                                          
029900     IF NYCKLAR-FEL                                                       
030000       MOVE ERR-WRONG-KEY                 TO MED-IDMFSFEL                 
030100       CALL WMEDKONV USING MED-WMEDAREA                                   
030200       MOVE MED-MFSFEL                    TO MOD-TEMFSFEL                 
030300       PERFORM MFS-RENSA-FAELT-IN                                         
030400       PERFORM MFS-RENSA-FAELT-UT                                         
030500     END-IF                                                               
030600     .                                                                    
030700     EJECT                                                                
030800 C-FOERSTA-SIDA SECTION.                                                  
030900                                                                          
031000     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
031100     CALL WMEDKONV USING MED-WMEDAREA                                     
031200     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
031300                                                                          
031400     MOVE SPACE TO W-IDLISTTYP                                            
031500     MOVE ZEROS TO W-IDLISTA                                              
031600     MOVE JA TO ALLT-SW                                                   
031700     .                                                                    
031800     EJECT                                                                
031900 D-NAESTA-SIDA SECTION.                                                   
032000                                                                          
032100     MOVE MID-IDLISTTYP-NEXT TO W-IDLISTTYP                               
032200     MOVE MID-IDLISTA-NEXT TO W-IDLISTA                                   
032300     MOVE JA TO ALLT-SW                                                   
032400     .                                                                    
032500     EJECT                                                                
032600 E-SAMMA-SIDA SECTION.                                                    
032700                                                                          
032800     IF MID-INPUT = ALL '+'                                               
032900       MOVE MID-IDLISTTYP-ENTER TO W-IDLISTTYP                            
033000       MOVE MID-IDLISTA-ENTER TO W-IDLISTA                                
033100       MOVE JA TO ALLT-SW                                                 
033200     ELSE                                                                 
033300       MOVE NEJ TO ALLT-SW                                                
033400       MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
033500       CALL WMEDKONV USING MED-WMEDAREA                                   
033600       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
033700       PERFORM MFS-ROR-EJ-FAELT-IN                                        
033800       PERFORM MFS-ROR-EJ-FAELT-UT                                        
033900       PERFORM MFS-LAS-IN-IGEN                                            
034000     END-IF                                                               
034100     .                                                                    
034200     EJECT                                                                
034300 F-LAES-VISA-INFO SECTION.                                                
034400                                                                          
034500     PERFORM IMS-GET-XXKX-KX01                                            
034600                                                                          
034700     IF SEGMENT-SAKNAS                                                    
034800        PERFORM FA-ERR-INF-MISS                                           
034900        MOVE SPACE TO MOD-IDLISTTYP-ENTER                                 
035000                      MOD-IDLISTA-ENTER                                   
035100     ELSE                                                                 
035200       MOVE +1 TO INDX                                                    
035300       PERFORM S01-FLYTTA-VAERDEN                                         
035400       PERFORM IMS-GNP-FIRST-XXKX-KX11                                    
035500       MOVE 4474-IDLISTTYP TO MOD-IDLISTTYP-ENTER                         
035600       MOVE 4474-IDLISTA   TO MOD-IDLISTA-ENTER                           
035700                                                                          
035800       PERFORM UNTIL INDX > MAX-INDX                                      
035900         IF SEGMENT-FINNS                                                 
036000            PERFORM S01-FLYTTA-VAERDEN                                    
036100            PERFORM IMS-GNP-XXKX-KX11                                     
036200         ELSE                                                             
036300            PERFORM MFS-RENSA-RAD-FAELT-UT                                
036400         END-IF                                                           
036500         ADD 1 TO INDX                                                    
036600       END-PERFORM                                                        
036700                                                                          
036800       IF SEGMENT-FINNS                                                   
036900          PERFORM FC-INF-MORE-INFO                                        
037000       ELSE                                                               
037100          MOVE MID-IDLISTTYP-NEXT  TO MOD-IDLISTTYP-ENTER                 
037200                                      MOD-IDLISTTYP-NEXT                  
037300          MOVE MID-IDLISTA-NEXT    TO MOD-IDLISTA-ENTER                   
037400                                      MOD-IDLISTA-NEXT                    
037500       END-IF                                                             
037600       IF MFS-FIRST                                                       
037700          PERFORM FB-INF-FIRST-PAGE                                       
037800       END-IF                                                             
037900                                                                          
038000     END-IF                                                               
038100     PERFORM MFS-RENSA-FAELT-IN                                           
038200     .                                                                    
038300     EJECT                                                                
038400 FA-ERR-INF-MISS SECTION.                                                 
038500                                                                          
038600     MOVE ERR-INF-MISS TO MED-IDMFSFEL                                    
038700     CALL WMEDKONV USING MED-WMEDAREA                                     
038800     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
038900     PERFORM MFS-RENSA-FAELT-IN                                           
039000     .                                                                    
039100     SKIP2                                                                
039200 FB-INF-FIRST-PAGE SECTION.                                               
039300                                                                          
039400     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
039500     CALL WMEDKONV USING MED-WMEDAREA                                     
039600     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
039700     .                                                                    
039800     SKIP2                                                                
039900 FC-INF-MORE-INFO SECTION.                                                
040000                                                                          
040100     MOVE 4474-IDLISTTYP TO MOD-IDLISTTYP-NEXT                            
040200     MOVE 4474-IDLISTA TO MOD-IDLISTA-NEXT                                
040300     MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                            
040400     CALL WMEDKONV USING MED-WMEDAREA                                     
040500     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
040600     .                                                                    
040700     EJECT                                                                
040800 G-KOLLA-INPUT SECTION.                                                   
040900                                                                          
041000     MOVE JA  TO INDATA-SW                                                
041100                                                                          
041200     IF MID-INPUT = ALL '+'                                               
041300       PERFORM GA-ERR-PF11-AND-NO-DATA                                    
041400     ELSE                                                                 
041500                                                                          
041600        IF MID-IDLISTTYP-IN = ALL '+'  OR                                 
041700           MID-IDLISTTYP-IN = ALL SPACE                                   
041800           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLISTTYP-IN-ATTR               
041900           MOVE NEJ TO INDATA-SW                                          
042000        ELSE                                                              
042100           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLISTTYP-IN-ATTR             
042200        END-IF                                                            
042300        IF MID-IDLISTA-IN NUMERIC AND                                     
042400           MID-IDLISTA-IN > ZERO                                          
042500          MOVE MFS-NUM-FAELT-RAETT TO MOD-IDLISTA-IN-ATTR                 
042600        ELSE                                                              
042700          MOVE MFS-NUM-FAELT-FEL TO MOD-IDLISTA-IN-ATTR                   
042800          MOVE NEJ TO INDATA-SW                                           
042900        END-IF                                                            
043000        IF INDATA-FEL                                                     
043100           PERFORM GB-ERR-CORR-HILITE-FLDS                                
043200        END-IF                                                            
043300        IF MID-FLBEST-IN = JA OR YES OR NEJ                               
043400           MOVE MID-FLBEST-IN TO W-FLBEST-IN                              
043500           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLBEST-IN-ATTR                
043600        ELSE                                                              
043700           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLBEST-IN-ATTR                  
043800           PERFORM GC-ERR-CORR-HILITE-FLDS                                
043900           MOVE NEJ TO INDATA-SW                                          
044000        END-IF                                                            
044100                                                                          
044200        IF MID-FLBORT-IN = JA OR YES OR NEJ                               
044300           MOVE MID-FLBORT-IN TO W-FLBORT-IN                              
044400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLBORT-IN-ATTR                
044500        ELSE                                                              
044600           IF MID-FLBORT-IN = ALL '+' OR SPACE                            
044700              MOVE  NEJ          TO W-FLBORT-IN                           
044800              MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLBORT-IN-ATTR             
044900           ELSE                                                           
045000              MOVE MFS-ALFA-FAELT-FEL TO MOD-FLBORT-IN-ATTR               
045100              PERFORM GD-ERR-CORR-HILITE-FLDS                             
045200              MOVE NEJ TO INDATA-SW                                       
045300           END-IF                                                         
045400        END-IF                                                            
045500                                                                          
045600        MOVE MFS-NUM-FAELT-RAETT TO MOD-BELISTA-IN-ATTR                   
045700                                                                          
045800        IF INDATA-OK                                                      
045900           PERFORM GE-FLYTTA-VAERDEN                                      
046000        END-IF                                                            
046100                                                                          
046200     END-IF                                                               
046300     .                                                                    
046400     EJECT                                                                
046500 GA-ERR-PF11-AND-NO-DATA SECTION.                                         
046600                                                                          
046700     MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                            
046800     CALL WMEDKONV USING MED-WMEDAREA                                     
046900     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
047000     PERFORM MFS-ROR-EJ-FAELT-IN                                          
047100     PERFORM MFS-ROR-EJ-FAELT-UT                                          
047200     MOVE NEJ TO INDATA-SW                                                
047300     .                                                                    
047400     SKIP2                                                                
047500 GB-ERR-CORR-HILITE-FLDS SECTION.                                         
047600                                                                          
047700     MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                            
047800     CALL WMEDKONV USING MED-WMEDAREA                                     
047900     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
048000     PERFORM MFS-ROR-EJ-FAELT-UT                                          
048100     PERFORM MFS-ROR-EJ-FAELT-IN                                          
048200     PERFORM MFS-LAS-IN-IGEN                                              
048300     MOVE MFS-ADD-LAES-IN-FAELT-HI TO                                     
048400              MOD-IDLISTTYP-IN-ATTR                                       
048500              MOD-IDLISTA-IN-ATTR                                         
048600     .                                                                    
048700     EJECT                                                                
048800 GC-ERR-CORR-HILITE-FLDS SECTION.                                         
048900                                                                          
049000     MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                            
049100     CALL WMEDKONV USING MED-WMEDAREA                                     
049200     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
049300     PERFORM MFS-ROR-EJ-FAELT-UT                                          
049400     PERFORM MFS-ROR-EJ-FAELT-IN                                          
049500     PERFORM MFS-LAS-IN-IGEN                                              
049600     MOVE MFS-ADD-LAES-IN-FAELT-HI TO                                     
049700              MOD-FLBEST-IN-ATTR                                          
049800     .                                                                    
049900     SKIP2                                                                
050000 GD-ERR-CORR-HILITE-FLDS SECTION.                                         
050100                                                                          
050200     MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                            
050300     CALL WMEDKONV USING MED-WMEDAREA                                     
050400     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
050500     PERFORM MFS-ROR-EJ-FAELT-UT                                          
050600     PERFORM MFS-ROR-EJ-FAELT-IN                                          
050700     PERFORM MFS-LAS-IN-IGEN                                              
050800     MOVE MFS-ADD-LAES-IN-FAELT-HI TO                                     
050900              MOD-FLBORT-IN-ATTR                                          
051000     .                                                                    
051100     EJECT                                                                
051200 GE-FLYTTA-VAERDEN SECTION.                                               
051300                                                                          
051400     MOVE MID-IDLISTTYP-IN TO WS-4474-IDLISTTYP                           
051500                              W-IDLISTTYP                                 
051600                              W-IDLISTTYP-IN                              
051700     MOVE MID-IDLISTA-IN   TO WS-4474-IDLISTA                             
051800                              W-IDLISTA                                   
051900                              W-IDLISTA-IN                                
052000                                                                          
052100     IF MID-BELISTA-IN = ALL '+'                                          
052200        MOVE SPACE         TO W-BELISTA-IN                                
052300     ELSE                                                                 
052400        MOVE MID-BELISTA-IN TO W-BELISTA-IN                               
052500     END-IF                                                               
052600     .                                                                    
052700     EJECT                                                                
052800 H-UPPDATERA SECTION.                                                     
052900                                                                          
053000     MOVE JA  TO UPDATE-SW                                                
053100     MOVE JA  TO TEXTKOLL-SW                                              
053200     MOVE NEJ TO FLNYRAD                                                  
053300                                                                          
053400     PERFORM IMS-GHU-XXKX-KX01                                            
053500     IF SEGMENT-SAKNAS                                                    
053600        MOVE W-4473-X TO 4473-WDGX4473                                    
053700        PERFORM IMS-ISRT-XXKX-KX01                                        
053800     END-IF                                                               
053900                                                                          
054000     PERFORM IMS-GHU-XXKX-KX11                                            
054100     IF SEGMENT-SAKNAS                                                    
054200        IF W-FLBORT-IN = JA OR YES                                        
054300           PERFORM HA-SAKNAS                                              
054400        ELSE                                                              
054500           IF W-BELISTA-IN = ALL SPACE                                    
054600              MOVE MFS-ALFA-FAELT-FEL TO MOD-BELISTA-IN-ATTR              
054700              PERFORM HI-ERR-CORR-HILITE-FLDS                             
054800              MOVE NEJ TO UPDATE-SW                                       
054900           ELSE                                                           
055000              PERFORM HB-TILLAEGG                                         
055100           END-IF                                                         
055200        END-IF                                                            
055300     ELSE                                                                 
055400        PERFORM HC-KOLLA-TEXT                                             
055500        IF W-FLBORT-IN = NEJ                                              
055600           IF AENDRAD-TEXT                                                
055700              PERFORM HD-AENDRA                                           
055800           ELSE                                                           
055900              PERFORM HE-SAMMA-TEXT                                       
056000           END-IF                                                         
056100        ELSE                                                              
056200           PERFORM HF-TA-BORT                                             
056300           PERFORM IMS-GET-XXKX-KX01                                      
056400           PERFORM IMS-GNP-XXKX-KX11                                      
056500           IF SEGMENT-SAKNAS                                              
056600              PERFORM HG-TA-BORT-ROT                                      
056700           END-IF                                                         
056800        END-IF                                                            
056900     END-IF                                                               
057000                                                                          
057100     IF UPDATE-OK                                                         
057200       PERFORM HH-VISA-INFO                                               
057300       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
057400       CALL WMEDKONV USING MED-WMEDAREA                                   
057500       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
057600       PERFORM MFS-FORM-ATTR                                              
057700       PERFORM MFS-RENSA-FAELT-IN                                         
057800     END-IF                                                               
057900     .                                                                    
058000     EJECT                                                                
058100 HA-SAKNAS SECTION.                                                       
058200                                                                          
058300     MOVE ERR-INF-MISS TO MED-IDMFSFEL                                    
058400     CALL WMEDKONV USING MED-WMEDAREA                                     
058500     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
058600     PERFORM MFS-ROR-EJ-FAELT-UT                                          
058700     PERFORM MFS-ROR-EJ-FAELT-IN                                          
058800     PERFORM MFS-LAS-IN-IGEN                                              
058900     MOVE NEJ TO UPDATE-SW                                                
059000     .                                                                    
059100     SKIP2                                                                
059200 HB-TILLAEGG SECTION.                                                     
059300                                                                          
059400     MOVE W-4473-X          TO 4473-WDGX4473                              
059500     MOVE W-4474-X          TO 4474-WDGX4474                              
059600     MOVE W-BELISTA-IN      TO 4474-BELISTA                               
059700     MOVE W-FLBEST-IN       TO 4474-FLBEST                                
059800     PERFORM IMS-ISRT-XXKX-KX11                                           
059900     MOVE JA TO FLNYRAD                                                   
060000     MOVE W-IDLISTTYP-IN TO MOD-IDLISTTYP-ENTER                           
060100                            W-IDLISTTYP                                   
060200     MOVE W-IDLISTA-IN   TO MOD-IDLISTA-ENTER                             
060300                            W-IDLISTA                                     
060400     .                                                                    
060500     SKIP2                                                                
060600 HC-KOLLA-TEXT SECTION.                                                   
060700                                                                          
060800     IF W-IDLISTTYP-IN = 4474-IDLISTTYP AND                               
060900        W-IDLISTA-IN   = 4474-IDLISTA   AND                               
061000        W-BELISTA-IN   = 4474-BELISTA   AND                               
061100        W-FLBEST-IN    = 4474-FLBEST    AND                               
061200        (MID-FLBORT-IN NOT = JA OR YES)                                   
061300        MOVE NEJ TO TEXTKOLL-SW                                           
061400                    UPDATE-SW                                             
061500     END-IF                                                               
061600     .                                                                    
061700     EJECT                                                                
061800 HD-AENDRA SECTION.                                                       
061900                                                                          
062000     MOVE W-IDLISTTYP-IN TO 4474-IDLISTTYP                                
062100     MOVE W-IDLISTA-IN   TO 4474-IDLISTA                                  
062200     MOVE W-FLBEST-IN    TO 4474-FLBEST                                   
062300     IF W-BELISTA-IN = ALL SPACE                                          
062400        CONTINUE                                                          
062500     ELSE                                                                 
062600        MOVE W-BELISTA-IN   TO 4474-BELISTA                               
062700     END-IF                                                               
062800     PERFORM IMS-REPL-XXKX                                                
062900     MOVE JA TO FLNYRAD                                                   
063000     MOVE W-IDLISTTYP-IN TO MOD-IDLISTTYP-ENTER                           
063100                            W-IDLISTTYP                                   
063200     MOVE W-IDLISTA-IN   TO MOD-IDLISTA-ENTER                             
063300                            W-IDLISTA                                     
063400     .                                                                    
063500     SKIP2                                                                
063600 HE-SAMMA-TEXT SECTION.                                                   
063700                                                                          
063800     MOVE ERR-NO-CHANGE TO MED-IDMFSFEL                                   
063900     CALL WMEDKONV USING MED-WMEDAREA                                     
064000     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
064100     PERFORM MFS-ROR-EJ-FAELT-IN                                          
064200     PERFORM MFS-ROR-EJ-FAELT-UT                                          
064300     PERFORM MFS-LAS-IN-IGEN                                              
064400     .                                                                    
064500     EJECT                                                                
064600 HF-TA-BORT SECTION.                                                      
064700                                                                          
064800     PERFORM IMS-DLET-XXKX-KX11                                           
064900     MOVE MID-IDLISTTYP-ENTER TO  MOD-IDLISTTYP-ENTER                     
065000     MOVE MID-IDLISTA-ENTER   TO  MOD-IDLISTA-ENTER                       
065100     MOVE SPACE TO W-IDLISTTYP                                            
065200     MOVE ZEROS TO W-IDLISTA                                              
065300     .                                                                    
065400     SKIP2                                                                
065500 HG-TA-BORT-ROT SECTION.                                                  
065600                                                                          
065700     PERFORM IMS-GHU-XXKX-KX01                                            
065800     PERFORM IMS-DLET-XXKX-KX01                                           
065900     MOVE NEJ TO UPDATE-SW                                                
066000     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
066100     CALL WMEDKONV USING MED-WMEDAREA                                     
066200     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
066300     PERFORM MFS-FORM-ATTR                                                
066400     PERFORM MFS-RENSA-FAELT-IN                                           
066500     .                                                                    
066600     EJECT                                                                
066700 HH-VISA-INFO SECTION.                                                    
066800                                                                          
066900     PERFORM IMS-GET-XXKX-KX01                                            
067000     MOVE +1 TO INDX                                                      
067100     PERFORM S01-FLYTTA-VAERDEN                                           
067200     PERFORM IMS-GNP-FIRST-XXKX-KX11                                      
067300                                                                          
067400     IF FLNYRAD = JA                                                      
067500        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-LISTA-ATTR(1)                   
067600                                      MOD-FLBEST-UT-ATTR(1)               
067700                                      MOD-BELISTA-UT-ATTR(1)              
067800     END-IF                                                               
067900                                                                          
068000     PERFORM UNTIL INDX > MAX-INDX                                        
068100       IF SEGMENT-FINNS                                                   
068200          PERFORM S01-FLYTTA-VAERDEN                                      
068300          PERFORM IMS-GNP-XXKX-KX11                                       
068400       ELSE                                                               
068500          PERFORM MFS-RENSA-RAD-FAELT-UT                                  
068600       END-IF                                                             
068700       ADD 1 TO INDX                                                      
068800     END-PERFORM                                                          
068900                                                                          
069000     IF SEGMENT-FINNS                                                     
069100       PERFORM HHA-INF-MORE-INFO                                          
069200     ELSE                                                                 
069300       MOVE SPACE TO MOD-IDLISTTYP-NEXT                                   
069400       MOVE SPACE TO MOD-IDLISTA-NEXT                                     
069500     END-IF                                                               
069600     .                                                                    
069700     SKIP2                                                                
069800 HHA-INF-MORE-INFO SECTION.                                               
069900                                                                          
070000     MOVE 4474-IDLISTTYP TO MOD-IDLISTTYP-NEXT                            
070100     MOVE 4474-IDLISTA TO MOD-IDLISTA-NEXT                                
070200     MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                            
070300     CALL WMEDKONV USING MED-WMEDAREA                                     
070400     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
070500     .                                                                    
070600     EJECT                                                                
070700 HI-ERR-CORR-HILITE-FLDS SECTION.                                         
070800                                                                          
070900     MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                            
071000     CALL WMEDKONV USING MED-WMEDAREA                                     
071100     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
071200     PERFORM MFS-ROR-EJ-FAELT-UT                                          
071300     PERFORM MFS-ROR-EJ-FAELT-IN                                          
071400     PERFORM MFS-LAS-IN-IGEN                                              
071500     MOVE MFS-ADD-LAES-IN-FAELT-HI TO                                     
071600              MOD-BELISTA-IN-ATTR                                         
071700     .                                                                    
071800     EJECT                                                                
071900 S01-FLYTTA-VAERDEN SECTION.                                              
072000                                                                          
072100     MOVE 4474-IDLISTTYP TO W-IDLISTTYP-UT                                
072200     MOVE 4474-IDLISTA   TO W-IDLISTA-UT                                  
072300     MOVE W-UTDATA       TO MOD-LISTA (INDX)                              
072400     MOVE 4474-BELISTA   TO MOD-BELISTA-UT (INDX)                         
072500     MOVE 4474-FLBEST    TO MOD-FLBEST-UT (INDX)                          
072600     .                                                                    
072700     EJECT                                                                
072800 MFS-RENSA-FAELT-UT SECTION.                                              
072900                                                                          
073000*    --- ALLA UTDATA-FÄLT                                                 
073100*    --- INKL. BLÄDDRINGSNYCKLAR                                          
073200     MOVE MFS-RENSA-FAELT TO MOD-IDLISTTYP-UT (INDX)                      
073300                             MOD-FILLER-UT (INDX)                         
073400                             MOD-IDLISTA-UT (INDX)                        
073500                             MOD-BELISTA-UT (INDX)                        
073600                             MOD-FLBEST-UT (INDX)                         
073700                             MOD-IDLISTTYP-ENTER                          
073800                             MOD-IDLISTTYP-NEXT                           
073900                             MOD-IDLISTA-ENTER                            
074000                             MOD-IDLISTA-NEXT                             
074100     .                                                                    
074200     SKIP2                                                                
074300 MFS-RENSA-RAD-FAELT-UT  SECTION.                                         
074400                                                                          
074500*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
074600     MOVE MFS-RENSA-FAELT TO MOD-IDLISTTYP-UT (INDX)                      
074700                             MOD-FILLER-UT (INDX)                         
074800                             MOD-IDLISTA-UT (INDX)                        
074900                             MOD-BELISTA-UT (INDX)                        
075000                             MOD-FLBEST-UT  (INDX)                        
075100     .                                                                    
075200     SKIP2                                                                
075300 MFS-RENSA-FAELT-IN SECTION.                                              
075400                                                                          
075500*    --- ALLA INDATA-FÄLT                                                 
075600     MOVE MFS-RENSA-FAELT TO MOD-IDLISTTYP-IN                             
075700                             MOD-IDLISTA-IN                               
075800                             MOD-BELISTA-IN                               
075900                             MOD-FLBEST-IN                                
076000                             MOD-FLBORT-IN                                
076100     .                                                                    
076200     EJECT                                                                
076300 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
076400                                                                          
076500*    --- ALLA UTDATA-FÄLT                                                 
076600*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
076700     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLISTTYP-ENTER                        
076800                               MOD-IDLISTTYP-NEXT                         
076900                               MOD-IDLISTA-ENTER                          
077000                               MOD-IDLISTA-NEXT                           
077100     MOVE +1 TO INDX                                                      
077200     PERFORM UNTIL INDX > MAX-INDX                                        
077300       PERFORM MFS-ROR-EJ-RAD-FAELT-UT                                    
077400       ADD +1 TO INDX                                                     
077500     END-PERFORM                                                          
077600     .                                                                    
077700     SKIP2                                                                
077800 MFS-ROR-EJ-RAD-FAELT-UT  SECTION.                                        
077900                                                                          
078000*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
078100     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLISTTYP-UT (INDX)                    
078200                               MOD-FILLER-UT (INDX)                       
078300                               MOD-IDLISTA-UT (INDX)                      
078400                               MOD-BELISTA-UT (INDX)                      
078500                               MOD-FLBEST-UT  (INDX)                      
078600     .                                                                    
078700     SKIP2                                                                
078800 MFS-ROR-EJ-FAELT-IN  SECTION.                                            
078900                                                                          
079000*    --- ALLA INDATA-FÄLT                                                 
079100     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLISTTYP-IN                           
079200                               MOD-IDLISTA-IN                             
079300                               MOD-BELISTA-IN                             
079400                               MOD-FLBEST-IN                              
079500                               MOD-FLBORT-IN                              
079600                                                                          
079700     .                                                                    
079800     EJECT                                                                
079900 MFS-FORM-ATTR SECTION.                                                   
080000                                                                          
080100*    --- ALLA INDATA-FÄLT                                                 
080200     MOVE MFS-FORMATETS-ATTR TO MOD-IDLISTTYP-IN-ATTR                     
080300                                MOD-IDLISTA-IN-ATTR                       
080400                                MOD-BELISTA-IN-ATTR                       
080500                                MOD-FLBEST-IN-ATTR                        
080600                                MOD-FLBORT-IN-ATTR                        
080700     .                                                                    
080800     SKIP2                                                                
080900 MFS-LAS-IN-IGEN SECTION.                                                 
081000                                                                          
081100*    --- ALLA INDATA-FÄLT                                                 
081200     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLISTTYP-IN-ATTR                  
081300                                   MOD-IDLISTA-IN-ATTR                    
081400                                   MOD-BELISTA-IN-ATTR                    
081500                                   MOD-FLBEST-IN-ATTR                     
081600                                   MOD-FLBORT-IN-ATTR                     
081700     .                                                                    
081800     EJECT                                                                
081900* --- IMS SEKTIONER ---                                                   
082000     SKIP3                                                                
082100 IMS-GET-MSG SECTION.                                                     
082200                                                                          
082300     MOVE '  QC' TO GODK-STATUSKODER                                      
082400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
082500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
082600     PERFORM IMS-STATUSKONTROLL                                           
082700     .                                                                    
082800     SKIP3                                                                
082900 IMS-INSERT-MSG SECTION.                                                  
083000                                                                          
083100     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
083200       MOVE '0' TO MFS-KDHUVOMR                                           
083300     END-IF                                                               
083400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
083500     MOVE SPACE TO GODK-STATUSKODER                                       
083600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
083700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
083800     PERFORM IMS-STATUSKONTROLL                                           
083900     .                                                                    
084000     EJECT                                                                
084100 IMS-GET-XXKX-KX01 SECTION.                                               
084200                                                                          
084300     STRING 'WLXXKX01(WDGXKEY  =' W-4473-X ')'                            
084400          DELIMITED BY SIZE INTO SSA1                                     
084500     MOVE '  GE' TO GODK-STATUSKODER                                      
084600     CALL CBLTDLI USING GU XXKX-PCB DLI-IO-AREA SSA1                      
084700     MOVE XXKX-STATUS-CODE TO STATUS-WS                                   
084800     PERFORM IMS-STATUSKONTROLL                                           
084900     .                                                                    
085000     SKIP2                                                                
085100 IMS-GHU-XXKX-KX01 SECTION.                                               
085200                                                                          
085300     STRING 'WLXXKX01(WDGXKEY  =' W-4473-X ')'                            
085400          DELIMITED BY SIZE INTO SSA1                                     
085500     MOVE '  GE' TO GODK-STATUSKODER                                      
085600     CALL CBLTDLI USING GHU XXKX-PCB DLI-IO-AREA SSA1                     
085700     MOVE XXKX-STATUS-CODE TO STATUS-WS                                   
085800     PERFORM IMS-STATUSKONTROLL                                           
085900     .                                                                    
086000     EJECT                                                                
086100 IMS-ISRT-XXKX-KX01 SECTION.                                              
086200                                                                          
086300     MOVE 'WLXXKX01 ' TO SSA1                                             
086400     MOVE '  II' TO GODK-STATUSKODER                                      
086500     CALL CBLTDLI USING ISRT XXKX-PCB DLI-IO-AREA SSA1                    
086600     MOVE XXKX-STATUS-CODE TO STATUS-WS                                   
086700     PERFORM IMS-STATUSKONTROLL                                           
086800     .                                                                    
086900     SKIP2                                                                
087000 IMS-DLET-XXKX-KX01 SECTION.                                              
087100                                                                          
087200     MOVE '  ' TO GODK-STATUSKODER                                        
087300     CALL CBLTDLI USING DLET XXKX-PCB DLI-IO-AREA                         
087400     MOVE XXKX-STATUS-CODE TO STATUS-WS                                   
087500     PERFORM IMS-STATUSKONTROLL                                           
087600     .                                                                    
087700     EJECT                                                                
087800 IMS-GNP-FIRST-XXKX-KX11 SECTION.                                         
087900                                                                          
088000     STRING 'WLXXKX01(WDGXKEY  =' W-4473-X ')'                            
088100          DELIMITED BY SIZE INTO SSA1                                     
088200     STRING 'WLXXKX11(WDGXKEY =>' W-4474-X ')'                            
088300          DELIMITED BY SIZE INTO SSA2                                     
088400     MOVE '  GE' TO GODK-STATUSKODER                                      
088500     CALL CBLTDLI USING GNP XXKX-PCB DLI-IO-AREA SSA1 SSA2                
088600     MOVE XXKX-STATUS-CODE TO STATUS-WS                                   
088700     PERFORM IMS-STATUSKONTROLL                                           
088800     .                                                                    
088900     SKIP2                                                                
089000 IMS-GNP-XXKX-KX11 SECTION.                                               
089100                                                                          
089200     STRING 'WLXXKX01(WDGXKEY  =' W-4473-X ')'                            
089300          DELIMITED BY SIZE INTO SSA1                                     
089400     MOVE 'WLXXKX11 ' TO SSA2                                             
089500     MOVE '  GE' TO GODK-STATUSKODER                                      
089600     CALL CBLTDLI USING GNP XXKX-PCB DLI-IO-AREA SSA1 SSA2                
089700     MOVE XXKX-STATUS-CODE TO STATUS-WS                                   
089800     PERFORM IMS-STATUSKONTROLL                                           
089900     .                                                                    
090000     SKIP2                                                                
090100                                                                          
090200 IMS-GHU-XXKX-KX11 SECTION.                                               
090300                                                                          
090400     STRING 'WLXXKX11(WDGXKEY  =' W-4474-X ')'                            
090500          DELIMITED BY SIZE INTO SSA1                                     
090600     MOVE '  GE' TO GODK-STATUSKODER                                      
090700     CALL CBLTDLI USING GHU XXKX-PCB DLI-IO-AREA SSA1                     
090800     MOVE XXKX-STATUS-CODE TO STATUS-WS                                   
090900     PERFORM IMS-STATUSKONTROLL                                           
091000     .                                                                    
091100     EJECT                                                                
091200 IMS-ISRT-XXKX-KX11 SECTION.                                              
091300                                                                          
091400     STRING 'WLXXKX01(WDGXKEY  =' W-4473-X ')'                            
091500          DELIMITED BY SIZE INTO SSA1                                     
091600     MOVE 'WLXXKX11 ' TO SSA2                                             
091700     MOVE '  II' TO GODK-STATUSKODER                                      
091800     CALL CBLTDLI USING ISRT XXKX-PCB DLI-IO-AREA SSA1 SSA2               
091900     MOVE XXKX-STATUS-CODE TO STATUS-WS                                   
092000     PERFORM IMS-STATUSKONTROLL                                           
092100     .                                                                    
092200     SKIP2                                                                
092300 IMS-REPL-XXKX SECTION.                                                   
092400                                                                          
092500     MOVE '  ' TO GODK-STATUSKODER                                        
092600     CALL CBLTDLI USING REPL XXKX-PCB DLI-IO-AREA                         
092700     MOVE XXKX-STATUS-CODE TO STATUS-WS                                   
092800     PERFORM IMS-STATUSKONTROLL                                           
092900     .                                                                    
093000     SKIP2                                                                
093100 IMS-DLET-XXKX-KX11 SECTION.                                              
093200                                                                          
093300     MOVE '  ' TO GODK-STATUSKODER                                        
093400     CALL CBLTDLI USING DLET XXKX-PCB DLI-IO-AREA                         
093500     MOVE XXKX-STATUS-CODE TO STATUS-WS                                   
093600     PERFORM IMS-STATUSKONTROLL                                           
093700     .                                                                    
093800     EJECT                                                                
093900 IMS-STATUSKONTROLL SECTION.                                              
094000                                                                          
094100     SET STATUS-IX TO 1                                                   
094200     SEARCH GODK-STATUS                                                   
094300       AT END CALL FELLOG                                                 
094400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
094500     END-SEARCH                                                           
094600     .                                                                    
