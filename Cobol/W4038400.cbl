000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4038400.                                                
000400 AUTHOR.         CAO-VAN NGU.                                             
000500 DATE-WRITTEN.   90/02/07.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    DISPLAY AND UPDATE WDGX4448 FOR IDPRC/KDPRCGRP/KDPRODKL              
001000*    INDATA.                                                              
001100*        TRANSAKTION: W4T384                                              
001200*        MID:         W4I38401                                            
001300*    UTDATA.                                                              
001400*        MOD:         W4O38401                                            
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     EJECT                                                                
001900 DATA DIVISION.                                                           
002000 WORKING-STORAGE SECTION.                                                 
002001                                                                          
002010*    -- CHECKED BY WY2000                                                 
002100 77  IDPGM                       PIC X(08)   VALUE 'W4038400'.            
002200                                                                          
002300 77  JA                          PIC X       VALUE 'J'.                   
002400 77  NEJ                         PIC X       VALUE 'N'.                   
002500                                                                          
002600*    --- INDEX FÖR BLÄDDRINGSRADER                                        
002700 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
002800 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
002900                                                                          
003000 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +738  COMP SYNC.        
003200                                                                          
003300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003400 77  WS-KDPRCGRP                 PIC X(5)    VALUE SPACE.                 
003500 77  WS-IDPRC                    PIC X(4)    VALUE SPACE.                 
003600 77  WS-KDPRODKL                 PIC X(1)    VALUE SPACE.                 
003700                                                                          
003800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
003900     88  INDATA-OK                           VALUE 'J'.                   
004000     88  INDATA-FEL                          VALUE 'N'.                   
004100                                                                          
004200 77  PAGE-SW                     PIC X       VALUE 'J'.                   
004300     88  PAGE-LAST                           VALUE 'J'.                   
004400     88  PAGE-NO                             VALUE 'N'.                   
004500                                                                          
004600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004700     88  NYCKLAR-OK                          VALUE 'J'.                   
004800     88  NYCKLAR-FEL                         VALUE 'N'.                   
004900                                                                          
005000 77  IDPRC-SW                    PIC X       VALUE 'N'.                   
005100     88  IDPRC-FINNS                         VALUE 'J'.                   
005200                                                                          
005300 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005400     88  ALLT-OK                             VALUE 'J'.                   
005500                                                                          
005600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005700     88  EGEN-MID                            VALUE '4384'.                
005800     88  GODK-MID                            VALUE '4381'                 
005900                                                   '4382'                 
006000                                                   '4384'.                
006100*      --- VALID IDDC CODES                                               
006110*                                                                         
006120*01    -COPY WWDC99                                                       
006130       EJECT                                                              
006200*                   CONVERION OF MID-INPUT                                
006300 01  FILLER.                                                              
006400     05  WS-MID-KVARBTID         PIC S9(2)V9(1) COMP-3 VALUE +0.          
006500     05  WS-MID-KVBEMAN-ORD      PIC S9(2)V9(1) COMP-3 VALUE +0.          
006600     05  WS-MID-KVBEMAN-EXT      PIC S9(2)V9(1) COMP-3 VALUE +0.          
006700*                                                                         
006800*            IDPRCBAS FROM WDGX4448                                       
006900*                                                                         
007000     05  WS-DB-IDPRCBAS-01       PIC X(3) VALUE SPACES.                   
007100     05  WS-DB-IDPRCBAS-02       PIC X(3) VALUE SPACES.                   
007200     EJECT                                                                
007300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007400 01  GENERELLA-SUBPROGRAM.                                                
007500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007600     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
007700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007810     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008100*   -COPY WMEDAREA                                                        
008200     EJECT                                                                
008300*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
008400*   -COPY WDECAREA                                                        
008410     EJECT                                                                
008420*                   ****    PARAMETRAR TILL W005INIT                      
008430*01  -COPY WMSGINIT                                                       
008500     EJECT                                                                
008600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008700*                                                                         
008800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008900     SKIP3                                                                
009000*01  MID -COPY W4I38401                                                   
009100     EJECT                                                                
009200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009300     SKIP3                                                                
009400*01  -COPY WMSGAREA                                                       
009500     EJECT                                                                
009600*    03  MOD -COPY W4O38401   -RED MSG-AREA.                              
009700     EJECT                                                                
009800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009900     SKIP3                                                                
010000*01  -COPY WMFSAREA                                                       
010100     EJECT                                                                
010200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010300*                                                                         
010400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010500     SKIP3                                                                
010600*                                                                         
010700 01  NYCKLAR-TILL-DLI.                                                    
010800*                                                                         
010900     03  W-4447-WDGXKEY.                                                  
011000         07 W-4447-IDHTYP        PIC X(4)    VALUE '4447'.                
011100         07 W-4447-IDDC          PIC X(02).                               
011200         07 W-4447-LOW-VALUE     PIC X(24)   VALUE LOW-VALUE.             
011300*                                                                         
011400     03  W-4448-WDGXKEY.                                                  
011500         07 W-4448-IDPRC         PIC X(4)    VALUE SPACE.                 
011600         07 W-4448-LOW-VALUE     PIC X(1)    VALUE LOW-VALUE.             
011700*                                                                         
011800*    --- STATUS-KOD FRÅN IMS                                              
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FINNS                       VALUE '  '.                  
012100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012300     SKIP2                                                                
012400 01  GODK-STATUSKODER.                                                    
012500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012600     SKIP3                                                                
012700 01  SSA1                        PIC X(64).                               
012800 01  SSA2                        PIC X(64).                               
012900     EJECT                                                                
013000*    --- IMS FUNKTIONSKODER                                               
013100*01  -COPY W0003                                                          
013200     EJECT                                                                
013300*    ---  DLI INPUT-OUTPUT AREA                                           
013400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013500     SKIP3                                                                
013600 01  DLI-IO-AREA.                                                         
013700     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
013800     SKIP3                                                                
013900     03  WLXXKH01 REDEFINES IO-AREA.                                      
014000*        05  -COPY WDGX4447   -PRE XXKH-                                  
014100     EJECT                                                                
014200     03  WLXXKH11 REDEFINES IO-AREA.                                      
014300*        05  -COPY WDGX4448   -PRE XXKH-                                  
014400     EJECT                                                                
014500 LINKAGE SECTION.                                                         
014600                                                                          
014700*01  -COPY W0009      -PRE MSG-                                           
014710     EJECT                                                                
014720*01  -COPY W0008     -PRE USEA-                                           
014730     05  FILLER              PIC X.                                       
014800     EJECT                                                                
014900*01  -COPY W0008      -PRE XXKH-                                          
015000     05  FILLER                  PIC X.                                   
015100     EJECT                                                                
015200 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
015210                                   XXKH-PCB.                              
015300                                                                          
015400 W40384 SECTION.                                                          
015500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
015510                                   XXKH-PCB.                              
015600     PERFORM IMS-GET-MSG                                                  
015700     IF SEGMENT-FINNS                                                     
015800       PERFORM A-INIT                                                     
015900       PERFORM B-KOLLA-NYCKLAR                                            
016000       IF NYCKLAR-OK                                                      
016100         IF MFS-UPDATE                                                    
016200           PERFORM G-KOLLA-INPUT                                          
016300           IF INDATA-OK                                                   
016400             PERFORM H-UPPDATERA                                          
016500           END-IF                                                         
016600         ELSE                                                             
016700           IF MFS-FIRST                                                   
016800             PERFORM C-FOERSTA-SIDA                                       
016900           ELSE                                                           
017000             IF MFS-NEXT                                                  
017100               PERFORM D-NAESTA-SIDA                                      
017200             ELSE                                                         
017300               PERFORM E-SAMMA-SIDA                                       
017400             END-IF                                                       
017500           END-IF                                                         
017600         END-IF                                                           
017700           IF ALLT-OK                                                     
017800             PERFORM F-LAES-VISA-INFO                                     
017900           END-IF                                                         
018000       END-IF                                                             
018100       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
018200       PERFORM IMS-INSERT-MSG                                             
018300     END-IF                                                               
018400     MOVE ZERO TO RETURN-CODE                                             
018500     GOBACK                                                               
018600     .                                                                    
018700     EJECT                                                                
018800 A-INIT SECTION.                                                          
018900                                                                          
019000     IF MSG-DUBBLA-TRANSKODER                                             
019100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I38401                 
019200       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
019300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
019400     ELSE                                                                 
019500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I38401                  
019600       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
019700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
019800     END-IF                                                               
019900                                                                          
020000     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
020100     MOVE MSG-IDPFK TO MFS-IDPFK                                          
020200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
020300                                                                          
020400     MOVE LOW-VALUE TO MSG-AREA                                           
020500     MOVE '4384' TO MOD-IDTRANS                                           
020600     MOVE 'W4O384N1' TO MFS-IDMOD                                         
020700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
020800                                                                          
020900     IF NOT EGEN-MID                                                      
021000       MOVE SPACE TO MFS-KDTRTYP                                          
021100       MOVE '7' TO MFS-IDPFK                                              
021200     END-IF                                                               
021300                                                                          
021400     IF ENGLISH-TEXT                                                      
021600       MOVE 'B  ' TO MED-IDSKYLT                                          
021700     ELSE                                                                 
021900       MOVE 'S  ' TO MED-IDSKYLT                                          
022000     END-IF                                                               
022200     .                                                                    
022300     EJECT                                                                
022400 B-KOLLA-NYCKLAR SECTION.                                                 
022410                                                                          
022420     MOVE ALL '+'           TO MSGI-WMSGINIT                              
022430     MOVE '001'             TO MSGI-KDCALL                                
022440     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
022450     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
022500                                                                          
022600     MOVE JA  TO NYCKLAR-SW                                               
022700     MOVE MFS-RENSA-FAELT TO MOD-KDPRCGRP-IN                              
022800                             MOD-KDPRODKL-IN                              
022900                             MOD-IDPRC-IN                                 
022910                             MOD-IDDC-IN                                  
023000                                                                          
023100     IF MID-KDPRODKL-IN = ALL '+'                                         
023200       MOVE MID-KDPRODKL-UT  TO WS-KDPRODKL                               
023300     ELSE                                                                 
023400       MOVE MID-KDPRODKL-IN TO WS-KDPRODKL                                
023500       MOVE '7'             TO MFS-IDPFK                                  
023600       MOVE SPACE           TO MFS-KDTRTYP                                
023700     END-IF                                                               
023800                                                                          
023900     IF WS-KDPRODKL ALPHABETIC                                            
024000        OR                                                                
024100        WS-KDPRODKL = SPACE                                               
024200        CONTINUE                                                          
024300     ELSE                                                                 
024400        MOVE NEJ TO NYCKLAR-SW                                            
024500     END-IF                                                               
024600                                                                          
024700     IF MID-KDPRCGRP-IN = ALL '+'                                         
024800       MOVE MID-KDPRCGRP-UT  TO WS-KDPRCGRP                               
024900     ELSE                                                                 
025000       MOVE MID-KDPRCGRP-IN TO WS-KDPRCGRP                                
025100       MOVE '7'             TO MFS-IDPFK                                  
025200       MOVE SPACE           TO MFS-KDTRTYP                                
025300     END-IF                                                               
025400                                                                          
025500     IF WS-KDPRCGRP NOT NUMERIC                                           
025600        MOVE SPACE          TO WS-KDPRCGRP                                
025700     END-IF                                                               
025800                                                                          
025900     IF MID-IDPRC-IN = ALL '+'                                            
026000       MOVE MID-IDPRC-UT  TO WS-IDPRC                                     
026100     ELSE                                                                 
026200       MOVE MID-IDPRC-IN TO WS-IDPRC                                      
026300       MOVE '7'          TO MFS-IDPFK                                     
026400       MOVE SPACE        TO MFS-KDTRTYP                                   
026500     END-IF                                                               
026501                                                                          
026502     MOVE MSGI-IDDC               TO WS-IDDC                              
026503                                                                          
026597     IF WS-IDDC IS > SPACE                                                
026598       MOVE WS-IDDC                       TO W-4447-IDDC                  
026603     ELSE                                                                 
026604       MOVE NEJ                           TO NYCKLAR-SW                   
026606     END-IF                                                               
026610                                                                          
026700     IF WS-IDPRC (1:3) NOT NUMERIC                                        
026800        MOVE SPACE          TO WS-IDPRC                                   
026900     END-IF                                                               
027200                                                                          
027300     IF (WS-KDPRCGRP = SPACES) AND (WS-IDPRC = SPACES)                    
027400        MOVE NEJ TO NYCKLAR-SW                                            
027500     END-IF                                                               
027510                                                                          
027600     IF (WS-KDPRCGRP NOT = SPACES)                                        
027700       IF (WS-IDPRC NOT = SPACES)                                         
027800          MOVE NEJ TO NYCKLAR-SW                                          
027900       END-IF                                                             
028000     END-IF                                                               
028100                                                                          
028200     IF GODK-MID OR NYCKLAR-OK                                            
028300         MOVE WS-KDPRODKL     TO MOD-KDPRODKL-UT                          
028400         MOVE WS-KDPRCGRP     TO MOD-KDPRCGRP-UT                          
028410         INSPECT MOD-KDPRCGRP-UT REPLACING LEADING ZERO BY SPACE          
028500         MOVE WS-IDPRC        TO MOD-IDPRC-UT                             
028501         INSPECT MOD-IDPRC-UT REPLACING LEADING ZERO BY SPACE             
028510         MOVE WS-IDDC         TO MOD-IDDC-UT                              
028600     ELSE                                                                 
028700         MOVE MFS-RENSA-FAELT TO MOD-KDPRODKL-UT                          
028800                                 MOD-KDPRCGRP-UT                          
028900                                 MOD-IDPRC-UT                             
028910                                 MOD-IDDC-UT                              
029000     END-IF                                                               
029100                                                                          
029200     IF NYCKLAR-FEL                                                       
029300       MOVE '401' TO MED-IDMFSFEL                                         
029400       CALL WMEDKONV USING MED-WMEDAREA                                   
029500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
029600       PERFORM MFS-RENSA-FAELT-IN                                         
029700       PERFORM MFS-RENSA-FAELT-UT                                         
029800     END-IF                                                               
029900     .                                                                    
030000     EJECT                                                                
030100 C-FOERSTA-SIDA SECTION.                                                  
030200                                                                          
030300     MOVE '006' TO MED-IDMFSFEL                                           
030400     CALL WMEDKONV USING MED-WMEDAREA                                     
030500     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
030600     PERFORM MFS-RENSA-FAELT-IN                                           
030700     PERFORM MFS-RENSA-FAELT-UT                                           
030800     MOVE JA TO ALLT-SW                                                   
030900     .                                                                    
031000     EJECT                                                                
031100 D-NAESTA-SIDA SECTION.                                                   
031200                                                                          
031300     IF MID-IDPRC-PF8 = 'SLUT'                                            
031400        MOVE '115' TO MED-IDMFSFEL                                        
031500        CALL WMEDKONV USING MED-WMEDAREA                                  
031600        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
031700        PERFORM MFS-RENSA-FAELT-IN                                        
031800        MOVE 1 TO INDX                                                    
031900        PERFORM UNTIL INDX > MAX-INDX                                     
032000           PERFORM MFS-ERASE-LINE                                         
032100           ADD 1 TO INDX                                                  
032200        END-PERFORM                                                       
032300        MOVE 'SLUT' TO MOD-IDPRC-PF8                                      
032400        MOVE NEJ TO ALLT-SW                                               
032500     ELSE                                                                 
032600        MOVE MID-IDPRC-PF8     TO W-4448-IDPRC                            
032700        MOVE MID-IDPRC-PF8     TO WS-DB-IDPRCBAS-01                       
032800        MOVE JA TO ALLT-SW                                                
032900     END-IF                                                               
033000     .                                                                    
033100     EJECT                                                                
033200                                                                          
033300 E-SAMMA-SIDA SECTION.                                                    
033400                                                                          
033500     IF MID-INPUT            = ALL '+'                                    
033600       MOVE MID-IDPRC-PFE    TO W-4448-IDPRC                              
033700       MOVE MID-IDPRC-PFE    TO WS-DB-IDPRCBAS-01                         
033800       MOVE JA               TO ALLT-SW                                   
033900     ELSE                                                                 
034000       MOVE NEJ TO ALLT-SW                                                
034100       MOVE '003' TO MED-IDMFSFEL                                         
034200       CALL WMEDKONV USING MED-WMEDAREA                                   
034300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
034400       PERFORM MFS-ROR-EJ-FAELT-IN                                        
034500       PERFORM MFS-ROR-EJ-FAELT-UT                                        
034600       PERFORM MFS-LAS-IN-IGEN                                            
034700     END-IF                                                               
034800     .                                                                    
034900     EJECT                                                                
035000 F-LAES-VISA-INFO SECTION.                                                
035100                                                                          
035200     PERFORM FA-GET-WDGX4448                                              
035300                                                                          
035400     IF SEGMENT-SAKNAS                                                    
035500        PERFORM FB-WRONG-DATA                                             
035600     ELSE                                                                 
035700        MOVE XXKH-4448-IDPRC      TO MOD-IDPRC-PFE                        
035800                                     MOD-IDPRC-PF8                        
035900                                                                          
036000       MOVE +1            TO INDX                                         
036100       PERFORM UNTIL INDX > MAX-INDX                                      
036200           IF SEGMENT-FINNS                                               
036300               IF WS-KDPRODKL  = SPACE OR                                 
036400                  WS-KDPRODKL  = XXKH-4448-KDPRODKL                       
036500                   PERFORM FF-FILL-SCRLINE                                
036600                   ADD 1 TO INDX                                          
036700               END-IF                                                     
036800               IF WS-IDPRC NOT = SPACES                                   
036900                   MOVE 'GE'   TO   STATUS-WS                             
037000                ELSE                                                      
037100                   PERFORM IMS-GET-XXKH-WDGX4448                          
037200               END-IF                                                     
037300            ELSE                                                          
037400               PERFORM MFS-ERASE-LINE                                     
037500               ADD 1 TO INDX                                              
037600           END-IF                                                         
037700       END-PERFORM                                                        
037800                                                                          
037900       IF SEGMENT-FINNS                                                   
038000           MOVE '105'              TO MED-IDMFSINF                        
038100           CALL WMEDKONV USING MED-WMEDAREA                               
038200           MOVE MED-MFSINF         TO MOD-TEMFSINF                        
038300           MOVE XXKH-4448-IDPRC    TO MOD-IDPRC-PF8                       
038400       ELSE                                                               
038500           MOVE '106'              TO MED-IDMFSINF                        
038600           CALL WMEDKONV USING MED-WMEDAREA                               
038700           MOVE MED-MFSINF         TO MOD-TEMFSINF                        
038800           MOVE MFS-RENSA-FAELT    TO MOD-TEMFSFEL                        
038900           MOVE 'SLUT'             TO MOD-IDPRC-PF8                       
039000       END-IF                                                             
039100     END-IF                                                               
039200     .                                                                    
039300     EJECT                                                                
039400 FA-GET-WDGX4448 SECTION.                                                 
039500                                                                          
039600      IF WS-IDPRC = SPACES                                                
039700         PERFORM IMS-GET-XXKH-WDGX4447                                    
039710         IF SEGMENT-FINNS                                                 
039800           IF MFS-FIRST                                                   
039810              PERFORM IMS-GET-XXKH-WDGX4448                               
039820           ELSE                                                           
039830              PERFORM IMS-LST-XXKH-WDGX4448                               
039840           END-IF                                                         
039850         END-IF                                                           
039900      ELSE                                                                
039910         MOVE WS-IDPRC TO W-4448-IDPRC                                    
039920         PERFORM IMS-GU-XXKH-WDGX4448                                     
040600      END-IF                                                              
040700     .                                                                    
040800     EJECT                                                                
040900 FB-WRONG-DATA SECTION.                                                   
040910                                                                          
041000     IF MFS-UPDATE                                                        
041100        MOVE '106' TO MED-IDMFSFEL                                        
041200     ELSE                                                                 
041300        MOVE '413' TO MED-IDMFSFEL                                        
041400     END-IF                                                               
041500     CALL WMEDKONV USING MED-WMEDAREA                                     
041600     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
041700     PERFORM MFS-RENSA-FAELT-UT                                           
041800     .                                                                    
041900     EJECT                                                                
042000*FC-GET-DATA-WDGX4448 SECTION.                                            
042100*    PERFORM UNTIL (WS-KDPRODKL = SPACE) OR                               
042200*                   (XXKH-4448-KDPRODKL = WS-KDPRODKL)                    
042300*                            OR (SEGMENT-SAKNAS)                          
042400*        PERFORM IMS-GET-XXKH-WDGX4448                                    
042500*    END-PERFORM                                                          
042600*    .                                                                    
042700     EJECT                                                                
042800 FF-FILL-SCRLINE SECTION.                                                 
042900                                                                          
043000     MOVE XXKH-4448-IDPRC       TO MOD-IDPRC-RAD (INDX)                   
043100     MOVE XXKH-4448-BEPRC       TO MOD-BEPRC-RAD (INDX)                   
043200     MOVE XXKH-4448-KDPRODKL    TO MOD-KDPRODKL-RAD (INDX)                
043300     MOVE XXKH-4448-KVARBTID    TO MOD-KVARBTID-RAD (INDX)                
043400     MOVE XXKH-4448-KVBEMAN-ORD TO                                        
043500                                MOD-KVBEMAN-ORD-RAD (INDX)                
043600     MOVE XXKH-4448-KVBEMAN-EXT TO                                        
043700                                 MOD-KVBEMAN-EXT-RAD (INDX)               
043800     .                                                                    
043900     EJECT                                                                
044000                                                                          
044100 G-KOLLA-INPUT SECTION.                                                   
044200                                                                          
044300     MOVE JA  TO INDATA-SW                                                
044400     IF MID-INPUT = ALL '+'                                               
044500       PERFORM GA-WRONG-DATA                                              
044600     ELSE                                                                 
044700       PERFORM GB-IDPRC                                                   
044800       PERFORM GC-KVARBTID                                                
044900       PERFORM GD-KVBEMAN-ORD                                             
045000       PERFORM GE-KVBEMAN-EXT                                             
045100       IF INDATA-FEL                                                      
045200          MOVE '001' TO MED-IDMFSFEL                                      
045300          CALL WMEDKONV USING MED-WMEDAREA                                
045400          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
045500          PERFORM MFS-ROR-EJ-FAELT-UT                                     
045600          PERFORM MFS-ROR-EJ-FAELT-IN                                     
045700          MOVE NEJ TO ALLT-SW                                             
045800       END-IF                                                             
045900     END-IF                                                               
046000     .                                                                    
046100     EJECT                                                                
046200 GA-WRONG-DATA SECTION.                                                   
046300                                                                          
046400       MOVE '011' TO MED-IDMFSFEL                                         
046500       CALL WMEDKONV USING MED-WMEDAREA                                   
046600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
046700       PERFORM MFS-ROR-EJ-FAELT-IN                                        
046800       PERFORM MFS-ROR-EJ-FAELT-UT                                        
046900       MOVE NEJ TO INDATA-SW                                              
047000     .                                                                    
047100     EJECT                                                                
047200 GB-IDPRC        SECTION.                                                 
047300                                                                          
047400     MOVE NEJ       TO IDPRC-SW                                           
047500     MOVE +1        TO INDX                                               
047600     PERFORM UNTIL INDX > MAX-INDX OR                                     
047700                   IDPRC-FINNS                                            
047800         IF MID-IDPRC-RAD(INDX) =  MID-IDPRC                              
047900             MOVE JA            TO IDPRC-SW                               
048000          ELSE                                                            
048100             ADD +1             TO INDX                                   
048200         END-IF                                                           
048300     END-PERFORM                                                          
048400                                                                          
048500     MOVE MID-IDPRC TO W-4448-IDPRC                                       
048600     PERFORM IMS-GU-XXKH-WDGX4448                                         
048700     IF SEGMENT-FINNS  AND IDPRC-FINNS                                    
048800        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPRC-ATTR                       
048900     ELSE                                                                 
049000        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPRC-ATTR                         
049100        MOVE NEJ TO INDATA-SW                                             
049200     END-IF                                                               
049300     .                                                                    
049400     EJECT                                                                
049500 GC-KVARBTID SECTION.                                                     
049600                                                                          
049700     IF MID-KVARBTID NOT = ALL '+'                                        
049800        MOVE MID-KVARBTID TO DEC-IDFRIDATA                                
049900        PERFORM S00-CALL-WDECEDIT                                         
050000        IF DEC-KDSVAR-FEL                                                 
050100           MOVE +0  TO  WS-MID-KVARBTID                                   
050200           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVARBTID-ATTR                   
050300           MOVE NEJ TO INDATA-SW                                          
050400        ELSE                                                              
050500           MOVE DEC-IDEDITDATA      TO WS-MID-KVARBTID                    
050600           IF WS-MID-KVARBTID       > 24                                  
050700               MOVE MFS-ALFA-FAELT-FEL TO MOD-KVARBTID-ATTR               
050800               MOVE NEJ TO INDATA-SW                                      
050900            ELSE                                                          
051000               MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVARBTID-ATTR             
051010            END-IF                                                        
051100        END-IF                                                            
051200     ELSE                                                                 
051300        IF SEGMENT-FINNS                                                  
051400           MOVE XXKH-4448-KVARBTID    TO WS-MID-KVARBTID                  
051500        END-IF                                                            
051600     END-IF                                                               
051700     .                                                                    
051800     EJECT                                                                
051900 GD-KVBEMAN-ORD SECTION.                                                  
052000                                                                          
052100     IF MID-KVBEMAN-ORD NOT = ALL '+'                                     
052200        MOVE MID-KVBEMAN-ORD TO DEC-IDFRIDATA                             
052300        PERFORM S00-CALL-WDECEDIT                                         
052400        IF DEC-KDSVAR-FEL                                                 
052500           MOVE +0                TO WS-MID-KVBEMAN-ORD                   
052600           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVBEMAN-ORD-ATTR                
052700           MOVE NEJ TO INDATA-SW                                          
052800        ELSE                                                              
052900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVBEMAN-ORD-ATTR              
053000           MOVE DEC-IDEDITDATA      TO WS-MID-KVBEMAN-ORD                 
053100        END-IF                                                            
053200     ELSE                                                                 
053300        IF SEGMENT-FINNS                                                  
053400           MOVE XXKH-4448-KVBEMAN-ORD TO WS-MID-KVBEMAN-ORD               
053500        END-IF                                                            
053600     END-IF                                                               
053700     .                                                                    
053800     EJECT                                                                
053900 GE-KVBEMAN-EXT SECTION.                                                  
054000                                                                          
054100     IF MID-KVBEMAN-EXT NOT = ALL '+'                                     
054200        MOVE MID-KVBEMAN-EXT TO DEC-IDFRIDATA                             
054300        PERFORM S00-CALL-WDECEDIT                                         
054400        IF DEC-KDSVAR-FEL                                                 
054500           MOVE +0                TO WS-MID-KVBEMAN-EXT                   
054600           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVBEMAN-EXT-ATTR                
054700           MOVE NEJ TO INDATA-SW                                          
054800         ELSE                                                             
054900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVBEMAN-EXT-ATTR              
055000           MOVE DEC-IDEDITDATA      TO WS-MID-KVBEMAN-EXT                 
055100         END-IF                                                           
055200     ELSE                                                                 
055300         IF SEGMENT-FINNS                                                 
055400            MOVE XXKH-4448-KVBEMAN-EXT TO WS-MID-KVBEMAN-EXT              
055500         END-IF                                                           
055600     END-IF                                                               
055700     .                                                                    
055800     EJECT                                                                
055900*                                                                         
056000 H-UPPDATERA SECTION.                                                     
056100*                                                                         
056200     PERFORM IMS-GET-XXKH-WDGX4447                                        
056300     IF SEGMENT-FINNS                                                     
056400       MOVE MID-IDPRC TO  W-4448-IDPRC                                    
056500       PERFORM IMS-GHU-XXKH-WDGX4448                                      
056600       IF SEGMENT-FINNS                                                   
056700           MOVE WS-MID-KVARBTID TO XXKH-4448-KVARBTID                     
056800           MOVE WS-MID-KVBEMAN-ORD TO XXKH-4448-KVBEMAN-ORD               
056900           MOVE WS-MID-KVBEMAN-EXT TO XXKH-4448-KVBEMAN-EXT               
057000           PERFORM IMS-REPL-XXKH                                          
057100           IF SEGMENT-FINNS                                               
057200              MOVE '101' TO MED-IDMFSINF                                  
057300              CALL WMEDKONV USING MED-WMEDAREA                            
057400              MOVE MED-MFSINF TO MOD-TEMFSINF                             
057500              PERFORM MFS-FORM-ATTR                                       
057600              PERFORM MFS-RENSA-FAELT-IN                                  
057700              MOVE XXKH-4448-IDPRC TO  WS-DB-IDPRCBAS-01                  
057800              PERFORM HA-TO-TOPLINE                                       
057900              MOVE JA TO ALLT-SW                                          
058000           END-IF                                                         
058100       ELSE                                                               
058200           MOVE '413' TO MED-IDMFSFEL                                     
058300           CALL WMEDKONV USING MED-WMEDAREA                               
058400           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
058500           PERFORM MFS-RENSA-FAELT-IN                                     
058600           PERFORM MFS-RENSA-FAELT-UT                                     
058700       END-IF                                                             
058800     END-IF                                                               
058900     .                                                                    
059000     EJECT                                                                
059100 HA-TO-TOPLINE SECTION.                                                   
059200                                                                          
059300*    MOVE XXKH-4448-IDPRC    TO MOD-IDPRC-RAD (1)                         
059400*    MOVE XXKH-4448-BEPRC    TO MOD-BEPRC-RAD (1)                         
059500*    MOVE XXKH-4448-KDPRODKL TO MOD-KDPRODKL-RAD (1)                      
059600*    MOVE XXKH-4448-KVARBTID TO MOD-KVARBTID-RAD (1)                      
059700*    MOVE XXKH-4448-KVBEMAN-ORD TO                                        
059800*                            MOD-KVBEMAN-ORD-RAD (1)                      
059900*    MOVE XXKH-4448-KVBEMAN-EXT TO                                        
060000*                            MOD-KVBEMAN-EXT-RAD (1)                      
060100     MOVE SPACES            TO MOD-IDPRC                                  
060200                               MOD-KVARBTID                               
060300                               MOD-KVBEMAN-ORD                            
060400                               MOD-KVBEMAN-EXT                            
060500                                                                          
060600     IF MID-KVARBTID            = ALL '+'                                 
060700         CONTINUE                                                         
060800      ELSE                                                                
060900         MOVE MFS-ADD-HILIGHT-FIELD TO                                    
061000                           MOD-KVARBTID-RAD-ATTR (1)                      
061100     END-IF                                                               
061200                                                                          
061300     IF MID-KVBEMAN-ORD         = ALL '+'                                 
061400         CONTINUE                                                         
061500      ELSE                                                                
061600         MOVE MFS-ADD-HILIGHT-FIELD TO                                    
061700                          MOD-KVBEMAN-ORD-RAD-ATTR (1)                    
061800     END-IF                                                               
061900                                                                          
062000     IF MID-KVBEMAN-EXT         = ALL '+'                                 
062100         CONTINUE                                                         
062200      ELSE                                                                
062300         MOVE MFS-ADD-HILIGHT-FIELD TO                                    
062400                          MOD-KVBEMAN-EXT-RAD-ATTR (1)                    
062500     END-IF                                                               
062600     .                                                                    
062700     EJECT                                                                
062800 S00-CALL-WDECEDIT SECTION.                                               
062900     MOVE +2  TO DEC-KVHELTAL                                             
063000     MOVE +1  TO DEC-KVDECIMAL                                            
063100     CALL WDECEDIT USING DEC-WDECAREA                                     
063200     .                                                                    
063300     EJECT                                                                
063400 MFS-ERASE-LINE SECTION.                                                  
063500     MOVE MFS-RENSA-FAELT TO MOD-IDPRC-RAD (INDX)                         
063600                             MOD-BEPRC-RAD (INDX)                         
063700                             MOD-KDPRODKL-RAD (INDX)                      
063800                             MOD-KVARBTID-RAD (INDX)                      
063900                             MOD-KVBEMAN-ORD-RAD (INDX)                   
064000                             MOD-KVBEMAN-EXT-RAD (INDX)                   
064100     .                                                                    
064200     EJECT                                                                
064300 MFS-RENSA-FAELT-UT SECTION.                                              
064400                                                                          
064500     MOVE MFS-RENSA-FAELT TO MOD-IDPRC-PF8                                
064600                             MOD-IDPRC-PFE                                
064700     .                                                                    
064800*                                                                         
064900     SKIP2                                                                
065000 MFS-RENSA-FAELT-IN SECTION.                                              
065100                                                                          
065200     MOVE MFS-RENSA-FAELT TO MOD-IDPRC                                    
065300                             MOD-KVARBTID                                 
065400                             MOD-KVBEMAN-ORD                              
065500                             MOD-KVBEMAN-EXT                              
065600                             MOD-IDPRC-IN                                 
065700                             MOD-KDPRCGRP-IN                              
065800                             MOD-KDPRODKL-IN                              
065810                             MOD-IDDC-IN                                  
065900     .                                                                    
066000     EJECT                                                                
066100 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
066200                                                                          
066300*    --- ALLA UTDATA-FÄLT                                                 
066400*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
066500     MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRCGRP-UT                            
066600                               MOD-KDPRODKL-UT                            
066700                               MOD-IDPRC-UT                               
066710                               MOD-IDDC-UT                                
066800                               MOD-IDPRC-PFE                              
066900                               MOD-IDPRC-PF8                              
067000*                                                                         
067100*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
067200     MOVE +1 TO INDX                                                      
067300     PERFORM UNTIL INDX > MAX-INDX                                        
067400     MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRC-RAD (INDX)                       
067500                               MOD-BEPRC-RAD (INDX)                       
067600                               MOD-KVARBTID-RAD (INDX)                    
067700                               MOD-KDPRODKL-RAD (INDX)                    
067800                               MOD-KVBEMAN-ORD-RAD (INDX)                 
067900                               MOD-KVBEMAN-EXT-RAD (INDX)                 
068000       ADD +1 TO INDX                                                     
068100     END-PERFORM                                                          
068200     .                                                                    
068300     SKIP2                                                                
068400*                                                                         
068500 MFS-ROR-EJ-FAELT-IN  SECTION.                                            
068600*                                                                         
068700*    --- ALLA INDATA-FÄLT                                                 
068800     MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRCGRP-IN                            
068900                               MOD-KDPRODKL-IN                            
069000                               MOD-IDPRC-IN                               
069010                               MOD-IDDC-IN                                
069100     MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRC                                  
069200                               MOD-KVARBTID                               
069300                               MOD-KVBEMAN-ORD                            
069400                               MOD-KVBEMAN-EXT                            
069500     .                                                                    
069600     EJECT                                                                
069700 MFS-FORM-ATTR SECTION.                                                   
069800                                                                          
069900*    --- ALLA INDATA-FÄLT                                                 
070000     MOVE MFS-FORMATETS-ATTR TO MOD-IDPRC-ATTR                            
070100                                MOD-KVARBTID-ATTR                         
070200                                MOD-KVBEMAN-ORD-ATTR                      
070300                                MOD-KVBEMAN-EXT-ATTR                      
070400     .                                                                    
070500     SKIP2                                                                
070600 MFS-LAS-IN-IGEN SECTION.                                                 
070700                                                                          
070800*    --- ALLA INDATA-FÄLT                                                 
070900*    ------------------------------ ATTRIBUTE OF INPUT FIELDS             
071000     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRC-ATTR                         
071100                                   MOD-KVARBTID-ATTR                      
071200                                   MOD-KVBEMAN-ORD-ATTR                   
071300                                   MOD-KVBEMAN-EXT-ATTR                   
071400     .                                                                    
071500     EJECT                                                                
071600* --- IMS SEKTIONER ---                                                   
071700     SKIP3                                                                
071800 IMS-GET-MSG SECTION.                                                     
071900                                                                          
072000     MOVE '  QC' TO GODK-STATUSKODER                                      
072100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
072200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
072300     PERFORM IMS-STATUSKONTROLL                                           
072400     .                                                                    
072500     SKIP3                                                                
072600*                                                                         
072700 IMS-INSERT-MSG SECTION.                                                  
072800*                                                                         
072900     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
073000       MOVE '0' TO MFS-KDHUVOMR                                           
073100     END-IF                                                               
073200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
073300     MOVE SPACE TO GODK-STATUSKODER                                       
073400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
073500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
073600     PERFORM IMS-STATUSKONTROLL                                           
073700     .                                                                    
073800     EJECT                                                                
073900*                                                                         
074000 IMS-GET-XXKH-WDGX4447 SECTION.                                           
074100*                                                                         
074200     STRING 'WLXXKH01(WDGXKEY  =' W-4447-WDGXKEY ')'                      
074300          DELIMITED BY SIZE INTO SSA1                                     
074400     MOVE '  GE' TO GODK-STATUSKODER                                      
074500     CALL CBLTDLI USING GU XXKH-PCB DLI-IO-AREA SSA1                      
074600     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
074700     PERFORM IMS-STATUSKONTROLL                                           
074800     .                                                                    
074900*                                                                         
075000*                                                                         
075100 IMS-GET-XXKH-WDGX4448 SECTION.                                           
075200*                                                                         
075300*                   GET SEGMENT WDGX4448                                  
075400*                                                                         
075500     STRING 'WLXXKH11(KDPRCGRP= ' WS-KDPRCGRP ')'                         
075600          DELIMITED BY SIZE INTO SSA1                                     
075700*                                                                         
075800     MOVE '  GE' TO GODK-STATUSKODER                                      
075900     CALL CBLTDLI USING GNP XXKH-PCB DLI-IO-AREA SSA1                     
076000     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
076100     PERFORM IMS-STATUSKONTROLL                                           
076200     .                                                                    
076300                                                                          
076400*                                                                         
076500*               GET UNIQUE 4448 WHEN MID-IDPRC-IN IS FILLED IN            
076600*                                                                         
076700 IMS-GU-XXKH-WDGX4448 SECTION.                                            
076800*                                                                         
076900     STRING 'WLXXKH01(WDGXKEY  =' W-4447-WDGXKEY ')'                      
077000          DELIMITED BY SIZE INTO SSA1                                     
077100     STRING 'WLXXKH11(WDGXKEY  =' W-4448-WDGXKEY ')'                      
077200          DELIMITED BY SIZE INTO SSA2                                     
077300     MOVE '  GE' TO GODK-STATUSKODER                                      
077400     CALL CBLTDLI USING GU  XXKH-PCB DLI-IO-AREA SSA1 SSA2                
077500     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
077600     PERFORM IMS-STATUSKONTROLL                                           
077700     .                                                                    
077800                                                                          
077900*                                                                         
078000 IMS-GHU-XXKH-WDGX4448 SECTION.                                           
078100*                                                                         
078200     STRING 'WLXXKH11(WDGXKEY  =' W-4448-WDGXKEY ')'                      
078300          DELIMITED BY SIZE INTO SSA2                                     
078400     MOVE '  GE' TO GODK-STATUSKODER                                      
078500     CALL CBLTDLI USING GHU  XXKH-PCB DLI-IO-AREA SSA2                    
078600     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
078700     PERFORM IMS-STATUSKONTROLL                                           
078800     .                                                                    
078900                                                                          
079000*                                                                         
079100 IMS-LST-XXKH-WDGX4448 SECTION.                                           
079200*                                                                         
079300     STRING 'WLXXKH11(WDGXKEY  =' W-4448-WDGXKEY                          
079400                    '&KDPRCGRP= ' WS-KDPRCGRP ')'                         
079500          DELIMITED BY SIZE INTO SSA2                                     
079600*                                                                         
079700     MOVE '  GE' TO GODK-STATUSKODER                                      
079800     CALL CBLTDLI USING GNP XXKH-PCB DLI-IO-AREA SSA2                     
079900     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
080000     PERFORM IMS-STATUSKONTROLL                                           
080100     .                                                                    
080200 IMS-REPL-XXKH SECTION.                                                   
080300                                                                          
080400     MOVE '  ' TO GODK-STATUSKODER                                        
080500     CALL CBLTDLI USING REPL XXKH-PCB DLI-IO-AREA                         
080600     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
080700     PERFORM IMS-STATUSKONTROLL                                           
080800     .                                                                    
080900*                                                                         
081000*                                                                         
081100 IMS-STATUSKONTROLL SECTION.                                              
081200                                                                          
081300     SET STATUS-IX TO 1                                                   
081400     SEARCH GODK-STATUS                                                   
081500       AT END CALL FELLOG                                                 
081600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
081700     END-SEARCH                                                           
081800     .                                                                    
