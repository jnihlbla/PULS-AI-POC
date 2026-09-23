000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4038900.                                                
000400 AUTHOR.         PER BERGH.                                               
000500 DATE-WRITTEN.   90/09/14.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*                                                                         
001100*        PROGRAMMET ÄR EN FRÅGE- OCH UPPDATERINGS-MPP                     
001200*        PROGRAMMET UPPDATERAR WLXXLB (WDR1)                              
001300*                                                                         
001400*        KNYTA PACKARE TILL SKIFT                                         
001500*       *FRÅGA PÅ PACKARE FÖR ATT FÅ REDA VILKET SKIFT HAN                
001600*        ARBETAR                                                          
001700*       *FRÅGA PÅ SKIFT FÖR ATT FÅ REDA PÅ VILKA PACKARE SOM              
001800*        ARBETAR  DET SKIFTET                                             
001900*       *UPPDATERA MED NY PACKARE (ANGE PACKARE OCH SKIFT)                
002000*                ELLER GAMMAL PACKARE/NYTT SKIFT (ANGE PACKARE            
002100*                                  OCH NYTT SKIFT)                        
002200*                ELLER TA BORT PACKARE (ANGE ENDAST PACKARE)              
002300*                                                                         
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W4T389                                              
002700*        MID:         W4I38901                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W4O38901                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003601                                                                          
003610*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'W4038900'.            
003800                                                                          
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100                                                                          
004200*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004300 77  INDX                        PIC S9(4)  VALUE +1    COMP SYNC.        
004400 77  MAX-INDX                    PIC S9(4)  VALUE +65   COMP SYNC.        
004500                                                                          
004700 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +991  COMP SYNC.        
004800                                                                          
004900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005000 77  WS-IDUSER                   PIC X(08)   VALUE SPACE.                 
005100 77  WS-IDSHIFT                  PIC X(01)   VALUE SPACE.                 
005200*                                                                         
005210*      --- VALID IDDC CODES                                               
005220*                                                                         
005230*01    -COPY WWDC99                                                       
005240       EJECT                                                              
005300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005400     88  INDATA-OK                           VALUE 'J'.                   
005500     88  INDATA-FEL                          VALUE 'N'.                   
005600                                                                          
005700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005800     88  NYCKLAR-OK                          VALUE 'J'.                   
005900     88  NYCKLAR-FEL                         VALUE 'N'.                   
006000                                                                          
006100 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006200     88  ALLT-OK                             VALUE 'J'.                   
006300                                                                          
006400 77  LAES-SW                     PIC X       VALUE '0'.                   
006500     88  IDUSER-ONLY                         VALUE '1'.                   
006600     88  IDSHIFT-ONLY                        VALUE '2'.                   
006700     88  IDUSER-IDSHIFT                      VALUE '3'.                   
006800                                                                          
006900 77  UPDATE-SW                   PIC X       VALUE 'J'.                   
007000     88  UPDATE-OK                           VALUE 'J'.                   
007100                                                                          
007200                                                                          
007300 77  INFO-SW                     PIC X       VALUE 'J'.                   
007400     88  INFO-OK                             VALUE 'J'.                   
007500                                                                          
007600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007700     88  EGEN-MID                            VALUE '4389'.                
007800     88  GODK-MID                            VALUE '4389'.                
007900                                                                          
008000                                                                          
008100     SKIP3                                                                
008200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008300 01  GENERELLA-SUBPROGRAM.                                                
008400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008610     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008700     EJECT                                                                
008800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008900*   -COPY WMEDAREA                                                        
009000     EJECT                                                                
009010*                   ****    PARAMETRAR TILL W005INIT                      
009020*01  -COPY WMSGINIT                                                       
009030     EJECT                                                                
009200 01  MESSAGE-CODES.                                                       
009300     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009400     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
009500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
009600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
009700     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
009800     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
009900     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
010000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010100     03  ERR-INF-MISS            PIC X(3)    VALUE '413'.                 
010200     03  ERR-NO-CHANGE           PIC X(3)    VALUE '789'.                 
010300     EJECT                                                                
010400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010500*                                                                         
010600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010700     SKIP3                                                                
010800*01  MID -COPY W4I38901                                                   
011000     EJECT                                                                
011100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011200     SKIP3                                                                
011300*01  -COPY WMSGAREA                                                       
011500     EJECT                                                                
011600     03  MOD REDEFINES MSG-AREA.                                          
011700*      05  -COPY W4O38901                                                 
011900     EJECT                                                                
012000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012100     SKIP3                                                                
012200*01  -COPY WMFSAREA                                                       
012400     EJECT                                                                
012500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012600*                                                                         
012700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012800     SKIP3                                                                
012900 01  NYCKLAR-TILL-DLI.                                                    
013000     03  W-4477-X.                                                        
013100         05  W-IDHTYP            PIC X(04)    VALUE '4477'.               
013200         05  W-IDDC              PIC X(02).                               
013300         05  W-4477-LOW-VALUE    PIC X(24)    VALUE LOW-VALUE.            
013400     03  W-4478-X.                                                        
013500         05  W-IDSHIFT           PIC X(01)    VALUE SPACE.                
013600         05  W-IDUSER            PIC X(08)    VALUE SPACE.                
013700         05  W-4478-LOW-VALUE    PIC X        VALUE LOW-VALUE.            
013800                                                                          
013900*    --- DIREKTNYCKEL MIN & MAX TILL PACKARE                              
014000     03  W-4478-IDUSER-MIN-X.                                             
014100         05  W-IDSHIFT-MIN       PIC X(01).                               
014200         05  W-IDUSER-MIN        PIC X(08).                               
014300         05  W-4478-LOW-VALUE-MIN PIC X.                                  
014400                                                                          
014500*                                                                         
014600     03  W-4478-IDUSER-MAX-X.                                             
014700         05  W-IDSHIFT-MAX       PIC X(01).                               
014800         05  FILLER              PIC X(08).                               
014900         05  W-4478-LOW-VALUE-MAX PIC X.                                  
015000                                                                          
015100*    --- DIREKTNYCKEL TILL PACKARE                                        
015200     03  W-IDUSER-SOEK           PIC X(08)    VALUE SPACE.                
015300                                                                          
015400                                                                          
015500*    --- STATUS-KOD FRÅN IMS                                              
015600 01  STATUS-WS                   PIC XX.                                  
015700     88  SEGMENT-FINNS                       VALUE '  '.                  
015800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016000     SKIP2                                                                
016100 01  GODK-STATUSKODER.                                                    
016200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016300     SKIP3                                                                
016400 01  SSA1                        PIC X(96).                               
016500 01  SSA2                        PIC X(96).                               
016600     EJECT                                                                
016700*    --- IMS FUNKTIONSKODER                                               
016800*01  -COPY W0003                                                          
017000     EJECT                                                                
017100*    ---  DLI INPUT-OUTPUT AREA                                           
017200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
017300     SKIP3                                                                
017400 01  DLI-IO-AREA.                                                         
017500     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
017600     SKIP3                                                                
017700     03  WLXXLB01 REDEFINES IO-AREA.                                      
017800*        05  -COPY WDGX4477                                               
018000     EJECT                                                                
018100     03  WLXXLB11 REDEFINES IO-AREA.                                      
018200*        05  -COPY WDGX4478                                               
018400     EJECT                                                                
018500 LINKAGE SECTION.                                                         
018600                                                                          
018700*01  -COPY W0009      -PRE MSG-                                           
018800     EJECT                                                                
018810*01  -COPY W0008     -PRE USEA-                                           
018820     05  FILLER              PIC X.                                       
018900     EJECT                                                                
019000*01  -COPY W0008      -PRE XXLB-                                          
019200     05  FILLER                  PIC X.                                   
019300     EJECT                                                                
019400 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
019410                                   XXLB-PCB.                              
019500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
019510                                   XXLB-PCB.                              
019600                                                                          
019700     PERFORM IMS-GET-MSG                                                  
019800     IF SEGMENT-FINNS                                                     
019900       PERFORM A-INIT                                                     
020000       PERFORM B-KOLLA-NYCKLAR                                            
020100       IF NYCKLAR-OK                                                      
020200         IF MFS-UPDATE                                                    
020300           PERFORM G-KOLLA-INPUT                                          
020400           IF INDATA-OK                                                   
020500             PERFORM H-UPPDATERA                                          
020600           END-IF                                                         
020700         ELSE                                                             
020800           IF MFS-FIRST AND IDSHIFT-ONLY                                  
020900             PERFORM C-FOERSTA-SIDA                                       
021000           ELSE                                                           
021100             IF MFS-NEXT AND IDSHIFT-ONLY                                 
021200               PERFORM D-NAESTA-SIDA                                      
021300             ELSE                                                         
021400               PERFORM E-SAMMA-SIDA                                       
021500             END-IF                                                       
021600           END-IF                                                         
021700           IF ALLT-OK                                                     
021800             PERFORM F-LAES-VISA-INFO                                     
021900           END-IF                                                         
022000         END-IF                                                           
022100       END-IF                                                             
022200       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
022300       PERFORM IMS-INSERT-MSG                                             
022400     END-IF                                                               
022500                                                                          
022600     MOVE ZERO TO RETURN-CODE                                             
022700     GOBACK                                                               
022800     .                                                                    
022900     EJECT                                                                
023000 A-INIT SECTION.                                                          
023100                                                                          
023200     IF MSG-DUBBLA-TRANSKODER                                             
023300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I38901                 
023400       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
023500       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
023600     ELSE                                                                 
023700       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I38901                 
023800       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
023900       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
024000     END-IF                                                               
024100                                                                          
024200     MOVE MSG-KDTRTYP        TO MFS-KDTRTYP                               
024300     MOVE MSG-IDPFK          TO MFS-IDPFK                                 
024400     MOVE MFS-IDTRANS        TO W-IDTRANS                                 
024500                                                                          
024600     MOVE LOW-VALUE          TO MSG-AREA                                  
024700     MOVE 'W4O389N1'         TO MFS-IDMOD                                 
024800     MOVE '4389'             TO MOD-IDTRANS                               
024900     MOVE MFS-RENSA-FAELT    TO MOD-TEMFSFEL MOD-TEMFSINF                 
025000                                                                          
025100     IF NOT EGEN-MID                                                      
025200       MOVE SPACE                         TO MFS-KDTRTYP                  
025300       MOVE '7'                           TO MFS-IDPFK                    
025400     END-IF                                                               
025500                                                                          
025600     IF ENGLISH-TEXT                                                      
025800       MOVE 'GB '                         TO MED-IDSKYLT                  
025900     ELSE                                                                 
026100       MOVE 'S  '                         TO MED-IDSKYLT                  
026200     END-IF                                                               
026300     .                                                                    
026400     EJECT                                                                
026500 B-KOLLA-NYCKLAR SECTION.                                                 
026510                                                                          
026520     MOVE ALL '+'           TO MSGI-WMSGINIT                              
026530     MOVE '001'             TO MSGI-KDCALL                                
026540     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
026541     MOVE '4389'            TO MSGI-IDTRANS                               
026542     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
026550     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
026600                                                                          
026800     MOVE JA                            TO NYCKLAR-SW                     
026900     MOVE MFS-RENSA-FAELT               TO MOD-IDUSER-IN                  
027000                                           MOD-IDSHIFT-IN                 
027010                                           MOD-IDDC-IN                    
027100     MOVE '0'                           TO LAES-SW                        
027300                                                                          
027400     MOVE LOW-VALUE                     TO W-4478-IDUSER-MIN-X            
027600     MOVE HIGH-VALUE                    TO W-4478-IDUSER-MAX-X            
027700                                                                          
027800*    -- KONTROLL AV IN-DATA                                               
027900                                                                          
028000*    --SÖKNING MÖJLIG ÄVEN PÅ ENBART IDUSER ELLER IDSHIFT                 
028100     IF MID-IDUSER-IN  NOT = ALL '+' OR                                   
028200        MID-IDSHIFT-IN NOT = ALL '+'                                      
028300        MOVE SPACE                        TO MID-IDUSER-UT                
028400                                             MID-IDSHIFT-UT               
028500     END-IF                                                               
028600                                                                          
028700     IF MID-IDUSER-IN  = ALL '+'                                          
028800        MOVE MID-IDUSER-UT                TO WS-IDUSER                    
028900     ELSE                                                                 
029000       MOVE MID-IDUSER-IN                 TO WS-IDUSER                    
029100       MOVE '7'                           TO MFS-IDPFK                    
029200       MOVE SPACE                         TO MFS-KDTRTYP                  
029300     END-IF                                                               
029400                                                                          
029500     INSPECT WS-IDUSER REPLACING LEADING SPACE BY ZERO                    
029600                                                                          
029700     IF WS-IDUSER NOT NUMERIC                                             
029800        OR                                                                
029900        WS-IDUSER (1:3) NOT = ZERO                                        
030000        MOVE NEJ                          TO NYCKLAR-SW                   
030100     END-IF                                                               
030200                                                                          
030300     IF WS-IDUSER (4:5) > ZERO                                            
030400        MOVE '1'                          TO LAES-SW                      
030500     END-IF                                                               
030600                                                                          
030700     IF MID-IDSHIFT-IN  = ALL '+'                                         
030800        MOVE MID-IDSHIFT-UT               TO WS-IDSHIFT                   
030900     ELSE                                                                 
031000        MOVE MID-IDSHIFT-IN               TO WS-IDSHIFT                   
031100        MOVE '7'                          TO MFS-IDPFK                    
031200        MOVE SPACE                        TO MFS-KDTRTYP                  
031300     END-IF                                                               
031310                                                                          
031400     IF WS-IDSHIFT NOT = SPACE                                            
031500        IF WS-IDSHIFT NUMERIC                                             
031600           IF WS-IDSHIFT > 0 AND                                          
031700              WS-IDSHIFT < 4                                              
031800              IF LAES-SW = '1'                                            
031900                 MOVE '3'                 TO LAES-SW                      
032000              ELSE                                                        
032100                 MOVE '2'                 TO LAES-SW                      
032200              END-IF                                                      
032300           ELSE                                                           
032400              MOVE NEJ                    TO NYCKLAR-SW                   
032500           END-IF                                                         
032600        ELSE                                                              
032700           MOVE NEJ                       TO NYCKLAR-SW                   
032800        END-IF                                                            
032900     END-IF                                                               
033000     IF LAES-SW = '0'                                                     
033100        IF MFS-UPDATE                                                     
033200           CONTINUE                                                       
033300        ELSE                                                              
033400           MOVE NEJ                       TO NYCKLAR-SW                   
033500        END-IF                                                            
033600     END-IF                                                               
033601                                                                          
033602     MOVE MSGI-IDDC               TO WS-IDDC                              
033697                                                                          
033698     IF WS-IDDC IS <= SPACE                                               
033706       MOVE NEJ                           TO NYCKLAR-SW                   
033707     END-IF                                                               
033710                                                                          
033800     IF GODK-MID OR NYCKLAR-OK                                            
033900        PERFORM BA-FLYTTA-VAERDEN                                         
034000     ELSE                                                                 
034100        MOVE MFS-RENSA-FAELT TO MOD-IDUSER-UT                             
034200                                MOD-IDSHIFT-UT                            
034210                                MOD-IDDC-UT                               
034300     END-IF                                                               
034400                                                                          
034500     IF NYCKLAR-FEL                                                       
034600        PERFORM S01-ERR-WRONG-KEY                                         
034700     END-IF                                                               
034800     .                                                                    
034900     EJECT                                                                
035000 BA-FLYTTA-VAERDEN SECTION.                                               
035100                                                                          
035200        MOVE WS-IDUSER  TO W-IDUSER                                       
035300                           W-IDUSER-SOEK                                  
035400                           MOD-IDUSER-UT                                  
035500                           MID-IDUSER-ENTER                               
035600        INSPECT MOD-IDUSER-UT REPLACING LEADING ZERO BY SPACE             
035700        MOVE WS-IDSHIFT TO W-IDSHIFT                                      
035800                           W-IDSHIFT-MIN                                  
035900                           W-IDSHIFT-MAX                                  
036000                           MOD-IDSHIFT-UT                                 
036010        MOVE WS-IDDC    TO W-IDDC                                         
036020                           MOD-IDDC-UT                                    
036100     .                                                                    
036200     EJECT                                                                
036300 C-FOERSTA-SIDA SECTION.                                                  
036400                                                                          
036500     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
036600     CALL WMEDKONV USING MED-WMEDAREA                                     
036700     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
036800                                                                          
036900     MOVE SPACE TO MOD-IDUSER-ENTER                                       
037000                   MOD-IDUSER-NEXT                                        
037100     MOVE JA TO ALLT-SW                                                   
037200     .                                                                    
037300     EJECT                                                                
037400 D-NAESTA-SIDA SECTION.                                                   
037500                                                                          
037600     MOVE MID-IDUSER-NEXT TO W-IDUSER-MIN                                 
037700     MOVE JA TO ALLT-SW                                                   
037800     .                                                                    
037900     EJECT                                                                
038000 E-SAMMA-SIDA SECTION.                                                    
038100                                                                          
038200     IF (MID-IDUSER-INPUT  = ALL '+' OR SPACE) AND                        
038300        (MID-IDSHIFT-INPUT = ALL '+' OR SPACE)                            
038400       MOVE MID-IDUSER-ENTER TO W-IDUSER                                  
038500       MOVE JA TO ALLT-SW                                                 
038600     ELSE                                                                 
038700       MOVE NEJ TO ALLT-SW                                                
038800       MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
038900       CALL WMEDKONV USING MED-WMEDAREA                                   
039000       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
039100       PERFORM MFS-ROR-EJ-FAELT-IN                                        
039200       PERFORM MFS-ROR-EJ-FAELT-UT                                        
039300       PERFORM MFS-LAS-IN-IGEN                                            
039400     END-IF                                                               
039500     .                                                                    
039600     EJECT                                                                
039700 F-LAES-VISA-INFO SECTION.                                                
039800                                                                          
039900     PERFORM IMS-GET-XXLB-4477                                            
040000                                                                          
040100     IF SEGMENT-SAKNAS                                                    
040200        PERFORM S03-ERR-INF-MISS                                          
040300        MOVE SPACE TO MOD-IDUSER-ENTER                                    
040400     ELSE                                                                 
040500                                                                          
040600       IF IDSHIFT-ONLY                                                    
040700                                                                          
040800         PERFORM FA-SHIFT-ONLY                                            
040900       ELSE                                                               
041000                                                                          
041100         IF IDUSER-ONLY                                                   
041200                                                                          
041300           PERFORM FB-USER-ONLY                                           
041400         ELSE                                                             
041500                                                                          
041600            IF IDUSER-IDSHIFT                                             
041700                                                                          
041800               PERFORM FC-USER-SHIFT                                      
041900            END-IF                                                        
042000         END-IF                                                           
042100       END-IF                                                             
042200     END-IF                                                               
042300     PERFORM MFS-RENSA-FAELT-IN                                           
042400     .                                                                    
042500     EJECT                                                                
042600 FA-SHIFT-ONLY SECTION.                                                   
042700                                                                          
042800     MOVE +1 TO INDX                                                      
042900     PERFORM IMS-GNP-XXLB-4478-IDSHIFT                                    
043000                                                                          
043100     IF SEGMENT-SAKNAS                                                    
043200       MOVE SPACE        TO MOD-IDUSER-ENTER                              
043300     ELSE                                                                 
043400       MOVE 4478-IDUSER  TO MOD-IDUSER-ENTER                              
043500                                                                          
043600        PERFORM UNTIL INDX > MAX-INDX                                     
043700          IF SEGMENT-FINNS                                                
043800            MOVE 4478-IDUSER  TO MOD-IDUSER-RAD (INDX)                    
043900            MOVE 4478-IDSHIFT TO MOD-IDSHIFT-RAD (INDX)                   
044000            INSPECT MOD-IDUSER-RAD (INDX) REPLACING                       
044100                              LEADING ZERO BY SPACE                       
044200            PERFORM IMS-GNP-XXLB-4478-IDSHIFT                             
044300          ELSE                                                            
044400            MOVE MFS-RENSA-FAELT TO MOD-IDUSER-RAD  (INDX)                
044500                                    MOD-IDSHIFT-RAD (INDX)                
044600          END-IF                                                          
044700          ADD 1 TO INDX                                                   
044800        END-PERFORM                                                       
044900                                                                          
045000        IF SEGMENT-FINNS                                                  
045100           PERFORM FAA-INF-MORE-INFO-EXISTS                               
045200        ELSE                                                              
045300           IF MFS-FIRST                                                   
045400              PERFORM FAB-INF-FIRST-PAGE                                  
045500           ELSE                                                           
045600              PERFORM FAC-INF-LAST-PAGE                                   
045700           END-IF                                                         
045800        END-IF                                                            
045900     END-IF                                                               
046000     .                                                                    
046100     EJECT                                                                
046200 FAA-INF-MORE-INFO-EXISTS SECTION.                                        
046300                                                                          
046400     MOVE 4478-IDUSER  TO MOD-IDUSER-NEXT                                 
046500     MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                            
046600     CALL WMEDKONV USING MED-WMEDAREA                                     
046700     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
046800     .                                                                    
046900 FAB-INF-FIRST-PAGE SECTION.                                              
047000                                                                          
047100     MOVE SPACE TO MOD-IDUSER-NEXT                                        
047200     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
047300     CALL WMEDKONV USING MED-WMEDAREA                                     
047400     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
047500     .                                                                    
047600 FAC-INF-LAST-PAGE SECTION.                                               
047700                                                                          
047800     MOVE SPACE TO MOD-IDUSER-NEXT                                        
047900     MOVE INF-LAST-PAGE TO MED-IDMFSINF                                   
048000     CALL WMEDKONV USING MED-WMEDAREA                                     
048100     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
048200     .                                                                    
048300     EJECT                                                                
048400 FB-USER-ONLY SECTION.                                                    
048500                                                                          
048600     PERFORM IMS-GNP-XXLB-4478-IDUSER                                     
048700     IF SEGMENT-FINNS                                                     
048800       MOVE 4478-IDUSER  TO MOD-IDUSER-RAD (1)                            
048900       MOVE 4478-IDSHIFT TO MOD-IDSHIFT-RAD (1)                           
049000       INSPECT MOD-IDUSER-RAD (1) REPLACING                               
049100                              LEADING ZERO BY SPACE                       
049200     ELSE                                                                 
049300       PERFORM S03-ERR-INF-MISS                                           
049400     END-IF                                                               
049500     .                                                                    
049600 FC-USER-SHIFT SECTION.                                                   
049700                                                                          
049800     PERFORM IMS-GET-XXLB-4478                                            
049900     IF SEGMENT-FINNS                                                     
050000       MOVE 4478-IDUSER  TO MOD-IDUSER-RAD (1)                            
050100       MOVE 4478-IDSHIFT TO MOD-IDSHIFT-RAD (1)                           
050200       INSPECT MOD-IDUSER-RAD (1) REPLACING                               
050300                              LEADING ZERO BY SPACE                       
050400     ELSE                                                                 
050500       PERFORM S03-ERR-INF-MISS                                           
050600     END-IF                                                               
050700     .                                                                    
050800     EJECT                                                                
050900 G-KOLLA-INPUT SECTION.                                                   
051000                                                                          
051100     MOVE JA  TO INDATA-SW                                                
051200     IF MID-INDATA = ALL '+' OR                                           
051300        MID-INDATA = SPACE                                                
051400        PERFORM S04-ERR-PF11-AND-NO-DATA                                  
051500     ELSE                                                                 
051600       IF MID-IDUSER-INPUT           = ALL '+'                            
051700          OR                                                              
051800          MID-IDUSER-INPUT       NOT NUMERIC                              
051900          OR                                                              
052000          MID-IDUSER-INPUT (1:3) NOT = ZERO                               
052100          OR                                                              
052200          MID-IDUSER-INPUT (4:5)     = ZERO                               
052300          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDUSER-INPUT-ATTR                
052400          MOVE NEJ TO INDATA-SW                                           
052500       ELSE                                                               
052600          MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDUSER-INPUT-ATTR              
052700       END-IF                                                             
052800       IF MID-IDSHIFT-INPUT = ALL '+' OR                                  
052900          MID-IDSHIFT-INPUT = SPACE                                       
053000          CONTINUE                                                        
053100       ELSE                                                               
053200          IF MID-IDSHIFT-INPUT NOT NUMERIC OR                             
053300             MID-IDSHIFT-INPUT < 0 OR                                     
053400             MID-IDSHIFT-INPUT > 3                                        
053500             MOVE MFS-ALFA-FAELT-FEL TO                                   
053600                  MOD-IDSHIFT-INPUT-ATTR                                  
053700             MOVE NEJ TO INDATA-SW                                        
053800          ELSE                                                            
053900             MOVE MFS-ALFA-FAELT-RAETT TO                                 
054000                  MOD-IDSHIFT-INPUT-ATTR                                  
054100          END-IF                                                          
054200       END-IF                                                             
054300       IF INDATA-FEL                                                      
054400          PERFORM S02-ERR-CORR-HILITE-FLDS                                
054500       ELSE                                                               
054600          PERFORM GA-FLYTTA-VAERDEN                                       
054700       END-IF                                                             
054800     END-IF                                                               
054900     .                                                                    
055000     EJECT                                                                
055100 GA-FLYTTA-VAERDEN SECTION.                                               
055200                                                                          
055300     MOVE MID-IDUSER-INPUT     TO W-IDUSER                                
055400                                  W-IDUSER-SOEK                           
055500     IF MID-IDSHIFT-INPUT = ALL '+'                                       
055600        MOVE SPACE             TO W-IDSHIFT                               
055700     ELSE                                                                 
055800        MOVE MID-IDSHIFT-INPUT TO W-IDSHIFT                               
055900     END-IF                                                               
056000     .                                                                    
056100                                                                          
056200     EJECT                                                                
056300 H-UPPDATERA SECTION.                                                     
056400                                                                          
056500     MOVE JA TO UPDATE-SW                                                 
056600     MOVE JA TO INFO-SW                                                   
056700     PERFORM IMS-GET-XXLB-4477                                            
056800     IF SEGMENT-SAKNAS                                                    
056900        PERFORM S03-ERR-INF-MISS                                          
057000        MOVE NEJ TO UPDATE-SW                                             
057100     ELSE                                                                 
057200        IF MID-IDSHIFT-INPUT = ALL '+' OR                                 
057300           MID-IDSHIFT-INPUT = ALL SPACE                                  
057400           PERFORM IMS-GHU-XXLB-4478-IDUSER                               
057500           IF SEGMENT-FINNS                                               
057600                                                                          
057700              PERFORM HA-TA-BORT                                          
057800                                                                          
057900              MOVE MFS-ALFA-FAELT-RAETT TO                                
058000                   MOD-IDUSER-INPUT-ATTR                                  
058100              MOVE NEJ TO INFO-SW                                         
058200           ELSE                                                           
058300              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDUSER-INPUT-ATTR            
058400              MOVE NEJ TO INDATA-SW                                       
058500              MOVE NEJ TO UPDATE-SW                                       
058600           END-IF                                                         
058700        ELSE                                                              
058800           PERFORM IMS-GHU-XXLB-4478-IDUSER                               
058900           IF SEGMENT-FINNS                                               
059000              IF MID-IDSHIFT-INPUT = 4478-IDSHIFT                         
059100                 PERFORM S05-ERR-NO-CHANGE                                
059200                 MOVE NEJ TO UPDATE-SW                                    
059300              ELSE                                                        
059400                                                                          
059500                 PERFORM HB-BYT-SKIFT                                     
059600                                                                          
059700              END-IF                                                      
059800           ELSE                                                           
059900                                                                          
060000              PERFORM HC-TILLAEGG                                         
060100                                                                          
060200           END-IF                                                         
060300        END-IF                                                            
060400                                                                          
060500       IF INDATA-FEL                                                      
060600          PERFORM S02-ERR-CORR-HILITE-FLDS                                
060700       ELSE                                                               
060800          IF UPDATE-OK                                                    
060900             IF INFO-OK                                                   
061000                PERFORM HD-VISA-INFO                                      
061100             END-IF                                                       
061200             PERFORM HE-INF-UPDATE-DONE                                   
061300          END-IF                                                          
061400       END-IF                                                             
061500     END-IF                                                               
061600     .                                                                    
061700     EJECT                                                                
061800 HA-TA-BORT SECTION.                                                      
061900                                                                          
062000     PERFORM IMS-DLET-XXLB                                                
062100     MOVE SPACE TO MOD-IDUSER-ENTER                                       
062200     MOVE SPACE TO W-IDSHIFT                                              
062300     MOVE SPACE TO W-IDUSER                                               
062400     .                                                                    
062500 HB-BYT-SKIFT SECTION.                                                    
062600                                                                          
062700     PERFORM IMS-DLET-XXLB                                                
062800     MOVE MID-IDSHIFT-INPUT TO W-IDSHIFT                                  
062900     MOVE MID-IDUSER-INPUT  TO W-IDUSER                                   
063000     MOVE W-4478-X          TO 4478-WDGX4478                              
063100     PERFORM IMS-ISRT-XXLB                                                
063200     .                                                                    
063300 HC-TILLAEGG SECTION.                                                     
063400     MOVE MID-IDSHIFT-INPUT TO W-IDSHIFT                                  
063500     MOVE MID-IDUSER-INPUT  TO W-IDUSER                                   
063600     MOVE W-4478-X          TO 4478-WDGX4478                              
063700     PERFORM IMS-ISRT-XXLB                                                
063800     .                                                                    
063900 HD-VISA-INFO SECTION.                                                    
064000                                                                          
064100     PERFORM IMS-GET-XXLB-4477                                            
064200     PERFORM IMS-GET-XXLB-4478                                            
064300     IF SEGMENT-FINNS                                                     
064400       MOVE W-IDUSER  TO MOD-IDUSER-RAD (1)                               
064500       MOVE W-IDSHIFT TO MOD-IDSHIFT-RAD (1)                              
064600       INSPECT MOD-IDUSER-RAD (1) REPLACING                               
064700                              LEADING ZERO BY SPACE                       
064800     ELSE                                                                 
064900       PERFORM S03-ERR-INF-MISS                                           
065000     END-IF                                                               
065100     .                                                                    
065200     EJECT                                                                
065300 HE-INF-UPDATE-DONE SECTION.                                              
065400                                                                          
065500     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
065600     CALL WMEDKONV USING MED-WMEDAREA                                     
065700     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
065800     PERFORM MFS-FORM-ATTR                                                
065900     PERFORM MFS-RENSA-FAELT-IN                                           
066000     .                                                                    
066100     EJECT                                                                
066200 S01-ERR-WRONG-KEY SECTION.                                               
066300     MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                   
066400     CALL WMEDKONV USING MED-WMEDAREA                                     
066500     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
066600     PERFORM MFS-RENSA-FAELT-IN                                           
066700     PERFORM MFS-RENSA-FAELT-UT                                           
066800     .                                                                    
066900 S02-ERR-CORR-HILITE-FLDS SECTION.                                        
067000     MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                            
067100     CALL WMEDKONV USING MED-WMEDAREA                                     
067200     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
067300     PERFORM MFS-ROR-EJ-FAELT-UT                                          
067400     PERFORM MFS-ROR-EJ-FAELT-IN                                          
067500     .                                                                    
067600 S03-ERR-INF-MISS SECTION.                                                
067700     MOVE ERR-INF-MISS TO MED-IDMFSFEL                                    
067800     CALL WMEDKONV USING MED-WMEDAREA                                     
067900     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
068000     PERFORM MFS-RENSA-FAELT-UT                                           
068100     .                                                                    
068200 S04-ERR-PF11-AND-NO-DATA SECTION.                                        
068300     MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                            
068400     CALL WMEDKONV USING MED-WMEDAREA                                     
068500     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
068600     PERFORM MFS-ROR-EJ-FAELT-IN                                          
068700     PERFORM MFS-ROR-EJ-FAELT-UT                                          
068800     MOVE NEJ TO INDATA-SW                                                
068900     .                                                                    
069000 S05-ERR-NO-CHANGE SECTION.                                               
069100     MOVE ERR-NO-CHANGE TO MED-IDMFSFEL                                   
069200     CALL WMEDKONV USING MED-WMEDAREA                                     
069300     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
069400     PERFORM MFS-ROR-EJ-FAELT-UT                                          
069500     PERFORM MFS-ROR-EJ-FAELT-IN                                          
069600     .                                                                    
069700     EJECT                                                                
069800 MFS-RENSA-FAELT-UT SECTION.                                              
069900                                                                          
070000*    --- ALLA UTDATA-FÄLT                                                 
070100*    --- INKL. BLÄDDRINGSNYCKLAR                                          
070200     MOVE MFS-RENSA-FAELT TO MOD-IDUSER-RAD (INDX)                        
070300                             MOD-IDSHIFT-RAD (INDX)                       
070400                             MOD-IDUSER-ENTER                             
070500                             MOD-IDUSER-NEXT                              
070600     .                                                                    
070700     SKIP2                                                                
070800*MFS-RENSA-RAD-FAELT-UT SECTION.                                          
070900                                                                          
071000*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
071100*    MOVE MFS-RENSA-FAELT TO MOD-IDUSER-RAD (INDX)                        
071200*                            MOD-IDSHIFT-RAD (INDX)                       
071300*    .                                                                    
071400     SKIP2                                                                
071500 MFS-RENSA-FAELT-IN SECTION.                                              
071600                                                                          
071700*    --- ALLA INDATA-FÄLT                                                 
071800     MOVE MFS-RENSA-FAELT TO MOD-IDUSER-INPUT                             
071900                             MOD-IDSHIFT-INPUT                            
072000     .                                                                    
072100     EJECT                                                                
072200 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
072300                                                                          
072400*    --- ALLA UTDATA-FÄLT                                                 
072500*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
072600     MOVE MFS-ROER-EJ-FAELT TO MOD-IDUSER-RAD (INDX)                      
072700                               MOD-IDSHIFT-RAD (INDX)                     
072800                               MOD-IDUSER-ENTER                           
072900                               MOD-IDUSER-NEXT                            
073000                                                                          
073100     MOVE +1 TO INDX                                                      
073200     PERFORM UNTIL INDX > MAX-INDX                                        
073300       PERFORM MFS-ROR-EJ-RAD-FAELT-UT                                    
073400       ADD +1 TO INDX                                                     
073500     END-PERFORM                                                          
073600     .                                                                    
073700     SKIP2                                                                
073800 MFS-ROR-EJ-RAD-FAELT-UT  SECTION.                                        
073900                                                                          
074000*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
074100     MOVE MFS-ROER-EJ-FAELT TO MOD-IDUSER-RAD (INDX)                      
074200                               MOD-IDSHIFT-RAD (INDX)                     
074300     .                                                                    
074400     SKIP2                                                                
074500 MFS-ROR-EJ-FAELT-IN  SECTION.                                            
074600                                                                          
074700*    --- ALLA INDATA-FÄLT                                                 
074800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDUSER-INPUT                           
074900                               MOD-IDSHIFT-INPUT                          
075000     .                                                                    
075100     EJECT                                                                
075200 MFS-FORM-ATTR SECTION.                                                   
075300                                                                          
075400*    --- ALLA INDATA-FÄLT                                                 
075500     MOVE MFS-FORMATETS-ATTR TO MOD-IDUSER-INPUT-ATTR                     
075600                                MOD-IDSHIFT-INPUT-ATTR                    
075700     .                                                                    
075800     SKIP2                                                                
075900 MFS-LAS-IN-IGEN SECTION.                                                 
076000                                                                          
076100*    --- ALLA INDATA-FÄLT                                                 
076200     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDUSER-INPUT-ATTR                  
076300                                   MOD-IDSHIFT-INPUT-ATTR                 
076400     .                                                                    
076500     EJECT                                                                
076600* --- IMS SEKTIONER ---                                                   
076700     SKIP3                                                                
076800 IMS-GET-MSG SECTION.                                                     
076900                                                                          
077000     MOVE '  QC' TO GODK-STATUSKODER                                      
077100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
077200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
077300     PERFORM IMS-STATUSKONTROLL                                           
077400     .                                                                    
077500     SKIP3                                                                
077600 IMS-INSERT-MSG SECTION.                                                  
077700                                                                          
077800     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
077900       MOVE '0' TO MFS-KDHUVOMR                                           
078000     END-IF                                                               
078100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
078200     MOVE SPACE TO GODK-STATUSKODER                                       
078300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
078400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
078500     PERFORM IMS-STATUSKONTROLL                                           
078600     .                                                                    
078700     EJECT                                                                
078800 IMS-GET-XXLB-4477 SECTION.                                               
078900                                                                          
079000     STRING 'WLXXLB01(WDGXKEY  =' W-4477-X ')'                            
079100          DELIMITED BY SIZE INTO SSA1                                     
079200     MOVE '  GE' TO GODK-STATUSKODER                                      
079300     CALL CBLTDLI USING GU XXLB-PCB DLI-IO-AREA SSA1                      
079400     MOVE XXLB-STATUS-CODE TO STATUS-WS                                   
079500     PERFORM IMS-STATUSKONTROLL                                           
079600     .                                                                    
079700                                                                          
079800 IMS-GET-XXLB-4478 SECTION.                                               
079900                                                                          
080000     STRING 'WLXXLB11(WDGXKEY  =' W-4478-X ')'                            
080100          DELIMITED BY SIZE INTO SSA1                                     
080200     MOVE '  GE' TO GODK-STATUSKODER                                      
080300     CALL CBLTDLI USING GNP XXLB-PCB DLI-IO-AREA SSA1                     
080400     MOVE XXLB-STATUS-CODE TO STATUS-WS                                   
080500     PERFORM IMS-STATUSKONTROLL                                           
080600     .                                                                    
080700                                                                          
080800 IMS-GHU-XXLB-4478-IDUSER SECTION.                                        
080900                                                                          
081000     STRING 'WLXXLB11(IDUSER   =' W-IDUSER-SOEK ')'                       
081100          DELIMITED BY SIZE INTO SSA1                                     
081200     MOVE '  GE' TO GODK-STATUSKODER                                      
081300     CALL CBLTDLI USING GHU XXLB-PCB DLI-IO-AREA SSA1                     
081400     MOVE XXLB-STATUS-CODE TO STATUS-WS                                   
081500     PERFORM IMS-STATUSKONTROLL                                           
081600     .                                                                    
081700     EJECT                                                                
081800 IMS-GNP-XXLB-4478-IDUSER SECTION.                                        
081900                                                                          
082000     STRING 'WLXXLB11(IDUSER   =' W-IDUSER-SOEK ')'                       
082100          DELIMITED BY SIZE INTO SSA1                                     
082200     MOVE '  GE' TO GODK-STATUSKODER                                      
082300     CALL CBLTDLI USING GNP XXLB-PCB DLI-IO-AREA SSA1                     
082400     MOVE XXLB-STATUS-CODE TO STATUS-WS                                   
082500     PERFORM IMS-STATUSKONTROLL                                           
082600     .                                                                    
082700                                                                          
082800 IMS-GNP-XXLB-4478-IDSHIFT SECTION.                                       
082900                                                                          
083000     STRING 'WLXXLB11(WDGXKEY =>' W-4478-IDUSER-MIN-X                     
083100                    '&WDGXKEY =<' W-4478-IDUSER-MAX-X ')'                 
083200          DELIMITED BY SIZE INTO SSA1                                     
083300     MOVE '  GE' TO GODK-STATUSKODER                                      
083400     CALL CBLTDLI USING GNP XXLB-PCB DLI-IO-AREA SSA1                     
083500     MOVE XXLB-STATUS-CODE TO STATUS-WS                                   
083600     PERFORM IMS-STATUSKONTROLL                                           
083700     .                                                                    
083800     EJECT                                                                
083900 IMS-ISRT-XXLB SECTION.                                                   
084000                                                                          
084100     STRING 'WLXXLB01(WDGXKEY  =' W-4477-X ')'                            
084200          DELIMITED BY SIZE INTO SSA1                                     
084300     MOVE 'WLXXLB11 ' TO SSA2                                             
084400     MOVE '  II' TO GODK-STATUSKODER                                      
084500     CALL CBLTDLI USING ISRT XXLB-PCB DLI-IO-AREA SSA1 SSA2               
084600     MOVE XXLB-STATUS-CODE TO STATUS-WS                                   
084700     PERFORM IMS-STATUSKONTROLL                                           
084800     .                                                                    
084900                                                                          
085000                                                                          
085100 IMS-DLET-XXLB SECTION.                                                   
085200                                                                          
085300     MOVE '  ' TO GODK-STATUSKODER                                        
085400     CALL CBLTDLI USING DLET XXLB-PCB DLI-IO-AREA                         
085500     MOVE XXLB-STATUS-CODE TO STATUS-WS                                   
085600     PERFORM IMS-STATUSKONTROLL                                           
085700     .                                                                    
085800     EJECT                                                                
085900 IMS-STATUSKONTROLL SECTION.                                              
086000                                                                          
086100     SET STATUS-IX TO 1                                                   
086200     SEARCH GODK-STATUS                                                   
086300       AT END CALL FELLOG                                                 
086400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
086500     END-SEARCH                                                           
086600     .                                                                    
