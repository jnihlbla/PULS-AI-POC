000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W0081400.                                                
000400 AUTHOR.         LASSI OLGRENER.                                          
000500     DATE-WRITTEN.   FEB 1990.                                            
000600*                                                                         
000700*    REMARKS.                                                             
000800*    FUNKTION.                                                            
000900*        UPPDATERINGS-MPP PROGRAM SOM INGÅR I ORDERSYSTEMET               
001000*        PGM-ET UPPDATERAR WDR1 (WLXXKM), RESP 4456-SEGMENTET             
001100*                                                                         
001200*        MÖJLIGHETER- LÄSER RANSONERINGSFAKTORSTABELLER. UNDER            
001300*                     VARJE TABELLID FINNS INFO OM RANSFAKT PER           
001400*                     VARJE KLASS UPPDELADE PÅ 5 INTERVALLER/KL.          
001500*                   - VID UPPLÄGG AV NY TABELL VISAS EN TABELL            
001600*                     MED DEFAULT VÄRDEN                                  
001700*               OBS - EJ MÖJLIGT ATT RADERA BEFINTLIG TABELLID            
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W0T814  W0T814U                                     
002100*        MID:         W0I81401                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W0O81401                                            
002500                                                                          
002600     EJECT                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 DATA DIVISION.                                                           
003000 WORKING-STORAGE SECTION.                                                 
003001                                                                          
003010*    -- CHECKED BY WY2000                                                 
003100 77    PROGRAM-NAMN              PIC X(8)   VALUE 'W0081400'.             
003200 77    JA                        PIC X      VALUE 'J'.                    
003300 77    NEJ                       PIC X      VALUE 'N'.                    
003400 77    SPRAK-IX                  PIC S9(9)  VALUE +0    COMP SYNC.        
003500 77    KL                        PIC S9(9)  VALUE +0    COMP SYNC.        
003600 77    INT                       PIC S9(9)  VALUE +0    COMP SYNC.        
003700 77    INTE                      PIC S9(9)  VALUE +0    COMP SYNC.        
003800 77    MAX-IX                    PIC  9(1)  VALUE  5.                     
003900 77    MAX-MOD-LAENGD            PIC S9(4)  VALUE +613  COMP SYNC.        
004000 77    WS-IDRFTAB                PIC X(3)   VALUE SPACE.                  
004100                                                                          
004200 77    NOLL-FLAGGA-SW            PIC X       VALUE 'N'.                   
004300   88    NOLL-FLAGGA                         VALUE 'J'.                   
004400                                                                          
004500 77    NYCKLAR-SW                PIC X       VALUE 'J'.                   
004600   88    NYCKEL-OK                           VALUE 'J'.                   
004700                                                                          
004800 77    INDATA-SW                 PIC X       VALUE 'J'.                   
004900   88    INDATA-OK                           VALUE 'J'.                   
005000   88    INDATA-FEL                          VALUE 'N'.                   
005100                                                                          
005200 01    WS-IDTRANS                PIC X(4).                                
005300   88    EGEN-TRANS                          VALUE '0814'.                
005400   88    GODK-TRANS                          VALUE '0814'.                
005500     EJECT                                                                
005600                                                                          
005700 01    GENERELLA-SUBPROGRAM.                                              
005800   03    CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900   03    FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006000   03    WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006100   03    WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
006200                                                                          
006300*   -COPY WDECAREA.                                                       
006500     EJECT                                                                
006600                                                                          
006700*   -COPY WMEDAREA.                                                       
006900     EJECT                                                                
007000******************************************************************        
007100*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
007200*                                                                         
007300 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
007400                                                                          
007500*01    MID -COPY W0I81401.                                                
007700     EJECT                                                                
007800*01    -COPY WMSGAREA                                                     
008000     EJECT                                                                
008100*  03    MOD -COPY W0O81401 -RED MSG-AREA.                                
008300     EJECT                                                                
008400*01    -COPY WMFSAREA                                                     
008600     EJECT                                                                
008700******************************************************************        
008800*                                                                         
008900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009000*                                                                         
009100 01    IMS-WS.                                                            
009200   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
009300     SKIP3                                                                
009400*                        **** STATUS-KOD FRÅN IMS                         
009500   03    STATUS-WS               PIC XX.                                  
009600     88    SEGMENT-FINNS                     VALUE '  '.                  
009700     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
009800     SKIP3                                                                
009900   03    GODK-STATUSKODER.                                                
010000     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
010100     SKIP3                                                                
010200 01    SSA1                      PIC X(64).                               
010300 01    SSA2                      PIC X(64).                               
010400     EJECT                                                                
010500***************************  IMS FUNKTIONSKODER                           
010600*01    -COPY W0003                                                        
010800     EJECT                                                                
010900                                                                          
011000 01    NYCKLAR-TILL-DLI.                                                  
011100                                                                          
011200   03    W-WDGX01KEY-X.                                                   
011300     05    W-IDHTYP              PIC  X(4)   VALUE '4455'.                
011400     05    FILLER                PIC  X(26)  VALUE LOW-VALUE.             
011500                                                                          
011600   03    W-WDGX11KEY-X.                                                   
011700     05    W-IDRFTAB             PIC  X(3)   VALUE SPACE.                 
011800     05    FILLER                PIC  X(7)   VALUE LOW-VALUE.             
011900     EJECT                                                                
012000***************************  DLI INPUT-OUTPUT AREA                        
012100 01    DLI-IO-AREA.                                                       
012200   03    IO-AREA                 PIC X(270)  VALUE SPACE.                 
012300     SKIP3                                                                
012400*  03    WLXXKM11 -COPY WDGX4456              -RED IO-AREA.               
012600     EJECT                                                                
012700*                                                                         
012800 LINKAGE SECTION.                                                         
012900*01    -COPY W0009     -PRE MSG-                                          
013100     EJECT                                                                
013200*01    -COPY W0008     -PRE XXKM-                                         
013400     05  FILLER                  PIC X.                                   
013500     EJECT                                                                
013600 PROCEDURE DIVISION USING  MSG-PCB XXKM-PCB.                              
013700     ENTRY 'DLITCBL' USING MSG-PCB XXKM-PCB.                              
013800 STYR SECTION.                                                            
013900     PERFORM IMS-GET-MSG                                                  
014000     IF SEGMENT-FINNS                                                     
014100       PERFORM A-INIT-INPUT                                               
014200       PERFORM B-NYCKEL                                                   
014300       IF NYCKEL-OK                                                       
014400         PERFORM IMS-GET-WDGX4455                                         
014500         PERFORM IMS-GET-TABELL                                           
014600         IF MFS-FIRST                                                     
014700           PERFORM C-BEHANDLA-TABELL                                      
014800         ELSE                                                             
014900           IF MFS-NEXT                                                    
015000             PERFORM D-NAESTA-TABELL                                      
015100           ELSE                                                           
015200             IF MFS-UPDATE                                                
015300               PERFORM E-UPPDATERING                                      
015400             ELSE                                                         
015500               PERFORM F-ENTER                                            
015600             END-IF                                                       
015700           END-IF                                                         
015800         END-IF                                                           
015900         IF INDATA-OK                                                     
016000           PERFORM S02-VISA-TABELL                                        
016100         END-IF                                                           
016200       END-IF                                                             
016300       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
016400       PERFORM IMS-INSERT-MSG                                             
016500     END-IF                                                               
016600     MOVE ZERO TO RETURN-CODE                                             
016700     GOBACK                                                               
016800     .                                                                    
016900     EJECT                                                                
017000                                                                          
017100 A-INIT-INPUT SECTION.                                                    
017200     IF MSG-DUBBLA-TRANSKODER                                             
017300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I81401                 
017400       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
017500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017600     ELSE                                                                 
017700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I81401                  
017800       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
017900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018000     END-IF                                                               
018100                                                                          
018200     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
018300     MOVE MSG-IDPFK TO MFS-IDPFK                                          
018400     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
018500                                                                          
018600     MOVE LOW-VALUE  TO MSG-AREA                                          
018700     MOVE 'W0O81401' TO MFS-IDMOD                                         
018800     MOVE '0814'     TO MOD-IDTRANS                                       
018900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
019000                             MOD-TEMFSINF                                 
019100                                                                          
019200     IF NOT EGEN-TRANS                                                    
019300       MOVE '7'   TO MFS-IDPFK                                            
019400       MOVE SPACE TO MFS-KDTRTYP                                          
019500     END-IF                                                               
019600                                                                          
019700     IF ENGLISH-TEXT                                                      
019800       MOVE 'GB ' TO MED-IDSKYLT                                          
019900       MOVE +2 TO SPRAK-IX                                                
020000     ELSE                                                                 
020100       MOVE 'S  ' TO MED-IDSKYLT                                          
020200       MOVE +1 TO SPRAK-IX                                                
020300     END-IF                                                               
020400     .                                                                    
020500     EJECT                                                                
020600                                                                          
020700 B-NYCKEL SECTION.                                                        
020800                                                                          
020900     MOVE JA              TO NYCKLAR-SW                                   
021000     MOVE MFS-RENSA-FAELT TO MOD-IDRFTAB-IN                               
021100                                                                          
021200     IF MID-IDRFTAB-IN = ALL '+'                                          
021300       MOVE MID-IDRFTAB-UT TO WS-IDRFTAB                                  
021400                              W-IDRFTAB                                   
021500     ELSE                                                                 
021600       MOVE MID-IDRFTAB-IN TO WS-IDRFTAB                                  
021700                              W-IDRFTAB                                   
021800       MOVE '7'            TO MFS-IDPFK                                   
021900       MOVE SPACE          TO MFS-KDTRTYP                                 
022000     END-IF                                                               
022100                                                                          
022200     IF GODK-TRANS                                                        
022300       MOVE WS-IDRFTAB TO MOD-IDRFTAB-UT                                  
022400     ELSE                                                                 
022500       MOVE MFS-RENSA-FAELT TO MOD-IDRFTAB-UT                             
022600       MOVE NEJ             TO NYCKLAR-SW                                 
022700       MOVE '401' TO MED-IDMFSFEL                                         
022800       CALL WMEDKONV USING MED-WMEDAREA                                   
022900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
023000       PERFORM MFS-RENSA-BILD                                             
023100     END-IF                                                               
023200     .                                                                    
023300     EJECT                                                                
023400                                                                          
023500 C-BEHANDLA-TABELL SECTION.                                               
023600                                                                          
023700     IF MID-IDRFTAB-IN = ALL '+'                                          
023800       MOVE SPACE TO W-IDRFTAB                                            
023900       PERFORM IMS-GET-TABELL-FIRST                                       
024000       IF SEGMENT-SAKNAS                                                  
024100         MOVE NEJ TO INDATA-SW                                            
024200         MOVE '760' TO MED-IDMFSFEL                                       
024300         CALL WMEDKONV USING MED-WMEDAREA                                 
024400         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
024500         PERFORM MFS-RENSA-BILD                                           
024600         MOVE MFS-RENSA-FAELT TO MOD-IDRFTAB-UT                           
024700       ELSE                                                               
024800         MOVE 4456-IDRFTAB TO MOD-IDRFTAB-UT                              
024900       END-IF                                                             
025000     ELSE                                                                 
025100       IF SEGMENT-SAKNAS                                                  
025200         PERFORM S03-FLYTTA-DEFAULT-TABELL                                
025300         MOVE '407' TO MED-IDMFSINF                                       
025400         MOVE '023' TO MED-IDMFSFEL                                       
025500         CALL WMEDKONV USING MED-WMEDAREA                                 
025600         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
025700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
025800       END-IF                                                             
025900     END-IF                                                               
026000     .                                                                    
026100     EJECT                                                                
026200                                                                          
026300 D-NAESTA-TABELL SECTION.                                                 
026400                                                                          
026500     IF SEGMENT-FINNS                                                     
026600       PERFORM IMS-GN-TABELL                                              
026700       IF SEGMENT-SAKNAS                                                  
026800         MOVE NEJ TO INDATA-SW                                            
026900         MOVE '106' TO MED-IDMFSFEL                                       
027000         CALL WMEDKONV USING MED-WMEDAREA                                 
027100         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
027200         PERFORM MFS-ROER-EJ-BILD                                         
027300       ELSE                                                               
027400         MOVE 4456-IDRFTAB TO MOD-IDRFTAB-UT                              
027500       END-IF                                                             
027600     ELSE                                                                 
027700       MOVE NEJ TO INDATA-SW                                              
027800       MOVE 'FLER TABELLER FINNS EJ' TO MOD-TEMFSFEL                      
027900       PERFORM MFS-RENSA-BILD                                             
028000     END-IF                                                               
028100     .                                                                    
028200     EJECT                                                                
028300                                                                          
028400 E-UPPDATERING SECTION.                                                   
028500     IF SEGMENT-SAKNAS                                                    
028600       PERFORM S03-FLYTTA-DEFAULT-TABELL                                  
028700     END-IF                                                               
028800     PERFORM S01-INDATA-KONTROLL                                          
028900     IF INDATA-OK                                                         
029000       IF SEGMENT-FINNS                                                   
029100         PERFORM EA-AENDRING                                              
029200       ELSE                                                               
029300         MOVE W-IDRFTAB TO 4456-IDRFTAB                                   
029400         MOVE LOW-VALUE TO 4456-LOW-VALUE                                 
029500         PERFORM IMS-ISRT-TABELL                                          
029600       END-IF                                                             
029700       MOVE '101' TO MED-IDMFSINF                                         
029800       CALL WMEDKONV USING MED-WMEDAREA                                   
029900       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
030000     END-IF                                                               
030100     .                                                                    
030200     EJECT                                                                
030300                                                                          
030400 EA-AENDRING SECTION.                                                     
030500     PERFORM IMS-REPL-TABELL                                              
030600     IF MID-BERFTAB NOT = ALL '+'                                         
030700       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BERFTAB-ATTR                     
030800     END-IF                                                               
030900     MOVE +1 TO KL                                                        
031000     PERFORM UNTIL KL > MAX-IX                                            
031100       MOVE +1 TO INT                                                     
031200       PERFORM UNTIL INT > MAX-IX                                         
031300         IF MID-RERF-FOM(KL, INT) NOT = ALL '+'                           
031400           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
031500                            MOD-RERF-FOM-ATTR(KL, INT)                    
031600         END-IF                                                           
031700         IF MID-RERF-TOM(KL, INT) NOT = ALL '+'                           
031800           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
031900                            MOD-RERF-TOM-ATTR(KL, INT)                    
032000         END-IF                                                           
032100         IF MID-RERF-NY(KL, INT) NOT = ALL '+'                            
032200           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
032300                            MOD-RERF-NY-ATTR(KL, INT)                     
032400         END-IF                                                           
032500         ADD +1 TO INT                                                    
032600       END-PERFORM                                                        
032700       ADD +1 TO KL                                                       
032800     END-PERFORM                                                          
032900     .                                                                    
033000     EJECT                                                                
033100                                                                          
033200 F-ENTER SECTION.                                                         
033300                                                                          
033400     IF MID-TABELL NOT = ALL '+' OR MID-BERFTAB NOT = ALL '+'             
033500       MOVE NEJ TO INDATA-SW                                              
033600       PERFORM MFS-ROER-EJ-BILD                                           
033700       PERFORM MFS-LAES-INDATA-IGEN                                       
033800       MOVE '003' TO MED-IDMFSFEL                                         
033900       CALL WMEDKONV USING MED-WMEDAREA                                   
034000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
034100     ELSE                                                                 
034200       IF SEGMENT-SAKNAS                                                  
034300         PERFORM S03-FLYTTA-DEFAULT-TABELL                                
034400         MOVE '010' TO MED-IDMFSFEL                                       
034500         CALL WMEDKONV USING MED-WMEDAREA                                 
034600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
034700       END-IF                                                             
034800     END-IF                                                               
034900     .                                                                    
035000     EJECT                                                                
035100                                                                          
035200 S01-INDATA-KONTROLL SECTION.                                             
035300                                                                          
035400     MOVE JA  TO INDATA-SW                                                
035500     IF MID-BERFTAB NOT = ALL '+'                                         
035600       MOVE MID-BERFTAB TO 4456-BERFTAB                                   
035700     END-IF                                                               
035800     MOVE +1 TO KL                                                        
035900     PERFORM UNTIL KL > MAX-IX                                            
036000       MOVE +1 TO INT                                                     
036100       PERFORM UNTIL INT > MAX-IX                                         
036200         IF MID-RERF-FOM(KL, INT) NOT = ALL '+'                           
036300           MOVE +1                    TO DEC-KVHELTAL                     
036400           MOVE +2                    TO DEC-KVDECIMAL                    
036500           MOVE MID-RERF-FOM(KL, INT) TO DEC-IDFRIDATA                    
036600           CALL WDECEDIT USING DEC-WDECAREA                               
036700           IF DEC-KDSVAR-OK                                               
036800             MOVE DEC-IDEDITDATA TO                                       
036900                          4456-RERF-FOM(KL, INT)                          
037000             MOVE MFS-NUM-FAELT-RAETT                                     
037100                    TO MOD-RERF-FOM-ATTR(KL, INT)                         
037200           ELSE                                                           
037300             MOVE MFS-NUM-FAELT-FEL TO MOD-RERF-FOM-ATTR(KL, INT)         
037400             MOVE NEJ TO INDATA-SW                                        
037500           END-IF                                                         
037600         END-IF                                                           
037700                                                                          
037800         IF MID-RERF-TOM(KL, INT) NOT = ALL '+'                           
037900           MOVE +1                    TO DEC-KVHELTAL                     
038000           MOVE +2                    TO DEC-KVDECIMAL                    
038100           MOVE MID-RERF-TOM(KL, INT) TO DEC-IDFRIDATA                    
038200           CALL WDECEDIT USING DEC-WDECAREA                               
038300           IF DEC-KDSVAR-OK                                               
038400             MOVE DEC-IDEDITDATA TO                                       
038500                          4456-RERF-TOM(KL, INT)                          
038600             MOVE MFS-NUM-FAELT-RAETT                                     
038700                    TO MOD-RERF-TOM-ATTR(KL, INT)                         
038800           ELSE                                                           
038900             MOVE MFS-NUM-FAELT-FEL TO MOD-RERF-TOM-ATTR(KL, INT)         
039000             MOVE NEJ TO INDATA-SW                                        
039100           END-IF                                                         
039200         END-IF                                                           
039300                                                                          
039400         IF MID-RERF-NY(KL, INT) NOT = ALL '+'                            
039500           MOVE +1                    TO DEC-KVHELTAL                     
039600           MOVE +3                    TO DEC-KVDECIMAL                    
039700           MOVE MID-RERF-NY(KL, INT)  TO DEC-IDFRIDATA                    
039800           CALL WDECEDIT USING DEC-WDECAREA                               
039900           IF DEC-KDSVAR-OK                                               
040000             MOVE DEC-IDEDITDATA TO                                       
040100                          4456-RERF-NY(KL, INT)                           
040200             MOVE MFS-NUM-FAELT-RAETT                                     
040300                    TO MOD-RERF-NY-ATTR(KL, INT)                          
040400           ELSE                                                           
040500             MOVE MFS-NUM-FAELT-FEL TO MOD-RERF-NY-ATTR(KL, INT)          
040600             MOVE NEJ TO INDATA-SW                                        
040700           END-IF                                                         
040800         END-IF                                                           
040900                                                                          
041000         ADD +1 TO INT                                                    
041100       END-PERFORM                                                        
041200       ADD +1 TO KL                                                       
041300     END-PERFORM                                                          
041400                                                                          
041500     IF INDATA-OK                                                         
041600       MOVE +1 TO KL                                                      
041700       PERFORM UNTIL KL > MAX-IX                                          
041800         MOVE NEJ TO NOLL-FLAGGA-SW                                       
041900         MOVE +1 TO INT                                                   
042000         PERFORM UNTIL INT > MAX-IX                                       
042100           COMPUTE INTE = INT - 1                                         
042200           PERFORM S011-RERFFOM-KONTROLL                                  
042300           PERFORM S012-RERFTOM-KONTROLL                                  
042400           PERFORM S013-RERFNY-KONTROLL                                   
042500           ADD +1 TO INT                                                  
042600         END-PERFORM                                                      
042700         ADD +1 TO KL                                                     
042800       END-PERFORM                                                        
042900     END-IF                                                               
043000                                                                          
043100     IF INDATA-FEL                                                        
043200       MOVE NEJ TO INDATA-SW                                              
043300       MOVE '001' TO MED-IDMFSFEL                                         
043400       CALL WMEDKONV USING MED-WMEDAREA                                   
043500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
043600       PERFORM MFS-ROER-EJ-BILD                                           
043700     END-IF                                                               
043800     .                                                                    
043900     EJECT                                                                
044000                                                                          
044100 S011-RERFFOM-KONTROLL SECTION.                                           
044200     IF INT = +1                                                          
044300       IF 4456-RERF-FOM(KL, INT) NOT = +1.00                              
044400         MOVE MFS-NUM-FAELT-FEL TO MOD-RERF-FOM-ATTR(KL, INT)             
044500         MOVE NEJ TO INDATA-SW                                            
044600       END-IF                                                             
044700     ELSE                                                                 
044800       IF 4456-RERF-TOM(KL, INTE) = +0.00                                 
044900         IF 4456-RERF-FOM(KL, INT) NOT = +0.00                            
045000           MOVE MFS-NUM-FAELT-FEL TO MOD-RERF-FOM-ATTR(KL, INT)           
045100           MOVE NEJ TO INDATA-SW                                          
045200         END-IF                                                           
045300       ELSE                                                               
045400         IF 4456-RERF-FOM(KL, INT) NOT =                                  
045500                     4456-RERF-TOM(KL, INTE) - 0.01                       
045600           MOVE MFS-NUM-FAELT-FEL TO MOD-RERF-FOM-ATTR(KL, INT)           
045700           MOVE NEJ TO INDATA-SW                                          
045800         END-IF                                                           
045900       END-IF                                                             
046000     END-IF                                                               
046100     .                                                                    
046200     EJECT                                                                
046300                                                                          
046400 S012-RERFTOM-KONTROLL SECTION.                                           
046500     IF INT = +5                                                          
046600       IF 4456-RERF-TOM(KL, INT) NOT = +0.00                              
046700         MOVE MFS-NUM-FAELT-FEL TO MOD-RERF-TOM-ATTR(KL, INT)             
046800         MOVE NEJ TO INDATA-SW                                            
046900       END-IF                                                             
047000     ELSE                                                                 
047100       IF 4456-RERF-TOM(KL, INT) = +0.00                                  
047200         MOVE JA TO NOLL-FLAGGA-SW                                        
047300       ELSE                                                               
047400         IF NOLL-FLAGGA                                                   
047500           MOVE MFS-NUM-FAELT-FEL TO MOD-RERF-TOM-ATTR(KL, INT)           
047600           MOVE NEJ TO INDATA-SW                                          
047700         ELSE                                                             
047800           IF 4456-RERF-TOM(KL, INT) NOT < 4456-RERF-FOM(KL, INT)         
047900                OR 4456-RERF-TOM(KL, INT) = +0.01                         
048000             MOVE MFS-NUM-FAELT-FEL TO MOD-RERF-TOM-ATTR(KL, INT)         
048100             MOVE NEJ TO INDATA-SW                                        
048200           END-IF                                                         
048300         END-IF                                                           
048400       END-IF                                                             
048500     END-IF                                                               
048600     .                                                                    
048700     EJECT                                                                
048800                                                                          
048900 S013-RERFNY-KONTROLL SECTION.                                            
049000     IF INT = +1                                                          
049100       IF 4456-RERF-NY(KL, INT) > +1.000                                  
049200         MOVE MFS-NUM-FAELT-FEL TO MOD-RERF-NY-ATTR(KL, INT)              
049300         MOVE NEJ TO INDATA-SW                                            
049400       END-IF                                                             
049500     ELSE                                                                 
049600       IF 4456-RERF-NY(KL, INT) > 4456-RERF-NY(KL, INTE)                  
049700               OR  4456-RERF-NY(KL, INT) < ZERO                           
049800         MOVE MFS-NUM-FAELT-FEL TO MOD-RERF-NY-ATTR(KL, INT)              
049900         MOVE NEJ TO INDATA-SW                                            
050000       END-IF                                                             
050100     END-IF                                                               
050200     .                                                                    
050300     EJECT                                                                
050400                                                                          
050500 S02-VISA-TABELL SECTION.                                                 
050600                                                                          
050700     MOVE 4456-BERFTAB TO MOD-BERFTAB                                     
050800     MOVE +1 TO KL                                                        
050900     PERFORM UNTIL KL > MAX-IX                                            
051000       MOVE +1 TO INT                                                     
051100       PERFORM UNTIL INT > MAX-IX                                         
051200         MOVE 4456-RERF-FOM(KL, INT) TO MOD-RERF-FOM(KL, INT)             
051300         MOVE 4456-RERF-TOM(KL, INT) TO MOD-RERF-TOM(KL, INT)             
051400         MOVE 4456-RERF-NY(KL, INT) TO MOD-RERF-NY(KL, INT)               
051500         ADD +1 TO INT                                                    
051600       END-PERFORM                                                        
051700       ADD +1 TO KL                                                       
051800     END-PERFORM                                                          
051900     .                                                                    
052000     EJECT                                                                
052100                                                                          
052200 S03-FLYTTA-DEFAULT-TABELL SECTION.                                       
052300     MOVE SPACE TO 4456-BERFTAB                                           
052400     MOVE +1 TO KL                                                        
052500     PERFORM UNTIL KL > MAX-IX                                            
052600       MOVE +1 TO INT                                                     
052700       PERFORM UNTIL INT > MAX-IX                                         
052800         IF INT = +1                                                      
052900           MOVE +1.000 TO 4456-RERF-FOM(KL, INT)                          
053000           MOVE +1.000 TO 4456-RERF-NY(KL, INT)                           
053100         ELSE                                                             
053200           MOVE +0.000 TO 4456-RERF-FOM(KL, INT)                          
053300           MOVE +0.000 TO 4456-RERF-NY(KL, INT)                           
053400         END-IF                                                           
053500         MOVE +0.000 TO 4456-RERF-TOM(KL, INT)                            
053600         ADD +1 TO INT                                                    
053700       END-PERFORM                                                        
053800       ADD +1 TO KL                                                       
053900     END-PERFORM                                                          
054000     .                                                                    
054100     EJECT                                                                
054200                                                                          
054300 MFS-RENSA-BILD SECTION.                                                  
054400     MOVE MFS-RENSA-FAELT TO MOD-BERFTAB                                  
054500     MOVE +1 TO KL                                                        
054600     PERFORM UNTIL KL > MAX-IX                                            
054700       MOVE +1 TO INT                                                     
054800       PERFORM UNTIL INT > MAX-IX                                         
054900         MOVE MFS-RENSA-FAELT TO MOD-RERF-FOM(KL, INT)                    
055000                                 MOD-RERF-TOM(KL, INT)                    
055100                                 MOD-RERF-NY(KL, INT)                     
055200         ADD +1 TO INT                                                    
055300       END-PERFORM                                                        
055400       ADD +1 TO KL                                                       
055500     END-PERFORM                                                          
055600     .                                                                    
055700     EJECT                                                                
055800                                                                          
055900 MFS-ROER-EJ-BILD SECTION.                                                
056000     MOVE MFS-ROER-EJ-FAELT TO MOD-BERFTAB                                
056100     MOVE +1 TO KL                                                        
056200     PERFORM UNTIL KL > MAX-IX                                            
056300       MOVE +1 TO INT                                                     
056400       PERFORM UNTIL INT > MAX-IX                                         
056500         MOVE MFS-ROER-EJ-FAELT TO MOD-RERF-FOM(KL, INT)                  
056600                                   MOD-RERF-TOM(KL, INT)                  
056700                                   MOD-RERF-NY(KL, INT)                   
056800         ADD +1 TO INT                                                    
056900       END-PERFORM                                                        
057000       ADD +1 TO KL                                                       
057100     END-PERFORM                                                          
057200     .                                                                    
057300     EJECT                                                                
057400                                                                          
057500 MFS-LAES-INDATA-IGEN SECTION.                                            
057600     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BERFTAB-ATTR                       
057700     MOVE +1 TO KL                                                        
057800     PERFORM UNTIL KL > MAX-IX                                            
057900       MOVE +1 TO INT                                                     
058000       PERFORM UNTIL INT > MAX-IX                                         
058100         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-RERF-FOM-ATTR(KL, INT)         
058200                                       MOD-RERF-TOM-ATTR(KL, INT)         
058300                                       MOD-RERF-NY-ATTR(KL, INT)          
058400         ADD +1 TO INT                                                    
058500       END-PERFORM                                                        
058600       ADD +1 TO KL                                                       
058700     END-PERFORM                                                          
058800     .                                                                    
058900     EJECT                                                                
059000                                                                          
059100* IMS SEKTIONER ******************************                            
059200     SKIP3                                                                
059300 IMS-GET-MSG SECTION.                                                     
059400     SKIP2                                                                
059500     MOVE '  QC' TO GODK-STATUSKODER                                      
059600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
059700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
059800     PERFORM IMS-STATUSKONTROLL                                           
059900     .                                                                    
060000     SKIP3                                                                
060100 IMS-INSERT-MSG SECTION.                                                  
060200     SKIP2                                                                
060300     IF ENGLISH-TEXT                                                      
060400       MOVE 'N' TO MFS-KDHUVOMR                                           
060500     END-IF                                                               
060600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
060700     MOVE SPACE TO GODK-STATUSKODER                                       
060800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
060900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
061000     PERFORM IMS-STATUSKONTROLL                                           
061100     .                                                                    
061200     EJECT                                                                
061300 IMS-GET-WDGX4455 SECTION.                                                
061400     SKIP2                                                                
061500     STRING 'WLXXKM01(WDGXKEY  =' W-WDGX01KEY-X ')'                       
061600            DELIMITED BY SIZE INTO SSA1                                   
061700     MOVE '  ' TO GODK-STATUSKODER                                        
061800     CALL CBLTDLI USING GU XXKM-PCB DLI-IO-AREA SSA1                      
061900     MOVE XXKM-STATUS-CODE TO STATUS-WS                                   
062000     PERFORM IMS-STATUSKONTROLL                                           
062100     .                                                                    
062200     SKIP2                                                                
062300 IMS-GET-TABELL SECTION.                                                  
062400     SKIP2                                                                
062500     STRING 'WLXXKM11(WDGXKEY  =' W-WDGX11KEY-X ')'                       
062600            DELIMITED BY SIZE INTO SSA1                                   
062700     MOVE '  GE' TO GODK-STATUSKODER                                      
062800     CALL CBLTDLI USING GHNP XXKM-PCB DLI-IO-AREA SSA1                    
062900     MOVE XXKM-STATUS-CODE TO STATUS-WS                                   
063000     PERFORM IMS-STATUSKONTROLL                                           
063100     .                                                                    
063200     SKIP2                                                                
063300 IMS-GET-TABELL-FIRST SECTION.                                            
063400     SKIP2                                                                
063500     STRING 'WLXXKM11*F(WDGXKEY >=' W-WDGX11KEY-X ')'                     
063600            DELIMITED BY SIZE INTO SSA1                                   
063700     MOVE '  GE' TO GODK-STATUSKODER                                      
063800     CALL CBLTDLI USING GNP XXKM-PCB DLI-IO-AREA SSA1                     
063900     MOVE XXKM-STATUS-CODE TO STATUS-WS                                   
064000     PERFORM IMS-STATUSKONTROLL                                           
064100     .                                                                    
064200     SKIP2                                                                
064300 IMS-GN-TABELL SECTION.                                                   
064400     SKIP2                                                                
064500     STRING 'WLXXKM11(WDGXKEY  >' W-WDGX11KEY-X ')'                       
064600            DELIMITED BY SIZE INTO SSA1                                   
064700     MOVE '  GE' TO GODK-STATUSKODER                                      
064800     CALL CBLTDLI USING GNP XXKM-PCB DLI-IO-AREA SSA1                     
064900     MOVE XXKM-STATUS-CODE TO STATUS-WS                                   
065000     PERFORM IMS-STATUSKONTROLL                                           
065100     .                                                                    
065200     EJECT                                                                
065300 IMS-REPL-TABELL SECTION.                                                 
065400     SKIP2                                                                
065500     MOVE '  ' TO GODK-STATUSKODER                                        
065600     CALL CBLTDLI USING REPL XXKM-PCB DLI-IO-AREA                         
065700     MOVE XXKM-STATUS-CODE TO STATUS-WS                                   
065800     PERFORM IMS-STATUSKONTROLL                                           
065900     .                                                                    
066000     SKIP3                                                                
066100 IMS-ISRT-TABELL SECTION.                                                 
066200     SKIP2                                                                
066300     MOVE 'WLXXKM11' TO SSA1                                              
066400     MOVE '  ' TO GODK-STATUSKODER                                        
066500     CALL CBLTDLI USING ISRT XXKM-PCB DLI-IO-AREA SSA1                    
066600     MOVE XXKM-STATUS-CODE TO STATUS-WS                                   
066700     PERFORM IMS-STATUSKONTROLL                                           
066800     .                                                                    
066900     SKIP3                                                                
067000 IMS-STATUSKONTROLL SECTION.                                              
067100     SKIP2                                                                
067200     SET STATUS-IX TO 1                                                   
067300     SEARCH GODK-STATUS AT END CALL FELLOG                                
067400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
067500     END-SEARCH                                                           
067600     .                                                                    
