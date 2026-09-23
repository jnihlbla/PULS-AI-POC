000100 ID DIVISION.                                                             
000200     SKIP3                                                                
000300 PROGRAM-ID.     W5011200.                                                
000400 AUTHOR.         ROYNA LUND.                                              
000500 DATE-WRITTEN.   SEP   89.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        IMSDC UPPDATERINGSPROGRAM FÖR EKONOMI.                           
001100*                                                                         
001200*        PROGRAMMET LÄSER WLARTC (WDK6) OCH PLOCKAR BORT                  
001300*        BEFINTLIGA BESTÄLLNINGSPRISER  PÅ WLARTC21 (WDK621).             
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W5T112                                              
001700*                     W5T112U                                             
001800*                                                                         
001900*        MID:         W5I11201                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W5O11201                                            
002300*                                                                         
002400*  E-TRACKER 1286763, MÄRKNING AV PRISÄNDRING PÅ ONDEMANDLISTOR           
002500     EJECT                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP3                                                                
002800 DATA DIVISION.                                                           
002900                                                                          
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200*    -COPY WY2000W1                                                       
003300     SKIP3                                                                
003400 77  JA                      PIC X          VALUE 'J'.                    
003500 77  NEJ                     PIC X          VALUE 'N'.                    
003600                                                                          
003700 77  NYCKLAR-OK              PIC X          VALUE 'J'.                    
003800 77  FLFEL-FAELT             PIC X          VALUE 'N'.                    
003900 77  FLSLUTA-LAS             PIC X          VALUE 'N'.                    
004000                                                                          
004100 77  INDX                    PIC S9(9)      VALUE +0   COMP SYNC.         
004200                                                                          
004300 01  SUBPGM.                                                              
004400     03  CBLTDLI             PIC X(8)       VALUE 'CBLTDLI '.             
004500     03  FELLOG              PIC X(8)       VALUE 'FELLOG  '.             
004600     03  WDATKONV            PIC X(8)       VALUE 'WDATKONV'.             
004700     03  W005INIT            PIC X(8)       VALUE 'W005INIT'.             
004800                                                                          
004900 01  W-IDTRANS               PIC X(4)       VALUE SPACE.                  
005000     88 EGEN-TRANS                          VALUE '5112'.                 
005100     88 GODK-TRANS                          VALUE '5111' '5112'           
005110                                                  '5207'.                 
005200                                                                          
005300 01  DAGENS-DAT.                                                          
005400     03  DAGENS-SEKEL        PIC 9(2).                                    
005500     03  DAGENS-DATUM        PIC 9(6).                                    
005600     03  FILLER  REDEFINES DAGENS-DATUM.                                  
005700      05 DAGENS-AAR          PIC 9(2).                                    
005800      05 FILLER              PIC 9(4).                                    
005900                                                                          
006000 01  DAGENS-TID              PIC 9(8).                                    
006100 01  FILLER  REDEFINES DAGENS-TID.                                        
006200     03  DAGENS-KLOCK        PIC 9(6).                                    
006300     03  FILLER              PIC 9(2).                                    
006400                                                                          
006500                                                                          
006600 01  IDARTNR-WS              PIC X(9)    VALUE SPACE.                     
006700                                                                          
006800 01  DATUM-FAELT.                                                         
006900     03  W-TIPRLIST          PIC 9(6).                                    
007000     03  W-DAPRLIST-MAX      PIC 9(8)    VALUE 99999999.                  
007100     03  W-DAPRLIST          PIC 9(8).                                    
007200     03  W-DAPRLIST-X  REDEFINES W-DAPRLIST.                              
007300      05 FILLER              PIC 9(2).                                    
007400      05 W-LISTDATUM         PIC 9(6).                                    
007500                                                                          
007600 01  MAX-MOD-LAENGD          PIC S9(4)   VALUE +411  COMP SYNC.           
007700     EJECT                                                                
007800 01  NYCKLAR-TILL-DLI.                                                    
007900     03  W-IDARTNR-X.                                                     
008000         05  W-IDARTNR       PIC S9(9)   VALUE +0  COMP-3.                
008100     03  W-KDSEGKEY-X.                                                    
008200         05  W-KDSEGKEY      PIC X(1)    VALUE SPACE.                     
008300     03  W-IDSKYLT-X.                                                     
008400         05    W-IDSKYLT     PIC X(3)    VALUE 'S  '.                     
008500     SKIP3                                                                
008600 01  MEDDELANDE.                                                          
008700     03  W-FEL-1               PIC X(26)   VALUE                          
008800             'ARTIKEL SAKNAS            '.                                
008900                                                                          
009000     03  W-FEL-2               PIC X(26)   VALUE                          
009100             'ARTIKEL UTGÅNGEN          '.                                
009200                                                                          
009300     03  W-FEL-3               PIC X(26)   VALUE                          
009400             'UPPLYSTA FÄLT FEL         '.                                
009500                                                                          
009600     03  W-FEL-4               PIC X(26)   VALUE                          
009700             'ARTIKELNUMMER EJ NUMERISKT'.                                
009800                                                                          
009900     03  W-FEL-5               PIC X(40)   VALUE                          
010000             'TRYCK PF-TANGENT FÖR UPPDATERING     '.                     
010100                                                                          
010200     03  W-FEL-6               PIC X(40)   VALUE                          
010300             'FYLL I INMATNINGSFÄLT VID UPPDATERING'.                     
010400                                                                          
010500     03  W-MED-1               PIC X(26)   VALUE                          
010600             'UPPDATERING UTFÖRD        '.                                
010700     EJECT                                                                
010710 01  MESSAGE-CODES.                                                       
010720     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010730     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010740                                                                          
010800 01  WDECEDIT                  PIC X(8)    VALUE 'WDECEDIT'.              
010900     SKIP3                                                                
011000*01       -COPY WDECAREA                                                  
011100     EJECT                                                                
011200*                   ****    PARAMETRAR TILL W005INIT                      
011300*01  -COPY WMSGINIT                                                       
011400     EJECT                                                                
011500 01  WDATAREA                  PIC X(8)    VALUE 'WDATAREA'.              
011600     SKIP3                                                                
011700*01       -COPY WDATAREA                                                  
011800     EJECT                                                                
011900 01  FILLER                    PIC X(16) VALUE 'WDH801-AREA   '.          
012000*01  WLPRIG01  -COPY WDH801                                               
012100     EJECT                                                                
012200******************************************************************        
012300*                                                                         
012400*                AREOR FOR MFS OCH SKÄRMHANTERING                         
012500*                                                                         
012600******************************************************************        
012700 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
012800     SKIP3                                                                
012900*01  MID  -COPY W5I11201                                                  
013000     EJECT                                                                
013100*01       -COPY WMSGAREA                                                  
013200     EJECT                                                                
013300*  03 MOD -COPY W5O11201   -RED MSG-AREA                                  
013400     EJECT                                                                
013500*01       -COPY WMFSAREA                                                  
013600     EJECT                                                                
013700******************************************************************        
013800*                                                                         
013900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014000*                                                                         
014100******************************************************************        
014200                                                                          
014300 01  IMS-WS.                                                              
014400     03    FILLER              PIC X(16)   VALUE 'IMS-WS     '.           
014500     SKIP3                                                                
014600*                        **** STATUS-KOD FRÅN IMS ****                    
014700     03    STATUS-WS           PIC XX.                                    
014800         88    SEGMENT-FINNS               VALUE '  '.                    
014900         88    SEGMENT-SAKNAS              VALUE 'GE'.                    
015000     SKIP3                                                                
015100     03    GODK-STATUSKODER.                                              
015200         05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.          
015300     SKIP3                                                                
015400 01  SSA1                      PIC X(64).                                 
015500 01  SSA2                      PIC X(64).                                 
015600     EJECT                                                                
015700*                        **** IMS FUNKTIONSKODER ****                     
015800*01       -COPY W0003                                                     
015900     EJECT                                                                
016000*                                DLI INPUT-OUTPUT AREA                    
016100*01  WLARTC01 -COPY WDK601                                                
016200     EJECT                                                                
016300*01  WLARTC21 -COPY WDK621                                                
016400     EJECT                                                                
016500*01  WLBENA11 -COPY WDD311 -PRE BEN-                                      
016600     EJECT                                                                
016610 01  KOM-IO-AREA.                                                         
016620*    03  -COPY WMSGKOM                                                    
016630     EJECT                                                                
016700 LINKAGE SECTION.                                                         
016800*01       -COPY W0009     -PRE MSG-                                       
016900     EJECT                                                                
016910*01       -COPY W0009     -PRE ALT-                                       
016920     EJECT                                                                
017000*01       -COPY W0008     -PRE USEA-                                      
017100         05  FILLER           PIC X.                                      
017200     EJECT                                                                
017300*01       -COPY W0008     -PRE ARTC-                                      
017400     05  FILLER                  PIC X(11).                               
017500     EJECT                                                                
017600*01       -COPY W0008     -PRE BEN-                                       
017700     05  FILLER                  PIC X(8).                                
017800     EJECT                                                                
017900*01       -COPY W0008     -PRE PRIG-                                      
018000      05 FILLER                  PIC X(18).                               
018100     EJECT                                                                
018200 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB  USEA-PCB                      
018300                                   ARTC-PCB BEN-PCB PRIG-PCB.             
018400 MAIN SECTION.                                                            
018500     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB  USEA-PCB                      
018600                                   ARTC-PCB BEN-PCB PRIG-PCB.             
018700     SKIP3                                                                
018800     PERFORM IMS-GET-MSG                                                  
018900                                                                          
019000     IF SEGMENT-FINNS                                                     
019100       PERFORM A-INIT                                                     
019200       PERFORM B-KOLLA-NYCKEL                                             
019400       IF MFS-UPD-X                                                       
019410         PERFORM IMS-GN-MSG-KOM                                           
019420       END-IF                                                             
019500       IF NYCKLAR-OK = JA                                                 
019700           IF MFS-UPDATE                                                  
019800           OR MFS-UPD-X                                                   
019900* **          HÄR BÖRJAR UPPDATERING                **                    
020000              IF MID-TIPRLIST-U NUMERIC                                   
020100                 PERFORM C-KONTROLLERA-MID-MOT-BAS                        
020200              ELSE                                                        
020300                 MOVE MFS-NUM-FAELT-FEL TO MOD-TIPRLIST-U-ATTR            
020400                 MOVE W-FEL-3           TO MOD-TEMFSFEL                   
020500                 MOVE JA                TO FLFEL-FAELT                    
020600              END-IF                                                      
020700              IF MID-IDLEVNR = ALL '+'                                    
020800                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDLEVNR-ATTR               
020900                 MOVE W-FEL-3           TO MOD-TEMFSFEL                   
021000                 MOVE JA                TO FLFEL-FAELT                    
021100              END-IF                                                      
021200                                                                          
021300              PERFORM MFS-ROER-EJ-FAELT-UTDATA                            
021400                                                                          
021500              IF FLFEL-FAELT = JA                                         
021600                 MOVE MFS-ROER-EJ-FAELT  TO  MOD-TIPRLIST-U               
021700                 MOVE MFS-ROER-EJ-FAELT  TO  MOD-IDLEVNR                  
021800              ELSE                                                        
021900                 MOVE MFS-RENSA-FAELT    TO MOD-TIPRLIST-U                
022000                 MOVE MFS-RENSA-FAELT    TO MOD-IDLEVNR                   
022100                 MOVE MFS-FORMATETS-ATTR TO MOD-TIPRLIST-U-ATTR           
022200                 MOVE MFS-FORMATETS-ATTR TO MOD-IDLEVNR-ATTR              
022300                 PERFORM D-UPPDATERA                                      
022400                 PERFORM S-BEHANDLA-BESTPRIS-INFO                         
022500                 MOVE W-MED-1            TO MOD-TEMFSINF                  
022600              END-IF                                                      
022700                                                                          
022800* **          HÄR SLUTAR UPPDATERING                 **                   
022900     EJECT                                                                
023000           ELSE                                                           
023100* **          HÄR BÖRJAR SÖKNING                     **                   
023200              IF MID-IDARTNR-IN = ALL '+' AND                             
023300                 MID-TIPRLIST-U NOT = ALL '+' AND EGEN-TRANS              
023400                 MOVE W-FEL-5 TO MOD-TEMFSFEL                             
023500                 PERFORM MFS-ROER-EJ-FAELT-UTDATA                         
023600                 MOVE MFS-ROER-EJ-FAELT TO  MOD-TIPRLIST-U                
023700                 MOVE MFS-ADD-LAES-IN-FAELT TO                            
023800                      MOD-TIPRLIST-U-ATTR                                 
023900                      MOD-IDLEVNR-ATTR                                    
024000              ELSE                                                        
024100                 PERFORM IMS-GHU-WLARTC01                                 
024200                                                                          
024300                 IF SEGMENT-FINNS                                         
024400                    IF ART-KDERS-UTG = 0                                  
024500                       PERFORM S-BEHANDLA-BESTPRIS-INFO                   
024600                       PERFORM F-BEHANDLA-BENAMNING                       
024700                       MOVE MFS-RENSA-FAELT TO MOD-TIPRLIST-U             
024800                       MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR                
024900                    ELSE                                                  
025000                       MOVE W-FEL-2 TO MOD-TEMFSFEL                       
025100                       PERFORM MFS-RENSA-FAELT-UTDATA                     
025200                       MOVE MFS-RENSA-FAELT TO MOD-TIPRLIST-U             
025300                       MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR                
025400                    END-IF                                                
025500                 ELSE                                                     
025600                    MOVE W-FEL-1 TO MOD-TEMFSFEL                          
025700                    PERFORM MFS-RENSA-FAELT-UTDATA                        
025800                    MOVE MFS-RENSA-FAELT TO MOD-TIPRLIST-U                
025900                    MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR                   
026000                 END-IF                                                   
026100                                                                          
026200              END-IF                                                      
026300* **          HÄR SLUTAR SÖKNING                    **                    
026400                                                                          
026500           END-IF                                                         
026600     EJECT                                                                
026700        ELSE                                                              
026800           MOVE W-FEL-4 TO MOD-TEMFSFEL                                   
026900           PERFORM MFS-RENSA-FAELT-UTDATA                                 
027000           MOVE MFS-RENSA-FAELT TO MOD-TIPRLIST-U                         
027100           MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR                            
027200        END-IF                                                            
027210        IF MFS-UPD-X                                                      
027220          IF MSG-KOM-IDMFSMED = SPACE                                     
027230            MOVE INF-UPDATE-DONE TO MSG-KOM-IDMFSMED                      
027240          END-IF                                                          
027250          MOVE SPACE             TO MSG-KOM-KDSVAR                        
027260          PERFORM IMS-INSERT-MSG-KOM                                      
027270        ELSE                                                              
027400          MOVE MAX-MOD-LAENGD TO MSG-KVLL                                 
027500          PERFORM IMS-INSERT-MSG                                          
027510        END-IF                                                            
027600     END-IF                                                               
027610*    CALL FELLOG                                                          
027700                                                                          
027800     MOVE ZERO TO RETURN-CODE                                             
027900     GOBACK                                                               
028000     .                                                                    
028100     EJECT                                                                
028200 A-INIT SECTION.                                                          
028300                                                                          
028400     ACCEPT DAGENS-DATUM FROM DATE                                        
028500                                                                          
028600     IF DAGENS-AAR < 50                                                   
028700       MOVE 20     TO DAGENS-SEKEL                                        
028800     ELSE                                                                 
028900       MOVE 19     TO DAGENS-SEKEL                                        
029000     END-IF                                                               
029100                                                                          
029200     ACCEPT DAGENS-TID    FROM TIME                                       
029300                                                                          
029400     MOVE 'J'      TO  JA                                                 
029500     MOVE 'N'      TO  NEJ                                                
029600                                                                          
029700     MOVE 'J'      TO  NYCKLAR-OK                                         
029800     MOVE 'N'      TO  FLFEL-FAELT                                        
029900     MOVE 'N'      TO  FLSLUTA-LAS                                        
030000                                                                          
030100     MOVE +0       TO  INDX                                               
030200                                                                          
030300     MOVE SPACE    TO  W-IDTRANS                                          
030400                       IDARTNR-WS                                         
030500                                                                          
030600     MOVE +693     TO  MAX-MOD-LAENGD                                     
030700                                                                          
030800     MOVE +0       TO  W-IDARTNR                                          
030900                                                                          
031000     IF MSG-DUBBLA-TRANSKODER                                             
031100        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I11201                
031200        MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                          
031300        MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                         
031400     ELSE                                                                 
031500        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I11201                 
031600        MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                          
031700        MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                         
031800     END-IF                                                               
031900                                                                          
032000     MOVE MSG-KDTRTYP             TO MFS-KDTRTYP                          
032100     MOVE MFS-IDTRANS             TO W-IDTRANS                            
032200                                                                          
032300     MOVE LOW-VALUE       TO MSG-AREA                                     
032400     MOVE 'W5O112N1'      TO MFS-IDMOD                                    
032500     MOVE '5112'          TO MOD-IDTRANS                                  
032600                                                                          
032700     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
032800                             MOD-TEMFSFEL                                 
032900                             MOD-TEMFSINF                                 
033000     .                                                                    
033100     EJECT                                                                
033200 B-KOLLA-NYCKEL SECTION.                                                  
033300     SKIP3                                                                
033310     IF MFS-UPD-X                                                         
033320****************  DISPATCHANROP                                           
033330                                                                          
033340       IF MID-IDARTNR-IN = ALL '+'                                        
033350         MOVE MID-IDARTNR-UT TO IDARTNR-WS                                
033360         INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO               
033370       ELSE                                                               
033380         MOVE MID-IDARTNR-IN TO IDARTNR-WS                                
033390       END-IF                                                             
033391     ELSE                                                                 
033400       MOVE ALL '+' TO MSGI-WMSGINIT                                      
033500       MOVE '001'           TO MSGI-KDCALL                                
033600       MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                              
033700       MOVE '5112'          TO MSGI-IDTRANS                               
033800       MOVE MSG-LTERM-NAME  TO MSGI-IDLTERM-USER                          
033900       IF MFS-IDTRANS = '5112'                                            
034000       OR (MID-IDARTNR-IN NUMERIC                                         
034100       AND MID-IDARTNR-IN > ZERO)                                         
034200           MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                            
034300       END-IF                                                             
034400       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
034500                                                                          
034600       IF MSGI-IDLAND-SPR NOT = 'GB'                                      
034700          MOVE 'S  '  TO  W-IDSKYLT                                       
034800       ELSE                                                               
034900          MOVE 'GB '  TO  W-IDSKYLT                                       
035000       END-IF                                                             
035100                                                                          
035200       MOVE MSGI-IDARTNR TO IDARTNR-WS                                    
035300       INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                     
035400                                                                          
035500       IF MID-IDARTNR-IN = ALL '+'                                        
035600          CONTINUE                                                        
035700       ELSE                                                               
035800          MOVE ' '          TO MFS-KDTRTYP                                
035900       END-IF                                                             
036000     END-IF                                                               
036010                                                                          
036100     IF NOT EGEN-TRANS                                                    
036200        MOVE ' '            TO MFS-KDTRTYP                                
036300     END-IF                                                               
036400                                                                          
036500     IF IDARTNR-WS NOT NUMERIC                                            
036600        MOVE NEJ TO NYCKLAR-OK                                            
036700     ELSE                                                                 
036800        MOVE IDARTNR-WS TO W-IDARTNR                                      
036900     END-IF                                                               
037000                                                                          
037100     IF NYCKLAR-OK = NEJ                                                  
037200        IF GODK-TRANS                                                     
037300           MOVE IDARTNR-WS TO MOD-IDARTNR-UT                              
037400           INSPECT MOD-IDARTNR-UT REPLACING                               
037500                   LEADING ZERO BY SPACE                                  
037600        ELSE                                                              
037700           MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                         
037800        END-IF                                                            
037900     ELSE                                                                 
038000        MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                 
038100        INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE            
038200     END-IF                                                               
038300     .                                                                    
038400     EJECT                                                                
038500 C-KONTROLLERA-MID-MOT-BAS SECTION.                                       
038600     SKIP3                                                                
038700     PERFORM IMS-GHU-WLARTC01                                             
038800                                                                          
038900     IF SEGMENT-FINNS                                                     
039000                                                                          
039100        IF MID-TIPRLIST-U = ALL '+' OR MID-IDLEVNR = ALL '+'              
039200           MOVE W-FEL-6 TO MOD-TEMFSFEL                                   
039300           MOVE JA TO FLFEL-FAELT                                         
039400        ELSE                                                              
039500                                                                          
039600* **       HÄR LÄSES BEST.PRIS SEGMENT FÖR ATT               **           
039700* **       KONTROLLERA OM PRIS FÖR ANGIVET                   **           
039800* **       DATUM FINNS LAGRAT OCH KAN TAS BORT               **           
039900                                                                          
040000           MOVE 1 TO INDX                                                 
040100           PERFORM UNTIL SEGMENT-SAKNAS OR INDX > 5 OR                    
040200                         FLSLUTA-LAS = JA                                 
040300              PERFORM IMS-GNP-WLARTC21                                    
040400                                                                          
040500              IF SEGMENT-FINNS                                            
040600                IF PRL-FLHUVLEV = JA                                      
040700                 SUBTRACT PRL-DAPRLIST-9KOMPL FROM W-DAPRLIST-MAX         
040800                 GIVING W-DAPRLIST                                        
040900                 MOVE MID-TIPRLIST-U   TO TMP1-YYMMDD                     
041000                 MOVE W-LISTDATUM      TO TMP2-YYMMDD                     
041100                 PERFORM WY2000P1                                         
041200                 IF TMP1-YYMMDD > TMP2-YYMMDD                             
041300                    MOVE MFS-NUM-FAELT-FEL TO                             
041400                         MOD-TIPRLIST-U-ATTR                              
041500                    MOVE W-FEL-3           TO MOD-TEMFSFEL                
041600                    MOVE JA TO FLFEL-FAELT                                
041700                    MOVE JA TO FLSLUTA-LAS                                
041800                                                                          
041900                 ELSE                                                     
042000                                                                          
042100                    IF MID-TIPRLIST-U = W-LISTDATUM AND                   
042200                      MID-IDLEVNR = PRL-IDLEVNR                           
042300                      MOVE MFS-NUM-FAELT-RAETT TO                         
042400                           MOD-TIPRLIST-U-ATTR                            
042500                           MOD-IDLEVNR-ATTR                               
042600                      MOVE JA TO FLSLUTA-LAS                              
042700                    ELSE                                                  
042800                       ADD  1 TO INDX                                     
042900                    END-IF                                                
043000                                                                          
043100                 END-IF                                                   
043200                END-IF                                                    
043300              ELSE                                                        
043400                 MOVE MFS-NUM-FAELT-FEL TO                                
043500                      MOD-TIPRLIST-U-ATTR                                 
043600                      MOD-IDLEVNR-ATTR                                    
043700                 MOVE W-FEL-3           TO MOD-TEMFSFEL                   
043800                 MOVE JA TO FLFEL-FAELT                                   
043900              END-IF                                                      
044000                                                                          
044100           END-PERFORM                                                    
044200                                                                          
044300           IF INDX > 5                                                    
044400              MOVE MFS-NUM-FAELT-FEL TO                                   
044500                   MOD-TIPRLIST-U-ATTR                                    
044600                   MOD-IDLEVNR-ATTR                                       
044700              MOVE W-FEL-3           TO MOD-TEMFSFEL                      
044800              MOVE JA TO FLFEL-FAELT                                      
044900           END-IF                                                         
045000                                                                          
045100        END-IF                                                            
045200                                                                          
045300     ELSE                                                                 
045400        MOVE W-FEL-1 TO MOD-TEMFSFEL                                      
045500        PERFORM MFS-RENSA-FAELT-UTDATA                                    
045600        MOVE MFS-RENSA-FAELT TO MOD-TIPRLIST-U                            
045700        MOVE JA      TO FLFEL-FAELT                                       
045800     END-IF                                                               
045900     .                                                                    
046000     EJECT                                                                
046100 D-UPPDATERA SECTION.                                                     
046200                                                                          
046300     PERFORM IMS-GHU-WLARTC01                                             
046400     MOVE ART-IDLEVNR                TO PRI-N-IDLEVNR-PR                  
046500                                                                          
046600     MOVE 1 TO INDX                                                       
046700     MOVE NEJ TO FLSLUTA-LAS                                              
046800     PERFORM UNTIL SEGMENT-SAKNAS OR INDX > 5 OR                          
046900                   FLSLUTA-LAS = JA                                       
047000        PERFORM IMS-GHNP-WLARTC21                                         
047100        IF SEGMENT-FINNS                                                  
047200          IF PRL-FLHUVLEV = JA                                            
047300           SUBTRACT PRL-DAPRLIST-9KOMPL FROM W-DAPRLIST-MAX               
047400           GIVING W-DAPRLIST                                              
047500           MOVE W-LISTDATUM          TO W-TIPRLIST                        
047600                                                                          
047700           IF MID-TIPRLIST-U = W-TIPRLIST  AND                            
047800              MID-IDLEVNR    = PRL-IDLEVNR                                
047900                                                                          
048000              MOVE PRL-KDPRURSP      TO PRI-O-KDPRURSP                    
048100              MOVE W-TIPRLIST        TO PRI-O-TIPRLIST                    
048200*             COMPUTE PRI-O-TIPRLIST = PRL-TIPRLIST * -1                  
048300              MOVE PRL-IDLEVNR       TO PRI-O-IDLEVNR-PR                  
048400              MOVE PRL-PRARTBES-PR   TO PRI-O-PRARTBES-PR                 
048500              MOVE PRL-PRARTBEL-PR   TO PRI-O-PRARTBEL-PR                 
048600              MOVE PRL-KDSTATUS-PR   TO PRI-O-KDSTATUS-PR                 
048700              MOVE PRL-SUINLEV-PR    TO PRI-O-SUINLEV-PR                  
048800              MOVE PRL-KDVALISO      TO PRI-O-KDVALISO                    
048900                                                                          
049000              PERFORM IMS-DLET-WLARTC                                     
049100                                                                          
049200              MOVE SPACE             TO PRI-N-KDPRURSP                    
049300              MOVE MID-TIPRLIST-U    TO PRI-N-TIPRLIST                    
049400              MOVE ZERO              TO PRI-N-PRARTBES-PR                 
049500                                        PRI-N-PRARTBEL-PR                 
049600                                        PRI-N-KDSTATUS-PR                 
049700                                        PRI-N-SUINLEV-PR                  
049800              MOVE SPACE             TO PRI-N-KDVALISO                    
049900              MOVE W-IDARTNR         TO PRI-IDARTNR                       
050000              MOVE DAGENS-DAT        TO PRI-DAREGDAT                      
050100              MOVE DAGENS-KLOCK      TO PRI-TIREGTID                      
050200              MOVE 'J'               TO PRI-FLKLAR                        
050300              MOVE 'N'               TO PRI-FLPRFIL                       
050400              MOVE 'N'               TO PRI-FLPRIBES                      
050500              MOVE 'N'               TO PRI-FLPRIGO                       
050600              MOVE MSG-SIGNON-USERID TO PRI-IDUSER                        
050700              MOVE 'B'               TO PRI-KDPRIBEH                      
050800              MOVE ZERO              TO PRI-REDIRLEV                      
050900              MOVE ' '               TO PRI-O-KDCMD                       
051000              MOVE ZERO              TO PRI-O-PRARTBES                    
051100                                        PRI-O-PRARTSJK                    
051200                                        PRI-O-PRARTSTD                    
051300                                        PRI-O-PRDIRLON                    
051400                                        PRI-O-PRDMTRL                     
051500                                        PRI-O-PRINK                       
051600                                        PRI-O-PRLFKST                     
051700                                        PRI-O-PROVRPAL                    
051800                                        PRI-O-RETULF                      
051900              MOVE SPACE             TO PRI-O-TEARTNOT                    
052000              MOVE ' '               TO PRI-N-KDCMD                       
052100              MOVE ZERO              TO PRI-N-PRARTBES                    
052200                                        PRI-N-PRARTSJK                    
052300                                        PRI-N-PRARTSTD                    
052400                                        PRI-N-PRDIRLON                    
052500                                        PRI-N-PRDMTRL                     
052600                                        PRI-N-PRINK                       
052700                                        PRI-N-PRLFKST                     
052800                                        PRI-N-PROVRPAL                    
052900                                        PRI-N-RETULF                      
053000              MOVE SPACE             TO PRI-N-TEARTNOT                    
053100              PERFORM IMS-ISRT-WDH801                                     
053200              MOVE JA                TO FLSLUTA-LAS                       
053300           ELSE                                                           
053400              ADD 1 TO INDX                                               
053500           END-IF                                                         
053600          END-IF                                                          
053700        END-IF                                                            
053800     END-PERFORM                                                          
053900     .                                                                    
054000     EJECT                                                                
054100 F-BEHANDLA-BENAMNING SECTION.                                            
054200     SKIP3                                                                
054300     PERFORM IMS-GU-WLBENA11                                              
054400     IF SEGMENT-FINNS                                                     
054500        MOVE BEN-TEXT-BEART  TO MOD-BEART                                 
054600     ELSE                                                                 
054700        MOVE MFS-RENSA-FAELT TO MOD-BEART                                 
054800     END-IF                                                               
054900     .                                                                    
055000     EJECT                                                                
055100 S-BEHANDLA-BESTPRIS-INFO SECTION.                                        
055200     SKIP3                                                                
055300     MOVE 1 TO INDX                                                       
055400     PERFORM IMS-GNP-WLARTC21                                             
055500     PERFORM UNTIL INDX > 5 OR SEGMENT-SAKNAS                             
055600                                                                          
055700       IF SEGMENT-FINNS                                                   
055800         IF PRL-FLHUVLEV = JA                                             
055900           SUBTRACT PRL-DAPRLIST-9KOMPL FROM W-DAPRLIST-MAX               
056000           GIVING W-DAPRLIST                                              
056100           MOVE W-LISTDATUM         TO W-TIPRLIST                         
056200           MOVE W-TIPRLIST          TO MOD-TIPRLIST-PR (INDX)             
056300           MOVE PRL-IDLEVNR         TO MOD-IDLEVNR-PR (INDX)              
056400           MOVE PRL-PRARTBES-PR     TO MOD-PRARTBES-PR (INDX)             
056500           MOVE PRL-PRARTBEL-PR     TO MOD-PRARTBEL-PR (INDX)             
056600                                                                          
056700           IF PRL-SUINLEV-PR > 0    AND PRL-KDSTATUS-PR = 1               
056800             MOVE 'INLEV' TO MOD-KDSTATUS-PR (INDX)                       
056900           ELSE                                                           
057000             IF PRL-KDSTATUS-PR = 1                                       
057100               MOVE 'GODK' TO  MOD-KDSTATUS-PR (INDX)                     
057200             ELSE                                                         
057300               MOVE 'PREL' TO MOD-KDSTATUS-PR (INDX)                      
057400             END-IF                                                       
057500           END-IF                                                         
057600           MOVE PRL-KDPRURSP    TO MOD-KDPRURSP (INDX)                    
057700           MOVE PRL-KDVALISO TO MOD-KDVALISO (INDX)                       
057800           ADD 1 TO INDX                                                  
057900         END-IF                                                           
058000         IF INDX < 6                                                      
058100           PERFORM IMS-GNP-WLARTC21                                       
058200         END-IF                                                           
058300                                                                          
058400       END-IF                                                             
058500     END-PERFORM                                                          
058600     PERFORM UNTIL INDX > 5                                               
058700       MOVE MFS-RENSA-FAELT TO MOD-BEST-PRIS (INDX)                       
058800       ADD 1 TO INDX                                                      
058900     END-PERFORM                                                          
059000     .                                                                    
059100     EJECT                                                                
059200 MFS-ROER-EJ-FAELT-UTDATA SECTION.                                        
059300     SKIP3                                                                
059400     MOVE MFS-ROER-EJ-FAELT TO  MOD-BEART                                 
059500                                MOD-BEST-PRIS(1)                          
059600                                MOD-BEST-PRIS(2)                          
059700                                MOD-BEST-PRIS(3)                          
059800                                MOD-BEST-PRIS(4)                          
059900                                MOD-BEST-PRIS(5)                          
060000                                MOD-IDLEVNR                               
060100     .                                                                    
060200     SKIP3                                                                
060300 MFS-RENSA-FAELT-UTDATA SECTION.                                          
060400     SKIP3                                                                
060500                                                                          
060600     MOVE MFS-RENSA-FAELT  TO   MOD-BEART                                 
060700                                MOD-BEST-PRIS(1)                          
060800                                MOD-BEST-PRIS(2)                          
060900                                MOD-BEST-PRIS(3)                          
061000                                MOD-BEST-PRIS(4)                          
061100                                MOD-BEST-PRIS(5)                          
061200                                MOD-IDLEVNR                               
061300     .                                                                    
061400     EJECT                                                                
061410                                                                          
061500 IMS-GET-MSG SECTION.                                                     
061700     MOVE '  QC' TO GODK-STATUSKODER                                      
061800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
061900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
062000     PERFORM IMS-STATUSKONTROLL                                           
062100     .                                                                    
062200     SKIP3                                                                
062210                                                                          
062300 IMS-INSERT-MSG SECTION.                                                  
062500     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
062600       MOVE '0' TO MFS-KDHUVOMR                                           
062700     END-IF                                                               
062800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
062900     MOVE SPACE TO GODK-STATUSKODER                                       
063000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
063100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
063200     PERFORM IMS-STATUSKONTROLL                                           
063300     .                                                                    
063400     EJECT                                                                
063410                                                                          
063411 IMS-INSERT-MSG-KOM SECTION.                                              
063413     MOVE SPACE TO GODK-STATUSKODER                                       
063414     CALL CBLTDLI USING ISRT ALT-PCB KOM-IO-AREA                          
063415     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
063416     PERFORM IMS-STATUSKONTROLL                                           
063417     .                                                                    
063418     EJECT                                                                
063419                                                                          
063420 IMS-GN-MSG-KOM SECTION.                                                  
063440     MOVE '  QD' TO GODK-STATUSKODER                                      
063450     CALL CBLTDLI USING GN MSG-PCB KOM-IO-AREA                            
063460     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
063470     PERFORM IMS-STATUSKONTROLL                                           
063480     .                                                                    
063490     EJECT                                                                
063491                                                                          
063500 IMS-GHU-WLARTC01 SECTION.                                                
063700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
063800     DELIMITED  BY SIZE INTO SSA1                                         
063900     MOVE '  GE' TO GODK-STATUSKODER                                      
064000     CALL CBLTDLI USING GHU ARTC-PCB WLARTC01 SSA1                        
064100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
064200     PERFORM IMS-STATUSKONTROLL                                           
064300     .                                                                    
064400     SKIP3                                                                
064500 IMS-GNP-WLARTC21 SECTION.                                                
064600                                                                          
064700     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
064800     MOVE 'WLARTC21 ' TO SSA2                                             
064900     MOVE '  GE' TO GODK-STATUSKODER                                      
065000     CALL CBLTDLI USING GNP ARTC-PCB WLARTC21 SSA1 SSA2                   
065100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
065200     PERFORM IMS-STATUSKONTROLL                                           
065300     .                                                                    
065400     SKIP3                                                                
065500 IMS-GHNP-WLARTC21 SECTION.                                               
065600                                                                          
065700     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
065800     MOVE 'WLARTC21 ' TO SSA2                                             
065900     MOVE '  GE' TO GODK-STATUSKODER                                      
066000     CALL CBLTDLI USING GHNP ARTC-PCB WLARTC21 SSA1 SSA2                  
066100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
066200     PERFORM IMS-STATUSKONTROLL                                           
066300     .                                                                    
066400     EJECT                                                                
066500 IMS-DLET-WLARTC SECTION.                                                 
066600                                                                          
066700     MOVE '  GE' TO GODK-STATUSKODER                                      
066800     CALL CBLTDLI USING DLET ARTC-PCB WLARTC21                            
066900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
067000     PERFORM IMS-STATUSKONTROLL                                           
067100     .                                                                    
067200     SKIP3                                                                
067300 IMS-GU-WLBENA11 SECTION.                                                 
067400                                                                          
067500     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
067600     DELIMITED BY SIZE INTO SSA1                                          
067700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X  ')'                        
067800     DELIMITED BY SIZE INTO SSA2                                          
067900     MOVE '  GE' TO GODK-STATUSKODER                                      
068000     CALL CBLTDLI USING GU BEN-PCB BEN-TEXT-WDD311 SSA1 SSA2              
068100     MOVE BEN-STATUS-CODE TO STATUS-WS                                    
068200     PERFORM IMS-STATUSKONTROLL                                           
068300     .                                                                    
068400     SKIP3                                                                
068500 IMS-ISRT-WDH801   SECTION.                                               
068600                                                                          
068700     MOVE 'WLPRIG01 ' TO SSA1                                             
068800     MOVE '  II'            TO GODK-STATUSKODER                           
068900     CALL CBLTDLI USING ISRT PRIG-PCB PRI-WDH801 SSA1                     
069000     MOVE PRIG-STATUS-CODE  TO STATUS-WS                                  
069100     PERFORM IMS-STATUSKONTROLL                                           
069200     .                                                                    
069300     EJECT                                                                
069400 IMS-STATUSKONTROLL SECTION.                                              
069500                                                                          
069600     SET STATUS-IX TO 1                                                   
069700     SEARCH GODK-STATUS AT END CALL FELLOG                                
069800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
069900     END-SEARCH                                                           
070000     .                                                                    
070100     EJECT                                                                
070200*    -COPY WY2000P1                                                       
