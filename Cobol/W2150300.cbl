000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2150300.                                                
000400 AUTHOR.         FRONTEC, GÖTEBORG.                                       
000500 DATE-WRITTEN.   96/06/19.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        UPPDATERAR WDD9, WDG3, WDJ1, WDK6 UTIFRÅN FILEN                  
001100*        W21510 SOM SKAPAS I PROGRAM W21502                               
001200*                                                                         
001300*        CHECKPOINT TAS INTE, ISTÄLLET SÅ SKRIVS DE POSTER                
001400*        SOM FINNS KVAR PÅ INFILEN PÅ EN UTFIL NÄR CA 500                 
001500*        UPPDATERINGAR HAR SKETT. SENARE GÖRS EN OMSTART                  
001600*        AV PROGRAMMET MED DEN NYA FILEN                                  
001700*                                                                         
001800*                                                                         
001900*                                                                         
002000*        PROGRAMMET UPPDATERAR WLSATB (WDJ1)                              
002100*                              WLARTC (WDK6)                              
002200*                              WLXXBY (WDG3)                              
002300*                              WLXXBM (WDG3)                              
002400*                              WLINLB (WDD9)                              
002500*                                                                         
002600*    ABENDKODER:                                                          
002700*        U0016 -  . . . .                                                 
002800*        U1000 -  . . . .                                                 
002900*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP2                                                                
003400 INPUT-OUTPUT SECTION.                                                    
003500                                                                          
003600 FILE-CONTROL.                                                            
003700     SKIP2                                                                
003800*          --- UPPDATERINGSFIL                                            
003900     SELECT W21510-IN                  ASSIGN TO W21503D1.                
004000     SKIP2                                                                
004100*          --- UPPDATERINGSFIL FÖR RESTERANDE POSTER                      
004200     SELECT W21510-UT                  ASSIGN TO W21503D2.                
004300     EJECT                                                                
004400 DATA DIVISION.                                                           
004500     SKIP3                                                                
004600 FILE SECTION.                                                            
004700     SKIP3                                                                
004800 FD  W21510-IN                                                            
004900     RECORDING       V                                                    
005000     BLOCK CONTAINS  0.                                                   
005100                                                                          
005200*01  -COPY W215100      -L.                                               
005300                                                                          
005400*01  -COPY W215101      -L.                                               
005500                                                                          
005600*01  -COPY W215102      -L.                                               
005700                                                                          
005800*01  -COPY W215103      -L.                                               
005900                                                                          
006000*01  -COPY W215104      -L.                                               
006100                                                                          
006200*01  -COPY W215105      -L.                                               
006300     SKIP3                                                                
006400 FD  W21510-UT                                                            
006500     RECORDING       V                                                    
006600     BLOCK CONTAINS  0.                                                   
006700                                                                          
006800 01  W21510-POST     PIC X(32).                                           
006900     EJECT                                                                
007000 WORKING-STORAGE SECTION.                                                 
007100     SKIP2                                                                
007200*    -- CHECKED BY WY2000                                                 
007300     SKIP3                                                                
007400 77  IDPGM                       PIC X(8)    VALUE 'W2150300'.            
007500 77  JA                          PIC X       VALUE 'J'.                   
007600 77  NEJ                         PIC X       VALUE 'N'.                   
007601                                                                          
007610*01  -COPY WWDCKONS                                                       
007620                                                                          
007700     SKIP2                                                                
007800 01  FELTEXT.                                                             
007900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008100                                                                          
008200 77  W21510-EOF-SW               PIC X       VALUE 'N'.                   
008300     88  END-OF-W21510                       VALUE 'J'.                   
008400     EJECT                                                                
008500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008600 01  FILLER REDEFINES DAGENS-DATUM.                                       
008700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009000*                                                                         
009100 01  W-SP-IDARTNR-SATS           PIC S9(9)   COMP-3.                      
009200*                                                                         
009300 01  W-ANT-UPPDAT                PIC 9(4)    VALUE ZERO.                  
009400 01  W-MAX-UPPDAT                PIC 9(3)    VALUE 500.                   
009500*                                                                         
009600     EJECT                                                                
009700 01  DYNAMISKA-SUBPROGRAM.                                                
009800*                                                                         
009900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010200     EJECT                                                                
010300*    --- PARAMETRAR TILL POSTSUM                                          
010400*                                                                         
010500*01  -COPY W0005   -PRE  POSTSUM-                                         
010600     EJECT                                                                
010700 01  IN-AREA-START               PIC X(24)   VALUE                        
010800                                             'IN-AREA-START'.             
010900     SKIP2                                                                
011000 01  IN-AREA.                                                             
011100     03  IN-AREA-0.                                                       
011200         05  IN-IDPTYP           PIC X(3).                                
011300         05  FILLER              PIC X(29).                               
011400*   03  FILLER -COPY W215100  -PRE IN-100-  -RED  IN-AREA-0               
011500*   03  FILLER -COPY W215101  -PRE IN-101-  -RED  IN-AREA-0               
011600*   03  FILLER -COPY W215102  -PRE IN-102-  -RED  IN-AREA-0               
011700*   03  FILLER -COPY W215103  -PRE IN-103-  -RED  IN-AREA-0               
011800*   03  FILLER -COPY W215104  -PRE IN-104-  -RED  IN-AREA-0               
011900*   03  FILLER -COPY W215105  -PRE IN-105-  -RED  IN-AREA-0               
012000     EJECT                                                                
012100 01  UT-AREA-START               PIC X(24)   VALUE                        
012200                                             'UT-AREA-START'.             
012300     SKIP2                                                                
012400                                                                          
012500*01  AREA -COPY W215100     -PRE UT-                                      
012600*                                                                         
012700     EJECT                                                                
012800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012900     SKIP3                                                                
013000 01  NYCKLAR-TILL-DLI.                                                    
013100     03  W-IDARTNR-SATS-X.                                                
013200         05  W-IDARTNR-SATS      PIC S9(9)   VALUE ZERO COMP-3.           
013300     03  W-IDARTNR-X.                                                     
013400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013500     03  W-WDD901KY-X.                                                    
013600         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
013700         05  W-IDDC-D9           PIC  X(2)   VALUE SPACE.                 
013800     03  W-IDLEVNR-X.                                                     
013900         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
014000     03  W-KDAVROP-X.                                                     
014100         05  W-KDAVROP           PIC S9(1)   VALUE ZERO COMP-3.           
014200     03  W-DAAVROP-AVS-X.                                                 
014300         05  W-DAAVROP-AVS       PIC  9(6).                               
014400     03  W-WDJ111KY-X.                                                    
014500         05  W-KDSTRRAD          PIC X(1)    VALUE SPACE.                 
014600         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
014700     03  W-IDORDNSB-X.                                                    
014800         05  W-IDORDNSB          PIC S9(5)   VALUE ZERO COMP-3.           
014900     03 W-WDGXKEY-2201-X.                                                 
015000         05 W-IDHTYP-2201        PIC X(4)    VALUE '2201'.                
015100         05 FILLER               PIC X(26)   VALUE LOW-VALUE.             
015200     03 W-WDGXKEY-2233-X.                                                 
015300         05 W-IDHTYP-2233        PIC X(4)    VALUE '2233'.                
015400         05 FILLER               PIC X(26)   VALUE LOW-VALUE.             
015500     SKIP2                                                                
015600*    --- STATUS-KOD FRÅN IMS                                              
015700 01  STATUS-WS                   PIC XX.                                  
015800     88  SEGMENT-FINNS                       VALUE '  '.                  
015900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
016200     88  IMS-EJ-OK                           VALUE 'XD'.                  
016300     SKIP2                                                                
016400 01  GODK-STATUSKODER.                                                    
016500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016600     SKIP3                                                                
016700 01  SSA1                        PIC X(64).                               
016800 01  SSA2                        PIC X(64).                               
016900 01  SSA3                        PIC X(64).                               
017000     EJECT                                                                
017100*    --- IMS FUNKTIONSKODER                                               
017200*01  -COPY W0003                                                          
017300     EJECT                                                                
017400*    ---  DLI INPUT-OUTPUT AREA                                           
017500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
017600     SKIP3                                                                
017700 01  DLI-IO-AREA.                                                         
017800     03  IO-AREA                 PIC X(250)  VALUE SPACE.                 
017900     SKIP3                                                                
018000     03  WLSATB01 REDEFINES IO-AREA.                                      
018100*        05  -COPY WDJ101  -PRE SATB-                                     
018200     SKIP3                                                                
018300     03  WLSATB11 REDEFINES IO-AREA.                                      
018400*        05  -COPY WDJ111  -PRE SATB-                                     
018500     SKIP3                                                                
018600     03  WLARTC01 REDEFINES IO-AREA.                                      
018700*        05  -COPY WDK601  -PRE ARTC-                                     
018800     SKIP3                                                                
018900     03  WLXXBY02 REDEFINES IO-AREA.                                      
019000*        05  -COPY WDGX2234 -PRE XXBY-                                    
019100     SKIP3                                                                
019200     03  WLXXBM02 REDEFINES IO-AREA.                                      
019300*        05  -COPY WDG32201 -PRE XXBM-                                    
019400     SKIP3                                                                
019500     03  WLINLB32 REDEFINES IO-AREA.                                      
019600*        05  -COPY WDD907  -PRE INLB-                                     
019700     EJECT                                                                
019800 LINKAGE SECTION.                                                         
019900                                                                          
020000*01  -COPY W0009   -PRE MSG-                                              
020100     EJECT                                                                
020200*01  -COPY W0008  -PRE SATB-                                              
020300     05  FILLER                  PIC X.                                   
020400     EJECT                                                                
020500*01  -COPY W0008  -PRE ARTC-                                              
020600     05  FILLER                  PIC X.                                   
020700     EJECT                                                                
020800*01  -COPY W0008  -PRE XXBY-                                              
020900     05  FILLER                  PIC X.                                   
021000     EJECT                                                                
021100*01  -COPY W0008  -PRE XXBM-                                              
021200     05  FILLER                  PIC X.                                   
021300     EJECT                                                                
021400*01  -COPY W0008  -PRE INLB-                                              
021500     05  FILLER                  PIC X.                                   
021600     EJECT                                                                
021700 PROCEDURE DIVISION  USING MSG-PCB SATB-PCB ARTC-PCB                      
021800                           XXBY-PCB XXBM-PCB INLB-PCB.                    
021900 MAIN SECTION.                                                            
022000     ENTRY 'DLITCBL' USING MSG-PCB SATB-PCB ARTC-PCB                      
022100                           XXBY-PCB XXBM-PCB INLB-PCB.                    
022200                                                                          
022300     SKIP2                                                                
022400     PERFORM A-INIT                                                       
022500     PERFORM S01-LAES-W21510                                              
022600     PERFORM UNTIL (   END-OF-W21510                                      
022700                    OR W-ANT-UPPDAT > W-MAX-UPPDAT)                       
022800       MOVE IN-100-IDARTNR-SATS TO W-SP-IDARTNR-SATS                      
022900                                                                          
023000       PERFORM UNTIL (   END-OF-W21510                                    
023100                      OR IN-100-IDARTNR-SATS                              
023200                            NOT = W-SP-IDARTNR-SATS)                      
023300                                                                          
023400         IF IN-IDPTYP = '100'                                             
023500           PERFORM B-UPPDATERA-SATB01                                     
023600         ELSE                                                             
023700           IF IN-IDPTYP = '101'                                           
023800             PERFORM C-UPPDATERA-SATB11                                   
023900           ELSE                                                           
024000             IF IN-IDPTYP = '102'                                         
024100               PERFORM D-UPPDATERA-ARTC01                                 
024200             ELSE                                                         
024300               IF IN-IDPTYP = '103'                                       
024400                 PERFORM E-ISRT-XXBY11                                    
024500               ELSE                                                       
024600                 IF IN-IDPTYP = '104'                                     
024700                   PERFORM F-ISRT-XXBM11                                  
024800                 ELSE                                                     
024900                   PERFORM G-ISRT-INLB32                                  
025000                 END-IF                                                   
025100               END-IF                                                     
025200             END-IF                                                       
025300           END-IF                                                         
025400         END-IF                                                           
025500         PERFORM S01-LAES-W21510                                          
025600       END-PERFORM                                                        
025700     END-PERFORM                                                          
025800                                                                          
025900     PERFORM H-SKRIV-OBEHANDLADE                                          
026000                                                                          
026100     PERFORM Z-FINIT                                                      
026200                                                                          
026300     MOVE ZERO TO RETURN-CODE                                             
026400     GOBACK                                                               
026500     .                                                                    
026600     EJECT                                                                
026700 A-INIT SECTION.                                                          
026800     SKIP2                                                                
026900                                                                          
027000     OPEN INPUT W21510-IN                                                 
027100                                                                          
027200     OPEN OUTPUT W21510-UT                                                
027300                                                                          
027400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
027500     .                                                                    
027600     EJECT                                                                
027700 B-UPPDATERA-SATB01 SECTION.                                              
027800***                                                                       
027900* UPPDATERAR SATB01 (WDJ101) MED DAGENS DATUM                             
028000***                                                                       
028100                                                                          
028200     MOVE IN-100-IDARTNR-SATS TO W-IDARTNR                                
028300     PERFORM IMS-GHU-SATB01                                               
028400                                                                          
028500     MOVE IN-100-TIUPPDAT     TO SATB-STR-TIUPPDAT                        
028600     PERFORM IMS-REPL-SATB01                                              
028700     ADD +1                   TO W-ANT-UPPDAT                             
028800     .                                                                    
028900     EJECT                                                                
029000 C-UPPDATERA-SATB11 SECTION.                                              
029100***                                                                       
029200* UPPDATERAR SATB11 (WDJ111)                                              
029300***                                                                       
029400                                                                          
029500     MOVE IN-101-IDARTNR-SATS TO W-IDARTNR                                
029600     MOVE IN-101-KDSTRRAD     TO W-KDSTRRAD                               
029700     MOVE IN-101-IDRADNR      TO W-IDRADNR                                
029800                                                                          
029900     PERFORM IMS-GHU-SATB11                                               
030000                                                                          
030100     MOVE IN-101-KDISATS  TO SATB-RAD-KDISATS                             
030200     PERFORM IMS-REPL-SATB11                                              
030300     ADD +1               TO W-ANT-UPPDAT                                 
030400     .                                                                    
030500     EJECT                                                                
030600 D-UPPDATERA-ARTC01 SECTION.                                              
030700***                                                                       
030800* UPPDATERAR ARTC01 (WDK601)                                              
030900***                                                                       
031000                                                                          
031100     MOVE IN-102-IDARTNR-SATS TO W-IDARTNR-SATS                           
031200     PERFORM IMS-GHU-ARTC01                                               
031300                                                                          
031400     MOVE IN-102-FLIART       TO ARTC-ART-FLIART                          
031500     PERFORM IMS-REPL-ARTC01                                              
031600     ADD +1                   TO W-ANT-UPPDAT                             
031700     .                                                                    
031800     EJECT                                                                
031900 E-ISRT-XXBY11 SECTION.                                                   
032000***                                                                       
032100* SKAPAR NYTT SEGMENT PÅ XXBY11 (WDG311)                                  
032200***                                                                       
032300                                                                          
032400     MOVE IN-103-IDARTNR-SATS TO XXBY-2234-IDARTNR-SATS                   
032500     MOVE IN-103-IDARTNR-ING  TO XXBY-2234-IDARTNR-ING                    
032600     MOVE IN-103-KDISATS      TO XXBY-2234-KDISATS                        
032700     MOVE IN-103-KVPB-SEP-TOT TO XXBY-2234-KVPB-SEP-TOT                   
032800     MOVE IN-103-REANTPSA-NY  TO XXBY-2234-REANTPSA-NY                    
032900     MOVE IN-103-REANTPSA-GAMMAL                                          
033000                              TO XXBY-2234-REANTPSA-GAMMAL                
033100     MOVE IN-103-TIBEHDAT     TO XXBY-2234-TIBEHDAT                       
033200                                                                          
033300     PERFORM IMS-ISRT-XXBY11                                              
033400     ADD +1                   TO W-ANT-UPPDAT                             
033500     .                                                                    
033600     EJECT                                                                
033700 F-ISRT-XXBM11 SECTION.                                                   
033800***                                                                       
033900* SKAPAR NYTT SEGMENT PÅ XXBM11 (WDG311)                                  
034000***                                                                       
034100                                                                          
034200     MOVE IN-104-IDARTNR-SATS      TO XXBM-IDARTNR-SATS                   
034300     MOVE IN-104-FLAGGA-LPKNTL-ING TO XXBM-FLAGGA-LPKNTL-ING              
034400     PERFORM IMS-ISRT-XXBM11                                              
034500     ADD +1                        TO W-ANT-UPPDAT                        
034600     .                                                                    
034700     EJECT                                                                
034800 G-ISRT-INLB32 SECTION.                                                   
034900***                                                                       
035000* SKAPAR NYTT SEGMENT PÅ INLB32 (WDD932)                                  
035100***                                                                       
035200                                                                          
035300     MOVE IN-105-IDARTNR-SATS TO W-IDARTNR-SATS                           
035400     MOVE IN-105-IDLEVNR      TO W-IDLEVNR                                
035500     MOVE IN-105-DAAVROP-AVS  TO W-DAAVROP-AVS                            
035600     MOVE IN-105-KDAVROP      TO W-KDAVROP                                
035700                                                                          
035710     MOVE W-IDARTNR-SATS      TO W-IDARTNR-D9                             
035720     MOVE WC-CDC-SE           TO W-IDDC-D9                                
035800     PERFORM IMS-GU-INLB23                                                
035900                                                                          
036000     MOVE IN-105-TIBEODAT-SATS TO INLB-TIBEODAT-SATS                      
036100     MOVE IN-105-IDORDNSB      TO INLB-IDORDNSB                           
036200     PERFORM IMS-ISRT-INLB32                                              
036300     ADD +1                    TO W-ANT-UPPDAT                            
036400     .                                                                    
036500     EJECT                                                                
036600 H-SKRIV-OBEHANDLADE SECTION.                                             
036700***                                                                       
036800* SKRIVER DE POSTER SOM INTE HAR BEHANDLATS                               
036900* PÅ EN NY GENERATION AV FILEN W21510                                     
037000***                                                                       
037100                                                                          
037200     PERFORM UNTIL (END-OF-W21510)                                        
037300       MOVE IN-AREA TO UT-AREA                                            
037400                                                                          
037500       PERFORM S11-SKRIV-W21510                                           
037600       PERFORM S01-LAES-W21510                                            
037700     END-PERFORM                                                          
037800     .                                                                    
037900     EJECT                                                                
038000 Z-FINIT SECTION.                                                         
038100                                                                          
038200                                                                          
038300     CLOSE W21510-IN                                                      
038400           W21510-UT                                                      
038500     SKIP2                                                                
038600     MOVE 'S' TO POSTSUM-OPKOD                                            
038700     CALL POSTSUM USING POSTSUM-PARM                                      
038800     .                                                                    
038900     EJECT                                                                
039000 S01-LAES-W21510  SECTION.                                                
039100     SKIP2                                                                
039200     READ W21510-IN INTO IN-AREA                                          
039300     AT END                                                               
039400        SET END-OF-W21510 TO TRUE                                         
039500                                                                          
039600     NOT AT END                                                           
039700        MOVE 'W21510' TO POSTSUM-FDNAMN                                   
039800        MOVE 'W21503D1' TO POSTSUM-DDNAMN2                                
039900        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
040000        CALL POSTSUM USING POSTSUM-PARM                                   
040100     END-READ                                                             
040200     .                                                                    
040300     EJECT                                                                
040400 S11-SKRIV-W21510 SECTION.                                                
040500     SKIP2                                                                
040600     WRITE W21510-POST FROM UT-AREA                                       
040700                                                                          
040800     MOVE UT-IDPTYP TO POSTSUM-TRANSTYP                                   
040900     MOVE 'W21510 ' TO POSTSUM-FDNAMN                                     
041000     MOVE 'W21503D2' TO POSTSUM-DDNAMN2                                   
041100     CALL POSTSUM USING POSTSUM-PARM                                      
041200     .                                                                    
041300     EJECT                                                                
041400* --- IMS SEKTIONER ---                                                   
041500     SKIP3                                                                
041600                                                                          
041700 IMS-GHU-SATB01 SECTION.                                                  
041800                                                                          
041900     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
042000            DELIMITED BY SIZE INTO SSA1                                   
042100     MOVE '  ' TO GODK-STATUSKODER                                        
042200     CALL CBLTDLI USING GHU SATB-PCB DLI-IO-AREA SSA1                     
042300     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
042400     PERFORM IMS-STATUSKONTROLL                                           
042500     .                                                                    
042600 IMS-REPL-SATB01 SECTION.                                                 
042700                                                                          
042800     MOVE '  '   TO GODK-STATUSKODER                                      
042900     CALL CBLTDLI USING REPL SATB-PCB DLI-IO-AREA                         
043000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
043100     PERFORM IMS-STATUSKONTROLL                                           
043200     .                                                                    
043300     EJECT                                                                
043400 IMS-GHU-SATB11 SECTION.                                                  
043500                                                                          
043600     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
043700            DELIMITED BY SIZE INTO SSA1                                   
043800     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-X ')'                        
043900            DELIMITED BY SIZE INTO SSA2                                   
044000     MOVE '  ' TO GODK-STATUSKODER                                        
044100     CALL CBLTDLI USING GHU SATB-PCB DLI-IO-AREA SSA1 SSA2                
044200     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
044300     PERFORM IMS-STATUSKONTROLL                                           
044400     .                                                                    
044500 IMS-REPL-SATB11 SECTION.                                                 
044600                                                                          
044700     MOVE '  '   TO GODK-STATUSKODER                                      
044800     CALL CBLTDLI USING REPL SATB-PCB DLI-IO-AREA                         
044900     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
045000     PERFORM IMS-STATUSKONTROLL                                           
045100     .                                                                    
045200     EJECT                                                                
045300 IMS-GHU-ARTC01 SECTION.                                                  
045400                                                                          
045500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-SATS-X ')'                    
045600            DELIMITED BY SIZE INTO SSA1                                   
045700     MOVE '  ' TO GODK-STATUSKODER                                        
045800     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1                     
045900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
046000     PERFORM IMS-STATUSKONTROLL                                           
046100     SKIP3                                                                
046200     .                                                                    
046300 IMS-REPL-ARTC01 SECTION.                                                 
046400                                                                          
046500     MOVE '  '   TO GODK-STATUSKODER                                      
046600     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA                         
046700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
046800     PERFORM IMS-STATUSKONTROLL                                           
046900     .                                                                    
047000     EJECT                                                                
047100 IMS-GU-INLB23 SECTION.                                                   
047200                                                                          
047300     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
047400            DELIMITED BY SIZE INTO SSA1                                   
047500     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
047600            DELIMITED BY SIZE INTO SSA2                                   
047700     STRING 'WLINLB23(DAAVROP  =' W-DAAVROP-AVS-X                         
047800           '&KDAVROP  =' W-KDAVROP-X ')'                                  
047900            DELIMITED BY SIZE INTO SSA3                                   
048000     MOVE '  ' TO GODK-STATUSKODER                                        
048100     CALL CBLTDLI USING GU   INLB-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
048200     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
048300     PERFORM IMS-STATUSKONTROLL                                           
048400     .                                                                    
048500 IMS-ISRT-INLB32 SECTION.                                                 
048600                                                                          
048700     MOVE 'WLINLB32 ' TO SSA1                                             
048800     MOVE '  ' TO GODK-STATUSKODER                                        
048900     CALL CBLTDLI USING ISRT INLB-PCB DLI-IO-AREA SSA1                    
049000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
049100     PERFORM IMS-STATUSKONTROLL                                           
049200     SKIP3                                                                
049300     .                                                                    
049400     EJECT                                                                
049500 IMS-ISRT-XXBY11 SECTION.                                                 
049600                                                                          
049700     STRING 'WLXXBY01(WDG3KEY  =' W-WDGXKEY-2233-X ')'                    
049800     DELIMITED BY SIZE INTO SSA1                                          
049900     MOVE 'WLXXBY11 ' TO SSA2                                             
050000     MOVE '  ' TO GODK-STATUSKODER                                        
050100     CALL CBLTDLI USING ISRT XXBY-PCB DLI-IO-AREA SSA1 SSA2               
050200     MOVE XXBY-STATUS-CODE TO STATUS-WS                                   
050300     PERFORM IMS-STATUSKONTROLL                                           
050400     .                                                                    
050500     EJECT                                                                
050600 IMS-ISRT-XXBM11 SECTION.                                                 
050700                                                                          
050800     STRING 'WLXXBM01(WDG3KEY  =' W-WDGXKEY-2201-X ')'                    
050900            DELIMITED BY SIZE INTO SSA1                                   
051000     MOVE 'WLXXBM11 ' TO SSA2                                             
051100     MOVE '  ' TO GODK-STATUSKODER                                        
051200     CALL CBLTDLI USING ISRT XXBM-PCB DLI-IO-AREA SSA1 SSA2               
051300     MOVE XXBM-STATUS-CODE TO STATUS-WS                                   
051400     PERFORM IMS-STATUSKONTROLL                                           
051500     .                                                                    
051600     EJECT                                                                
051700 IMS-STATUSKONTROLL SECTION.                                              
051800     SKIP2                                                                
051900     SET STATUS-IX TO 1                                                   
052000     SEARCH GODK-STATUS                                                   
052100       AT END                                                             
052200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
052300           DELIMITED BY SIZE INTO FELTEXT                                 
052400         DISPLAY FELTEXT                                                  
052500         CALL FELLOG                                                      
052600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
052700         CONTINUE                                                         
052800     END-SEARCH                                                           
052900     .                                                                    
