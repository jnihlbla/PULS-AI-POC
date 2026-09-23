000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W1111900.                                                
000400 AUTHOR.         FRONTEC, GÖTEBORG.                                       
000500 DATE-WRITTEN.   96/08/14.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        UPPDATERAR WDD7 OCH WDK6 UTIFRÅN UPPDATERINGSPOSTER              
001100*        SOM SKAPAS I W11118 (W11119) OCH W11120 (W11125)                 
001200*        PROGRAMMET BRYTS EFTER 500 UPPDATERINGAR, RESTERANDE             
001300*        POSTER SKRIVS PÅ EN NY GENERATION AV INFILEN                     
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR WLERSA (WDD7)                              
001600*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*        U0016 -  OTILLÅTEN POSTTYP PÅ INFILEN                            
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- UPPDATERINGSPOSTER                                         
003000     SELECT W111XX                     ASSIGN TO W11119D1.                
003100     SELECT W111XX-UT                  ASSIGN TO W11119D2.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W111XX                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  -COPY W111250      -L.                                               
004200 FD  W111XX-UT                                                            
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600 01  W111XX-POST     PIC X(20).                                           
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900     SKIP2                                                                
005000*    -- CHECKED BY WY2000                                                 
005100     SKIP3                                                                
005200 77  IDPGM                       PIC X(8)    VALUE 'W1111900'.            
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005500     SKIP2                                                                
005600 01  FELTEXT.                                                             
005700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005900                                                                          
006000 77  W111XX-EOF-SW               PIC X       VALUE 'N'.                   
006100     88  END-OF-W111XX                       VALUE 'J'.                   
006200                                                                          
006300 01  W-ANT-UPPDAT                PIC 9(4)    VALUE ZERO.                  
006400 01  W-MAX-UPPDAT                PIC 9(3)    VALUE 500.                   
006500     EJECT                                                                
006600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006700 01  FILLER REDEFINES DAGENS-DATUM.                                       
006800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007100     EJECT                                                                
007200 01  DYNAMISKA-SUBPROGRAM.                                                
007300*                                                                         
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007800     EJECT                                                                
007900*    --- PARAMETRAR TILL POSTSUM                                          
008000*                                                                         
008100*01  -COPY W0005   -PRE  POSTSUM-                                         
008200     SKIP3                                                                
008300*    --- PARAMETRAR TILL ABEND                                            
008400*                                                                         
008500 01  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008600     EJECT                                                                
008700 01  W111XX-AREA-START           PIC X(24)   VALUE                        
008800                                             'W111XX-AREA-START'.         
008900     SKIP2                                                                
009000                                                                          
009100 01  W111XX-AREA.                                                         
009200     03  W111XX-IDPTYP           PIC X(3).                                
009300     03  FILLER                  PIC X(17).                               
009400     SKIP2                                                                
009500*01  AREA -COPY W111250     -PRE W29-250-  -RED  W111XX-AREA              
009600     EJECT                                                                
009700*01  AREA -COPY W111251     -PRE W29-251-  -RED  W111XX-AREA              
009800     EJECT                                                                
009900*01  AREA -COPY W111252     -PRE W29-252-  -RED  W111XX-AREA              
010000     EJECT                                                                
010010*01  AREA -COPY W111253     -PRE NYP-      -RED  W111XX-AREA              
010020     EJECT                                                                
010100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010200     SKIP3                                                                
010300 01  NYCKLAR-TILL-DLI.                                                    
010400     03  W-IDARTNR-X.                                                     
010500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010600     03  W-KDSEGKEY-X.                                                    
010700         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
010800     SKIP2                                                                
010900*    --- STATUS-KOD FRÅN IMS                                              
011000 01  STATUS-WS                   PIC XX.                                  
011100     88  SEGMENT-FINNS                       VALUE '  '.                  
011200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011500     88  IMS-EJ-OK                           VALUE 'XD'.                  
011600     SKIP2                                                                
011700 01  GODK-STATUSKODER.                                                    
011800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011900     SKIP3                                                                
012000 01  SSA1                        PIC X(64).                               
012100 01  SSA2                        PIC X(64).                               
012200     EJECT                                                                
012300*    --- IMS FUNKTIONSKODER                                               
012400*01  -COPY W0003                                                          
012500     EJECT                                                                
012600*    ---  DLI INPUT-OUTPUT AREA                                           
012700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012800     SKIP3                                                                
012900 01  DLI-IO-AREA.                                                         
013000     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
013100     SKIP3                                                                
013200     03  WLERSA01 REDEFINES IO-AREA.                                      
013300*        05  -COPY WDD701  -PRE ERSA-                                     
013400     SKIP3                                                                
013410     03  WLERSA13 REDEFINES IO-AREA.                                      
013420*        05  -COPY WDD704  -PRE ERSA-                                     
013430     SKIP3                                                                
013500     03  WLARTC01 REDEFINES IO-AREA.                                      
013600*        05  -COPY WDK601                                                 
013700     SKIP3                                                                
013800     03  WLARTC11 REDEFINES IO-AREA.                                      
013900*        05  -COPY WDK611                                                 
013901 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD201'.                      
013910 01  DLI-IO-AREA-WDD2.                                                    
013920*    03  -COPY WDD201 -PRE NYPON-                                         
014000     EJECT                                                                
014100 LINKAGE SECTION.                                                         
014200                                                                          
014300*01  -COPY W0009   -PRE MSG-                                              
014400     EJECT                                                                
014500*01  -COPY W0008  -PRE ERSA-                                              
014600     05  FILLER                  PIC X.                                   
014700     EJECT                                                                
014800*01  -COPY W0008  -PRE ARTC-                                              
014900     05  FILLER                  PIC X.                                   
014910*01  -COPY W0008  -PRE WDD2-                                              
014920     05  FILLER                  PIC X.                                   
015000     EJECT                                                                
015100 PROCEDURE DIVISION  USING MSG-PCB ERSA-PCB ARTC-PCB WDD2-PCB.            
015200 MAIN SECTION.                                                            
015300     ENTRY 'DLITCBL' USING MSG-PCB ERSA-PCB ARTC-PCB WDD2-PCB.            
015400                                                                          
015500     SKIP2                                                                
015700     PERFORM A-INIT                                                       
015800     PERFORM S01-LAES-W111XX                                              
015900                                                                          
016000     PERFORM UNTIL (   END-OF-W111XX                                      
016100                    OR W-ANT-UPPDAT > W-MAX-UPPDAT)                       
016200       EVALUATE W111XX-IDPTYP                                             
016300       WHEN '100'                                                         
016400         PERFORM B-UPPDATERA-ARTC01                                       
016500       WHEN '101'                                                         
016600       WHEN '102'                                                         
016700           PERFORM D-UPPDATERA-ARTC11                                     
016800       WHEN '103'                                                         
016900           PERFORM C-UPPDATERA-ERSA13                                     
016910       WHEN '104'                                                         
016920           PERFORM F-UPPDATERA-WDD2                                       
016930       WHEN '105'                                                         
016940           PERFORM G-DELETE-ERSA01                                        
017000       WHEN OTHER                                                         
017100           DISPLAY 'W11119 - OTILLÅTEN POSTTYP'                           
017200           CALL ABEND USING RKOD-ABEND-UTAN-DUMP                          
017300       END-EVALUATE                                                       
017400                                                                          
017500       ADD +1 TO W-ANT-UPPDAT                                             
017600                                                                          
017700       PERFORM S01-LAES-W111XX                                            
017800     END-PERFORM                                                          
017900                                                                          
018000     PERFORM E-SKRIV-OBEHANDLADE                                          
018100                                                                          
018200     PERFORM Z-FINIT                                                      
018300                                                                          
018400     MOVE ZERO TO RETURN-CODE                                             
018500     GOBACK                                                               
018600     .                                                                    
018700     EJECT                                                                
018800 A-INIT SECTION.                                                          
018900     SKIP2                                                                
019000                                                                          
019100     OPEN INPUT  W111XX                                                   
019200     OPEN OUTPUT W111XX-UT                                                
019300                                                                          
019400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019500     .                                                                    
019600     EJECT                                                                
019700 B-UPPDATERA-ARTC01 SECTION.                                              
019800***                                                                       
019900* UPPDATERAR TIERSDAT PÅ WDK601                                           
020000***                                                                       
020100                                                                          
020200     MOVE W29-250-IDARTNR TO W-IDARTNR                                    
020300     PERFORM IMS-GHU-ARTC01                                               
020400                                                                          
020500     IF SEGMENT-FINNS                                                     
020600     MOVE W29-250-TIERSDAT TO ART-TIERSDAT                                
020700     PERFORM IMS-REPL-ARTC                                                
020800     END-IF                                                               
020900     .                                                                    
021000     EJECT                                                                
021100 C-UPPDATERA-ERSA13 SECTION.                                              
021200***                                                                       
021300* UPPDATERAR WDD704                                                       
021400***                                                                       
021500                                                                          
021600     MOVE W29-252-IDARTNR TO W-IDARTNR                                    
021700     PERFORM IMS-GHU-ERSA13                                               
021800                                                                          
021900     IF SEGMENT-FINNS                                                     
022000     MOVE W29-252-KDSTATUS-C1      TO ERSA-KDSTATUS-C1                    
022100     MOVE W29-252-KDSTATUS-C2      TO ERSA-KDSTATUS-C2                    
022200     MOVE W29-252-TIERSDAT-PREL-C1 TO ERSA-TIERSDAT-PREL-C1               
022300     MOVE W29-252-TIERSDAT-PREL-C2 TO ERSA-TIERSDAT-PREL-C2               
022400                                                                          
022500     PERFORM IMS-REPL-ERSA                                                
022600     END-IF                                                               
022700     .                                                                    
022800     EJECT                                                                
022900 D-UPPDATERA-ARTC11 SECTION.                                              
023000***                                                                       
023100* UPPDATERAR WDK611                                                       
023200***                                                                       
023300     MOVE W29-251-IDARTNR TO W-IDARTNR                                    
023400     PERFORM IMS-GHU-ARTC11                                               
023500                                                                          
023600     IF SEGMENT-FINNS                                                     
023700     EVALUATE W111XX-IDPTYP                                               
023800     WHEN '101'                                                           
023900       MOVE W29-251-FLTPO1 TO CLAG-FLTPO1                                 
024000       MOVE W29-251-KDERS  TO CLAG-KDERS                                  
024100       MOVE W29-251-KDKSP  TO CLAG-KDKSP                                  
024110       MOVE W29-251-KDAVT  TO CLAG-KDAVT                                  
024200     WHEN '102'                                                           
024300       MOVE W29-251-FLTPO1 TO CLAG-FLTPO1                                 
024400     END-EVALUATE                                                         
024500                                                                          
024600     PERFORM IMS-REPL-ARTC                                                
024700     END-IF                                                               
024810     .                                                                    
024900     EJECT                                                                
025000 E-SKRIV-OBEHANDLADE SECTION.                                             
025100***                                                                       
025200* SKRIVER DE POSTER SOM INTE HAR BEHANDLATS                               
025300* PÅ EN NY GENERATION AV FILEN W111XX                                     
025400***                                                                       
025500                                                                          
025600     PERFORM UNTIL (END-OF-W111XX)                                        
025700                                                                          
025800       PERFORM S11-SKRIV-W111XX                                           
025900       PERFORM S01-LAES-W111XX                                            
026000     END-PERFORM                                                          
026100     .                                                                    
026200     EJECT                                                                
026300 F-UPPDATERA-WDD2   SECTION.                                              
026301***                                                                       
026302* UPPDATERAR WDD2                                                         
026303***                                                                       
026304     MOVE NYP-IDARTNR TO W-IDARTNR                                        
026305     PERFORM IMS-GHU-WDD201                                               
026306                                                                          
026307     IF SEGMENT-FINNS                                                     
026308       MOVE NYP-KDANSKQ TO NYPON-ART-KDANSKQ                              
026317                                                                          
026318       PERFORM IMS-REPL-WDD2                                              
026319     END-IF                                                               
026320     .                                                                    
026321     EJECT                                                                
026322 G-DELETE-ERSA01     SECTION.                                             
026323***                                                                       
026324* DELETE  WDD701                                                          
026325***                                                                       
026326     MOVE W29-250-IDARTNR TO W-IDARTNR                                    
026327     PERFORM IMS-GHU-ERSA01                                               
026328                                                                          
026329     IF SEGMENT-FINNS                                                     
026331        PERFORM IMS-DLET-ERSA                                             
026332     END-IF                                                               
026341     .                                                                    
026350 Z-FINIT SECTION.                                                         
026400                                                                          
026500                                                                          
026600     CLOSE W111XX                                                         
026700           W111XX-UT                                                      
026800                                                                          
026900     SKIP2                                                                
027000     MOVE 'S' TO POSTSUM-OPKOD                                            
027100     CALL POSTSUM USING POSTSUM-PARM                                      
027200     .                                                                    
027300     EJECT                                                                
027400 S01-LAES-W111XX  SECTION.                                                
027500     SKIP2                                                                
027600     READ W111XX INTO W111XX-AREA                                         
027700     AT END                                                               
027800        SET END-OF-W111XX TO TRUE                                         
027900                                                                          
028000     NOT AT END                                                           
028100        MOVE 'W111XX' TO POSTSUM-FDNAMN                                   
028200        MOVE 'W11119D1' TO POSTSUM-DDNAMN2                                
028300        MOVE W111XX-IDPTYP TO POSTSUM-TRANSTYP                            
028400        CALL POSTSUM USING POSTSUM-PARM                                   
028500     END-READ                                                             
028600     .                                                                    
028700     EJECT                                                                
028800 S11-SKRIV-W111XX SECTION.                                                
028900     SKIP2                                                                
029000     WRITE W111XX-POST FROM W111XX-AREA                                   
029100                                                                          
029200     MOVE W111XX-IDPTYP TO POSTSUM-TRANSTYP                               
029300     MOVE 'W111XX'  TO POSTSUM-FDNAMN                                     
029400     MOVE 'W11119D2' TO POSTSUM-DDNAMN2                                   
029500     CALL POSTSUM USING POSTSUM-PARM                                      
029600     .                                                                    
029700     EJECT                                                                
029800* --- IMS SEKTIONER ---                                                   
029900     SKIP3                                                                
030000     EJECT                                                                
030100 IMS-GHU-ARTC01 SECTION.                                                  
030200                                                                          
030300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
030400          DELIMITED BY SIZE INTO SSA1                                     
030500*    MOVE '  ' TO GODK-STATUSKODER                                        
030510     MOVE '  GE' TO GODK-STATUSKODER                                      
030600     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1                     
030700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
030800     PERFORM IMS-STATUSKONTROLL                                           
030900     .                                                                    
031000     SKIP3                                                                
031100 IMS-GHU-ARTC11 SECTION.                                                  
031200                                                                          
031300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
031400          DELIMITED BY SIZE INTO SSA1                                     
031500     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
031600          DELIMITED BY SIZE INTO SSA2                                     
031700*    MOVE '  ' TO GODK-STATUSKODER                                        
031710     MOVE '  GE' TO GODK-STATUSKODER                                      
031800     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1 SSA2                
031900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
032000     PERFORM IMS-STATUSKONTROLL                                           
032100     .                                                                    
032200     SKIP3                                                                
032300 IMS-REPL-ARTC SECTION.                                                   
032400                                                                          
032500     MOVE '  ' TO GODK-STATUSKODER                                        
032600     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA                         
032700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
032800     PERFORM IMS-STATUSKONTROLL                                           
032900     .                                                                    
033000     EJECT                                                                
033100 IMS-GHU-ERSA13 SECTION.                                                  
033200*                                                                         
033300     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
033400     DELIMITED BY SIZE INTO SSA1                                          
033500     MOVE 'WLERSA13  ' TO SSA2                                            
033600*    MOVE '  ' TO GODK-STATUSKODER                                        
033610     MOVE '  GE' TO GODK-STATUSKODER                                      
033700     CALL CBLTDLI USING GHU ERSA-PCB DLI-IO-AREA SSA1 SSA2                
033800     MOVE ERSA-STATUS-CODE  TO STATUS-WS                                  
033900     PERFORM IMS-STATUSKONTROLL                                           
034000     .                                                                    
034100     SKIP3                                                                
034200 IMS-REPL-ERSA SECTION.                                                   
034300                                                                          
034400     MOVE '  ' TO GODK-STATUSKODER                                        
034500     CALL CBLTDLI USING REPL ERSA-PCB DLI-IO-AREA                         
034600     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
034700     PERFORM IMS-STATUSKONTROLL                                           
034800     .                                                                    
034900     EJECT                                                                
034901 IMS-GHU-ERSA01 SECTION.                                                  
034902*                                                                         
034903     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
034904     DELIMITED BY SIZE INTO SSA1                                          
034907     MOVE '  GE' TO GODK-STATUSKODER                                      
034908     CALL CBLTDLI USING GHU ERSA-PCB DLI-IO-AREA SSA1                     
034909     MOVE ERSA-STATUS-CODE  TO STATUS-WS                                  
034910     PERFORM IMS-STATUSKONTROLL                                           
034911     .                                                                    
034912     SKIP3                                                                
034913 IMS-DLET-ERSA SECTION.                                                   
034914                                                                          
034915     MOVE '  ' TO GODK-STATUSKODER                                        
034916     CALL CBLTDLI USING DLET ERSA-PCB DLI-IO-AREA                         
034917     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
034918     PERFORM IMS-STATUSKONTROLL                                           
034919     .                                                                    
034920     EJECT                                                                
034921 IMS-GHU-WDD201 SECTION.                                                  
034922                                                                          
034930     STRING 'WDD201  (IDARTNR  =' W-IDARTNR-X ')'                         
034940          DELIMITED BY SIZE INTO SSA1                                     
034950     MOVE '  ' TO GODK-STATUSKODER                                        
034970     CALL CBLTDLI USING GHU WDD2-PCB DLI-IO-AREA-WDD2 SSA1                
034980     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
034990     PERFORM IMS-STATUSKONTROLL                                           
034991     .                                                                    
034992     SKIP3                                                                
034993 IMS-REPL-WDD2 SECTION.                                                   
034994                                                                          
034995     MOVE '  ' TO GODK-STATUSKODER                                        
034996     CALL CBLTDLI USING REPL WDD2-PCB DLI-IO-AREA-WDD2                    
034997     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
034998     PERFORM IMS-STATUSKONTROLL                                           
034999     .                                                                    
035000     EJECT                                                                
035010 IMS-STATUSKONTROLL SECTION.                                              
035100     SKIP2                                                                
035200     SET STATUS-IX TO 1                                                   
035300     SEARCH GODK-STATUS                                                   
035400       AT END                                                             
035500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
035600           DELIMITED BY SIZE INTO FELTEXT                                 
035700         DISPLAY FELTEXT                                                  
035800         CALL FELLOG                                                      
035900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
036000         CONTINUE                                                         
036100     END-SEARCH                                                           
036200     .                                                                    
