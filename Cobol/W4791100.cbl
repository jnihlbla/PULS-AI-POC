000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4791100.                                                
000400*AUTHOR.         LARS CALAIS.                                             
000500*DATE-WRITTEN.   91/06/12.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        ROGRAMMET ÄR EN SB SOM                                           
001100*        LÄSER WDQ2. OM FLKLAR = "J" OCH Q212 FINNS KONTROLLERAS          
001200*        OM PLOCKLISTOR ÄR UTSKRIVNA. OM INGA PLOCKLISTOR SKRIVITS        
001300*        OCH INGA RADER FINNS PÅ NÅGOT CL, SKRIVS KONTROLLPOST            
001400*        (W47911) FÖR WDB5, ANNARS LÄSES Q213 OCH OM DENNA FINNS          
001500*        SKRIVS KONTROLLPOST (W47911) FÖR WDB5.                           
001600*        ÄVEN OM Q212 SAKNAS (RENSAD VIA E4), SKRIVS KONTROLLPOST.        
001700*                                                                         
001800*        PROGRAMMET LÄSER     WDQ2                                        
001900*                                                                         
002000*    ABENDKODER:                                                          
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- ORDER SOM KAN RENSAS OM OBEKR-LIGGTID OK                   
003100     SELECT W47911                     ASSIGN TO W47911D1.                
003200     SKIP2                                                                
003300*          --- ORDER MED STATUS "E".                                      
003400     SELECT W47916                     ASSIGN TO W47911D2.                
003500*          --- OUTPUT FILE WITH DEPT NUMBER                               
003600     SELECT W4791A                     ASSIGN TO W47911D3.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004100     SKIP3                                                                
004200 FD  W47911                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500     SKIP2                                                                
004600*01  POST -COPY W479011 -PRE OBEKR-   -L.                                 
004700     SKIP3                                                                
004800 FD  W47916                                                               
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100     SKIP2                                                                
005200*01  POST -COPY W479016 -PRE OREG-   -L.                                  
005300     SKIP2                                                                
005400 FD  W4791A                                                               
005500     RECORDING       F                                                    
005600     BLOCK CONTAINS  0.                                                   
005700     SKIP2                                                                
005800*01  POST -COPY W47901A -PRE UT-   -L.                                    
005900     EJECT                                                                
006000 WORKING-STORAGE SECTION.                                                 
006100     SKIP2                                                                
006200                                                                          
006300*    -- CHECKED BY WY2000                                                 
006400 77  IDPGM                       PIC X(8)    VALUE 'W4791100'.            
006500 77  JA                          PIC X       VALUE 'J'.                   
006600 77  NEJ                         PIC X       VALUE 'N'.                   
006700 77  FOERSTA                     PIC X       VALUE 'F'.                   
006900     SKIP2                                                                
007000 01  W-FLBORT                    PIC X.                                   
007100 01  W-FLKLAR                    PIC X.                                   
007200 01  W-KVPLKLST-TOT              PIC S9(3)   COMP-3.                      
007300 01  W-KVRADER                   PIC S9(7)   COMP-3.                      
007400 01  W-IDORDER                   PIC S9(7)   COMP-3.                      
007500 01  W-IDDISTR                   PIC S9(5)   COMP-3.                      
007600 01  W-IDKUNDNR                  PIC S9(7)   COMP-3.                      
007700 01  W-TIREGDAT                  PIC S9(7)   COMP-3.                      
008000 01  SW-A5-FINNS                 PIC X       VALUE 'N'.                   
008100     88 A5-SAKNAS                            VALUE 'N'.                   
008200     88 A5-FINNS                             VALUE 'J'.                   
008300 01  SW-Q211-FINNS               PIC X       VALUE 'N'.                   
008400     88 Q211-SAKNAS                          VALUE 'N'.                   
008500     88 Q211-FINNS                           VALUE 'J'.                   
008600     EJECT                                                                
008700                                                                          
008800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008900 01  FILLER REDEFINES DAGENS-DATUM.                                       
009000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009300     EJECT                                                                
009400 01  DYNAMISKA-SUBPROGRAM.                                                
009500*                                                                         
009600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010100     SKIP2                                                                
010200*    --- PARAMETRAR TILL ABEND                                            
010300                                                                          
010400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010600     SKIP2                                                                
010700 01  FELTEXT.                                                             
010800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011000     EJECT                                                                
011100*    --- PARAMETRAR TILL POSTSUM                                          
011200*                                                                         
011300*01  -COPY W0005   -PRE  POSTSUM-                                         
011400     EJECT                                                                
011500 01  OBEKR-AREA-START            PIC X(24)   VALUE                        
011600                                 'OBEKR-AREA-START  '.                    
011700     SKIP2                                                                
011800                                                                          
011900*01  AREA -COPY W479011     -PRE OBEKR-                                   
012000     EJECT                                                                
012100 01  OREG-AREA-START             PIC X(24)   VALUE                        
012200                                 'OREG-AREA-START  '.                     
012300     SKIP2                                                                
012400                                                                          
012500*01  AREA -COPY W479016     -PRE OREG-                                    
012600     EJECT                                                                
012700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012800*                                                                         
012900                                                                          
013000*01  AREA -COPY W47901A     -PRE UT-                                      
013100     EJECT                                                                
013200 01  UT-AREA-START               PIC X(24)   VALUE                        
013300                                 'UT-AREA-START    '.                     
013400     SKIP2                                                                
013500     EJECT                                                                
013600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013700     SKIP2                                                                
013800 01  NYCKLAR-TILL-DLI.                                                    
013900     03 W-WDA501KY-FOM.                                                   
014000        05 A5-IDDISTR-FOM        PIC S9(5)   COMP-3 VALUE ZERO.           
014100        05 A5-IDKUNDNR-FOM       PIC S9(7)   COMP-3 VALUE ZERO.           
014200        05 A5-IDKUNDRF-FOM.                                               
014300           07 A5-IDKUNDRF5-FOM   PIC X(5)           VALUE SPACE.          
014400           07 FILLER             PIC X(5)           VALUE SPACE.          
014500        05 FILLER                PIC X(7)    VALUE LOW-VALUE.             
014600                                                                          
014700     03 W-WDA501KY-TOM.                                                   
014800        05 A5-IDDISTR-TOM        PIC S9(5)   COMP-3 VALUE ZERO.           
014900        05 A5-IDKUNDNR-TOM       PIC S9(7)   COMP-3 VALUE ZERO.           
015000        05 A5-IDKUNDRF-TOM.                                               
015100           07 A5-IDKUNDRF5-TOM   PIC X(5)           VALUE SPACE.          
015200           07 FILLER             PIC X(5)           VALUE SPACE.          
015300        05 FILLER                PIC X(7)    VALUE HIGH-VALUE.            
015400                                                                          
015500*    --- STATUS-KOD FRÅN IMS                                              
015600 01  STATUS-WS                   PIC XX.                                  
015700     88  SEGMENT-FINNS                       VALUE '  '.                  
015800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015900     88  BASEN-SLUT                          VALUE 'GB'.                  
016000     SKIP2                                                                
016100 01  GODK-STATUSKODER.                                                    
016200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016300     SKIP2                                                                
016400 01  SSA1                        PIC X(128).                              
016500     EJECT                                                                
016600*    --- IMS FUNKTIONSKODER                                               
016700*01  -COPY W0003                                                          
016800     EJECT                                                                
016900*    ---  DLI INPUT-OUTPUT AREA                                           
017000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-A501'.         
017100     SKIP3                                                                
017200 01  DLI-IO-A501.                                                         
017300*    03  -COPY WDA501                                                     
017400     EJECT                                                                
017500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
017600     SKIP3                                                                
017700 01  DLI-IO-AREA.                                                         
017800     03  IO-AREA                 PIC X(4000) VALUE SPACE.                 
017900     SKIP3                                                                
018000     03  WLORQI01 REDEFINES IO-AREA.                                      
018100*        05  -COPY WDQ201                                                 
018200     SKIP3                                                                
018300     03  WLORQI11 REDEFINES IO-AREA.                                      
018400*        05  -COPY WDQ211                                                 
018500     SKIP3                                                                
018600     03  WLORQI12 REDEFINES IO-AREA.                                      
018700*        05  -COPY WDQ212                                                 
018800     SKIP3                                                                
018810     03  WLORQI21 REDEFINES IO-AREA.                                      
018820*        05  -COPY WDQ221                                                 
018830     SKIP3                                                                
018900 LINKAGE SECTION.                                                         
019000                                                                          
019100     EJECT                                                                
019200*01  -COPY W0008                                                          
019300     05  FILLER                  PIC X.                                   
019400     EJECT                                                                
019500*01  -COPY W0008      -PRE A5-                                            
019600     05  FILLER                  PIC X.                                   
019700     EJECT                                                                
019800 PROCEDURE DIVISION  USING PCB A5-PCB.                                    
019900     ENTRY 'DLITCBL' USING PCB A5-PCB.                                    
020000                                                                          
020100     SKIP2                                                                
020200     PERFORM A-INIT                                                       
020300     PERFORM IMS-GET-WDQ2                                                 
020400     PERFORM UNTIL BASEN-SLUT                                             
020500                                                                          
020600       EVALUATE SEG-NAME-FB                                               
020700         WHEN 'WDQ201'                                                    
020800           PERFORM B-SKRIV                                                
020900           PERFORM WDQ201-ORDERHUVUD                                      
021000         WHEN 'WDQ211'                                                    
021100           PERFORM WDQ211-DIRLEV                                          
021200         WHEN 'WDQ212'                                                    
021300           PERFORM WDQ212-DC-ARB                                          
021310         WHEN 'WDQ221'                                                    
021320           PERFORM WDQ221-LOR                                             
021400       END-EVALUATE                                                       
021500       PERFORM IMS-GET-WDQ2                                               
021600     END-PERFORM                                                          
021700     PERFORM B-SKRIV                                                      
021800                                                                          
021900     PERFORM Z-FINIT                                                      
022000                                                                          
022100     MOVE ZERO TO RETURN-CODE                                             
022200     GOBACK                                                               
022300     .                                                                    
022400     EJECT                                                                
022500 A-INIT SECTION.                                                          
022600                                                                          
022700     OPEN OUTPUT W47911                                                   
022800                 W47916                                                   
022900                 W4791A                                                   
023000     SKIP2                                                                
023100*    ACCEPT DAGENS-DATUM  FROM DATE                                       
023200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
023300*                                                                         
023400     MOVE FOERSTA            TO W-FLKLAR                                  
023500     .                                                                    
023600     EJECT                                                                
023700 B-SKRIV SECTION.                                                         
023800                                                                          
023900     IF W-FLKLAR              = JA                                        
024000       IF A5-SAKNAS                                                       
024100          IF (W-KVPLKLST-TOT = +0 AND W-KVRADER = +0)                     
024200                                 AND Q211-SAKNAS                          
024300            MOVE W-IDORDER      TO OBEKR-IDORDER                          
024400            MOVE W-IDDISTR      TO OBEKR-IDDISTR                          
024500            MOVE W-IDKUNDNR     TO OBEKR-IDKUNDNR                         
024600            MOVE W-TIREGDAT     TO OBEKR-TIREGDAT                         
024900            PERFORM S11-SKRIV-W47911                                      
025100          END-IF                                                          
025200       END-IF                                                             
025300     ELSE                                                                 
025400       IF W-FLKLAR              = NEJ                                     
025500          MOVE W-IDORDER      TO OREG-IDORDER                             
025600          MOVE W-IDDISTR      TO OREG-IDDISTR                             
025700          MOVE W-IDKUNDNR     TO OREG-IDKUNDNR                            
025800          MOVE W-TIREGDAT     TO OREG-TIREGDAT                            
025900          PERFORM S12-SKRIV-W47916                                        
026000       END-IF                                                             
026100     END-IF                                                               
026200     MOVE +0                 TO W-KVPLKLST-TOT                            
026300                                W-KVRADER                                 
026400     MOVE NEJ                TO SW-A5-FINNS                               
026500                                SW-Q211-FINNS                             
026600     .                                                                    
026700     EJECT                                                                
026800 WDQ201-ORDERHUVUD SECTION.                                               
026900                                                                          
027000     MOVE OHUV-FLBORT        TO W-FLBORT                                  
027100     MOVE OHUV-FLKLAR        TO W-FLKLAR                                  
027200     MOVE OHUV-IDORDER       TO W-IDORDER                                 
027300     MOVE OHUV-IDDISTR       TO W-IDDISTR UT-IDDISTR                      
027400                                A5-IDDISTR-FOM                            
027500                                A5-IDDISTR-TOM                            
027600     MOVE OHUV-IDKUNDNR      TO W-IDKUNDNR   UT-IDKUNDNR                  
027700                                A5-IDKUNDNR-TOM                           
027800                                A5-IDKUNDNR-FOM                           
027900     MOVE OHUV-TIREGDAT      TO W-TIREGDAT                                
028000     MOVE OHUV-IDKUNDRF (3:5)   TO A5-IDKUNDRF5-FOM                       
028100                                   A5-IDKUNDRF5-TOM                       
028200     MOVE OHUV-IDDEPT        TO UT-IDDEPT                                 
028300     MOVE OHUV-IDORDNR7      TO UT-IDORDNR7                               
028301     IF OHUV-IDDEPT IS NUMERIC AND OHUV-IDDEPT > 0                        
028310       PERFORM S13-SKRIV-W4791A                                           
028320     END-IF                                                               
028330                                                                          
028400     PERFORM IMS-GET-WDA501                                               
028500     IF SEGMENT-FINNS                                                     
028600       MOVE JA                  TO SW-A5-FINNS                            
028700     END-IF                                                               
028800     .                                                                    
028900     EJECT                                                                
029000 WDQ211-DIRLEV     SECTION.                                               
029100                                                                          
029200     ADD DIRL-KVRADER        TO W-KVRADER                                 
029300     MOVE JA                 TO SW-Q211-FINNS                             
029400     .                                                                    
029500     EJECT                                                                
029600 WDQ212-DC-ARB     SECTION.                                               
029700                                                                          
029800     ADD ARB-IDPLKLST-SISTA  TO W-KVPLKLST-TOT                            
030400     .                                                                    
030500     EJECT                                                                
030510 WDQ221-LOR        SECTION.                                               
030520                                                                          
030530     ADD LOR-KVRADER         TO W-KVRADER                                 
030540     .                                                                    
030550     EJECT                                                                
030600 Z-FINIT SECTION.                                                         
030700     CLOSE W47911                                                         
030800           W47916                                                         
030900           W4791A                                                         
031000     SKIP2                                                                
031100     MOVE 'S' TO POSTSUM-OPKOD                                            
031200     CALL POSTSUM USING POSTSUM-PARM                                      
031300     .                                                                    
031400     EJECT                                                                
031500 S11-SKRIV-W47911 SECTION.                                                
031600     SKIP2                                                                
031700     WRITE OBEKR-POST FROM OBEKR-AREA                                     
031800                                                                          
031900     MOVE '101'        TO POSTSUM-TRANSTYP                                
032000     MOVE 'W47911'     TO POSTSUM-FDNAMN                                  
032100     MOVE 'W47911D1'   TO POSTSUM-DDNAMN2                                 
032200     CALL POSTSUM USING POSTSUM-PARM                                      
032300     .                                                                    
032400     EJECT                                                                
032500 S12-SKRIV-W47916 SECTION.                                                
032600     SKIP2                                                                
032700     WRITE OREG-POST FROM OREG-AREA                                       
032800                                                                          
032900     MOVE '201'       TO POSTSUM-TRANSTYP                                 
033000     MOVE 'W47916'    TO POSTSUM-FDNAMN                                   
033100     MOVE 'W47911D2'  TO POSTSUM-DDNAMN2                                  
033200     CALL POSTSUM USING POSTSUM-PARM                                      
033300     .                                                                    
033400     EJECT                                                                
033500 S13-SKRIV-W4791A SECTION.                                                
033600     SKIP2                                                                
033700     WRITE UT-POST FROM UT-AREA                                           
033800                                                                          
033900     MOVE '   '        TO POSTSUM-TRANSTYP                                
034000     MOVE 'W4791A'     TO POSTSUM-FDNAMN                                  
034100     MOVE 'W47911D3'   TO POSTSUM-DDNAMN2                                 
034200     CALL POSTSUM USING POSTSUM-PARM                                      
034300     .                                                                    
034400     EJECT                                                                
034500* --- IMS SEKTIONER ---                                                   
034600     SKIP3                                                                
034700 IMS-GET-WDA501 SECTION.                                                  
034800     SKIP2                                                                
034900     STRING 'WDA501  (WDA501KY>=' W-WDA501KY-FOM                          
035000                    '&WDA501KY<=' W-WDA501KY-TOM ')'                      
035100          DELIMITED BY SIZE   INTO SSA1                                   
035200     MOVE '  GE'                TO GODK-STATUSKODER                       
035300     CALL CBLTDLI USING GU A5-PCB DLI-IO-A501 SSA1                        
035400     MOVE A5-STATUS-CODE        TO STATUS-WS                              
035500     PERFORM IMS-STATUSKONTROLL                                           
035600     .                                                                    
035700     EJECT                                                                
035800 IMS-GET-WDQ2   SECTION.                                                  
035900     SKIP2                                                                
036000     CALL CBLTDLI USING GN PCB DLI-IO-AREA                                
036100     MOVE '  GAGKGB'  TO GODK-STATUSKODER                                 
036200     MOVE STATUS-CODE TO STATUS-WS                                        
036300     PERFORM IMS-STATUSKONTROLL                                           
036400     .                                                                    
036500     EJECT                                                                
036600 IMS-STATUSKONTROLL SECTION.                                              
036700     SKIP2                                                                
036800     SET STATUS-IX TO 1                                                   
036900     SEARCH GODK-STATUS                                                   
037000       AT END                                                             
037100         MOVE 'FELAKTIG STATUSKOD FRÅN IMS' TO FELTEXT-STR                
037200         DISPLAY FELTEXT                                                  
037300         CALL FELLOG                                                      
037400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
037500     END-SEARCH                                                           
037600     .                                                                    
