000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     W0921400.                                                
000500*              PROGRAM CONVERTED BY                                       
000600*              COBOL CONVERSION AID PO 5785-ABJ                           
000700*              CONVERSION DATE 05/25/91 18:03:39.                         
000800 AUTHOR.         TOMAS 4103     INGRID DANIELSSON                         
000900 DATE-WRITTEN.   MAY 1976       AUG 1991                                  
000910 DATE-COMPILED.                                                           
001000*    REMARKS.                                                             
001100*        FUNKTION.                                                        
001200*                PROGRAMMET LÄSER IN SAMTLIGA FELFILER.                   
001300*                FELTRANSENS KORTTYP + FELKOD ANVÄNDS FÖR                 
001400*                ATT LÄSA FELKODSREGISTRET. DETTA REGISTER                
001500*                INNEHÅLLER EN POST TILL VARJE FELKOD.                    
001600*                FELTRANSEN IN KOMPLETTERAS MED SORTERINGS-               
001700*                ARGUMENT OCH FELTEXT SOM TAS FRÅN                        
001800*                FELKODSREGISTRET.                                        
001900*        INDATA.                                                          
002000*                SAMTLIGA FELFILER SOM HAR SKAPATS I DE                   
002100*                          OLIKA SYSTEMEN.                                
002200*                W09208IN  FELKODS-REGISTER-IN.                           
002300*                W092M002  R05-ADR-REGISTER.                              
002400*        UTDATA.                                                          
002500*                W09208UT  FELKODS-REGISTER-UT.                           
002600*                W09211    FELTRANSAKTIONER KOMPLETTERADE                 
002700*                          MED SORT-ARGUMENT OCH FELTEXT.                 
003300     EJECT                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500 INPUT-OUTPUT SECTION.                                                    
003600 FILE-CONTROL.                                                            
003700     SELECT  INFIL   ASSIGN TO UT-S-W09214D1.                             
003900     SELECT W09208IN ASSIGN TO UT-S-W09214D2.                             
004000     SELECT W092M002 ASSIGN TO UT-S-W09214D3.                             
004100     SELECT W09208UT ASSIGN TO UT-S-W09214D4.                             
004110     SELECT W09211   ASSIGN TO UT-S-W09214D5.                             
004200     EJECT                                                                
004300 DATA DIVISION.                                                           
004400 FILE SECTION.                                                            
004500     SKIP3                                                                
004600                                                                          
004700 FD  INFIL                                                                
004800     RECORDING V                                                          
004900     BLOCK CONTAINS 0 RECORDS.                                            
005000 01  INPOST                  PIC X(116).                                  
005100                                                                          
005200     SKIP3                                                                
005900                                                                          
006000 FD  W09208IN                                                             
006100     BLOCK CONTAINS 0 RECORDS.                                            
006200*    -COPY W092FREG  -PRE REGIN-   -L                                     
006300*++INCLUDE W092FREGC0  *GENER. STMT, REMOVE TOGETHER WITH -COPY           
006310                                                                          
006320     SKIP3                                                                
006330                                                                          
006600 FD  W092M002                                                             
006700     RECORDING F                                                          
006800     BLOCK CONTAINS 0 RECORDS.                                            
006900 01  M002POST                PIC X(80).                                   
007000                                                                          
007100     EJECT                                                                
007200                                                                          
007300 FD  W09208UT                                                             
007400     BLOCK CONTAINS 0 RECORDS.                                            
007500*01  AREA    -COPY W092FREG  -PRE REGUT-   -L                             
007600*++INCLUDE W092FREGC0  *GENER. STMT, REMOVE TOGETHER WITH -COPY           
007700                                                                          
007701     SKIP3                                                                
007710                                                                          
007720 FD  W09211                                                               
007730     RECORDING F                                                          
007740     BLOCK CONTAINS 0 RECORDS.                                            
007750 01  11-UTPOST               PIC X(122).                                  
007800                                                                          
007810     SKIP3                                                                
007820                                                                          
007896     EJECT                                                                
007900 WORKING-STORAGE SECTION.                                                 
007910                                                                          
008000*    -- CHECKED BY WY2000                                                 
008400 77  NEJ                     PIC X    VALUE 'N'.                          
008500 77  JA                      PIC X    VALUE 'J'.                          
008600 77  MAX-08-INDX             PIC S9(9)   COMP SYNC.                       
008610 77  MAX-R05-INDX            PIC S9(9)   COMP SYNC.                       
008710     SKIP3                                                                
008800 01  DATUM.                                                               
008810     03  AAR.                                                             
008811         05  FILLER          PIC 9.                                       
008812         05  A               PIC 9.                                       
008813     03       FILLER         PIC 9(4).                                    
008820                                                                          
008830     SKIP3                                                                
008831                                                                          
008840 01  W-IDNYCKEL.                                                          
008850     03  W-IDPTYP            PIC X(3).                                    
008900         88  IDPTYP-R50-R59           VALUE  'R50' THRU 'R59'.            
009000         88  IDPTYP-ORDER             VALUE  'R47' THRU 'R52'             
009100                                             'R54'                        
009200                                             'R55'                        
009300                                             'R57'                        
009400                                             'R73' THRU 'R75'.            
009410     03  W-IDFELKODX         PIC X(3).                                    
009500                                                                          
009510     SKIP3                                                                
009520                                                                          
009600 01  W-IDORDNR               PIC 9(5).                                    
009700     88  IDORDNR-08600-08799          VALUE  08600 THRU 08799.            
009800     EJECT                                                                
009900 01  SWITCHAR.                                                            
010000                                                                          
010100     03  INFIL-EOF-SW        PIC X   VALUE 'N'.                           
010200         88  INFIL-EOF               VALUE 'J'.                           
010300                                                                          
010400     03  08-EOF-SW           PIC X   VALUE 'N'.                           
010500         88  08-EOF                  VALUE 'J'.                           
010600                                                                          
010700     03  R05-EOF-SW          PIC X   VALUE 'N'.                           
010800         88  R05-EOF                 VALUE 'J'.                           
010900                                                                          
011000     03  FINNS-PA-R05-SW     PIC X   VALUE 'J'.                           
011100         88  FANNS-PA-R05-TABELL     VALUE 'J'.                           
011200                                                                          
011300     03  FINNS-PA-08-SW      PIC X   VALUE 'J'.                           
011400         88  FANNS-PA-08-TABELL      VALUE 'J'.                           
011410                                                                          
011420     03  ARTIKEL-SAKNAS-SW   PIC X   VALUE 'N'.                           
011430         88  ARTIKEL-SAKNAS          VALUE 'J'.                           
011500     EJECT                                                                
011600 01  FELMED1.                                                             
011700     03  FILLER      PIC X(22) VALUE 'ANSVARIG FÖR TRANSTYP '.            
011800     03  FELMED1-KTYP        PIC X(3).                                    
011900     03  FILLER      PIC X(35) VALUE ' SKALL UPPDATERA FELKODS-REG        
012000-                'ISTRET.'.                                               
012100     SKIP3                                                                
012200 01  FELMED2.                                                             
012300     03  FILLER      PIC X(35) VALUE 'TRANSAKTIONEN INNEHÖLL IDENT        
012400-                'ITETEN '.                                               
012500     03  FELMED2-GLURPKOD    PIC X(16).                                   
012600     03  FILLER      PIC X(25) VALUE ' DENNA SAKNAS I TABELLEN.'.         
012610     SKIP3                                                                
012620 01  FELMED3.                                                             
012630     03  FILLER      PIC X(35) VALUE 'TRANSAKTIONEN INNEHÖLL ARTIK        
012640-                'ELNR   '.                                               
012650     03  FELMED3-IDARTNR     PIC X(10).                                   
012660     03  FILLER      PIC X(25) VALUE ' DETTA SAKNAS I ART.REG. '.         
012700     EJECT                                                                
012800 01  FILLER                  PIC X(16) VALUE 'TESTDISTRIKT'.              
012900 01  TEST-IDDISTR            PIC 9(5) COMP-3.                             
013000*01  FILLER -COPY WWDIST18  -RED TEST-IDDISTR.                            
013200     EJECT                                                                
013300*01  FILLER -COPY WWDIST19  -RED TEST-IDDISTR.                            
013500     EJECT                                                                
013600*01  FILLER -COPY WWDIST20  -RED TEST-IDDISTR.                            
013800     EJECT                                                                
013900*01  FILLER -COPY WWDIST93  -RED TEST-IDDISTR.                            
014100     EJECT                                                                
014200 01  FILLER                  PIC X(20)   VALUE ALL 'A'.                   
014300                                                                          
014400 01  INAREA.                                                              
014500*    03  FILLER  -COPY W092W001  -PRE IN-.                                
014700                                                                          
014800                                                                          
014900                                                                          
015000                                                                          
015100     03  IN-KORT             PIC X(80).                                   
015200     03  FILLER REDEFINES IN-KORT.                                        
015300         05  IN-GLURPKOD     PIC X(16).                                   
015400         05  IN-FELTEXT      PIC X(64).                                   
015500                                                                          
015600     EJECT                                                                
015700 01  FILLER                  PIC X(20)   VALUE ALL 'B'.                   
015800                                                                          
016500*01  AREA          -COPY W09211   -PRE UT-.                               
016700     SKIP3                                                                
016800                                                                          
016810                                                                          
016900         07  FILLER REDEFINES UT-FELTEXT.                                 
017000             09  UT-GLURP-KOD          PIC X(16).                         
017100             09  UT-INFO               PIC X(64).                         
017101                                                                          
017110         07  FILLER REDEFINES UT-FELTEXT.                                 
017111             09                FILLER  PIC X(5).                          
017120             09  UT-KDPRODSL           PIC 9(2).                          
017121             09                FILLER  PIC X(6).                          
017130             09  UT-IDFKNGRP           PIC 9(4).                          
017140             09                FILLER  PIC X(58).                         
017200     EJECT                                                                
017300 01  FILLER                  PIC X(20)   VALUE ALL 'C'.                   
017400                                                                          
017500 01  08-TABELL.                                                           
017600     03  08-POST   OCCURS 1 TO 1500 DEPENDING ON MAX-08-INDX              
017700                   ASCENDING KEY IS 08-IDNYCKEL                           
017800                   INDEXED BY 08-INDX.                                    
017900                                                                          
018000*        05  FILLER    -COPY W092FREG  -PRE 08-                           
018900                                                                          
019000                                                                          
019100     EJECT                                                                
019200 01  FILLER                  PIC X(20)   VALUE ALL 'D'.                   
019300                                                                          
019400 01  R05-TABELL.                                                          
019500     03  R05-POST  OCCURS 1 TO 300 DEPENDING ON MAX-R05-INDX              
019600                   ASCENDING KEY IS R05-IDELMT                            
019700                   INDEXED BY R05-INDX.                                   
019800                                                                          
019810         05  -COPY W092M002                                               
020500     EJECT                                                                
020600 01  GRUPP-TAB.                                                           
020700     03  FILLER              PIC X(5)    VALUE 'RE001'.                   
020800     03  FILLER              PIC X(5)    VALUE 'RE102'.                   
020900     03  FILLER              PIC X(5)    VALUE 'RP103'.                   
021000     03  FILLER              PIC X(5)    VALUE 'RP204'.                   
021100     03  FILLER              PIC X(5)    VALUE 'RP305'.                   
021200     03  FILLER              PIC X(5)    VALUE 'RP406'.                   
021300     03  FILLER              PIC X(5)    VALUE 'RP507'.                   
021400     03  FILLER              PIC X(5)    VALUE 'RP608'.                   
021500     03  FILLER              PIC X(5)    VALUE 'RP709'.                   
021600     03  FILLER              PIC X(5)    VALUE 'R0110'.                   
021700     03  FILLER              PIC X(5)    VALUE 'R0211'.                   
021800     03  FILLER              PIC X(5)    VALUE 'R0412'.                   
021900     03  FILLER              PIC X(5)    VALUE 'R0503'.                   
022000     03  FILLER              PIC X(5)    VALUE 'R0614'.                   
022100     03  FILLER              PIC X(5)    VALUE 'R0715'.                   
022200     03  FILLER              PIC X(5)    VALUE 'R0816'.                   
022300     03  FILLER              PIC X(5)    VALUE 'R0980'.                   
022400     03  FILLER              PIC X(5)    VALUE 'R1018'.                   
022500     03  FILLER              PIC X(5)    VALUE 'R1119'.                   
022600     03  FILLER              PIC X(5)    VALUE 'R1280'.                   
022700     03  FILLER              PIC X(5)    VALUE 'R1380'.                   
022800     03  FILLER              PIC X(5)    VALUE 'R1480'.                   
022900     03  FILLER              PIC X(5)    VALUE 'R1580'.                   
023000     03  FILLER              PIC X(5)    VALUE 'R1624'.                   
023100     03  FILLER              PIC X(5)    VALUE 'R1725'.                   
023200     03  FILLER              PIC X(5)    VALUE 'R2026'.                   
023300     03  FILLER              PIC X(5)    VALUE 'R2227'.                   
023400     03  FILLER              PIC X(5)    VALUE 'R2328'.                   
023500     03  FILLER              PIC X(5)    VALUE 'R2429'.                   
023600     03  FILLER              PIC X(5)    VALUE 'R2530'.                   
023700     03  FILLER              PIC X(5)    VALUE 'R2631'.                   
023800     03  FILLER              PIC X(5)    VALUE 'R2732'.                   
023900     03  FILLER              PIC X(5)    VALUE 'R3133'.                   
024000     03  FILLER              PIC X(5)    VALUE 'R3234'.                   
024100     03  FILLER              PIC X(5)    VALUE 'R3335'.                   
024200     03  FILLER              PIC X(5)    VALUE 'R3436'.                   
024300     03  FILLER              PIC X(5)    VALUE 'R3537'.                   
024400     03  FILLER              PIC X(5)    VALUE 'R3738'.                   
024500     03  FILLER              PIC X(5)    VALUE 'R4039'.                   
024600     03  FILLER              PIC X(5)    VALUE 'R4140'.                   
024700     03  FILLER              PIC X(5)    VALUE 'R4341'.                   
024800     03  FILLER              PIC X(5)    VALUE 'R4489'.                   
024900     03  FILLER              PIC X(5)    VALUE 'R4589'.                   
025000     03  FILLER              PIC X(5)    VALUE 'R4689'.                   
025100     03  FILLER              PIC X(5)    VALUE 'R4789'.                   
025200     03  FILLER              PIC X(5)    VALUE 'R4889'.                   
025300     03  FILLER              PIC X(5)    VALUE 'R4989'.                   
025400     03  FILLER              PIC X(5)    VALUE 'R5089'.                   
025500     03  FILLER              PIC X(5)    VALUE 'R5189'.                   
025600     03  FILLER              PIC X(5)    VALUE 'R5289'.                   
025700     03  FILLER              PIC X(5)    VALUE 'R5389'.                   
025800     03  FILLER              PIC X(5)    VALUE 'R5489'.                   
025900     03  FILLER              PIC X(5)    VALUE 'R5589'.                   
026000     03  FILLER              PIC X(5)    VALUE 'R5689'.                   
026100     03  FILLER              PIC X(5)    VALUE 'R5789'.                   
026200     03  FILLER              PIC X(5)    VALUE 'R5989'.                   
026300     03  FILLER              PIC X(5)    VALUE 'R6242'.                   
026400     03  FILLER              PIC X(5)    VALUE 'R6489'.                   
026500     03  FILLER              PIC X(5)    VALUE 'R6543'.                   
026600     03  FILLER              PIC X(5)    VALUE 'R6689'.                   
026700     03  FILLER              PIC X(5)    VALUE 'R7344'.                   
026800     03  FILLER              PIC X(5)    VALUE 'R7445'.                   
026900     03  FILLER              PIC X(5)    VALUE 'R7589'.                   
027000     03  FILLER              PIC X(5)    VALUE 'R7646'.                   
027100     03  FILLER              PIC X(5)    VALUE 'R7747'.                   
027200     03  FILLER              PIC X(5)    VALUE 'R7848'.                   
027300     03  FILLER              PIC X(5)    VALUE 'R8049'.                   
027400     03  FILLER              PIC X(5)    VALUE 'R8250'.                   
027500     03  FILLER              PIC X(5)    VALUE 'R8351'.                   
027600     03  FILLER              PIC X(5)    VALUE 'R8552'.                   
027700     03  FILLER              PIC X(5)    VALUE 'R9453'.                   
027800     03  FILLER              PIC X(5)    VALUE '00354'.                   
027900     03  FILLER              PIC X(5)    VALUE '31055'.                   
028000     03  FILLER              PIC X(5)    VALUE '37156'.                   
028100     03  FILLER              PIC X(5)    VALUE '66657'.                   
028200     03  FILLER              PIC X(5)    VALUE '66758'.                   
028300     03  FILLER              PIC X(5)    VALUE '70159'.                   
028400     03  FILLER              PIC X(5)    VALUE '95760'.                   
028500 01  PTYP-TAB REDEFINES GRUPP-TAB.                                        
028600     03  PTYP-GRUPP OCCURS 78                                             
028700         INDEXED BY PTYP-INDX.                                            
028800         05  PTYP            PIC X(3).                                    
028900         05  GRUPP           PIC X(2).                                    
029000     EJECT                                                                
029100 01  SUBPROGRAM.                                                          
029200     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI'.                 
029300     03  FELLOG              PIC X(8)    VALUE 'FELLOG '.                 
029400     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
029500 01  OLD-PTYP.                                                            
029600     03  OLD-PT-POS-1-2      PIC XX.                                      
029700     03  FILLER              PIC X.                                       
029800     SKIP3                                                                
029900 01  NY-PTYP.                                                             
030000     03  NY-PT-POS-1         PIC X.                                       
030100     03  NY-PT-POS-2-3       PIC XX.                                      
030200     SKIP3                                                                
030300 01  KEY-IDNYCKEL.                                                        
030400     03  KEY-IDPTYP          PIC X(3).                                    
030500     03  KEY-IDFELKOD        PIC X(3).                                    
030600     SKIP3                                                                
031100*    -COPY W0005 -PRE POSTSUM-.                                           
031300     SKIP3                                                                
031400     03  PGM-NAMN            PIC X(6)    VALUE 'W09214'.                  
031500     03  ADDERA              PIC X       VALUE 'A'.                       
031600     03  SKRIV               PIC X       VALUE 'S'.                       
031700     SKIP3                                                                
031800     03  INFIL-TRANSID       PIC X(6)    VALUE 'INFIL'.                   
031900     03  INFIL-DDNAMN        PIC X(8)    VALUE 'W09214D1'.                
032000     03  INFIL-TRANSTYP      PIC X(4)    VALUE 'INTR'.                    
032100     EJECT                                                                
032200*****************************************************************         
032300*    IMS-VS                                                     *         
032400*****************************************************************         
032500                                                                          
032600 01  FILLER                  PIC X(16)   VALUE 'IMS-WS'.                  
032700                                                                          
032800 01  STATUS-WS               PIC XX.                                      
032900     88  SEGMENT-FINNS                   VALUE '  '.                      
033000     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
033100                                                                          
033200 01  GODK-STATUSKODER.                                                    
033300     03  GODK-STATUS   OCCURS 5  INDEXED BY STATUS-IX  PIC XX.            
033400                                                                          
033500 01  SSA1                    PIC  X(32).                                  
033600                                                                          
033700                                                                          
033800 01  NYCKLAR-TILL-DLI.                                                    
033900     03  W-IDARTNR-X.                                                     
034000         05  W-IDARTNR       PIC S9(9)  COMP-3.                           
034100                                                                          
034200                                                                          
034300     EJECT                                                                
034400*01  -COPY W0003                                                          
034600     EJECT                                                                
034601 01  FILLER                    PIC X(16) VALUE 'DLI-IO-AREA-01'.          
034602     SKIP3                                                                
034603 01  DLI-IO-AREA-01.                                                      
034606*    03  -COPY WDK601                                                     
034607     SKIP3                                                                
034608                                                                          
034609 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-11'.        
034610     SKIP3                                                                
034611 01  DLI-IO-AREA-11.                                                      
034614*    03  -COPY WDK611                                                     
034620     EJECT                                                                
035400 LINKAGE SECTION.                                                         
035500*01  -COPY W0008 -PRE ARTC-                                               
035700           05  FILLER PIC X.                                              
035800     EJECT                                                                
035900 PROCEDURE DIVISION USING ARTC-PCB.                                       
036000     ENTRY 'DLITCBL' USING ARTC-PCB                                       
036100                                                                          
036200******************************************************************        
036300*    FELTRANSAKTIONERNA KOMPLETTERAS MED SORT-ARGUMENT.          *        
036400*    OM TRANSAKTIONEN ÄR STOPPAD HÄMTAS FELTEXTEN FRÅN           *        
036500*    REGISTRET. EN ELLER FLERA TRANSAKTIONER SKAPAS.             *        
036700*    OM INTRANSEN INNEHÅLLER INTERN POSTTYP ÖVERSÄTTS            *        
036710*    INNEHÅLLER INTERN POSTTYP ÖVERSÄTTS POSTTYPEN TILL          *        
036800*    POSTTYPEN TILL DEN URSPRUNGLIGA KORTTYPEN.                  *        
036900******************************************************************        
037000                                                                          
037100     PERFORM A-INIT                                                       
037200     PERFORM S01-LAES-INFIL                                               
037300     PERFORM UNTIL INFIL-EOF                                              
037400                                                                          
037500       MOVE INAREA TO UT-TRANS                                            
037510       IF IN-KDFRAKT NUMERIC                                              
037520         MOVE IN-KDFRAKT TO UT-KDFRAKT                                    
037530       END-IF                                                             
037540       IF IN-IDORDNR NUMERIC                                              
037550         MOVE IN-IDORDNR TO UT-IDORDNR                                    
037560       END-IF                                                             
037600                                                                          
037610                                                                          
037700       PERFORM B-IDFTG-FRAN-WDK601                                        
037800                                                                          
037810       MOVE IN-IDPTYP  TO  NY-PTYP                                        
037900       IF NY-PT-POS-1 NOT = 'R'                                           
038000         PERFORM C-DOEP-OM-POSTTYP                                        
038100       END-IF                                                             
038110                                                                          
038120       MOVE IN-IDORDNR   TO W-IDORDNR                                     
038130       MOVE IN-IDFELKODX TO W-IDFELKODX                                   
038140       MOVE IN-IDPTYP    TO W-IDPTYP                                      
038141                           UT-IDPTYP                                      
038150                            POSTSUM-TRANSTYP                              
038160       PERFORM   S60-CALL-POSTSUM                                         
038200                                                                          
038300       PERFORM D-SOEK-I-08-TABELLEN                                       
038400                                                                          
038500       IF FANNS-PA-08-TABELL                                              
038510         ACCEPT DATUM FROM DATE                                           
038600         MOVE  A  TO 08-KDSVAR (08-INDX)                                  
038700         IF IN-IDPTYP = 'R05'                                             
038800           PERFORM E-BEHANDLA-R05                                         
038900         ELSE                                                             
039000           IF IDPTYP-ORDER                                                
039100             PERFORM F-BEHANDLA-ORDER                                     
039200           ELSE                                                           
039300             PERFORM G-BEHANDLA-OEVRIGA                                   
039400           END-IF                                                         
039500         END-IF                                                           
039600       ELSE                                                               
039700         PERFORM H-SAKNAS-PA-08-TABELLEN                                  
039800       END-IF                                                             
039900                                                                          
040000       PERFORM I-SOEK-I-GRUPP-TABELLEN                                    
040100                                                                          
040700       PERFORM S01-LAES-INFIL                                             
040800     END-PERFORM                                                          
040900                                                                          
041000     PERFORM Z-FINIT                                                      
041100     MOVE +0 TO RETURN-CODE                                               
041200     GOBACK                                                               
041300     .                                                                    
041400     EJECT                                                                
041500 A-INIT     SECTION.                                                      
041600                                                                          
041700     OPEN INPUT  INFIL                                                    
041800                 W09208IN                                                 
041810                 W092M002                                                 
041900          OUTPUT W09208UT                                                 
042000                 W09211                                                   
042100                                                                          
042200     MOVE  ADDERA    TO POSTSUM-OPKOD                                     
042300     MOVE PGM-NAMN   TO POSTSUM-PROGNAMN                                  
042400     MOVE  'INFIL'   TO POSTSUM-FDNAMN                                    
042500     MOVE 'W09214D1' TO POSTSUM-DDNAMN2                                   
042600                                                                          
042700                                                                          
042800     PERFORM AA-LAGG-UPP-08-I-TABELL                                      
042900     PERFORM AB-LAGG-UPP-R05-I-TABELL                                     
043000                                                                          
043100                                                                          
043200                                                                          
043300     .                                                                    
043400     EJECT                                                                
043500 AA-LAGG-UPP-08-I-TABELL      SECTION.                                    
043600                                                                          
043700******************************************************************        
043800*    W09208 = FELKODS-REGISTRET. LÄGGS I TABELL                  *        
043900******************************************************************        
044000                                                                          
044100     SET 08-INDX TO +1                                                    
044200     PERFORM UNTIL 08-EOF                                                 
044300                                                                          
044400       READ W09208IN INTO 08-POST (08-INDX)                               
044500                     AT END   MOVE 'J' TO 08-EOF-SW                       
044900                 NOT AT END   SET 08-INDX UP BY +1                        
045100       END-READ                                                           
045200     END-PERFORM                                                          
045210     SET MAX-08-INDX TO 08-INDX                                           
045220     SUBTRACT 1 FROM MAX-08-INDX                                          
045300     .                                                                    
045400     EJECT                                                                
045500 AB-LAGG-UPP-R05-I-TABELL     SECTION.                                    
045600                                                                          
045700******************************************************************        
045800*    W.PROD.CONSTANT(W092M002) R05-ADR-REGISTRET. LÄGGS I TABELL*         
045900******************************************************************        
046000                                                                          
046100     SET R05-INDX TO +1                                                   
046200     PERFORM UNTIL R05-EOF                                                
046300                                                                          
046400       READ W092M002 INTO R05-POST (R05-INDX)                             
046500                   AT END   MOVE 'J' TO R05-EOF-SW                        
046600               NOT AT END   SET R05-INDX UP BY +1                         
046700       END-READ                                                           
046800     END-PERFORM                                                          
046810     SET MAX-R05-INDX TO R05-INDX                                         
046820     SUBTRACT 1 FROM MAX-R05-INDX                                         
046900     .                                                                    
047000     EJECT                                                                
047100 B-IDFTG-FRAN-WDK601        SECTION.                                      
047200                                                                          
047300     MOVE IN-SORTBGP TO W-IDARTNR                                         
047400                                                                          
047500     PERFORM IMS-GET-ART                                                  
047600     IF SEGMENT-FINNS                                                     
047700       MOVE ART-IDFTG TO UT-IDFTG                                         
047800     ELSE                                                                 
047900       MOVE     +57      TO UT-IDFTG                                      
047960       MOVE  W-IDARTNR   TO FELMED3-IDARTNR                               
047970       MOVE  FELMED3     TO UT-FELTEXT                                    
047980       PERFORM S10A-FELKOD-SAKNAS                                         
048000     END-IF                                                               
048100     .                                                                    
048200     EJECT                                                                
048300 C-DOEP-OM-POSTTYP SECTION.                                               
048400                                                                          
048500******************************************************************        
048600*    HÄR DÖPS POSTTYPERNA OM I DE FALL DE GÅR ATT                *        
048700*    HÄRLEDA DEM TILL INPUT-TRANSAKTIONEN.                       *        
048800******************************************************************        
048900                                                                          
049000                                                                          
049300                                                                          
049400     IF (IN-IDPTYP > '003' AND < '200') OR                                
049500        (IN-IDPTYP > '200' AND < '310') OR                                
049600        (IN-IDPTYP > '310' AND < '360') OR                                
049700        (IN-IDPTYP > '371' AND < '400') OR                                
049800        (IN-IDPTYP > '499' AND < '572') OR                                
049900         IN-IDPTYP = '62 ' OR '620' OR '59 ' OR '590'                     
050000       MOVE IN-IDPTYP TO OLD-PTYP                                         
050100       MOVE OLD-PT-POS-1-2 TO NY-PT-POS-2-3                               
050200       MOVE 'R' TO NY-PT-POS-1                                            
050300       MOVE NY-PTYP TO IN-IDPTYP                                          
050400                                                                          
050500     ELSE                                                                 
050600       IF IN-IDPTYP = '575'                                               
050700         MOVE 'R75' TO IN-IDPTYP                                          
050800       ELSE                                                               
050900         IF IN-IDPTYP = '610' OR '611' OR '615' OR '625'                  
051000           MOVE 'R48' TO IN-IDPTYP                                        
051100         ELSE                                                             
051200           IF IN-IDPTYP = '612'                                           
051300             MOVE 'R49' TO IN-IDPTYP                                      
051400           ELSE                                                           
051500             IF IN-IDPTYP = '623'                                         
051600               MOVE 'R47' TO IN-IDPTYP                                    
051700             ELSE                                                         
051800               IF IN-IDPTYP = '609'                                       
051900                 MOVE 'R64' TO IN-IDPTYP                                  
052000               ELSE                                                       
052100                 IF IN-IDPTYP = '613'                                     
052200                   MOVE 'R66' TO IN-IDPTYP                                
052300                 ELSE                                                     
052400                   IF IN-IDPTYP = '651'                                   
052500                     MOVE 'R46' TO IN-IDPTYP                              
052600                   ELSE                                                   
052700                     IF IN-IDPTYP = '654'                                 
052800                       MOVE 'R44' TO IN-IDPTYP                            
052900                     ELSE                                                 
053000                       IF IN-IDPTYP = '650'                               
053100                         MOVE 'R45' TO IN-IDPTYP                          
053200                       ELSE                                               
053300                         IF IN-IDPTYP = '614'                             
053400                           MOVE 'R65' TO IN-IDPTYP                        
053500                         END-IF                                           
053600                       END-IF                                             
053700                     END-IF                                               
053800                   END-IF                                                 
053900                 END-IF                                                   
054000               END-IF                                                     
054100             END-IF                                                       
054200           END-IF                                                         
054300         END-IF                                                           
054400       END-IF                                                             
054500     END-IF                                                               
054600     .                                                                    
054700     EJECT                                                                
054800 D-SOEK-I-08-TABELLEN        SECTION.                                     
054900                                                                          
055000******************************************************************        
055100*    I 08-TABELLEN SÖKS  EFTER  ADRESSER OCH FELTEXTER.          *        
055200*    SÖKBEGREPP ÄR TRANSTYP OCH FELKOD                           *        
055300******************************************************************        
055400                                                                          
055600     SEARCH ALL 08-POST                                                   
055700         AT END                                                           
055710                  DISPLAY 'FANNS INGEN 08-TRANS    ' IN-IDPTYP            
055800                                                 ' ' IN-IDFELKODX         
055900                  MOVE 'N' TO FINNS-PA-08-SW                              
056000                                                                          
056100         WHEN 08-IDNYCKEL (08-INDX) = W-IDNYCKEL                          
056700                  MOVE 'J' TO FINNS-PA-08-SW                              
056800     END-SEARCH                                                           
056900     .                                                                    
057000     EJECT                                                                
057100 E-BEHANDLA-R05 SECTION.                                                  
057200                                                                          
057300******************************************************************        
057400*    SORT-ARGUMENTET HÄMTAS FRÅN R05-TABELLEN.                   *        
057500*    OM IDENTITETEN SAKNAS I TABELLEN SKAPAS EN TRANSAKTION TILL *        
057600*    VARJE FELLISTA.                                             *        
057700******************************************************************        
057800                                                                          
057900                                                                          
058000     PERFORM EA-SOEK-I-R05-TABELLEN                                       
058100                                                                          
058200     IF FANNS-PA-R05-TABELL                                               
058300       IF R05-KDLISTAD (R05-INDX) = 'AN'    AND                           
058400          R05-KDLISTSO (R05-INDX) =   40                                  
058500         IF UT-IDKUNDNR = +0                                              
058501           IF SEGMENT-FINNS                                               
058510             IF ART-KDERS-UTG = 0                                         
058600               PERFORM IMS-GET-CLAG                                       
058700               IF SEGMENT-FINNS                                           
058800                 MOVE CLAG-IDANSK TO UT-IDKUNDNR                          
058900               END-IF                                                     
058910             END-IF                                                       
058920           END-IF                                                         
059000         END-IF                                                           
059100       END-IF                                                             
059200       MOVE 08-BEFELTXT-SV (08-INDX) TO UT-INFO                           
059300                                                                          
059400       MOVE R05-KDLISTAD (R05-INDX)   TO UT-KDLISTAD                      
059500       MOVE R05-KDLISTSO (R05-INDX)   TO UT-KDLISTSO                      
059600       PERFORM S11-SKRIV-W09211                                           
059700       IF R05-KDLISTAD-S (R05-INDX) NOT = '  '                            
059800         MOVE R05-KDLISTAD-S (R05-INDX) TO UT-KDLISTAD                    
059900         MOVE R05-KDLISTSO-S (R05-INDX) TO UT-KDLISTSO                    
060000         PERFORM S11-SKRIV-W09211                                         
060100       END-IF                                                             
060200     ELSE                                                                 
060400       MOVE IN-GLURPKOD TO FELMED2-GLURPKOD                               
060500       MOVE FELMED2 TO UT-FELTEXT                                         
060600       PERFORM S10A-FELKOD-SAKNAS                                         
060700     END-IF                                                               
060800     .                                                                    
060900     EJECT                                                                
061000 EA-SOEK-I-R05-TABELLEN        SECTION.                                   
061100                                                                          
061200******************************************************************        
061300*    I R05-TABELLEN SÖKS  EFTER ADRESS TILL R05-TRANSAKTIONER.   *        
061400*    SÖKBEGREPP ÄR GLURPKODEN.                                   *        
061500******************************************************************        
061600                                                                          
061800     SEARCH ALL R05-POST                                                  
061900         AT END                                                           
061910                DISPLAY 'FANNS INGEN SÅN R05-GLURKOD ' IN-GLURPKOD        
062000                MOVE 'N' TO FINNS-PA-R05-SW                               
062100                                                                          
062200         WHEN R05-IDELMT   (R05-INDX) = IN-GLURPKOD                       
062700                MOVE 'J' TO FINNS-PA-R05-SW                               
062800     END-SEARCH                                                           
062900     .                                                                    
063000     EJECT                                                                
063100 F-BEHANDLA-ORDER SECTION.                                                
063200                                                                          
063300******************************************************************        
063400*    HÄR BEHANDLAS TRANSTYPERNA  R47-R52, R54, R55, R57, R73-R75 *        
063500*    UPPDELNING SKER PÅ DISTRIKT. EN TRANSAKTION GENERERAS ALLTID*        
063600*    TILL AVD "ORDER".  FÖR VISSA DISTRIKT SKAPAS TRANSAKTIONER  *        
063700*    TILL FELLISTAN FÖR AVD "ANSKAFFNING" ELLER "GODSMOTTAGNING" *        
063800******************************************************************        
063900                                                                          
064000     IF IN-KDCLAGER = +0 OR +1                                            
064100         MOVE 'OR'   TO UT-KDLISTAD                                       
064200         MOVE  40    TO UT-KDLISTSO                                       
064300         IF 08-FLFELMED (08-INDX) = 'R'                                   
064400           MOVE 08-BEFELTXT-SV (08-INDX) TO UT-FELTEXT                    
064500         END-IF                                                           
064600         PERFORM S11-SKRIV-W09211                                         
064700                                                                          
064800         IF 08-KDLISTAD-S (08-INDX) NOT = '  '                            
064900           MOVE 08-KDLISTAD-S (08-INDX) TO UT-KDLISTAD                    
065000           MOVE 08-KDLISTSO-S (08-INDX) TO UT-KDLISTSO                    
065100           PERFORM S11-SKRIV-W09211                                       
065200         END-IF                                                           
065300                                                                          
065400         MOVE IN-IDDISTR TO TEST-IDDISTR                                  
065500         IF DIST20-EMBALLAGE      OR                                      
065600            DIST93-BYTESRADIO-C2  OR                                      
065700            DIST18-SKROT          OR                                      
065800            DIST19-SATS           OR                                      
065900               (IN-IDDISTR = 8001 OR 8002)                                
066000           PERFORM FA-DISTRIKT-UPPDELNING                                 
066100         END-IF                                                           
066200     END-IF                                                               
066300     .                                                                    
066400     EJECT                                                                
066500 FA-DISTRIKT-UPPDELNING SECTION.                                          
066600                                                                          
066700******************************************************************        
066800* DISTRIKT = EMBALLAGE, SKROT, SATS, BYTESRADIO, 8001 ELLER 8002.*        
066900******************************************************************        
067000                                                                          
067100     MOVE 'AN' TO UT-KDLISTAD                                             
067200     MOVE  40  TO UT-KDLISTSO                                             
067300                                                                          
067500     IF  DIST18-SKROT  OR  DIST19-SATS                                    
067600       PERFORM S11-SKRIV-W09211                                           
067700     ELSE                                                                 
067800       IF IN-IDDISTR = 8001 OR 8002                                       
067900         IF IN-IDPTYP = 'R50' OR 'R51' OR 'R52' OR 'R55' OR 'R75'         
068000           IF IN-IDORDNR NOT = 66666                                      
068100             PERFORM S11-SKRIV-W09211                                     
068200           END-IF                                                         
068300         END-IF                                                           
068400       ELSE                                                               
068700         IF   DIST20-EMBALLAGE  OR   DIST93-BYTESRADIO-C2                 
068710                                                                          
068920           IF DIST20-EMBALLAGE  AND   NOT IDORDNR-08600-08799             
069000             MOVE 'GM' TO UT-KDLISTAD                                     
069100             MOVE  40  TO UT-KDLISTSO                                     
069300           END-IF                                                         
069310                                                                          
069400           PERFORM S11-SKRIV-W09211                                       
069410                                                                          
069500         END-IF                                                           
069600       END-IF                                                             
069700     END-IF                                                               
069800     .                                                                    
069900     EJECT                                                                
070000 G-BEHANDLA-OEVRIGA SECTION.                                              
070100                                                                          
070200                                                                          
070300       IF 08-KDLISTAD (08-INDX) = 'AN' AND                                
070400          08-KDLISTSO (08-INDX) =  40                                     
070500         IF UT-IDKUNDNR = +0                                              
070501           IF SEGMENT-FINNS                                               
070510              IF ART-KDERS-UTG = 0                                        
070600                PERFORM IMS-GET-CLAG                                      
070700                IF SEGMENT-FINNS                                          
070800                  MOVE CLAG-IDANSK TO UT-IDKUNDNR                         
070900                END-IF                                                    
070910              END-IF                                                      
070920           END-IF                                                         
071000         END-IF                                                           
071100       END-IF                                                             
071200                                                                          
071300       IF IN-IDPTYP = 'R22' AND IN-KDFELMRK = +1                          
071400         MOVE 'AN' TO UT-KDLISTAD                                         
071500         MOVE  30  TO UT-KDLISTSO                                         
071600       ELSE                                                               
071700         MOVE 08-KDLISTAD (08-INDX) TO UT-KDLISTAD                        
071800         MOVE 08-KDLISTSO (08-INDX) TO UT-KDLISTSO                        
071900       END-IF                                                             
072000                                                                          
072100       IF 08-FLFELMED (08-INDX) = 'R'                                     
072200         MOVE 08-BEFELTXT-SV (08-INDX) TO UT-FELTEXT                      
072300       END-IF                                                             
072400                                                                          
072500       IF IN-IDPTYP = 'R31' OR 'R32' OR '310'                             
072600         IF (UT-KDCLAGER = +2)       AND                                  
072700            (UT-KDLISTAD = 'GM')                                          
072800           CONTINUE                                                       
072900         ELSE                                                             
073000           PERFORM S11-SKRIV-W09211                                       
073100         END-IF                                                           
073200       ELSE                                                               
073300         PERFORM S11-SKRIV-W09211                                         
073400       END-IF                                                             
073500                                                                          
073600       IF 08-KDLISTAD-S (08-INDX) NOT = '  '                              
073700         MOVE 08-KDLISTAD-S (08-INDX) TO UT-KDLISTAD                      
073800         MOVE 08-KDLISTSO-S (08-INDX) TO UT-KDLISTSO                      
073900         PERFORM S11-SKRIV-W09211                                         
074000       END-IF                                                             
074100     .                                                                    
074200     EJECT                                                                
074300 H-SAKNAS-PA-08-TABELLEN        SECTION.                                  
074912                                                                          
074913                                                                          
074930     MOVE IN-IDPTYP TO FELMED1-KTYP                                       
074940     MOVE FELMED1   TO UT-FELTEXT                                         
074950     PERFORM S10-FELKOD-SAKNAS                                            
074960     .                                                                    
075000     EJECT                                                                
075100 I-SOEK-I-GRUPP-TABELLEN        SECTION.                                  
075200                                                                          
075300******************************************************************        
075400*    SÖKNING I TABELL EFTER SORTERINGS-BEGREPP FÖR .        *             
075500*    POSTER TILL FELLISTA PÅ SORTERAS PÅ GRUPP.            *              
075600*    VARJE POSTTYP ÄR INPLACERAD I EN BESTÄMD GRUPP.             *        
075700******************************************************************        
075800                                                                          
075900     SET PTYP-INDX TO 1                                                   
076000                                                                          
076100     SEARCH PTYP-GRUPP                                                    
076200            AT END MOVE '00' TO UT-KDLISTSO                               
076300            WHEN PTYP (PTYP-INDX) = IN-IDPTYP                             
076400                 MOVE GRUPP (PTYP-INDX) TO UT-KDLISTSO                    
076500     END-SEARCH                                                           
076600     .                                                                    
076700     EJECT                                                                
081600 Z-FINIT                SECTION.                                          
081700                                                                          
081800     MOVE SKRIV TO POSTSUM-OPKOD                                          
081900     PERFORM S60-CALL-POSTSUM                                             
082000                                                                          
082100                                                                          
082200                                                                          
082300                                                                          
082400                                                                          
082500                                                                          
082600     PERFORM  ZA-SKRIV-UT-08-TABELL-PA-08-UT                              
082700                                                                          
082800                                                                          
082900                                                                          
083000                                                                          
083100                                                                          
083200     CLOSE  INFIL                                                         
083300            W09208IN                                                      
083310            W092M002                                                      
083500            W09208UT                                                      
083510            W09211                                                        
083600     .                                                                    
083700     EJECT                                                                
083800                                                                          
083900 ZA-SKRIV-UT-08-TABELL-PA-08-UT     SECTION.                              
084000                                                                          
084100                                                                          
084200     SET 08-INDX TO +1                                                    
084300     PERFORM UNTIL 08-INDX > MAX-08-INDX                                  
084400                                                                          
084500       WRITE REGUT-AREA FROM 08-POST (08-INDX)                            
084600       END-WRITE                                                          
084601                                                                          
084630       SET 08-INDX UP BY 1                                                
084700                                                                          
084800     END-PERFORM                                                          
084900     .                                                                    
085000     EJECT                                                                
085100 S01-LAES-INFIL         SECTION.                                          
085200                                                                          
085300     READ INFIL INTO INAREA                                               
085400                AT END                                                    
085500                       MOVE JA TO INFIL-EOF-SW                            
086100     END-READ                                                             
086200     .                                                                    
086300     EJECT                                                                
086400 S10-FELKOD-SAKNAS      SECTION.                                          
086500                                                                          
086600******************************************************************        
086700*    DENNA SECTION UTFÖRS NÄR FELKODEN SAKNAS PÅ                 *        
086800*    FELKODS-REGISTRET ELLER NÄR R05 EJ KUNDE FÖRDELAS           *        
086900*    PÅ GLURPKODEN. EN FELPOST GENERERAS TILL VAR OCH            *        
087000*    EN AV DE OLIKA FELLISTORNA.                                 *        
087100******************************************************************        
087200                                                                          
087300     MOVE    99    TO UT-KDLISTSO                                         
087400                                                                          
087500     MOVE   'MF'   TO UT-KDLISTAD                                         
087600     PERFORM S11-SKRIV-W09211                                             
087700                                                                          
087800     MOVE   'AN'   TO UT-KDLISTAD                                         
087900     PERFORM S11-SKRIV-W09211                                             
088000                                                                          
088100     MOVE   'EK'   TO UT-KDLISTAD                                         
088200     PERFORM S11-SKRIV-W09211                                             
088300                                                                          
088400     MOVE   'GM'   TO UT-KDLISTAD                                         
088500     PERFORM S11-SKRIV-W09211                                             
088600                                                                          
088700     MOVE   'OR'   TO UT-KDLISTAD                                         
088800     PERFORM S11-SKRIV-W09211                                             
088900     .                                                                    
089000     EJECT                                                                
089010 S10A-FELKOD-SAKNAS      SECTION.                                         
089020                                                                          
089030******************************************************************        
089040*    DENNA SECTION UTFÖRS NÄR FELKODEN SAKNAS PÅ                 *        
089050*    FELKODS-REGISTRET ELLER NÄR R05 EJ KUNDE FÖRDELAS           *        
089060*    PÅ GLURPKODEN. EN FELPOST GENERERAS TILL FELLISTAN          *        
089070*    FÖR ANSKAFFNING.                                            *        
089080******************************************************************        
089090                                                                          
089091     MOVE    99    TO UT-KDLISTSO                                         
089092                                                                          
089096     MOVE   'AN'   TO UT-KDLISTAD                                         
089097     PERFORM S11-SKRIV-W09211                                             
089098                                                                          
089107     .                                                                    
089108     EJECT                                                                
090120 S11-SKRIV-W09211       SECTION.                                          
090130                                                                          
090140******************************************************************        
090150*    POSTERNA HAR KOMPLETTERATS MED ETT SORT-ARGUMENT.           *        
090160******************************************************************        
090170                                                                          
090180                                                                          
090204                                                                          
090205     WRITE 11-UTPOST FROM UT-AREA                                         
090213     .                                                                    
090220                                                                          
090610     EJECT                                                                
090715 S60-CALL-POSTSUM SECTION.                                                
090716                                                                          
090717     CALL POSTSUM USING POSTSUM-PARM                                      
090718     .                                                                    
090720     EJECT                                                                
090800 IMS-GET-ART            SECTION.                                          
090900                                                                          
091000                                                                          
091100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
091200                      DELIMITED BY SIZE INTO SSA1                         
091300     MOVE '  GE' TO GODK-STATUSKODER                                      
091400     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
091500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
091600     PERFORM IMS-STATUSKONTROLL                                           
091700     .                                                                    
091800     EJECT                                                                
091900 IMS-GET-CLAG SECTION.                                                    
092000                                                                          
092100                                                                          
092200     MOVE 'WLARTC11 ' TO SSA1                                             
092300     MOVE '  GE' TO GODK-STATUSKODER                                      
092400     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-11 SSA1                  
092500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
092600     PERFORM IMS-STATUSKONTROLL                                           
092700     .                                                                    
092800     EJECT                                                                
092900 IMS-STATUSKONTROLL   SECTION.                                            
093000                                                                          
093100                                                                          
093200     SET STATUS-IX TO 1                                                   
093300     SEARCH GODK-STATUS                                                   
093400            AT END    CALL FELLOG                                         
093500            WHEN GODK-STATUS(STATUS-IX) = STATUS-WS   CONTINUE            
093600     END-SEARCH                                                           
093700     .                                                                    
