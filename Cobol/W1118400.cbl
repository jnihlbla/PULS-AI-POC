000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W1118400.                                                
000400 AUTHOR.         STEFAN KIHLBERG.                                         
000500 DATE-WRITTEN.   94/04/27.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SAMLAR FARLIGT-GODS ARTIKLAR FRÅN WDD5 (MED SB-LÄSN)             
001000*        HÄMTAR UPPGIFTER OM DESSA FRÅN WDK6                              
001100*        SKRIVER FIL W11184                                               
001200*                                                                         
001300*        PROGRAMMET LÄSER      WLARTN (WDD5)                              
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- FARLIGT-GODS ARTIKLAR                                      
002800     SELECT W11184                     ASSIGN TO W11184D1.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W11184                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700     SKIP2                                                                
003800*01  POST -COPY W11184 -PRE  W11184-  -L.                                 
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W1118400'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  FIRST-ARTIKEL-SW            PIC X       VALUE 'J'.                   
004600     88  FIRST-ARTIKEL                       VALUE 'J'.                   
004700                                                                          
004800     EJECT                                                                
004900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005000 01  FILLER REDEFINES DAGENS-DATUM.                                       
005100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005400     EJECT                                                                
005500 01  ARBETSAREOR-TEXT            PIC X(24) VALUE 'ARBETSAREOR'.           
005600                                                                          
005700 01  ARBETSAREOR.                                                         
005800     03  WS-KDARTHNT                 PIC 9(6).                            
005900     03  FILLER REDEFINES WS-KDARTHNT.                                    
006000        05  WS-KDARTHNT-V           PIC 9(3).                             
006100        05  WS-KDARTHNT-H           PIC 9(3).                             
006200                                                                          
006300 01  WDD5-AREA-START        PIC X(24) VALUE 'WDD5-AREA-START'.            
006400                                                                          
006500 01  WDD501-AREA.                                                         
006600*    03  -COPY WDD501   -PRE WDD501-                                      
006700     EJECT                                                                
006800 01  DYNAMISKA-SUBPROGRAM.                                                
006900*                                                                         
007000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007500     SKIP2                                                                
007600*    --- PARAMETRAR TILL ABEND                                            
007700                                                                          
007800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008000     SKIP2                                                                
008100 01  FELTEXT.                                                             
008200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008400     EJECT                                                                
008500*    --- PARAMETRAR TILL POSTSUM                                          
008600*                                                                         
008700*01  -COPY W0005   -PRE  POSTSUM-                                         
008800     EJECT                                                                
008900 01  W11184-AREA-START           PIC X(24)   VALUE                        
009000                                 'W11184-AREA-START  '.                   
009100     SKIP2                                                                
009200                                                                          
009300*01  AREA -COPY W11184     -PRE W11184-                                   
009400     EJECT                                                                
009500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009600*                                                                         
009700     EJECT                                                                
009800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009900     SKIP3                                                                
010000 01  NYCKLAR-TILL-DLI.                                                    
010100     03  W-IDARTNR-X.                                                     
010200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010700     SKIP2                                                                
010800*    --- STATUS-KOD FRÅN IMS                                              
010900 01  STATUS-WS                   PIC XX.                                  
011000     88  SEGMENT-FINNS                       VALUE '  '.                  
011100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011300     SKIP2                                                                
011400 01  GODK-STATUSKODER.                                                    
011500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011600     SKIP3                                                                
011700 01  SSA1                        PIC X(64).                               
011800 01  SSA2                        PIC X(64).                               
011900     EJECT                                                                
012000*    --- IMS FUNKTIONSKODER                                               
012100*01  -COPY W0003                                                          
012200     EJECT                                                                
012300*    ---  DLI INPUT-OUTPUT AREA                                           
012400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012500     SKIP3                                                                
012600 01  DLI-IO-AREA.                                                         
012700     03  IO-AREA                 PIC X(200)  VALUE SPACE.                 
012800     SKIP3                                                                
012900     EJECT                                                                
013000*    ---  DLI INPUT-OUTPUT AREA 2                                         
013100 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-AREA2'.           
013200     SKIP3                                                                
013300 01  DLI-IO-AREA2.                                                        
013400     03  IO-AREA2              PIC X(900)  VALUE SPACE.                   
013500     SKIP3                                                                
013600     03  WLARTC01 REDEFINES IO-AREA2.                                     
013700*        05  -COPY WDK601                                                 
013800     03  WLARTC11 REDEFINES IO-AREA2.                                     
013900*        05  -COPY WDK611                                                 
014400     EJECT                                                                
014500 LINKAGE SECTION.                                                         
014600                                                                          
014700     EJECT                                                                
014800*01  -COPY W0008  -PRE WDD5-                                              
014900     05  FILLER                  PIC X.                                   
015000     EJECT                                                                
015100*01  -COPY W0008  -PRE ARTC-                                              
015200     05  FILLER                  PIC X.                                   
015300     EJECT                                                                
015400 PROCEDURE DIVISION  USING WDD5-PCB ARTC-PCB.                             
015500     ENTRY 'DLITCBL' USING WDD5-PCB ARTC-PCB.                             
015600                                                                          
015700     SKIP2                                                                
015800     PERFORM A-INIT                                                       
015900     PERFORM IMS-GET-WDD5                                                 
016000     PERFORM UNTIL SEGMENT-SLUT                                           
016100        EVALUATE WDD5-SEG-NAME-FB                                         
016200           WHEN 'WDD501  '                                                
016300              IF FIRST-ARTIKEL                                            
016400                 MOVE NEJ TO FIRST-ARTIKEL-SW                             
016500              ELSE                                                        
016600                 PERFORM B-BEHANDLA-ARTIKEL                               
016700                 PERFORM S11-SKRIV-W11184                                 
016800              END-IF                                                      
016900              MOVE DLI-IO-AREA TO WDD501-AREA                             
017000        END-EVALUATE                                                      
017100        PERFORM IMS-GET-WDD5                                              
017200     END-PERFORM                                                          
017300     PERFORM B-BEHANDLA-ARTIKEL                                           
017400     PERFORM S11-SKRIV-W11184                                             
017500     PERFORM Z-FINIT                                                      
017600                                                                          
017700     MOVE ZERO TO RETURN-CODE                                             
017800     GOBACK                                                               
017900     .                                                                    
018000     EJECT                                                                
018100                                                                          
018200                                                                          
018300 A-INIT SECTION.                                                          
018400                                                                          
018500     OPEN OUTPUT W11184                                                   
018600     SKIP2                                                                
018700     ACCEPT DAGENS-DATUM  FROM DATE                                       
018800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018900     .                                                                    
019000     EJECT                                                                
019100                                                                          
019200                                                                          
019300 B-BEHANDLA-ARTIKEL SECTION.                                              
019400                                                                          
019500     MOVE WDD501-ART-IDARTNR TO W-IDARTNR                                 
019600                            W11184-IDARTNR                                
019700     PERFORM BA-FLYTTA-WDD5                                               
019800     PERFORM BB-FLYTTA-WDK6                                               
019900     .                                                                    
020000     EJECT                                                                
020100                                                                          
020200                                                                          
020300 BA-FLYTTA-WDD5 SECTION.                                                  
020400                                                                          
020500     MOVE WDD501-ART-IDARTNR-RECEPT TO W11184-IDARTNR-RECEPT              
020510     MOVE WDD501-ART-BEEMBMAT       TO W11184-BEEMBMAT                    
020600     MOVE WDD501-ART-FLFROST        TO W11184-FLFROST                     
020700     MOVE WDD501-ART-FLTACTIL       TO W11184-FLTACTIL                    
020800     MOVE WDD501-ART-FLVARINF       TO W11184-FLVARINF                    
020900     MOVE WDD501-ART-FLVARINF-SDS   TO W11184-FLVARINF-SDS                
021000     MOVE WDD501-ART-IDAO-FG        TO W11184-IDAO-FG                     
021100     MOVE WDD501-ART-IDVARINF       TO W11184-IDVARINF                    
021200     MOVE WDD501-ART-IDVARINF-SDS   TO W11184-IDVARINF-SDS                
021300     MOVE WDD501-ART-KDFARG         TO W11184-KDFARG                      
021400     MOVE WDD501-ART-KDFGPRIO       TO W11184-KDFGPRIO                    
021500     MOVE WDD501-ART-KDLACK         TO W11184-KDLACK                      
021600     MOVE WDD501-ART-KDSORT-KVNTOFG TO W11184-KDSORT-KVNTOFG              
021700     MOVE WDD501-ART-KDSORT-VLFG    TO W11184-KDSORT-VLFG                 
021800     MOVE WDD501-ART-KVFLAMP        TO W11184-KVFLAMP                     
021900     MOVE WDD501-ART-KVNTOFG        TO W11184-KVNTOFG                     
022000     MOVE WDD501-ART-KVVOC          TO W11184-KVVOC                       
022200     MOVE WDD501-ART-SUEQFG         TO W11184-SUEQFG                      
022300     MOVE WDD501-ART-VKART-FG       TO W11184-VKART-FG                    
022400     MOVE WDD501-ART-VKFORSFG       TO W11184-VKFORSFG                    
022500     MOVE WDD501-ART-VLFG           TO W11184-VLFG                        
022600     MOVE WDD501-ART-TENOTE(1)      TO W11184-TENOTE(1)                   
022610     MOVE WDD501-ART-TENOTE(2)      TO W11184-TENOTE(2)                   
022700     MOVE WDD501-ART-TIREGDAT       TO W11184-TIREGDAT                    
022710     MOVE WDD501-ART-IDANMNR        TO W11184-IDANMNR                     
022720     MOVE WDD501-ART-REKSIFFR-ANMNR TO W11184-REKSIFFR-ANMNR              
022800     .                                                                    
022900     EJECT                                                                
023000                                                                          
023100                                                                          
023200 BB-FLYTTA-WDK6 SECTION.                                                  
023300                                                                          
023400     PERFORM IMS-GET-WDK601                                               
023500     IF SEGMENT-FINNS                                                     
023600        IF ART-KDERS-UTG > 0                                              
023700           MOVE ART-KDERS-UTG      TO W11184-KDERS                        
023800           MOVE ZERO               TO W11184-IDPSN                        
023900                                      W11184-KDFARLIG                     
024000                                      W11184-KDARTHNT-H                   
024100        ELSE                                                              
024200           PERFORM IMS-GET-WDK611                                         
024300           MOVE CLAG-IDPSN         TO W11184-IDPSN                        
024500           MOVE CLAG-KDFARLIG      TO W11184-KDFARLIG                     
024800           MOVE CLAG-KDERS         TO W11184-KDERS                        
024900           MOVE CLAG-KDARTHNT      TO WS-KDARTHNT                         
025000           MOVE WS-KDARTHNT-H      TO W11184-KDARTHNT-H                   
025100        END-IF                                                            
025200     ELSE                                                                 
025300        MOVE ZERO             TO W11184-IDPSN                             
025400                                 W11184-KDFARLIG                          
025500                                 W11184-KDERS                             
025600                                 W11184-KDARTHNT-H                        
025700     END-IF                                                               
025800     .                                                                    
025900     EJECT                                                                
026200 Z-FINIT SECTION.                                                         
026300     CLOSE W11184                                                         
026400     SKIP2                                                                
026500     MOVE 'S' TO POSTSUM-OPKOD                                            
026600     CALL POSTSUM USING POSTSUM-PARM                                      
026700     .                                                                    
026800     EJECT                                                                
027100 S11-SKRIV-W11184 SECTION.                                                
027200     SKIP2                                                                
027300     WRITE W11184-POST FROM W11184-AREA                                   
027400                                                                          
027500     MOVE 'W11184' TO POSTSUM-FDNAMN                                      
027600     MOVE 'W11184D1' TO POSTSUM-DDNAMN2                                   
027700     CALL POSTSUM USING POSTSUM-PARM                                      
027800     .                                                                    
027900     EJECT                                                                
028200* --- IMS SEKTIONER ---                                                   
028300     SKIP3                                                                
028500 IMS-GET-WDD5   SECTION.                                                  
028600                                                                          
028700     CALL CBLTDLI USING GN WDD5-PCB DLI-IO-AREA                           
028800     MOVE WDD5-STATUS-CODE TO STATUS-WS                                   
028900     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
029000     PERFORM IMS-STATUSKONTROLL                                           
029100     .                                                                    
029200     SKIP3                                                                
029500 IMS-GET-WDK601  SECTION.                                                 
029600                                                                          
029700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
029800            DELIMITED BY SIZE INTO SSA1                                   
029900     MOVE '  GE' TO GODK-STATUSKODER                                      
030000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA2 SSA1                     
030100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
030200     PERFORM IMS-STATUSKONTROLL                                           
030300     .                                                                    
030400     SKIP3                                                                
030700 IMS-GET-WDK611  SECTION.                                                 
030800                                                                          
030900     MOVE 'WLARTC11 ' TO SSA1                                             
031100     MOVE '  ' TO GODK-STATUSKODER                                        
031200     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA2 SSA1                    
031300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
031400     PERFORM IMS-STATUSKONTROLL                                           
031500     .                                                                    
031600     EJECT                                                                
034300 IMS-STATUSKONTROLL SECTION.                                              
034400                                                                          
034500     SET STATUS-IX TO 1                                                   
034600     SEARCH GODK-STATUS                                                   
034700       AT END                                                             
034800         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
034900         DISPLAY FELTEXT                                                  
035000         CALL FELLOG                                                      
035100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
035200         CONTINUE                                                         
035300     END-SEARCH                                                           
035400     .                                                                    
