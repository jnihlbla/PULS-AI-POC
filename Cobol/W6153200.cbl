000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6153200.                                                
000300 AUTHOR.         TOMMIE JIVARP.                                           
000400 DATE-WRITTEN.   97/12/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET SKRIVER UTFILEN W61532 MED LAGERPLATSINFO             
000900*        IFRÅN WDK6.                                                      
001000*                                                                         
001100*        PROGRAMMET LÄSER      WLARTS (WDK6)                              
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- UTFIL MED LAGERPLATSINFORMATION                            
002600                                                                          
002700     SELECT W61532                     ASSIGN TO W61532D1.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP2                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W61532                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  POST -COPY W61530  -PRE  UT1-  -L.                                   
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100                                                                          
004200*    -- CHECKED BY WY2000                                                 
004300 77  IDPGM                       PIC X(8)    VALUE 'W6153200'.            
004400 77  JA                          PIC X       VALUE 'Y'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600     EJECT                                                                
004700 01  SW-SKRIV-TILL-FIL           PIC X.                                   
004800     88  JAG-SKALL-SKRIVA-TILL-FIL           VALUE 'Y'.                   
004900                                                                          
005000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005100 01  FILLER REDEFINES DAGENS-DATUM.                                       
005200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005500     EJECT                                                                
005600                                                                          
005700*    --- SPARADE VÄRDEN                                                   
005800 01  SPAR-IDARTNR                PIC 9(8)    VALUE ZERO.                  
005900 01  SPAR-ADLAGOMR               PIC 9(8)    VALUE ZERO.                  
006000 01  SPAR-ADGANG                 PIC 9(8)    VALUE ZERO.                  
006100 01  SPAR-ADPLATS                PIC 9(8)    VALUE ZERO.                  
006110 01  SPAR-KVQPACK-3              PIC S9(5)   VALUE ZERO.                  
006120 01  SPAR-FLCDART                PIC X(1)    VALUE SPACE.                 
006130 01  SPAR-VKART                  PIC S9(7)   VALUE ZERO.                  
006200*    --- FLAGGA OM DET ÄR SÄSONGSARTIKEL                                  
006300 01  WS-FLSEASON                 PIC X       VALUE 'N'.                   
006400*    --- BERÄKNING REFILLBEHOV                                            
006500 01  WS-KVPB-TOT                 PIC S9(6)V9(1) VALUE ZERO COMP-3.        
006600                                                                          
006700 01  DYNAMISKA-SUBPROGRAM.                                                
006800*                                                                         
006900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007300     SKIP2                                                                
007400*    --- PARAMETRAR TILL ABEND                                            
007500                                                                          
007600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007900     SKIP2                                                                
008000 01  FELTEXT.                                                             
008100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008300     EJECT                                                                
008400*    --- PARAMETRAR TILL POSTSUM                                          
008500*                                                                         
008600*01  -COPY W0005   -PRE  POSTSUM-                                         
008700     EJECT                                                                
008800 01  UT1-AREA-START              PIC X(24)   VALUE                        
008900                                 'UT1-AREA-START  '.                      
009000     SKIP2                                                                
009100                                                                          
009200*01  AREA -COPY W61530     -PRE UT1-                                      
009300     EJECT                                                                
009400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009500*                                                                         
009600     EJECT                                                                
009700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009800     SKIP2                                                                
009900*    --- STATUS-KOD FRÅN IMS                                              
010000 01  STATUS-WS                   PIC XX.                                  
010100     88  SEGMENT-FINNS                       VALUE '  '.                  
010200     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
010300     SKIP2                                                                
010400 01  GODK-STATUSKODER.                                                    
010500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010600     EJECT                                                                
010700*    --- IMS FUNKTIONSKODER                                               
010800*01  -COPY W0003                                                          
010900     EJECT                                                                
011000*    ---  DLI INPUT-OUTPUT AREA                                           
011100 01  FILLER         PIC X(16) VALUE 'DLI-IO-AREA'.                        
011200 01  DLI-IO-AREA.                                                         
011300     03  IO-AREA    PIC X(900) VALUE SPACE.                               
011400         03  DLI-IO-WLARTS01 REDEFINES IO-AREA.                           
011500*            05  -COPY WDK601                                             
011600         03  DLI-IO-WLARTS11 REDEFINES IO-AREA.                           
011700*            05  -COPY WDK611                                             
011800         03  DLI-IO-WLARTS11 REDEFINES IO-AREA.                           
011900*            05  -COPY WDK626                                             
012000     EJECT                                                                
012100*    --- PARAMETER IDDC                                                   
012200*                                                                         
012300*01  -COPY WWDC99                                                         
012400     EJECT                                                                
012500 LINKAGE SECTION.                                                         
012600                                                                          
012700     EJECT                                                                
012800*01  -COPY W0008  -PRE WDK6-                                              
012900     05  FILLER                  PIC X.                                   
013000     EJECT                                                                
013100 PROCEDURE DIVISION  USING WDK6-PCB.                                      
013200 MAIN SECTION.                                                            
013300     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
013400                                                                          
013500                                                                          
013600     PERFORM A-INIT                                                       
013700     MOVE NEJ TO SW-SKRIV-TILL-FIL                                        
013800                                                                          
013900     PERFORM IMS-GN-WDK6                                                  
014000     PERFORM UNTIL SEGMENT-SAKNAS                                         
014100        EVALUATE WDK6-SEG-NAME-FB                                         
014200           WHEN 'WDK601'                                                  
014300                                                                          
014400              IF JAG-SKALL-SKRIVA-TILL-FIL                                
014500                 PERFORM S10-FLYTTA-OCH-SKRIV                             
014600                 MOVE NEJ TO SW-SKRIV-TILL-FIL                            
014700              END-IF                                                      
014800                                                                          
014900              MOVE ART-IDARTNR    TO SPAR-IDARTNR                         
015000                                                                          
015100           WHEN 'WDK611'                                                  
015200                                                                          
015300              COMPUTE WS-KVPB-TOT = CLAG-KVPB-SEP + CLAG-KVPB-TPO         
015500              MOVE CLAG-ADLAGOMR TO SPAR-ADLAGOMR                         
015600              MOVE CLAG-ADGANG     TO SPAR-ADGANG                         
015700              MOVE CLAG-ADPLATS    TO SPAR-ADPLATS                        
015710              MOVE CLAG-KVQPACK-3  TO SPAR-KVQPACK-3                      
015720              MOVE CLAG-FLCDART    TO SPAR-FLCDART                        
015730              MOVE CLAG-VKART      TO SPAR-VKART                          
015800                                                                          
015900              MOVE JA    TO SW-SKRIV-TILL-FIL                             
016000                                                                          
016100           WHEN 'WDK626'                                                  
016200                                                                          
016300              PERFORM B-KONTROLLERA-RESEASON                              
016400                                                                          
016500           WHEN OTHER                                                     
016600              CONTINUE                                                    
016700        END-EVALUATE                                                      
016800        PERFORM IMS-GN-WDK6                                               
016900     END-PERFORM                                                          
017000     PERFORM S10-FLYTTA-OCH-SKRIV                                         
017100     PERFORM Z-FINIT                                                      
017200                                                                          
017300     MOVE ZERO TO RETURN-CODE                                             
017400     GOBACK                                                               
017500     .                                                                    
017600     EJECT                                                                
017700 A-INIT SECTION.                                                          
017800                                                                          
017900     OPEN OUTPUT W61532                                                   
018000                                                                          
018100     ACCEPT DAGENS-DATUM  FROM DATE                                       
018200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018210     MOVE '11'  TO  UT1-IDDC                                              
018300     .                                                                    
018400     EJECT                                                                
018500                                                                          
018600 B-KONTROLLERA-RESEASON SECTION.                                          
018700                                                                          
018800     IF NOT (JUST-RESEASON(1) = 1.00                                      
018900        AND  JUST-RESEASON(2) = 1.00                                      
019000        AND  JUST-RESEASON(3) = 1.00                                      
019100        AND  JUST-RESEASON(4) = 1.00                                      
019200        AND  JUST-RESEASON(5) = 1.00                                      
019300        AND  JUST-RESEASON(6) = 1.00                                      
019400        AND  JUST-RESEASON(7) = 1.00                                      
019500        AND  JUST-RESEASON(8) = 1.00                                      
019600        AND  JUST-RESEASON(9) = 1.00                                      
019700        AND  JUST-RESEASON(10) = 1.00                                     
019800        AND  JUST-RESEASON(11) = 1.00                                     
019900        AND  JUST-RESEASON(12) = 1.00)                                    
020000       MOVE  JA TO  WS-FLSEASON                                           
020100     ELSE                                                                 
020200       MOVE NEJ TO  WS-FLSEASON                                           
020300     END-IF                                                               
020400     .                                                                    
020500     EJECT                                                                
020600                                                                          
020700                                                                          
020800 Z-FINIT SECTION.                                                         
020900     CLOSE W61532                                                         
021000     SKIP2                                                                
021100     MOVE 'S' TO POSTSUM-OPKOD                                            
021200     CALL POSTSUM USING POSTSUM-PARM                                      
021300     .                                                                    
021400     EJECT                                                                
021500 S10-FLYTTA-OCH-SKRIV SECTION.                                            
021600                                                                          
021700     MOVE SPAR-IDARTNR       TO UT1-IDARTNR                               
021800     MOVE SPAR-ADLAGOMR      TO UT1-ADLAGOMR                              
021900     MOVE SPAR-ADGANG        TO UT1-ADGANG                                
022000     MOVE SPAR-ADPLATS       TO UT1-ADPLATS                               
022010     MOVE SPAR-KVQPACK-3     TO UT1-KVQPACK-3                             
022020     MOVE SPAR-FLCDART       TO UT1-FLCDART                               
022030     MOVE SPAR-VKART         TO UT1-VKART                                 
022100     MOVE WS-KVPB-TOT        TO UT1-KVPB-TOT                              
022200     MOVE WS-FLSEASON        TO UT1-FLSEASON                              
022300                                                                          
022400     PERFORM S11-SKRIV-W61532                                             
022500                                                                          
022600*    NOLLSTÄLL SÄSONGSFLAGGA EFTERSOM ALLA ARTIKLAR INTE HAR 26SEG        
022700     MOVE NEJ                TO WS-FLSEASON                               
022800     .                                                                    
022900     EJECT                                                                
023000 S11-SKRIV-W61532 SECTION.                                                
023100                                                                          
023200     WRITE UT1-POST FROM UT1-AREA                                         
023300                                                                          
023400     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
023500     MOVE 'W61532'   TO POSTSUM-FDNAMN                                    
023600     MOVE 'W61532D1' TO POSTSUM-DDNAMN2                                   
023700     CALL POSTSUM USING POSTSUM-PARM                                      
023800     .                                                                    
023900     EJECT                                                                
024000 S99-ABEND SECTION.                                                       
024100                                                                          
024200     SKIP2                                                                
024300     MOVE 'S' TO POSTSUM-OPKOD                                            
024400     CALL POSTSUM USING POSTSUM-PARM                                      
024500     CALL ABEND USING RKOD-ABEND                                          
024600     .                                                                    
024700     EJECT                                                                
024800* --- IMS SEKTIONER ---                                                   
024900 IMS-GN-WDK6   SECTION.                                                   
025000                                                                          
025100     CALL CBLTDLI USING GN WDK6-PCB DLI-IO-AREA                           
025200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
025300     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
025400     PERFORM IMS-STATUSKONTROLL                                           
025500     .                                                                    
025600     SKIP3                                                                
025700 IMS-STATUSKONTROLL SECTION.                                              
025800                                                                          
025900     SET STATUS-IX TO 1                                                   
026000     SEARCH GODK-STATUS                                                   
026100       AT END                                                             
026200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
026300           DELIMITED BY SIZE INTO FELTEXT                                 
026400         DISPLAY FELTEXT                                                  
026500         CALL FELLOG                                                      
026600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
026700         CONTINUE                                                         
026800     END-SEARCH                                                           
026900     .                                                                    
