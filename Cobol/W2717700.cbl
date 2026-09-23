000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2717700.                                                
000400 AUTHOR.         STEFAN ANDREASSON.                                       
000500 DATE-WRITTEN.   99/09/16.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER ARTIKELBASEN WDK6 MED SB.                                  
001000*        151020 FIL W27277 TILLAGD FÖR KINA EXPORT TILL CDC /JN           
001100*                                                                         
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
002500     SELECT W27177                     ASSIGN TO W27177D1.                
002600     SKIP2                                                                
002700     SELECT W27277                     ASSIGN TO W27177D2.                
002800     SKIP2                                                                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP2                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W27177                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  POST -COPY W27177 -PRE  UT-  -L.                                     
003900     SKIP3                                                                
004000 FD  W27277                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  POST -COPY W27277 -PRE  UT2-  -L.                                    
004500     SKIP3                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800                                                                          
004900*    -- CHECKED BY WY2000                                                 
005000 77  IDPGM                       PIC X(8)    VALUE 'W2717700'.            
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300                                                                          
005400 01  ARBETSAREOR.                                                         
005500     03  WS-SPAR-IDARTNR         PIC S9(9)   VALUE ZERO COMP-3.           
005600     03  IX                      PIC S9(1)   VALUE ZERO COMP-3.           
005700                                                                          
005800                                                                          
005900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006000 01  FILLER REDEFINES DAGENS-DATUM.                                       
006100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006400     EJECT                                                                
006500                                                                          
006600                                                                          
006700 01  DYNAMISKA-SUBPROGRAM.                                                
006800*                                                                         
006900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007210     03  W271UTIL                PIC X(8)    VALUE 'W271UTIL'.            
007300     SKIP2                                                                
007400*    --- PARAMETRAR TILL ABEND                                            
007500                                                                          
007510 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007800     SKIP2                                                                
007900 01  FELTEXT.                                                             
008000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008200     EJECT                                                                
008300                                                                          
008400*    --- PARAMETRAR TILL POSTSUM                                          
008500                                                                          
008600*01  -COPY W0005   -PRE  POSTSUM-                                         
008700     EJECT                                                                
008710*    --- PARAMETRAR TILL W271UTIL                                         
008720*01 -COPY W271UTIL                                                        
008730     EJECT                                                                
008740                                                                          
008800 01  UT-AREA-START           PIC X(24)   VALUE                            
008900                                 'UT-AREA-START  '.                       
009000     SKIP2                                                                
009100                                                                          
009200*01  AREA -COPY W27177     -PRE UT-                                       
009300                                                                          
009400*01  AREA -COPY W27277     -PRE UT2-                                      
009500                                                                          
009600                                                                          
009700     EJECT                                                                
009800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009900*                                                                         
010000     EJECT                                                                
010100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010200     SKIP3                                                                
010300 01  NYCKLAR-TILL-DLI.                                                    
010400     03  W-IDARTNR-X.                                                     
010500        05 W-IDARTNR            PIC S9(9)  COMP-3.                        
010600                                                                          
010700                                                                          
010800     03  W-KDSEKEY-X.                                                     
010900        05 W-KDSEGKEY           PIC X.                                    
011000                                                                          
011100                                                                          
011200     EJECT                                                                
011300                                                                          
011400     SKIP2                                                                
011500*    --- STATUS-KOD FRÅN IMS                                              
011600 01  STATUS-WS                   PIC XX.                                  
011700     88  SEGMENT-FINNS                       VALUE '  '.                  
011800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011900     SKIP2                                                                
012000 01  GODK-STATUSKODER.                                                    
012100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012200     SKIP3                                                                
012300 01  SSA1                        PIC X(64).                               
012400 01  SSA2                        PIC X(64).                               
012500     EJECT                                                                
012600*    --- IMS FUNKTIONSKODER                                               
012700*01  -COPY W0003                                                          
012800     EJECT                                                                
012900*    ---  DLI INPUT-OUTPUT AREA                                           
013000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013100     SKIP3                                                                
013200 01  DLI-IO-AREA.                                                         
013300     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
013400                                                                          
013500     03  WDK601    REDEFINES  IO-AREA.                                    
013600         05  -COPY WDK601                                                 
013700                                                                          
013800     03  WDK611    REDEFINES  IO-AREA.                                    
013900         05  -COPY WDK611                                                 
014000                                                                          
014100     03  WDK629    REDEFINES  IO-AREA.                                    
014200         05  -COPY WDK629                                                 
014300                                                                          
014400                                                                          
014500     EJECT                                                                
014600 LINKAGE SECTION.                                                         
014700                                                                          
014800     EJECT                                                                
014900*01  -COPY W0008  -PRE WDK6-                                              
015000     05  FILLER                  PIC X.                                   
015100     EJECT                                                                
015110*01  -COPY W0008      -PRE UTIL-WDK6-                                     
015120     05  FILLER                  PIC X.                                   
015130     EJECT                                                                
015140*01  -COPY W0008      -PRE UTIL-WDK7-                                     
015150     05  FILLER                  PIC X.                                   
015160     EJECT                                                                
015170*01  -COPY W0008      -PRE UTIL-WDB6-                                     
015180     05  FILLER                  PIC X.                                   
015190     EJECT                                                                
015200 PROCEDURE DIVISION  USING WDK6-PCB                                       
015210                           UTIL-WDK6-PCB UTIL-WDK7-PCB                    
015220                           UTIL-WDB6-PCB.                                 
015300     ENTRY 'DLITCBL' USING WDK6-PCB                                       
015310                           UTIL-WDK6-PCB UTIL-WDK7-PCB                    
015320                           UTIL-WDB6-PCB.                                 
015400                                                                          
015500     PERFORM A-INIT                                                       
015600     PERFORM IMS-GET-WDK6                                                 
015700     PERFORM UNTIL SEGMENT-SLUT                                           
015800        EVALUATE WDK6-SEG-NAME-FB                                         
015900           WHEN 'WDK601  '                                                
016000              MOVE ART-IDARTNR TO WS-SPAR-IDARTNR                         
016100              IF ART-KDERS-UTG = 0                                        
016200                 PERFORM B-FLYTTA-WDK601                                  
016300              END-IF                                                      
016400           WHEN 'WDK611  '                                                
016500              PERFORM C-FLYTTA-WDK611                                     
016600              PERFORM S11-SKRIV-W27177                                    
016700           WHEN 'WDK629  '                                                
016800              PERFORM D-FLYTTA-WDK629                                     
016900              PERFORM S12-SKRIV-W27277                                    
017000                                                                          
017100         END-EVALUATE                                                     
017200         PERFORM IMS-GET-WDK6                                             
017300     END-PERFORM                                                          
017400     PERFORM Z-FINIT                                                      
017500     MOVE ZERO TO RETURN-CODE                                             
017600     GOBACK                                                               
017700     .                                                                    
017800     EJECT                                                                
017900                                                                          
018000                                                                          
018100 A-INIT SECTION.                                                          
018200                                                                          
018300     OPEN OUTPUT W27177                                                   
018400                 W27277                                                   
018500                                                                          
018600     ACCEPT DAGENS-DATUM  FROM DATE                                       
018700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018800     .                                                                    
018900     EJECT                                                                
019000                                                                          
019100 B-FLYTTA-WDK601 SECTION.                                                 
019200                                                                          
019300     MOVE ART-IDARTNR          TO UT-IDARTNR                              
019400                                  UT2-IDARTNR                             
019500     MOVE ART-IDFKNGRP         TO UT-IDFKNGRP                             
019600                                  UT2-IDFKNGRP                            
019700     MOVE ART-KDPRODSL         TO UT-KDPRODSL                             
019800                                  UT2-KDPRODSL                            
019900     MOVE ART-IDLEVNR          TO UT-IDLEVNR                              
020000                                  UT2-IDLEVNR                             
020010     MOVE ART-TISOP            TO UT-TISOP                                
020020                                  UT2-TISOP                               
020030     MOVE ART-TIURPROD         TO UT-TIURPROD                             
020040                                  UT2-TIURPROD                            
020050     MOVE ART-FLBSNES          TO UT-FLBSNES                              
020060                                  UT2-FLBSNES                             
020070     MOVE ART-KVEOP            TO UT-KVEOP                                
020080                                  UT2-KVEOP                               
020070     MOVE ART-KDANSKSEG        TO UT-KDANSKSEG                            
020100     .                                                                    
020200     EJECT                                                                
020300                                                                          
020400                                                                          
020500 C-FLYTTA-WDK611 SECTION.                                                 
020600                                                                          
020700     MOVE CLAG-IDPSN           TO UT-IDPSN                                
020800                                  UT2-IDPSN                               
021100     MOVE CLAG-IDPROJ          TO UT-IDPROJ                               
021200                                  UT2-IDPROJ                              
021300     MOVE CLAG-KDFARLIG        TO UT-KDFARLIG                             
021400                                  UT2-KDFARLIG                            
021500     MOVE CLAG-KDUART          TO UT-KDUART                               
021600                                  UT2-KDUART                              
021700     MOVE CLAG-IDANSK          TO UT-IDANSK                               
021800                                  UT2-IDANSK                              
021900     MOVE CLAG-IDSTATNR(3)     TO UT-IDSTATNR                             
022000                                  UT2-IDSTATNR                            
022010     MOVE CLAG-VLARTNTO        TO UT-VLARTNTO                             
022020                                  UT2-VLARTNTO                            
022010     MOVE CLAG-KDVSOP          TO UT-KDVSOP                               
022100     .                                                                    
022200     EJECT                                                                
022300                                                                          
022400 D-FLYTTA-WDK629 SECTION.                                                 
022500                                                                          
022600     MOVE CREF-IDPERSON-BUY    TO UT2-IDPERSON-BUY                        
022700     MOVE CREF-IDDC-REF        TO UT2-IDDC-REF                            
022701     MOVE CREF-FLFLYG          TO UT2-FLFLYG                              
022710     MOVE CREF-FLBUYUPD        TO UT2-FLBUYUPD                            
022720     MOVE CREF-FLTABUPD        TO UT2-FLTABUPD                            
022721     MOVE CREF-IDREFTAB        TO UT2-IDREFTAB                            
022722                                                                          
022730     PERFORM DA-GET-FC-TOT                                                
022740     MOVE UTIL-KVPB-TOT        TO UT2-KVPB-TOT                            
022800                                                                          
022900     .                                                                    
023000     EJECT                                                                
023010                                                                          
023020 DA-GET-FC-TOT        SECTION.                                            
023030                                                                          
023040     INITIALIZE UTIL-W271UTIL                                             
023050     MOVE WS-SPAR-IDARTNR           TO UTIL-IDARTNR                       
023060     MOVE 003                       TO UTIL-KDCALL                        
023070                                                                          
023080     CALL W271UTIL USING UTIL-W271UTIL                                    
023090                         UTIL-WDK6-PCB                                    
023100                         UTIL-WDK7-PCB                                    
023101                         UTIL-WDB6-PCB                                    
023102                                                                          
023103     IF UTIL-KDSVAR-OK                                                    
023104        CONTINUE                                                          
023105     ELSE                                                                 
023106        MOVE 'FEL FRÅN W271UTIL '   TO FELTEXT-STR                        
023108        DISPLAY FELTEXT                                                   
023109        PERFORM S99-ABEND                                                 
023110     END-IF                                                               
023111     .                                                                    
023112     EJECT                                                                
023113                                                                          
023120 Z-FINIT SECTION.                                                         
023200     CLOSE W27177                                                         
023300           W27277                                                         
023400     SKIP2                                                                
023500     MOVE 'S' TO POSTSUM-OPKOD                                            
023600     CALL POSTSUM USING POSTSUM-PARM                                      
023700     .                                                                    
023800     EJECT                                                                
023900                                                                          
024000                                                                          
024100 S11-SKRIV-W27177 SECTION.                                                
024200                                                                          
024300     WRITE UT-POST FROM UT-AREA                                           
024400                                                                          
024500     MOVE 'W27177' TO POSTSUM-FDNAMN                                      
024600     MOVE 'W27177D1' TO POSTSUM-DDNAMN2                                   
024700     CALL POSTSUM USING POSTSUM-PARM                                      
024800     .                                                                    
024900     EJECT                                                                
025000                                                                          
025100 S12-SKRIV-W27277 SECTION.                                                
025200                                                                          
025300     WRITE UT2-POST FROM UT2-AREA                                         
025400                                                                          
025500     MOVE 'W27277' TO POSTSUM-FDNAMN                                      
025600     MOVE 'W27177D2' TO POSTSUM-DDNAMN2                                   
025700     CALL POSTSUM USING POSTSUM-PARM                                      
025800     .                                                                    
025900     EJECT                                                                
026000                                                                          
026100                                                                          
026110 S99-ABEND SECTION.                                                       
026120     SKIP2                                                                
026130     MOVE 'S' TO POSTSUM-OPKOD                                            
026140     CALL POSTSUM USING POSTSUM-PARM                                      
026150     CALL ABEND USING RKOD-ABEND                                          
026160     .                                                                    
026170     EJECT                                                                
026180                                                                          
026200* --- IMS SEKTIONER ---                                                   
026500                                                                          
026600                                                                          
026700 IMS-GET-WDK6   SECTION.                                                  
026800                                                                          
026900     CALL CBLTDLI USING GN WDK6-PCB DLI-IO-AREA                           
027000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
027100     MOVE '  GAGKGBGE' TO GODK-STATUSKODER                                
027200     PERFORM IMS-STATUSKONTROLL                                           
027300     .                                                                    
027400     EJECT                                                                
027500 IMS-STATUSKONTROLL SECTION.                                              
027600                                                                          
027700     SET STATUS-IX TO 1                                                   
027800     SEARCH GODK-STATUS                                                   
027900       AT END                                                             
028000         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
028100         DISPLAY FELTEXT                                                  
028200         CALL FELLOG                                                      
028300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
028400         CONTINUE                                                         
028500     END-SEARCH                                                           
028600     .                                                                    
