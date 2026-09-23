000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6113200.                                                
000400*AUTHOR.         MÅNS SAMUELSSON.                                         
000500*DATE-WRITTEN.   93/11/17.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER NER ALLA WDD905-SEGMENT                                    
001100*        FÖR SENARE ANVÄNDNING FÖR PRODUKTIONSUPPFÖLJNING BLA I           
001200*        VIOS.                                                            
001300*                                                                         
001400*        PROGRAMMET LÄSER      WLINLB (WDD9)                              
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .FEL I DATKONV                                    
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- FIL MED INFO OM KOMMANDE LEVERANSER                        
002900     SELECT W61132                     ASSIGN TO W61132D1.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W61132                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800     SKIP2                                                                
003900*01  POST -COPY W6113201 -PRE  UT32-  -L.                                 
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200     SKIP2                                                                
004300                                                                          
004400*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W6113200'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  SPAR-IDARTNR                PIC S9(9)   VALUE +0 COMP-3.             
004900 77  SPAR-IDDC                   PIC  X(2)   VALUE SPACE.                 
005000 77  SPAR-IDLEVNR                PIC  X(5)   VALUE SPACE.                 
005100 77  W-TIAAVV                    PIC  9(4)   VALUE ZERO.                  
005200                                                                          
005210*01  -COPY WWDCKONS                                                       
005220                                                                          
005300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005400 01  FILLER REDEFINES DAGENS-DATUM.                                       
005500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005800     EJECT                                                                
005900 01  DYNAMISKA-SUBPROGRAM.                                                
006000*                                                                         
006100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006600     SKIP2                                                                
006700*    --- PARAMETRAR TILL ABEND                                            
006800                                                                          
006900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007100     SKIP2                                                                
007200 01  FELTEXT.                                                             
007300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007500     EJECT                                                                
007600*01      -COPY WDATAREA.                                                  
007700     EJECT                                                                
007800*    --- PARAMETRAR TILL POSTSUM                                          
007900*                                                                         
008000*01  -COPY W0005   -PRE  POSTSUM-                                         
008100     EJECT                                                                
008200 01  UT32-AREA-START             PIC X(24)   VALUE                        
008300                                 'UT32-AREA-START  '.                     
008400     SKIP2                                                                
008500                                                                          
008600*01  AREA -COPY W6113201     -PRE UT32-                                   
008700     EJECT                                                                
008800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008900*                                                                         
009000     EJECT                                                                
009100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009200     SKIP3                                                                
009300*    --- STATUS-KOD FRÅN IMS                                              
009400 01  STATUS-WS                   PIC XX.                                  
009500     88  SEGMENT-FINNS                       VALUE '  '.                  
009600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009700     SKIP2                                                                
009800 01  GODK-STATUSKODER.                                                    
009900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010000     SKIP3                                                                
010100 01  SSA1                        PIC X(64).                               
010200 01  SSA2                        PIC X(64).                               
010300     EJECT                                                                
010400*    --- IMS FUNKTIONSKODER                                               
010500*01  -COPY W0003                                                          
010600     EJECT                                                                
010700*    ---  DLI INPUT-OUTPUT AREA                                           
010800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010900     SKIP3                                                                
011000 01  DLI-IO-AREA.                                                         
011100     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
011200     SKIP3                                                                
011300     03  FILLER   REDEFINES IO-AREA.                                      
011400*        05  -COPY WDD901  -PRE INLB-                                     
011500     SKIP3                                                                
011600     03  FILLER   REDEFINES IO-AREA.                                      
011700*        05  -COPY WDD902  -PRE INLB-                                     
011800     SKIP3                                                                
011900     03  FILLER   REDEFINES IO-AREA.                                      
012000*        05  -COPY WDD905  -PRE INLB-                                     
012100     EJECT                                                                
012200 LINKAGE SECTION.                                                         
012300                                                                          
012400     EJECT                                                                
012500*01  -COPY W0008  -PRE INLB-                                              
012600     05  FILLER                  PIC X.                                   
012700     EJECT                                                                
012800 PROCEDURE DIVISION  USING INLB-PCB.                                      
012900     ENTRY 'DLITCBL' USING INLB-PCB.                                      
013000                                                                          
013100     SKIP2                                                                
013200     PERFORM A-INIT                                                       
013300     PERFORM IMS-GET-INLB                                                 
013400     PERFORM UNTIL SEGMENT-SLUT                                           
013500       EVALUATE INLB-SEG-NAME-FB                                          
013600         WHEN 'WDD901'                                                    
013700           MOVE INLB-IDARTNR       TO SPAR-IDARTNR                        
013800           MOVE INLB-IDDC          TO SPAR-IDDC                           
013900         WHEN 'WDD902'                                                    
014000           MOVE INLB-IDLEVNR       TO SPAR-IDLEVNR                        
014100         WHEN 'WDD905'                                                    
014200           IF  SPAR-IDDC = WC-CDC-SE                                      
014210           AND INLB-KDAVROP = +2                                          
014300             PERFORM B-SKAPA-UT32-D905                                    
014400           END-IF                                                         
014500       END-EVALUATE                                                       
014600       PERFORM IMS-GET-INLB                                               
014700     END-PERFORM                                                          
014800                                                                          
014900                                                                          
015000     PERFORM Z-FINIT                                                      
015100                                                                          
015200     MOVE ZERO TO RETURN-CODE                                             
015300     GOBACK                                                               
015400     .                                                                    
015500     EJECT                                                                
015600 A-INIT SECTION.                                                          
015700                                                                          
015800     OPEN OUTPUT W61132                                                   
015900     SKIP2                                                                
016000     ACCEPT DAGENS-DATUM  FROM DATE                                       
016100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016200     .                                                                    
016300     EJECT                                                                
016400 B-SKAPA-UT32-D905 SECTION.                                               
016500                                                                          
016600     MOVE SPAR-IDARTNR            TO UT32-IDARTNR                         
016700     MOVE SPAR-IDLEVNR            TO UT32-IDLEVNR                         
016800     MOVE INLB-KVAVROP            TO UT32-KVINLART                        
016900     MOVE ZERO                    TO UT32-BEFT                            
017000                                     UT32-IDFKNGRP                        
017100                                     UT32-ADLAGOMR                        
017200                                     UT32-VKART                           
017300                                     UT32-VLARTNTO                        
017400                                     UT32-PRARTSTD                        
017500     MOVE SPACE                   TO UT32-ADINLOMR                        
017600                                     UT32-IDDC                            
017700     MOVE NEJ                     TO UT32-FLFORAVI                        
017800     MOVE INLB-TIAVRDAT-INL       TO DAT-I-TIDATUM                        
017900     MOVE 'AAMMDD'                TO DAT-KDDATFORM                        
018000     CALL WDATKONV USING             DAT-KDDATFORM                        
018100                                     DAT-I-TIDATUM                        
018200                                     DAT-O-TIDATUM                        
018300                                     DAT-KDSVAR                           
018400     IF DAT-KDSVAR-OK                                                     
018500       MOVE DAT-TIAAVV-GRP   TO W-TIAAVV                                  
018600       MOVE W-TIAAVV         TO UT32-TIANKVV                              
018700     ELSE                                                                 
018800       MOVE 'FEL I DATKONV' TO FELTEXT-STR                                
018900       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
019000     END-IF                                                               
019100                                                                          
019200     PERFORM S11-SKRIV-W61132                                             
019300     .                                                                    
019400     EJECT                                                                
019500 Z-FINIT SECTION.                                                         
019600     CLOSE W61132                                                         
019700     SKIP2                                                                
019800     MOVE 'S' TO POSTSUM-OPKOD                                            
019900     CALL POSTSUM USING POSTSUM-PARM                                      
020000     .                                                                    
020100     EJECT                                                                
020200 S11-SKRIV-W61132 SECTION.                                                
020300     SKIP2                                                                
020400     WRITE UT32-POST FROM UT32-AREA                                       
020500                                                                          
020600     MOVE 'W61132' TO POSTSUM-FDNAMN                                      
020700     MOVE 'W61132D1' TO POSTSUM-DDNAMN2                                   
020800     CALL POSTSUM USING POSTSUM-PARM                                      
020900     .                                                                    
021000     EJECT                                                                
021100* --- IMS SEKTIONER ---                                                   
021200     SKIP3                                                                
021300     EJECT                                                                
021400 IMS-GET-INLB   SECTION.                                                  
021500     SKIP2                                                                
021600     CALL CBLTDLI USING GN INLB-PCB DLI-IO-AREA                           
021700     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
021800     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
021900     PERFORM IMS-STATUSKONTROLL                                           
022000     .                                                                    
022100     EJECT                                                                
022200 IMS-STATUSKONTROLL SECTION.                                              
022300     SKIP2                                                                
022400     SET STATUS-IX TO 1                                                   
022500     SEARCH GODK-STATUS                                                   
022600       AT END                                                             
022700         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
022800         DISPLAY FELTEXT                                                  
022900         CALL FELLOG                                                      
023000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
023100         CONTINUE                                                         
023200     END-SEARCH                                                           
023300     .                                                                    
