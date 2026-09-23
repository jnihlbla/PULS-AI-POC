000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             W4260500.                                        
000400 AUTHOR.                 INGER NILSSON.                                   
000500 DATE-WRITTEN.           MAJ 1990.                                        
000700     REMARKS.                                                             
000900*    FUNKTION:                                                            
001000*        SKAPAR FILER FÖR UTSKRIFT AV STATISTIK.                          
001110*                                                                         
001200*        RENSNING AV W6H6 FÖR KONTROLLGRUPP ÄLDRE ÄN                      
001300*        13 REDOVISNINGSPERIODER.                                         
001400*                                                                         
001500*        UTDATA.                                                          
001600*                W42605D1  FIL FÖR UTSKRIFT AV FELFÖRDELNING              
001700*                                                                         
001800*                W42605D2  FIL FÖR UTSKRIFT AV KVALITETSINDEX             
001900*                                                                         
002000*                W42605D3  FIL FÖR UTSKRIFT AV FÖRDELNING FELGRUPP        
002200*                                                                         
002300*        UPPDATERA W6H6                                                   
002310*        LÄSER     W6H5                                                   
002400*                                                                         
002500     EJECT                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700                                                                          
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     SELECT W4260501 ASSIGN TO W42605D1.                                  
003200     SELECT W4260502 ASSIGN TO W42605D2.                                  
003300     SELECT W4260503 ASSIGN TO W42605D3.                                  
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600                                                                          
003700 FILE SECTION.                                                            
003800     SKIP2                                                                
003900 FD  W4260501                                                             
004000     LABEL RECORD STANDARD                                                
004100     RECORDING F                                                          
004200     BLOCK CONTAINS 0.                                                    
004300*01  W4260501-POST -COPY W4260501   -L                                    
004500                                                                          
004600 FD  W4260502                                                             
004700     LABEL RECORD STANDARD                                                
004800     RECORDING F                                                          
004900     BLOCK CONTAINS 0.                                                    
005000*01  W4260503-POST -COPY W4260503   -L                                    
005200                                                                          
005300 FD  W4260503                                                             
005400     LABEL RECORD STANDARD                                                
005500     RECORDING F                                                          
005600     BLOCK CONTAINS 0.                                                    
005700*01  W4260504-POST -COPY W4260504   -L                                    
005800                                                                          
005900     EJECT                                                                
006000 WORKING-STORAGE SECTION.                                                 
006100     SKIP2                                                                
006110                                                                          
006120*    -- CHECKED BY WY2000                                                 
006130*                                                                         
006200*    ---- ARBETSVARIABLER                                                 
006300*                                                                         
006400 77  IDPGM                   PIC  X(8)         VALUE 'W4260500'.          
006500 77  W-ANT-PERIOD            PIC  9(2).                                   
006510 77  W-PROCSATS              PIC S9(1)V9(1) COMP-3 VALUE +0.              
006520 77  W-REKVAIND-BUF          PIC S9(5)V9(2) COMP-3 VALUE +0.              
006600 77  W-SUM-KVKVAFPO          PIC S9(5) COMP-3.                            
006700 77  W-OMR-REKVAOMR          PIC  99V9.                                   
006900 77  W-SPAR-IDDC             PIC  X(2).                                   
007000 77  W-SPAR-IDKVAOMR         PIC  X(1).                                   
007010 77  W-SPAR-IDKVATRG         PIC  9(2).                                   
007100 77  W-SPAR-IDARTNR          PIC S9(9) COMP-3 VALUE +0.                   
007200 77  W-SPAR-TIAARP           PIC  9(4)         VALUE 0.                   
007300 77  W-KVKVAFPO              PIC S9(5) COMP-3.                            
007400 77  W-KDKVAFG               PIC S9(1) COMP-3.                            
007410 77  W-SPARVARDE             PIC S9(5)V9(2).                              
007500 77  W-SKRIV-UTFIL           PIC  X(1).                                   
007600 77  W-PERIOD                PIC  X(1).                                   
007700 77  W-TOT-KVKVAFEL-OMR      PIC S9(7) COMP-3 VALUE +0.                   
007710 77  W-TOT-KVKVAFEL-TRG      PIC S9(7) COMP-3 VALUE +0.                   
007800 77  W-TOT-KVKVAFEL-DC       PIC S9(7) COMP-3 VALUE +0.                   
007900 77  W-TOT-KVKVAFEL-DC-KV    PIC S9(7) COMP-3 VALUE +0.                   
008000 77  W-TOT-KVARTFEL-OMR      PIC S9(7) COMP-3 VALUE +0.                   
008010 77  W-TOT-KVARTFEL-TRG      PIC S9(7) COMP-3 VALUE +0.                   
008100 77  W-TOT-KVARTFEL-DC       PIC S9(7) COMP-3 VALUE +0.                   
008200 77  W-TOT-KVARTFEL-DC-KV    PIC S9(7) COMP-3 VALUE +0.                   
008300 77  W-TOT-KVART-DC-KV       PIC S9(7) COMP-3 VALUE +0.                   
008400 77  W-OMR-BEKVAOMR-DC       PIC  X(10).                                  
008604 77  W-OMR-BEKVAOMR-GB       PIC  X(10).                                  
008610 77  WS-REKVAREL-OMR         PIC S9(5)V9(2) COMP-3.                       
008700                                                                          
008800 01  W-TIAARP                PIC  9(4).                                   
008900 01  FILLER REDEFINES W-TIAARP.                                           
009000     03  W-TIAARP-AA         PIC  9(2).                                   
009100     03  W-TIAARP-RP         PIC  9(2).                                   
009200                                                                          
009300 01  W-DAT-TIAARP            PIC  9(4).                                   
009400 01  FILLER REDEFINES W-DAT-TIAARP.                                       
009500     03  W-DAT-TIAARP-AA     PIC  9(2).                                   
009600     03  W-DAT-TIAARP-RP     PIC  9(2).                                   
009700                                                                          
009800 01  W-OMR-TIAARP            PIC  9(4).                                   
009900 01  FILLER REDEFINES W-OMR-TIAARP.                                       
010000     03  W-OMR-TIAARP-AA     PIC  9(2).                                   
010100     03  W-OMR-TIAARP-RP     PIC  9(2).                                   
010200*                                                                         
010201 77  W6KVAB01-SW                 PIC X       VALUE 'N'.                   
010202     88  W6KVAB01-SLUT                       VALUE 'J'.                   
010210                                                                          
010300 77  JA                      PIC  X(1) VALUE 'J'.                         
010400 77  NEJ                     PIC  X(1) VALUE 'N'.                         
010500*      --- VALID IDDC CODES                                               
010510*                                                                         
010520*01    -COPY WWDC99                                                       
010530       EJECT                                                              
010600                                                                          
010700*    ---- INDEXFÄLT                                                       
010800                                                                          
010900 77  IX1                     PIC S9(3)   VALUE +0   COMP SYNC.            
011000 77  IX2                     PIC S9(3)   VALUE +0   COMP SYNC.            
011001 77  DCIX                    PIC S9(3)   VALUE +0   COMP SYNC.            
011002                                                                          
011100     EJECT                                                                
011200*    TABELL1                                                              
011300*    ANTAL FEL / FELKOD FÖR OMRÅDE OCH LAGER SENASTE                      
011400*    REDOVISNINGSPERIODEN. FÖR LAGER ÄVEN SENASTE TRE                     
011500*    REDOVISNINGSPERIODER SUMMERADE.                                      
011600                                                                          
011700 01  TABELL1.                                                             
011800     03 TAB1-FELKOD OCCURS 99                                             
011900        ASCENDING KEY IS TAB1-IDKVAFEL                                    
012000        INDEXED BY T1X.                                                   
012100        05 TAB1-IDKVAFEL       PIC 9(2).                                  
012200        05 TAB1-KDKVAFG        PIC S9(1) COMP-3.                          
012300        05 TAB1-KVKVAFPO       PIC S9(3) COMP-3.                          
012310                                                                          
012400        05 TAB1-BEKVAFEL-S     PIC X(40).                                 
012500        05 TAB1-BEKVAFGR-S     PIC X(20).                                 
012600                                                                          
012800        05 TAB1-BEKVAFEL-NL    PIC X(40).                                 
012900        05 TAB1-BEKVAFGR-NL    PIC X(20).                                 
012901                                                                          
012910        05 TAB1-BEKVAFEL-GB    PIC X(40).                                 
012920        05 TAB1-BEKVAFGR-GB    PIC X(20).                                 
012930                                                                          
013000        05 TAB1-KVKVAANT-TRG   PIC S9(5) COMP-3.                          
013010        05 TAB1-KVKVAANT-OMR   PIC S9(5) COMP-3.                          
013100        05 TAB1-KVKVAANT-DC    PIC S9(5) COMP-3.                          
013200        05 TAB1-KVKVAANT-DC-KV PIC S9(5) COMP-3.                          
013300                                                                          
013400*    TABELL2                                                              
013500*    FÖRDELNING OCH ANTAL ARTIKLAR / REDOVISNINGSPERIOD                   
013600*    FÖR OMRÅDE OCH LAGER.                                                
013700                                                                          
013800 01  TABELL2.                                                             
013900     03 FILLER OCCURS 13.                                                 
014000        05 TAB2-TIAARP        PIC  9(4).                                  
014100        05 TAB2-REKVAREL-OMR  PIC S9(5)V9(2) COMP-3.                      
014200        05 TAB2-REKVAREL-DC   PIC S9(5)V9(2) COMP-3.                      
014210        05 TAB2-REKVAIND-BUF  PIC S9(5)V9(2) COMP-3.                      
014300        05 TAB2-KVART-TRG     PIC S9(9) COMP-3.                           
014310        05 TAB2-KVART-OMR     PIC S9(9) COMP-3.                           
014400        05 TAB2-KVART-DC      PIC S9(9) COMP-3.                           
014500                                                                          
014600*    TABELL3                                                              
014700*    FELPOÄNG PER REDOVISNINGSPERIOD / FELGRUPP                           
014800*    FÖR OMRÅDE OCH LAGER.                                                
014900                                                                          
015000 01  TABELL3.                                                             
015100     03 FILLER OCCURS 9.                                                  
015200        05 TAB3-KDKVAFG         PIC S9(1) COMP-3.                         
015300        05 FILLER OCCURS 13.                                              
015400           07 TAB3-TIAARP       PIC  9(4).                                
015500           07 TAB3-KVKVAFPO-OMR PIC S9(7) COMP-3.                         
015600           07 TAB3-KVKVAFPO-DC  PIC S9(7) COMP-3.                         
015700     EJECT                                                                
015800*    ---- UTFIL FELFÖRDELNING FELKOD                                      
015900*    -COPY W4260501        -PRE UT1-                                      
016100     EJECT                                                                
016200*    ---- UTFIL FELFÖRDELNING OMRÅDE                                      
016300*    -COPY W4260502        -PRE UT2-                                      
016500     EJECT                                                                
016600*    ---- UTFIL KVALITETSINDEX                                            
016700*    -COPY W4260503        -PRE UT3-                                      
016900     EJECT                                                                
017000*    ---- UTFIL RELATIV STORLEK                                           
017100*    -COPY W4260504        -PRE UT4-                                      
017300     EJECT                                                                
017400*    -COPY WWKVAOMR                                                       
017600     EJECT                                                                
017700*    ---- SUBPROGRAM OCH PARAMETER-AREOR                                  
017800     SKIP3                                                                
017900 01  DYNAMISKA-SUBPROGRAM.                                                
018000   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
018100   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
018200   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
018300   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
018400*    ----  PARAMETRAR TILL ABEND                                          
018500 01  RETURKODER.                                                          
018600   03  RKOD-ABEND-UTAN-DUMP  PIC S9(4) COMP SYNC VALUE +16.               
018700     EJECT                                                                
018800*    ----  PARAMETRAR TILL DATUMKORT                                      
018900                                                                          
019000 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
019100     SKIP3                                                                
019200*    -COPY WDATAREA                                                       
019400     EJECT                                                                
019500*    ---- POST-AREOR OCH IMS KOMMUNIKATIONS-AREOR                         
019600     SKIP3                                                                
019700*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
019800                                                                          
019900 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
020000     SKIP3                                                                
020100*    ---- STATUSKOD FRÅN IMS                                              
020200                                                                          
020300 01  STATUS-WS               PIC XX.                                      
020400     88  SEGMENT-FINNS                    VALUE '  '.                     
020500     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
020600     88  SEGMENT-SLUT                     VALUE 'GB'.                     
020700     SKIP3                                                                
020800 01  GODK-STATUSKODER.                                                    
020900   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
021000     SKIP3                                                                
021100 01  SSA1                    PIC X(100).                                  
021200     EJECT                                                                
021300*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
021400                                                                          
021500 01  FILLER                  PIC X(16) VALUE 'NYCKLAR-TILL-DLI'.          
021600 01  NYCKLAR-TILL-DLI.                                                    
021700                                                                          
021800   03  W-WDGX-4821-KEY-X.                                                 
021900     05  W-IDHTYP-4821      PIC X(4)   VALUE '4821'.                      
022000     05  FILLER             PIC X(26)  VALUE LOW-VALUE.                   
022100                                                                          
022200   03  W1-W6H601KY-X.                                                     
022300     05  W1-IDDC            PIC  X(2).                                    
022400     05  W1-IDKVAOMR        PIC  X(1).                                    
022500     05  W1-IDKVAGRP        PIC  9(3).                                    
022600     05  W1-DAREGDAT        PIC  9(8).                                    
022700     SKIP2                                                                
022800   03  W2-W6H6ASEQ-X.                                                     
022900     05  W2-IDDC            PIC  X(2).                                    
023000     05  W2-IDKVAOMR        PIC  X(1).                                    
023100     05  W2-IDKVAGRP        PIC  9(3).                                    
023200     05  W2-DAREGDAT        PIC  9(8).                                    
023300     05  W2-KDKVASTA-X.                                                   
023400       06 W2-KDKVASTA       PIC X(1).                                     
023500     SKIP2                                                                
023513   03  W-W6H501KY-MIN-X.                                                  
023514      05  W-IDDC-MIN         PIC  X(2)   VALUE LOW-VALUE.                 
023515      05  W-IDKVAOMR-MIN     PIC  X(1)   VALUE SPACE.                     
023516      05  W-IDKVATRG-MIN     PIC  9(2)   VALUE ZERO.                      
023517      05  W-IDKVAGRP-MIN     PIC  9(3)   VALUE ZERO.                      
023518      05  W-ADLAGOMR-FOM-MIN PIC S9(3)   VALUE +0    COMP-3.              
023519      05  W-ADGANG-FOM-MIN   PIC S9(3)   VALUE +0    COMP-3.              
023520      05  W-ADPLATS-FOM-MIN  PIC S9(5)   VALUE +0    COMP-3.              
023521     SKIP3                                                                
023522   03  W-W6H501KY-MAX-X.                                                  
023523      05  W-IDDC-MAX         PIC  X(2)   VALUE HIGH-VALUE.                
023524      05  W-IDKVAOMR-MAX     PIC  X(1)   VALUE HIGH-VALUE.                
023525      05  W-IDKVATRG-MAX     PIC  9(2)   VALUE 99.                        
023526      05  W-IDKVAGRP-MAX     PIC  9(3)   VALUE 999.                       
023527      05  W-ADLAGOMR-FOM-MAX PIC S9(3)   VALUE +999  COMP-3.              
023528      05  W-ADGANG-FOM-MAX   PIC S9(3)   VALUE +999  COMP-3.              
023529      05  W-ADPLATS-FOM-MAX  PIC S9(5)   VALUE +99999 COMP-3.             
023530     SKIP3                                                                
023532   03  W-IDKVAGRP-X.                                                      
023540     05  W-IDKVAGRP          PIC 9(3).                                    
023550     EJECT                                                                
023600*01  -COPY W0003                                                          
023800     EJECT                                                                
023900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA1'.              
024000     SKIP3                                                                
024100 01  DLI-IO-AREA1.                                                        
024200   03  IO-AREA1              PIC X(400).                                  
024300     SKIP3                                                                
024400*    03 WLXXJY11 -COPY WDGX4822       -RED IO-AREA1.                      
024600     EJECT                                                                
024700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA2'.              
024800 01  DLI-IO-AREA2.                                                        
024900   03  IO-AREA2              PIC X(27).                                   
025000     SKIP3                                                                
025100*    03 W6H601 -COPY W6H601         -RED IO-AREA2.                        
025300     EJECT                                                                
025400 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA3'.              
025500     SKIP3                                                                
025600 01  DLI-IO-AREA3.                                                        
025700   03  IO-AREA3              PIC X(8).                                    
025800*    03 W6H601 -COPY W6H611         -RED IO-AREA3.                        
026000     EJECT                                                                
026010 01  DLI-IO-AREA4.                                                        
026020   03  IO-AREA4              PIC X(24).                                   
026030*    03 W6H501 -COPY W6H501         -RED IO-AREA4.                        
026040     EJECT                                                                
026100 LINKAGE SECTION.                                                         
026200     SKIP2                                                                
026300*01  -COPY W0009      -PRE  MSG-                                          
026500     EJECT                                                                
026600*01  -COPY W0008      -PRE  W6H6-                                         
026800       05  FILLER                PIC X.                                   
026900     EJECT                                                                
027000*01  -COPY W0008      -PRE  W6H6A-                                        
027200       05  FILLER                PIC X.                                   
027300     EJECT                                                                
027400*01  -COPY W0008      -PRE  XXJY-                                         
027600       05  FILLER                PIC X.                                   
027700     EJECT                                                                
027710*01  -COPY W0008      -PRE  W6H5-                                         
027730       05  FILLER                PIC X.                                   
027740     EJECT                                                                
027800 PROCEDURE DIVISION  USING MSG-PCB W6H6A-PCB W6H6-PCB XXJY-PCB            
027810                                   W6H5-PCB.                              
027900     ENTRY 'DLITCBL' USING MSG-PCB W6H6A-PCB W6H6-PCB XXJY-PCB            
027910                                   W6H5-PCB.                              
028000                                                                          
028100     PERFORM A-INIT                                                       
028200                                                                          
028300     PERFORM B-FYLL-TABELL                                                
028400                                                                          
028500     MOVE LOW-VALUE       TO W2-W6H6ASEQ-X                                
028600     MOVE '3'             TO W2-KDKVASTA                                  
028700                                                                          
028800     PERFORM IMS-GN-W6KVAB01                                              
028900     IF NOT SEGMENT-SLUT                                                  
029020        MOVE OMR-IDDC     TO W-SPAR-IDDC     W-IDDC-MIN W-IDDC-MAX        
029030                             WS-IDDC                                      
029101        MOVE OMR-IDKVAOMR TO W-SPAR-IDKVAOMR                              
029102                             W-IDKVAOMR-MAX  W-IDKVAOMR-MIN               
029110        MOVE OMR-IDKVAGRP TO W-IDKVAGRP                                   
029111                                                                          
029120        PERFORM IMS-GU-W6KVAA01-MIN-MAX                                   
029121                                                                          
029122        IF SEGMENT-FINNS                                                  
029130          MOVE GRP-IDKVATRG TO W-SPAR-IDKVATRG                            
029140        ELSE                                                              
029160          MOVE ZERO         TO GRP-IDKVATRG  W-SPAR-IDKVATRG              
029170        END-IF                                                            
029200     END-IF                                                               
029210                                                                          
029300     PERFORM UNTIL W6KVAB01-SLUT                                          
029500       IF  OMR-IDDC     = W-SPAR-IDDC                                     
029600       AND OMR-IDKVAOMR = W-SPAR-IDKVAOMR                                 
029610         IF GRP-IDKVATRG NOT = W-SPAR-IDKVATRG                            
029620            PERFORM S01-SKRIV-FELFORDELNING-TORG                          
029621            PERFORM S02-NOLLSTALL-TORG                                    
029630         END-IF                                                           
029800       ELSE                                                               
029810         PERFORM S01-SKRIV-FELFORDELNING-TORG                             
029900         PERFORM D-SKRIV-FELFORDELNING                                    
030000         PERFORM E-SKRIV-FORDELNING-FELGRUPP                              
030100         PERFORM F-SKRIV-KVALITETSINDEX                                   
030200         PERFORM G-NOLLSTALL-TABELL                                       
030300       END-IF                                                             
030400                                                                          
030500       PERFORM C-REDOVISNINGS-PERIOD                                      
030600                                                                          
030700       PERFORM H-BERAKNA-ANTAL-PERIODER                                   
030800       IF W-ANT-PERIOD > +13                                              
030900          PERFORM I-RENSA-W6H6                                            
030910          CONTINUE                                                        
031000       ELSE                                                               
031100          PERFORM J-LAES-LAGERINDELNING                                   
031200          PERFORM K-ADD-ANT-KONTROLLERADE-ART                             
031300          PERFORM IMS-GNP-W6KVAB11                                        
031400          PERFORM UNTIL SEGMENT-SAKNAS                                    
031500             PERFORM L-BERAKNA-FELKOD                                     
031600             PERFORM IMS-GNP-W6KVAB11                                     
031700          END-PERFORM                                                     
031800       END-IF                                                             
031900       MOVE OMR-IDDC      TO W-SPAR-IDDC                                  
031910                             WS-IDDC                                      
032000       MOVE OMR-IDKVAOMR  TO W-SPAR-IDKVAOMR                              
032010       MOVE GRP-IDKVATRG  TO W-SPAR-IDKVATRG                              
032100                                                                          
032200       PERFORM IMS-GN-W6KVAB01                                            
032300                                                                          
032400       IF SEGMENT-SLUT                                                    
032410          PERFORM S01-SKRIV-FELFORDELNING-TORG                            
032500          PERFORM D-SKRIV-FELFORDELNING                                   
032600          PERFORM E-SKRIV-FORDELNING-FELGRUPP                             
032700          PERFORM F-SKRIV-KVALITETSINDEX                                  
032702          MOVE JA TO W6KVAB01-SW                                          
032710       ELSE                                                               
032740          MOVE OMR-IDDC     TO W-IDDC-MIN       W-IDDC-MAX                
032770          MOVE OMR-IDKVAOMR TO W-IDKVAOMR-MIN   W-IDKVAOMR-MAX            
032780          MOVE OMR-IDKVAGRP TO W-IDKVAGRP                                 
032790                                                                          
032791          PERFORM IMS-GU-W6KVAA01-MIN-MAX                                 
032800       END-IF                                                             
032900     END-PERFORM                                                          
033000                                                                          
033100     PERFORM Z-FINIT                                                      
033200                                                                          
033300     MOVE ZERO            TO RETURN-CODE                                  
033400     GOBACK                                                               
033500     .                                                                    
033600     EJECT                                                                
033700 A-INIT SECTION.                                                          
033800     SKIP2                                                                
033900     OPEN OUTPUT W4260501 W4260502 W4260503                               
034000                                                                          
034100     MOVE 'IDAG  '        TO DAT-KDDATFORM                                
034200     CALL WDATKONV     USING DAT-KDDATFORM                                
034300                             DAT-I-TIDATUM                                
034400                             DAT-O-TIDATUM                                
034500                             DAT-KDSVAR                                   
034600                                                                          
034700     MOVE DAT-TIAARP      TO W-DAT-TIAARP                                 
034710*    MOVE +9806           TO W-DAT-TIAARP                                 
034720***FIX IFALL KÖRNINGEN SKJUTS UPP                                         
034730                                                                          
034800     .                                                                    
034900     EJECT                                                                
035000 B-FYLL-TABELL SECTION.                                                   
035100     SKIP2                                                                
035200*    TABELL1                                                              
035300*         ANTAL FEL / FELKOD FÖR OMRÅDE OCH LAGER SENASTE                 
035400*         REDOVISNINGSPERIODEN. FÖR LAGER ÄVEN SENASTE TRE                
035500*         REDOVISNINGSPERIODEN SUMMERADE.                                 
035600*    TABELL2                                                              
035700*         FÖRDELNING OCH ANTAL ARTIKLAR / REDOVISNINGSPERIOD              
035800*         FÖR OMRÅDE OCH LAGER.                                           
035900*    TABELL3                                                              
036000*         FELPOÄNG PER REDOVISNINGSPERIOD / FELGRUPP                      
036100*         FÖR OMRÅDE OCH LAGER.                                           
036200                                                                          
036300     PERFORM IMS-GU-WLXXJY01                                              
036400                                                                          
036500     MOVE +0 TO IX1                                                       
036600                                                                          
036700     PERFORM IMS-GNP-WLXXJY11                                             
036800     PERFORM UNTIL SEGMENT-SAKNAS OR 4822-IDKVAFEL = 30                   
036900       ADD +1                  TO IX1                                     
037000       MOVE 4822-IDKVAFEL      TO TAB1-IDKVAFEL      (IX1)                
037100       MOVE 4822-KDKVAFG       TO TAB1-KDKVAFG       (IX1)                
037200       MOVE 4822-KVKVAFPO      TO TAB1-KVKVAFPO      (IX1)                
037300       MOVE 4822-BEKVAFEL (1)  TO TAB1-BEKVAFEL-S    (IX1)                
037400       MOVE 4822-BEKVAFGR (1)  TO TAB1-BEKVAFGR-S    (IX1)                
037500       MOVE 4822-BEKVAFEL (2)  TO TAB1-BEKVAFEL-NL   (IX1)                
037600       MOVE 4822-BEKVAFGR (2)  TO TAB1-BEKVAFGR-NL   (IX1)                
037610       MOVE 4822-BEKVAFEL (3)  TO TAB1-BEKVAFEL-GB   (IX1)                
037620       MOVE 4822-BEKVAFGR (3)  TO TAB1-BEKVAFGR-GB   (IX1)                
037621*  FÖRBERETT FÖR ÖVRIGA SDC:ER                                            
037700*      MOVE 4822-BEKVAFEL (4)  TO TAB1-BEKVAFEL-FR   (IX1)                
037800*      MOVE 4822-BEKVAFGR (4)  TO TAB1-BEKVAFGR-FR   (IX1)                
037810*      MOVE 4822-BEKVAFEL (5)  TO TAB1-BEKVAFEL-ES   (IX1)                
037820*      MOVE 4822-BEKVAFGR (5)  TO TAB1-BEKVAFGR-ES   (IX1)                
037830*      MOVE 4822-BEKVAFEL (6)  TO TAB1-BEKVAFEL-IT   (IX1)                
037840*      MOVE 4822-BEKVAFGR (6)  TO TAB1-BEKVAFGR-IT   (IX1)                
037850                                                                          
037900       MOVE  ZERO              TO TAB1-KVKVAANT-TRG  (IX1)                
037910                                  TAB1-KVKVAANT-OMR  (IX1)                
038000                                  TAB1-KVKVAANT-DC   (IX1)                
038100                                  TAB1-KVKVAANT-DC-KV(IX1)                
038300       PERFORM IMS-GNP-WLXXJY11                                           
038400     END-PERFORM                                                          
038500                                                                          
038600     PERFORM UNTIL IX1 = +99                                              
038700       ADD +1                  TO IX1                                     
038800       MOVE +99                TO TAB1-IDKVAFEL      (IX1)                
038900     END-PERFORM                                                          
039000     EJECT                                                                
039010                                                                          
039100     MOVE +13                  TO IX1                                     
039200     MOVE W-DAT-TIAARP         TO W-TIAARP                                
039300     PERFORM UNTIL IX1 = +0                                               
039400       MOVE  ZERO              TO TAB2-REKVAREL-OMR  (IX1)                
039500                                  TAB2-REKVAREL-DC   (IX1)                
039510                                  TAB2-REKVAIND-BUF  (IX1)                
039600                                  TAB2-KVART-TRG     (IX1)                
039610                                  TAB2-KVART-OMR     (IX1)                
039700                                  TAB2-KVART-DC      (IX1)                
039900       IF W-TIAARP-RP = ZERO                                              
040000          IF W-TIAARP-AA = ZERO                                           
040100             MOVE 99           TO W-TIAARP-AA                             
040200          ELSE                                                            
040300             SUBTRACT 1      FROM W-TIAARP-AA                             
040400          END-IF                                                          
040500          MOVE 12              TO W-TIAARP-RP                             
040600       END-IF                                                             
040700       MOVE W-TIAARP           TO TAB2-TIAARP (IX1)                       
040800       SUBTRACT 1            FROM W-TIAARP-RP, IX1                        
041000     END-PERFORM                                                          
041100                                                                          
041300     MOVE +1                    TO W-KDKVAFG, IX1                         
041500     PERFORM UNTIL IX1 > +9                                               
041800       MOVE W-KDKVAFG           TO TAB3-KDKVAFG (IX1)                     
041900       MOVE +1                  TO IX2                                    
042000       PERFORM UNTIL IX2 > +13                                            
042200         MOVE TAB2-TIAARP (IX2) TO TAB3-TIAARP       (IX1 IX2)            
042300         MOVE ZERO              TO TAB3-KVKVAFPO-OMR (IX1 IX2)            
042400                                   TAB3-KVKVAFPO-DC  (IX1 IX2)            
042410         ADD +1                 TO IX2                                    
042500       END-PERFORM                                                        
042520       ADD  +1                  TO W-KDKVAFG, IX1                         
042600     END-PERFORM                                                          
042700     .                                                                    
042800     EJECT                                                                
042900 C-REDOVISNINGS-PERIOD SECTION.                                           
043000     SKIP2                                                                
043100     MOVE OMR-TIKVAKON TO DAT-I-TIDATUM                                   
043200                                                                          
043300     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
043400     CALL WDATKONV  USING DAT-KDDATFORM                                   
043500                          DAT-I-TIDATUM                                   
043600                          DAT-O-TIDATUM                                   
043700                          DAT-KDSVAR                                      
043800                                                                          
043900     MOVE DAT-TIAARP   TO W-OMR-TIAARP                                    
044000     .                                                                    
044100     EJECT                                                                
044200 D-SKRIV-FELFORDELNING SECTION.                                           
044300     SKIP2                                                                
044400     PERFORM DA-SKRIV-FELFORDEL-OMRADE                                    
044500                                                                          
044600     PERFORM DB-SKRIV-FELFORDEL-DC                                        
044700                                                                          
044800     PERFORM DC-SKRIV-FELFORDEL-DC-KVARTAL                                
044900     .                                                                    
045000     EJECT                                                                
045100 DA-SKRIV-FELFORDEL-OMRADE SECTION.                                       
045200     SKIP2                                                                
045300     MOVE NEJ                       TO W-SKRIV-UTFIL                      
045400     MOVE +1                        TO IX1                                
045500     PERFORM UNTIL TAB1-IDKVAFEL (IX1) = +99                              
045700       MOVE 'RAD'                   TO UT1-IDPTYP                         
045800       MOVE W-SPAR-IDDC             TO UT1-IDDC                           
045900       MOVE W-SPAR-IDKVAOMR         TO UT1-IDKVAOMR                       
045910       MOVE W-SPAR-IDKVATRG         TO UT1-IDKVATRG                       
046300       MOVE W-OMR-BEKVAOMR-DC       TO UT1-BEKVAOMR  (1)                  
046500       MOVE W-OMR-BEKVAOMR-GB       TO UT1-BEKVAOMR  (2)                  
046600       MOVE TAB2-TIAARP       (13)  TO UT1-TIAARP-FOM                     
046700                                       UT1-TIAARP-TOM                     
046800       MOVE TAB1-IDKVAFEL     (IX1) TO UT1-IDKVAFEL                       
046900       EVALUATE  TRUE                                                     
046910         WHEN CDC-SE                                                      
047000           MOVE TAB1-BEKVAFEL-S(IX1) TO UT1-BEKVAFEL (1)                  
047100           MOVE TAB1-BEKVAFGR-S(IX1) TO UT1-BEKVAFGR (1)                  
047200         WHEN SDC-NL                                                      
047300           MOVE TAB1-BEKVAFEL-NL(IX1) TO UT1-BEKVAFEL (1)                 
047400           MOVE TAB1-BEKVAFGR-NL(IX1) TO UT1-BEKVAFGR (1)                 
047500       END-EVALUATE                                                       
047600       MOVE TAB1-BEKVAFEL-GB  (IX1) TO UT1-BEKVAFEL  (2)                  
047700       MOVE TAB1-BEKVAFGR-GB  (IX1) TO UT1-BEKVAFGR  (2)                  
047800       MOVE TAB1-KVKVAANT-OMR (IX1) TO UT1-KVKVAFEL                       
047900       MOVE TAB1-KVKVAFPO     (IX1) TO UT1-KVKVAFPO                       
048000                                                                          
048100       COMPUTE UT1-REKVAFEL ROUNDED =                                     
048200           (TAB1-KVKVAANT-OMR (IX1) * 100) / W-TOT-KVKVAFEL-OMR           
048300           ON SIZE ERROR MOVE +0    TO UT1-REKVAFEL                       
048400       END-COMPUTE                                                        
048500                                                                          
048600       COMPUTE UT1-REKVAFKA ROUNDED =                                     
048700           (TAB1-KVKVAANT-OMR (IX1) * 100) / TAB2-KVART-OMR (13)          
048800           ON SIZE ERROR MOVE +0    TO UT1-REKVAFKA                       
048900       END-COMPUTE                                                        
049000                                                                          
049100       IF UT1-KVKVAFEL > +0                                               
049200          MOVE JA                   TO W-SKRIV-UTFIL                      
049300          WRITE W4260501-POST FROM UT1-W4260501                           
049400       END-IF                                                             
049500                                                                          
049600       ADD +1                       TO IX1                                
049700     END-PERFORM                                                          
049800                                                                          
049900     IF W-SKRIV-UTFIL = JA                                                
050000        MOVE 'TOT'                  TO UT2-IDPTYP                         
050100        MOVE W-SPAR-IDDC            TO UT2-IDDC                           
050200        MOVE W-SPAR-IDKVAOMR        TO UT2-IDKVAOMR                       
050300        MOVE TAB2-TIAARP    (13)    TO UT2-TIAARP-FOM                     
050400                                       UT2-TIAARP-TOM                     
050500        MOVE W-TOT-KVKVAFEL-OMR     TO UT2-KVKVAFEL                       
050600        MOVE W-TOT-KVARTFEL-OMR     TO UT2-KVARTFEL                       
050700        MOVE TAB2-KVART-OMR (13)    TO UT2-KVART                          
050800                                                                          
050900        WRITE W4260501-POST       FROM UT2-W4260502                       
051000     END-IF                                                               
051100     .                                                                    
051200     EJECT                                                                
051300 DB-SKRIV-FELFORDEL-DC SECTION.                                           
051400                                                                          
051500     MOVE NEJ                       TO W-SKRIV-UTFIL                      
051600     IF SEGMENT-SLUT                                                      
051700     OR ( OMR-IDDC NOT = W-SPAR-IDDC  )                                   
051800        MOVE +1                     TO IX1                                
051900        PERFORM UNTIL TAB1-IDKVAFEL (IX1) = +99                           
052000                                                                          
052100          MOVE 'RAD'                TO UT1-IDPTYP                         
052200          MOVE W-SPAR-IDDC          TO UT1-IDDC                           
052300          MOVE SPACE                TO UT1-IDKVAOMR                       
052400                                       UT1-BEKVAOMR (1)                   
052500                                       UT1-BEKVAOMR (2)                   
052510          MOVE ZERO                 TO UT1-IDKVATRG                       
052600          MOVE TAB2-TIAARP (13)     TO UT1-TIAARP-FOM                     
052700                                       UT1-TIAARP-TOM                     
052800          MOVE TAB1-IDKVAFEL  (IX1) TO UT1-IDKVAFEL                       
052900          IF CDC-SE                                                       
053000             MOVE TAB1-BEKVAFEL-S(IX1) TO UT1-BEKVAFEL (1)                
053100             MOVE TAB1-BEKVAFGR-S(IX1) TO UT1-BEKVAFGR (1)                
053200          ELSE                                                            
053300             MOVE TAB1-BEKVAFEL-NL(IX1) TO UT1-BEKVAFEL (1)               
053400             MOVE TAB1-BEKVAFGR-NL(IX1) TO UT1-BEKVAFGR (1)               
053500          END-IF                                                          
053600          MOVE TAB1-BEKVAFEL-GB(IX1) TO UT1-BEKVAFEL (2)                  
053700          MOVE TAB1-BEKVAFGR-GB(IX1) TO UT1-BEKVAFGR (2)                  
053800          MOVE TAB1-KVKVAANT-DC(IX1) TO UT1-KVKVAFEL                      
053900          MOVE TAB1-KVKVAFPO  (IX1) TO UT1-KVKVAFPO                       
054000                                                                          
054100          COMPUTE UT1-REKVAFEL ROUNDED =                                  
054200              (TAB1-KVKVAANT-DC (IX1) * 100) / W-TOT-KVKVAFEL-DC          
054300              ON SIZE ERROR MOVE +0 TO UT1-REKVAFEL                       
054400          END-COMPUTE                                                     
054500                                                                          
054600          COMPUTE UT1-REKVAFKA ROUNDED =                                  
054700              (TAB1-KVKVAANT-DC (IX1) * 100) / TAB2-KVART-DC (13)         
054800              ON SIZE ERROR MOVE +0 TO UT1-REKVAFKA                       
054900          END-COMPUTE                                                     
055000                                                                          
055100          IF UT1-KVKVAFEL > +0                                            
055200             MOVE JA                TO W-SKRIV-UTFIL                      
055300             WRITE W4260501-POST  FROM UT1-W4260501                       
055400          END-IF                                                          
055500                                                                          
055600          ADD +1                    TO IX1                                
055700        END-PERFORM                                                       
055800                                                                          
055900        IF W-SKRIV-UTFIL = JA                                             
056000          MOVE 'TOT'                TO UT2-IDPTYP                         
056100          MOVE W-SPAR-IDDC          TO UT2-IDDC                           
056200          MOVE SPACE                TO UT2-IDKVAOMR                       
056300          MOVE TAB2-TIAARP (13)     TO UT2-TIAARP-FOM                     
056400                                       UT2-TIAARP-TOM                     
056500          MOVE W-TOT-KVKVAFEL-DC    TO UT2-KVKVAFEL                       
056600          MOVE W-TOT-KVARTFEL-DC    TO UT2-KVARTFEL                       
056700          MOVE TAB2-KVART-DC(13)    TO UT2-KVART                          
056800                                                                          
056900          WRITE W4260501-POST     FROM UT2-W4260502                       
057000        END-IF                                                            
057100     END-IF                                                               
057200     .                                                                    
057300     EJECT                                                                
057400 DC-SKRIV-FELFORDEL-DC-KVARTAL SECTION.                                   
057500     SKIP2                                                                
057600     MOVE NEJ                       TO W-SKRIV-UTFIL                      
057700     IF SEGMENT-SLUT                                                      
057800     OR ( OMR-IDDC  NOT = W-SPAR-IDDC )                                   
057900        MOVE +1                     TO IX1                                
058000        PERFORM UNTIL TAB1-IDKVAFEL (IX1) = +99                           
058100                                                                          
058200          MOVE 'RAD'                TO UT1-IDPTYP                         
058300          MOVE W-SPAR-IDDC          TO UT1-IDDC                           
058400          MOVE SPACE                TO UT1-IDKVAOMR                       
058500                                       UT1-BEKVAOMR (1)                   
058600                                       UT1-BEKVAOMR (2)                   
058610          MOVE ZERO                 TO UT1-IDKVATRG                       
058700          MOVE TAB2-TIAARP (11)     TO UT1-TIAARP-FOM                     
058800          MOVE TAB2-TIAARP (13)     TO UT1-TIAARP-TOM                     
058900          MOVE TAB1-IDKVAFEL  (IX1) TO UT1-IDKVAFEL                       
059000          IF CDC-SE                                                       
059100             MOVE TAB1-BEKVAFEL-S(IX1) TO UT1-BEKVAFEL (1)                
059200             MOVE TAB1-BEKVAFGR-S(IX1) TO UT1-BEKVAFGR (1)                
059300          ELSE                                                            
059400             MOVE TAB1-BEKVAFEL-NL(IX1) TO UT1-BEKVAFEL (1)               
059500             MOVE TAB1-BEKVAFGR-NL(IX1) TO UT1-BEKVAFGR (1)               
059600          END-IF                                                          
059700          MOVE TAB1-BEKVAFEL-GB(IX1) TO UT1-BEKVAFEL (2)                  
059800          MOVE TAB1-BEKVAFGR-GB(IX1) TO UT1-BEKVAFGR (2)                  
059900          MOVE TAB1-KVKVAANT-DC-KV(IX1) TO UT1-KVKVAFEL                   
060000          MOVE TAB1-KVKVAFPO  (IX1) TO UT1-KVKVAFPO                       
060100                                                                          
060200          COMPUTE UT1-REKVAFEL ROUNDED =                                  
060300          (TAB1-KVKVAANT-DC-KV (IX1) * 100) / W-TOT-KVKVAFEL-DC-KV        
060400           ON SIZE ERROR MOVE +0    TO UT1-REKVAFEL                       
060500          END-COMPUTE                                                     
060600                                                                          
060700          COMPUTE W-TOT-KVART-DC-KV = TAB2-KVART-DC (11) +                
060800                  TAB2-KVART-DC (12) + TAB2-KVART-DC (13)                 
060900          END-COMPUTE                                                     
061000                                                                          
061100          COMPUTE UT1-REKVAFKA ROUNDED =                                  
061200          (TAB1-KVKVAANT-DC-KV (IX1) * 100) / W-TOT-KVART-DC-KV           
061300           ON SIZE ERROR MOVE +0    TO UT1-REKVAFKA                       
061400          END-COMPUTE                                                     
061500                                                                          
061600          IF UT1-KVKVAFEL > +0                                            
061700             MOVE JA                TO W-SKRIV-UTFIL                      
061800             WRITE W4260501-POST  FROM UT1-W4260501                       
061900          END-IF                                                          
062000                                                                          
062100          ADD +1                    TO IX1                                
062200        END-PERFORM                                                       
062300                                                                          
062400        IF W-SKRIV-UTFIL = JA                                             
062500          MOVE 'TOT'                TO UT2-IDPTYP                         
062600          MOVE W-SPAR-IDDC          TO UT2-IDDC                           
062700          MOVE SPACE                TO UT2-IDKVAOMR                       
062800          MOVE TAB2-TIAARP (11)     TO UT2-TIAARP-FOM                     
062900          MOVE TAB2-TIAARP (13)     TO UT2-TIAARP-TOM                     
063000          MOVE W-TOT-KVKVAFEL-DC-KV TO UT2-KVKVAFEL                       
063100          MOVE W-TOT-KVARTFEL-DC-KV TO UT2-KVARTFEL                       
063200          MOVE W-TOT-KVART-DC-KV    TO UT2-KVART                          
063300                                                                          
063400          WRITE W4260501-POST     FROM UT2-W4260502                       
063500        END-IF                                                            
063600     END-IF                                                               
063700     .                                                                    
063800     EJECT                                                                
063900 E-SKRIV-FORDELNING-FELGRUPP SECTION.                                     
064000     SKIP2                                                                
064100     MOVE +0                     TO IX1                                   
064200     PERFORM UNTIL IX1 = +9                                               
064300       ADD +1                    TO IX1                                   
064400       MOVE 'GRP'                TO UT4-IDPTYP                            
064500       MOVE W-SPAR-IDDC          TO UT4-IDDC                              
064600       MOVE W-SPAR-IDKVAOMR      TO UT4-IDKVAOMR                          
064800       MOVE W-OMR-BEKVAOMR-DC    TO UT4-BEKVAOMR (1)                      
065200       MOVE W-OMR-BEKVAOMR-GB    TO UT4-BEKVAOMR (2)                      
065300       MOVE TAB3-KDKVAFG  (IX1)  TO UT4-KDKVAFG                           
065400       MOVE NEJ                  TO W-SKRIV-UTFIL                         
065500       MOVE +0                   TO IX2                                   
065600       PERFORM UNTIL IX2 = +13                                            
065700         ADD +1                  TO IX2                                   
065800         MOVE TAB3-TIAARP (IX1 IX2) TO UT4-TIAARP (IX2)                   
065900         IF TAB3-KVKVAFPO-OMR (IX1 IX2) > +0                              
065901           COMPUTE W-SPARVARDE ROUNDED =                                  
065910          (TAB3-KVKVAFPO-OMR (IX1 IX2) / TAB2-KVART-OMR (IX2))            
066000           COMPUTE UT4-REKVAREL(IX2) ROUNDED = W-OMR-REKVAOMR *           
066100                                        W-SPARVARDE                       
066101           END-COMPUTE                                                    
066102                                                                          
066110           COMPUTE WS-REKVAREL-OMR  ROUNDED = W-OMR-REKVAOMR *            
066120                                        W-SPARVARDE                       
066200           END-COMPUTE                                                    
066300                                                                          
066500            ADD WS-REKVAREL-OMR   TO TAB2-REKVAREL-OMR (IX2)              
066580                                                                          
066600            COMPUTE TAB3-KVKVAFPO-DC (IX1 IX2) ROUNDED =                  
066700                    TAB3-KVKVAFPO-DC (IX1 IX2) +                          
066800                 (TAB3-KVKVAFPO-OMR (IX1 IX2) * W-OMR-REKVAOMR )          
066900            END-COMPUTE                                                   
067000            MOVE JA              TO W-SKRIV-UTFIL                         
067100         ELSE                                                             
067200            MOVE +0              TO UT4-REKVAREL (IX2)                    
067300         END-IF                                                           
067400       END-PERFORM                                                        
067500       IF W-SKRIV-UTFIL = JA                                              
067600          WRITE W4260504-POST  FROM UT4-W4260504                          
067700       END-IF                                                             
067800     END-PERFORM                                                          
067900                                                                          
068000     MOVE 'OMR'                  TO UT4-IDPTYP                            
068100     MOVE +0                     TO UT4-KDKVAFG , IX1                     
068200     MOVE NEJ                    TO W-SKRIV-UTFIL                         
068400     PERFORM UNTIL IX1 = +13                                              
068500       ADD +1                    TO IX1                                   
068600       IF TAB2-REKVAREL-OMR (IX1) > +0                                    
068700          MOVE TAB2-REKVAREL-OMR (IX1) TO UT4-REKVAREL (IX1)              
068800          MOVE JA                TO W-SKRIV-UTFIL                         
068900       END-IF                                                             
069000     END-PERFORM                                                          
069100     IF W-SKRIV-UTFIL = JA                                                
069200        WRITE W4260504-POST    FROM UT4-W4260504                          
069300     END-IF                                                               
069400                                                                          
069500                                                                          
069600     IF SEGMENT-SLUT                                                      
069700     OR ( OMR-IDDC  NOT = W-SPAR-IDDC )                                   
069800        MOVE +0                  TO IX1                                   
069900        PERFORM UNTIL IX1 = +9                                            
070000          ADD +1                 TO IX1                                   
070100          MOVE 'TOT'             TO UT4-IDPTYP                            
070200          MOVE SPACE             TO UT4-BEKVAOMR (1)                      
070300                                    UT4-BEKVAOMR (2)                      
070400          MOVE W-SPAR-IDDC       TO UT4-IDDC                              
070500          MOVE W-SPAR-IDKVAOMR   TO UT4-IDKVAOMR                          
070600          MOVE TAB3-KDKVAFG (IX1) TO UT4-KDKVAFG                          
070700          MOVE NEJ               TO W-SKRIV-UTFIL                         
070800          MOVE +0                TO IX2                                   
070900          PERFORM UNTIL IX2 = +13                                         
071000            ADD +1               TO IX2                                   
071100            IF TAB3-KVKVAFPO-DC (IX1 IX2) > +0                            
071200               MOVE TAB3-TIAARP (IX1 IX2) TO UT4-TIAARP (IX2)             
071300               COMPUTE UT4-REKVAREL (IX2) ROUNDED =                       
071400                  TAB3-KVKVAFPO-DC (IX1 IX2) / TAB2-KVART-DC (IX2)        
071500               END-COMPUTE                                                
071600                                                                          
071700               ADD UT4-REKVAREL (IX2) TO TAB2-REKVAREL-DC (IX2)           
071800                                                                          
071900               MOVE JA           TO W-SKRIV-UTFIL                         
072000            ELSE                                                          
072100               MOVE +0           TO UT4-REKVAREL (IX2)                    
072200            END-IF                                                        
072300          END-PERFORM                                                     
072400          IF W-SKRIV-UTFIL = JA                                           
072500             WRITE W4260504-POST FROM UT4-W4260504                        
072600          END-IF                                                          
072700        END-PERFORM                                                       
072800                                                                          
072900        MOVE 'DC '               TO UT4-IDPTYP                            
073000        MOVE +0                  TO UT4-KDKVAFG    IX1                    
073100        MOVE NEJ                 TO W-SKRIV-UTFIL                         
073300        PERFORM UNTIL IX1 = +13                                           
073400          ADD +1                 TO IX1                                   
073500          IF TAB2-REKVAREL-DC (IX1) > +0                                  
073600             MOVE TAB2-REKVAREL-DC (IX1)  TO UT4-REKVAREL (IX1)           
073700             MOVE JA             TO W-SKRIV-UTFIL                         
073800          END-IF                                                          
073900        END-PERFORM                                                       
074000        IF W-SKRIV-UTFIL = JA                                             
074100           WRITE W4260504-POST FROM UT4-W4260504                          
074200        END-IF                                                            
074300     END-IF                                                               
074400     .                                                                    
074500     EJECT                                                                
074600 F-SKRIV-KVALITETSINDEX SECTION.                                          
074700     SKIP2                                                                
074800     MOVE 'OMR'                  TO UT3-IDPTYP                            
074900     MOVE W-SPAR-IDDC            TO UT3-IDDC                              
075000     MOVE W-SPAR-IDKVAOMR        TO UT3-IDKVAOMR                          
075001                                                                          
075010     EVALUATE W-SPAR-IDKVAOMR                                             
075020     WHEN 'A'                                                             
075030       MOVE +0.3 TO W-PROCSATS                                            
075040     WHEN 'B'                                                             
075041       MOVE +0.3 TO W-PROCSATS                                            
075042     WHEN 'C'                                                             
075043       MOVE +0.1 TO W-PROCSATS                                            
075044     WHEN 'D'                                                             
075045       MOVE +0.1 TO W-PROCSATS                                            
075046     WHEN 'E'                                                             
075047       MOVE +0.1 TO W-PROCSATS                                            
075048     WHEN 'F'                                                             
075049       MOVE +0.1 TO W-PROCSATS                                            
075050     END-EVALUATE                                                         
075060                                                                          
075400     MOVE W-OMR-BEKVAOMR-DC      TO UT3-BEKVAOMR (1)                      
075600     MOVE W-OMR-BEKVAOMR-GB      TO UT3-BEKVAOMR (2)                      
075700     MOVE +0                     TO IX1                                   
075800     MOVE NEJ                    TO W-SKRIV-UTFIL                         
075900     PERFORM UNTIL IX1 = +13                                              
076000       ADD +1                    TO IX1                                   
076100                                                                          
076200       MOVE TAB2-TIAARP  (IX1)   TO UT3-TIAARP (IX1)                      
076300                                                                          
076400       IF TAB2-REKVAREL-DC  (IX1) > +0                                    
076600         MOVE +1000          TO UT3-REKVAIND (IX1)                        
076601                                                                          
076700       ELSE                                                               
076800         MOVE +0             TO UT3-REKVAIND (IX1)                        
076802         MOVE JA             TO W-SKRIV-UTFIL                             
076803         COMPUTE UT3-REKVAIND (IX1) ROUNDED =                             
076804                 1000 - TAB2-REKVAREL-OMR (IX1)                           
076805         END-COMPUTE                                                      
076820       END-IF                                                             
077511       COMPUTE W-REKVAIND-BUF = UT3-REKVAIND (IX1) * W-PROCSATS           
077520       ADD W-REKVAIND-BUF TO TAB2-REKVAIND-BUF (IX1) ROUNDED              
077600     END-PERFORM                                                          
077700                                                                          
077800     IF W-SKRIV-UTFIL = JA                                                
077900        WRITE W4260503-POST FROM UT3-W4260503                             
078000     END-IF                                                               
078100     EJECT                                                                
078200     IF SEGMENT-SLUT                                                      
078300     OR ( OMR-IDDC NOT = W-SPAR-IDDC )                                    
078400        MOVE 'BUF'               TO UT3-IDPTYP                            
078500        MOVE W-SPAR-IDDC         TO UT3-IDDC                              
078600        MOVE SPACE               TO UT3-IDKVAOMR                          
078700        MOVE 'BUFFERT'           TO UT3-BEKVAOMR (1)                      
078800                                    UT3-BEKVAOMR (2)                      
078900        MOVE +0  TO IX1                                                   
079000        PERFORM UNTIL IX1 = +13                                           
079100          ADD +1 TO IX1                                                   
079200                                                                          
079300          MOVE TAB2-TIAARP (IX1) TO UT3-TIAARP  (IX1)                     
079500          MOVE TAB2-REKVAIND-BUF (IX1) TO UT3-REKVAIND (IX1)              
079600                                                                          
080200        END-PERFORM                                                       
080400        WRITE W4260503-POST    FROM UT3-W4260503                          
080401                                                                          
080410        MOVE 'DC '               TO UT3-IDPTYP                            
080420        MOVE W-SPAR-IDDC         TO UT3-IDDC                              
080430        MOVE SPACE               TO UT3-IDKVAOMR                          
080440                                    UT3-BEKVAOMR (1)                      
080450                                    UT3-BEKVAOMR (2)                      
080460        MOVE +0  TO IX1                                                   
080470        PERFORM UNTIL IX1 = +13                                           
080480          ADD +1 TO IX1                                                   
080490                                                                          
080491          MOVE TAB2-TIAARP (IX1) TO UT3-TIAARP  (IX1)                     
080492                                                                          
080493          IF TAB2-REKVAREL-DC (IX1) > +0                                  
080494             COMPUTE UT3-REKVAIND (IX1) =                                 
080495                     1000 - TAB2-REKVAREL-DC (IX1)                        
080496             END-COMPUTE                                                  
080497          ELSE                                                            
080498             MOVE +0             TO UT3-REKVAIND (IX1)                    
080499          END-IF                                                          
080500        END-PERFORM                                                       
080501                                                                          
080502        WRITE W4260503-POST    FROM UT3-W4260503                          
080510     END-IF                                                               
080600     .                                                                    
080700     EJECT                                                                
080800 G-NOLLSTALL-TABELL SECTION.                                              
080900     SKIP2                                                                
081000     MOVE +0 TO IX1                                                       
081100     PERFORM UNTIL IX1 = +99                                              
081200       ADD +1                    TO IX1                                   
081300       MOVE +0                   TO TAB1-KVKVAANT-OMR   (IX1)             
081310                                    TAB1-KVKVAANT-TRG   (IX1)             
081400       IF OMR-IDDC NOT = W-SPAR-IDDC                                      
081500          MOVE +0                TO TAB1-KVKVAANT-DC    (IX1)             
081600                                    TAB1-KVKVAANT-DC-KV (IX1)             
081700       END-IF                                                             
081800     END-PERFORM                                                          
081900                                                                          
082000                                                                          
082100     MOVE +0                     TO IX1                                   
082200     PERFORM UNTIL IX1 = +13                                              
082300       ADD +1                    TO IX1                                   
082400       MOVE +0                   TO TAB2-REKVAREL-OMR (IX1)               
082510                                    TAB2-KVART-OMR    (IX1)               
082520                                    TAB2-KVART-TRG    (IX1)               
082600       IF OMR-IDDC  NOT = W-SPAR-IDDC                                     
082700          MOVE +0                TO TAB2-REKVAREL-DC  (IX1)               
082800                                    TAB2-KVART-DC     (IX1)               
082900       END-IF                                                             
083000     END-PERFORM                                                          
083100                                                                          
083200     MOVE +0 TO IX1                                                       
083300     PERFORM UNTIL IX1 = +9                                               
083400       ADD +1                    TO IX1                                   
083500       MOVE +0                   TO IX2                                   
083600       PERFORM UNTIL IX2 = +13                                            
083700         ADD +1                  TO IX2                                   
083800         MOVE +0                 TO TAB3-KVKVAFPO-OMR (IX1 IX2)           
083900         IF OMR-IDDC NOT = W-SPAR-IDDC                                    
084000            MOVE +0              TO TAB3-KVKVAFPO-DC  (IX1 IX2)           
084100         END-IF                                                           
084200       END-PERFORM                                                        
084300     END-PERFORM                                                          
084400                                                                          
084500     IF OMR-IDDC NOT = W-SPAR-IDDC                                        
084600        MOVE +0                  TO W-TOT-KVARTFEL-DC                     
084700                                    W-TOT-KVARTFEL-DC-KV                  
084800                                    W-TOT-KVKVAFEL-DC                     
084900                                    W-TOT-KVKVAFEL-DC-KV                  
085000     END-IF                                                               
085100                                                                          
085200     MOVE +0                     TO W-TOT-KVARTFEL-OMR                    
085210                                    W-TOT-KVARTFEL-TRG                    
085300                                    W-TOT-KVKVAFEL-OMR                    
085310                                    W-TOT-KVKVAFEL-TRG                    
085400     .                                                                    
085500     EJECT                                                                
085600 H-BERAKNA-ANTAL-PERIODER SECTION.                                        
085700     SKIP2                                                                
085800     IF W-OMR-TIAARP-AA = W-DAT-TIAARP-AA                                 
085900        COMPUTE W-ANT-PERIOD =                                            
086000                W-DAT-TIAARP-RP - W-OMR-TIAARP-RP + 1                     
086100        END-COMPUTE                                                       
086200     ELSE                                                                 
086300        COMPUTE W-ANT-PERIOD =                                            
086400                W-DAT-TIAARP-RP + (13 - W-OMR-TIAARP-RP)                  
086500        END-COMPUTE                                                       
086600     END-IF                                                               
086700     .                                                                    
086800     EJECT                                                                
086900 I-RENSA-W6H6 SECTION.                                                    
087000     SKIP2                                                                
087100     MOVE OMR-IDDC     TO W1-IDDC                                         
087200     MOVE OMR-IDKVAOMR TO W1-IDKVAOMR                                     
087300     MOVE OMR-IDKVAGRP TO W1-IDKVAGRP                                     
087400     MOVE OMR-DAREGDAT TO W1-DAREGDAT                                     
087500                                                                          
087600     PERFORM IMS-GHU-W6KVAB01                                             
087700                                                                          
087710** OM KÖRNINGEN BEHÖVER KÖRAS OM *-MÄRK DELETEN                           
087720*FIX                                                                      
087800     IF SEGMENT-FINNS                                                     
087900        PERFORM IMS-DLET-W6KVAB01                                         
088000     END-IF                                                               
088100     .                                                                    
088200     EJECT                                                                
088300 J-LAES-LAGERINDELNING SECTION.                                           
088400     SKIP2                                                                
088410     EVALUATE TRUE                                                        
088411       WHEN CDC-SE  MOVE +1   TO DCIX                                     
088412       WHEN SDC-NL  MOVE +2   TO DCIX                                     
088413*      WHEN '22'    MOVE +4   TO DCIX                                     
088414       WHEN SDC-GB  MOVE +3   TO DCIX                                     
088415       WHEN SDC-ES  MOVE +5   TO DCIX                                     
088416       WHEN SDC-IT  MOVE +6   TO DCIX                                     
088417       WHEN SDC-AT  MOVE +7   TO DCIX                                     
088418       WHEN OTHER MOVE +3   TO DCIX                                       
088420     END-EVALUATE                                                         
088430                                                                          
088500     MOVE +1   TO  IX1                                                    
088610     PERFORM UNTIL IX1 > +7                                               
088700       IF KVA-IDKVAOMR (DCIX IX1) = OMR-IDKVAOMR                          
088900         MOVE KVA-BEKVAOMR-DC (DCIX IX1)  TO W-OMR-BEKVAOMR-DC            
089000         MOVE KVA-BEKVAOMR-GB (  3  IX1)  TO W-OMR-BEKVAOMR-GB            
089100         MOVE KVA-REKVAOMR    (DCIX IX1)  TO W-OMR-REKVAOMR               
089300       END-IF                                                             
089400       ADD +1      TO IX1                                                 
089510     END-PERFORM                                                          
089600     .                                                                    
089700     EJECT                                                                
089800 K-ADD-ANT-KONTROLLERADE-ART SECTION.                                     
089900     SKIP2                                                                
090000     MOVE +0                      TO IX1                                  
090100     PERFORM UNTIL IX1 = +13                                              
090200       ADD +1                     TO IX1                                  
090300       IF W-OMR-TIAARP = TAB2-TIAARP (IX1)                                
090400          ADD OMR-KVART   TO TAB2-KVART-TRG (IX1)                         
090410                             TAB2-KVART-OMR (IX1)                         
090500                             TAB2-KVART-DC (IX1)                          
090600       END-IF                                                             
090700     END-PERFORM                                                          
090800     .                                                                    
090900     EJECT                                                                
091000 L-BERAKNA-FELKOD SECTION.                                                
091100     SKIP2                                                                
091200     MOVE +0                TO W-KVKVAFPO W-KDKVAFG                       
091300                                                                          
091400     SEARCH ALL TAB1-FELKOD                                               
091500       AT END CONTINUE                                                    
091600       WHEN TAB1-IDKVAFEL (T1X) = ART-IDKVAFEL                            
091700         IF W-OMR-TIAARP = W-DAT-TIAARP                                   
091800            ADD +1          TO TAB1-KVKVAANT-TRG (T1X)                    
091810                               TAB1-KVKVAANT-OMR (T1X)                    
091900                               TAB1-KVKVAANT-DC  (T1X)                    
092000                               W-TOT-KVKVAFEL-TRG                         
092010                               W-TOT-KVKVAFEL-OMR                         
092100                               W-TOT-KVKVAFEL-DC                          
092200                                                                          
092300            IF  ART-IDARTNR  = W-SPAR-IDARTNR                             
092400            AND W-OMR-TIAARP = W-SPAR-TIAARP                              
092500               CONTINUE                                                   
092600            ELSE                                                          
092700               ADD +1       TO W-TOT-KVARTFEL-TRG                         
092710                               W-TOT-KVARTFEL-OMR                         
092800                               W-TOT-KVARTFEL-DC                          
092900            END-IF                                                        
093000         END-IF                                                           
093100                                                                          
093200         IF W-OMR-TIAARP = TAB2-TIAARP (11) OR                            
093300                           TAB2-TIAARP (12) OR                            
093400                           TAB2-TIAARP (13)                               
093500            IF  ART-IDARTNR  = W-SPAR-IDARTNR                             
093600            AND W-OMR-TIAARP = W-SPAR-TIAARP                              
093700                CONTINUE                                                  
093800            ELSE                                                          
093900               ADD +1       TO W-TOT-KVARTFEL-DC-KV                       
094000            END-IF                                                        
094100            ADD +1          TO W-TOT-KVKVAFEL-DC-KV                       
094200                                TAB1-KVKVAANT-DC-KV (T1X)                 
094300         END-IF                                                           
094400                                                                          
094500         MOVE TAB1-KVKVAFPO (T1X) TO W-KVKVAFPO                           
094600         MOVE TAB1-KDKVAFG  (T1X) TO W-KDKVAFG                            
094700     END-SEARCH                                                           
094800                                                                          
094900     IF W-KVKVAFPO > +0                                                   
095000        MOVE W-KDKVAFG      TO IX1                                        
095100        MOVE +0             TO IX2                                        
095200        PERFORM UNTIL IX2 = +13                                           
095300          ADD +1            TO IX2                                        
095400          IF W-OMR-TIAARP = TAB3-TIAARP (IX1 IX2)                         
095500             ADD W-KVKVAFPO TO TAB3-KVKVAFPO-OMR (IX1 IX2)                
095600          END-IF                                                          
095700        END-PERFORM                                                       
095800     END-IF                                                               
095900                                                                          
096000     MOVE ART-IDARTNR       TO W-SPAR-IDARTNR                             
096100     MOVE W-OMR-TIAARP      TO W-SPAR-TIAARP                              
096200     .                                                                    
096300     EJECT                                                                
096400 Z-FINIT SECTION.                                                         
096500     SKIP2                                                                
096600     CLOSE W4260501 W4260502 W4260503                                     
096700     .                                                                    
096800     EJECT                                                                
096810 S01-SKRIV-FELFORDELNING-TORG SECTION.                                    
096820     SKIP2                                                                
096830     MOVE NEJ                       TO W-SKRIV-UTFIL                      
096840     MOVE +1                        TO IX1                                
096850     PERFORM UNTIL TAB1-IDKVAFEL (IX1) = +99                              
096860                                                                          
096870       MOVE 'TRG'                   TO UT1-IDPTYP                         
096880       MOVE W-SPAR-IDDC             TO UT1-IDDC                           
096890       MOVE W-SPAR-IDKVAOMR         TO UT1-IDKVAOMR                       
096891       MOVE W-SPAR-IDKVATRG         TO UT1-IDKVATRG                       
096895       MOVE W-OMR-BEKVAOMR-DC       TO UT1-BEKVAOMR  (1)                  
096897       MOVE W-OMR-BEKVAOMR-GB       TO UT1-BEKVAOMR  (2)                  
096898       MOVE TAB2-TIAARP       (13)  TO UT1-TIAARP-FOM                     
096899                                       UT1-TIAARP-TOM                     
096900       MOVE TAB1-IDKVAFEL     (IX1) TO UT1-IDKVAFEL                       
096902       EVALUATE TRUE                                                      
096903         WHEN CDC-SE                                                      
096904           MOVE TAB1-BEKVAFEL-S(IX1) TO UT1-BEKVAFEL (1)                  
096905           MOVE TAB1-BEKVAFGR-S(IX1) TO UT1-BEKVAFGR (1)                  
096906         WHEN SDC-NL                                                      
096907           MOVE TAB1-BEKVAFEL-NL (IX1) TO UT1-BEKVAFEL (1)                
096908           MOVE TAB1-BEKVAFGR-NL (IX1) TO UT1-BEKVAFGR (1)                
096909       END-EVALUATE                                                       
096910       MOVE TAB1-BEKVAFEL-GB  (IX1) TO UT1-BEKVAFEL  (2)                  
096911       MOVE TAB1-BEKVAFGR-GB  (IX1) TO UT1-BEKVAFGR  (2)                  
096912       MOVE TAB1-KVKVAANT-TRG (IX1) TO UT1-KVKVAFEL                       
096913       MOVE TAB1-KVKVAFPO     (IX1) TO UT1-KVKVAFPO                       
096914                                                                          
096915       COMPUTE UT1-REKVAFEL ROUNDED =                                     
096916           (TAB1-KVKVAANT-TRG (IX1) * 100) / W-TOT-KVKVAFEL-TRG           
096917           ON SIZE ERROR MOVE +0    TO UT1-REKVAFEL                       
096918       END-COMPUTE                                                        
096919                                                                          
096920       COMPUTE UT1-REKVAFKA ROUNDED =                                     
096921           (TAB1-KVKVAANT-TRG (IX1) * 100) / TAB2-KVART-TRG (13)          
096922           ON SIZE ERROR MOVE +0    TO UT1-REKVAFKA                       
096923       END-COMPUTE                                                        
096924                                                                          
096925       IF UT1-KVKVAFEL > +0                                               
096926          MOVE JA                   TO W-SKRIV-UTFIL                      
096927          WRITE W4260501-POST FROM UT1-W4260501                           
096928       END-IF                                                             
096929                                                                          
096930       ADD +1                       TO IX1                                
096931     END-PERFORM                                                          
096932                                                                          
096933     IF W-SKRIV-UTFIL = JA                                                
096934        MOVE 'TOT'                  TO UT2-IDPTYP                         
096935        MOVE W-SPAR-IDDC            TO UT2-IDDC                           
096936        MOVE W-SPAR-IDKVAOMR        TO UT2-IDKVAOMR                       
096937        MOVE TAB2-TIAARP    (13)    TO UT2-TIAARP-FOM                     
096938                                       UT2-TIAARP-TOM                     
096939        MOVE W-TOT-KVKVAFEL-TRG     TO UT2-KVKVAFEL                       
096940        MOVE W-TOT-KVARTFEL-TRG     TO UT2-KVARTFEL                       
096941        MOVE TAB2-KVART-TRG (13)    TO UT2-KVART                          
096942                                                                          
096943        WRITE W4260501-POST       FROM UT2-W4260502                       
096944     END-IF                                                               
096945                                                                          
096946     .                                                                    
096947     EJECT                                                                
096948 S02-NOLLSTALL-TORG SECTION.                                              
096949     SKIP2                                                                
096950     MOVE +0 TO IX1                                                       
096951     PERFORM UNTIL IX1 = +99                                              
096952       ADD +1                    TO IX1                                   
096953       MOVE +0                   TO TAB1-KVKVAANT-TRG   (IX1)             
096954     END-PERFORM                                                          
096955                                                                          
096956                                                                          
096957     MOVE +0                     TO IX1                                   
096958     PERFORM UNTIL IX1 = +13                                              
096959       ADD +1                    TO IX1                                   
096961       MOVE +0                   TO TAB2-KVART-TRG    (IX1)               
096967     END-PERFORM                                                          
096968                                                                          
096989     MOVE +0                     TO W-TOT-KVARTFEL-TRG                    
096990                                    W-TOT-KVKVAFEL-TRG                    
096991     .                                                                    
096992     EJECT                                                                
096993*    ---- IMS SEKTIONER                                                   
097000                                                                          
097100 IMS-GU-WLXXJY01 SECTION.                                                 
097200                                                                          
097300     STRING 'WLXXJY01(WDGXKEY  =' W-WDGX-4821-KEY-X ')'                   
097400          DELIMITED BY SIZE INTO SSA1                                     
097500     MOVE '  '                  TO GODK-STATUSKODER                       
097600     CALL CBLTDLI USING GN XXJY-PCB DLI-IO-AREA1 SSA1                     
097700     MOVE XXJY-STATUS-CODE      TO STATUS-WS                              
097800     PERFORM IMS-STATUSKONTROLL                                           
097900     .                                                                    
098000     SKIP2                                                                
098100 IMS-GNP-WLXXJY11 SECTION.                                                
098200                                                                          
098300     MOVE 'WLXXJY11 '         TO SSA1                                     
098400     MOVE '  GE'                TO GODK-STATUSKODER                       
098500     CALL CBLTDLI USING GNP XXJY-PCB DLI-IO-AREA1 SSA1                    
098600     MOVE XXJY-STATUS-CODE      TO STATUS-WS                              
098700     PERFORM IMS-STATUSKONTROLL                                           
098800     .                                                                    
098900     EJECT                                                                
099000 IMS-GN-W6KVAB01 SECTION.                                                 
099100                                                                          
099200     STRING 'W6KVAB01(W6H6ASEQ=>' W2-W6H6ASEQ-X                           
099300                    '&KDKVASTA =' W2-KDKVASTA-X ')'                       
099400          DELIMITED BY SIZE INTO SSA1                                     
099500     MOVE '  GB'                TO GODK-STATUSKODER                       
099600     CALL CBLTDLI USING GN W6H6A-PCB DLI-IO-AREA2 SSA1                    
099700     MOVE W6H6A-STATUS-CODE      TO STATUS-WS                             
099800     PERFORM IMS-STATUSKONTROLL                                           
099900     .                                                                    
100000     SKIP2                                                                
100100 IMS-GNP-W6KVAB11 SECTION.                                                
100200                                                                          
100300     MOVE 'W6KVAB11 '           TO SSA1                                   
100400     MOVE '  GE'                TO GODK-STATUSKODER                       
100500     CALL CBLTDLI USING GNP W6H6A-PCB DLI-IO-AREA3 SSA1                   
100600     MOVE W6H6A-STATUS-CODE      TO STATUS-WS                             
100700     PERFORM IMS-STATUSKONTROLL                                           
100800     .                                                                    
100900     EJECT                                                                
101000 IMS-GHU-W6KVAB01 SECTION.                                                
101100                                                                          
101200     STRING 'W6KVAB01(W6H601KY =' W1-W6H601KY-X ')'                       
101300          DELIMITED BY SIZE INTO SSA1                                     
101400     MOVE '  GE'                TO GODK-STATUSKODER                       
101500     CALL CBLTDLI USING GHU W6H6-PCB DLI-IO-AREA2 SSA1                    
101600     MOVE W6H6-STATUS-CODE      TO STATUS-WS                              
101700     PERFORM IMS-STATUSKONTROLL                                           
101800     .                                                                    
101900     SKIP2                                                                
102000 IMS-DLET-W6KVAB01 SECTION.                                               
102100                                                                          
102200     MOVE 'W6KVAB01 '           TO SSA1                                   
102300     MOVE '  '                  TO GODK-STATUSKODER                       
102400     CALL CBLTDLI USING DLET  W6H6-PCB DLI-IO-AREA2 SSA1                  
102500     MOVE W6H6-STATUS-CODE      TO STATUS-WS                              
102600     PERFORM IMS-STATUSKONTROLL                                           
102700     .                                                                    
102800     SKIP2                                                                
102834 IMS-GU-W6KVAA01-MIN-MAX SECTION.                                         
102835                                                                          
102836     STRING 'W6KVAA01(W6H501KY=>' W-W6H501KY-MIN-X                        
102837                    '&W6H501KY=<' W-W6H501KY-MAX-X                        
102838                    '&IDKVAGRP= ' W-IDKVAGRP-X ')'                        
102839          DELIMITED BY SIZE INTO SSA1                                     
102840     MOVE '  GE'   TO GODK-STATUSKODER                                    
102841     CALL CBLTDLI USING GU W6H5-PCB DLI-IO-AREA4 SSA1                     
102842     MOVE W6H5-STATUS-CODE TO STATUS-WS                                   
102843     PERFORM IMS-STATUSKONTROLL                                           
102844     .                                                                    
102850     SKIP2                                                                
102900 IMS-STATUSKONTROLL SECTION.                                              
103000                                                                          
103100     SET STATUS-IX TO 1                                                   
103200     SEARCH GODK-STATUS                                                   
103300       AT END CALL FELLOG                                                 
103400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
103500         CONTINUE                                                         
103600     END-SEARCH                                                           
103700     .                                                                    
