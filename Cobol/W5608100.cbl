000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5608100.                                                
000300 AUTHOR.         ASPFJÄLL MARKUS.                                         
000400 DATE-WRITTEN.   04/05/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER USA-LAGERBAND O PRISTILLÄMPAR ALLA DESS ARTIKLAR           
001000*        VARS LEVERANTÖR = 1441 PÅ ALLA DC,                               
001100*        OCH STANDARDPRIS ELLER SJÄLVKOSTNADSPRIS EJ ÄR NOLL.             
001200*        DESSA SKRIVS SEDAN PÅ UTFIL                                      
001300*                                                                         
001400*        SKAPAD FÖR ETRACKER NO 1322340, INSTALLERAD 2004-09-05           
001500*        ÄNDRAD FÖR ETRACKER NO 1386358, INSTALLERAD 2004-09-29           
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- INFIL LAGERBAND NDC-NA                                     
002500     SELECT W01184                     ASSIGN TO W56081D1.                
002600     SKIP2                                                                
002700*          --- UTFIL POSTER FÖR RAPPORT TILL USA                          
002800     SELECT W56081                     ASSIGN TO W56081D2.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W01184                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  -COPY W01184      -L.                                                
003900     SKIP3                                                                
004000 FD  W56081                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  POST -COPY W56081 -PRE  UT-  -L.                                     
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800 77  IDPGM                       PIC X(8)    VALUE 'W5608100'.            
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100 77  LEV-ANT-1441                PIC 9       VALUE ZERO.                  
005200 77  LEV-ANT-DC-ART              PIC 9       VALUE ZERO.                  
005300 01  WS-IDARTNR                  PIC 9(9)    VALUE ZERO COMP-3.           
005400 01  WS-ANT                      PIC 9(7)    VALUE ZERO COMP-3.           
005500     SKIP2                                                                
005600*      --- VALID IDDC CODES                                               
005700*                                                                         
005800*01    -COPY WWDC99                                                       
005900       EJECT                                                              
006000 01  FELTEXT.                                                             
006100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006300                                                                          
006400 77  W01184-EOF-SW               PIC X       VALUE 'N'.                   
006500     88  END-OF-W01184                       VALUE 'J'.                   
006600     EJECT                                                                
006700 01  DYNAMISKA-SUBPROGRAM.                                                
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007100     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL POSTSUM                                          
007400*                                                                         
007500*                                                                         
007600*01  -COPY W0005   -PRE  POSTSUM-                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(24) VALUE 'PRISTILL-AREA'.         
007900*01  PRIS-AREA   -COPY W335PRIS                                           
008000                                                                          
008100     EJECT                                                                
008200 01  IN-84-AREA-START            PIC X(24) VALUE 'IN-84-AREA'.            
008300                                                                          
008400*01  AREA -COPY W01184     -PRE IN-                                       
008500     EJECT                                                                
008600 01  UT-AREA-START               PIC X(24) VALUE 'UT-AREA-START'.         
008700                                                                          
008800*01  AREA -COPY W56081     -PRE UT-                                       
008900*                                                                         
009000     EJECT                                                                
009100 01  NYCKLAR-TILL-DLI.                                                    
009200     03  W-IDARTNR-X.                                                     
009300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009400     03  W-KDSEGKEY-X.                                                    
009500         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
009600     EJECT                                                                
009700*    ---  DLI INPUT-OUTPUT AREA                                           
009800                                                                          
009900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
010000 01  DLI-IO-WDK601.                                                       
010100*    03 WDK601   -COPY WDK601                                             
010200     EJECT                                                                
010300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
010400 01  DLI-IO-WDK611.                                                       
010500*    03 WDK611   -COPY WDK611                                             
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800                                                                          
010900 01  STATUS-WS                   PIC XX.                                  
011000     88  SEGMENT-FINNS                       VALUE '  '.                  
011100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011400     88  IMS-EJ-OK                           VALUE 'XD'.                  
011500     SKIP2                                                                
011600 01  GODK-STATUSKODER.                                                    
011700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011800     SKIP3                                                                
011900 01  SSA1                        PIC X(64).                               
012000 01  SSA2                        PIC X(64).                               
012100     EJECT                                                                
012200*    --- IMS FUNKTIONSKODER                                               
012300*01  -COPY W0003                                                          
012400     EJECT                                                                
012500 LINKAGE SECTION.                                                         
012600*01  -COPY W0008 -PRE  PRIS-ARTC-.                                        
012700     05  FILLER        PIC X.                                             
012800     EJECT                                                                
012900                                                                          
012910*01  -COPY W0008 -PRE  PRIS-WDK7-.                                        
012920     05  FILLER        PIC X.                                             
012930     EJECT                                                                
012940                                                                          
013000*01  -COPY W0008 -PRE  PRIS-GMTA-.                                        
013100     05  FILLER        PIC X.                                             
013200     EJECT                                                                
013300                                                                          
013400*01  -COPY W0008 -PRE  PRIS-BETA-.                                        
013500     05  FILLER        PIC X.                                             
013600                                                                          
013700*01  -COPY W0008 -PRE  PRIS-GPRIA-.                                       
013800     05  FILLER        PIC X.                                             
013900     EJECT                                                                
014000*01  -COPY W0008 -PRE  PRIS-GPRIB-.                                       
014100     05  FILLER        PIC X.                                             
014200     EJECT                                                                
014300 01  PRIS-COST-WDK6-PCB          PIC X.                                   
014400 01  PRIS-COST-WDK7-PCB          PIC X.                                   
014500 01  PRIS-COST-WDF1-PCB          PIC X.                                   
014600 01  PRIS-COST-9305-PCB          PIC X.                                   
014700 01  PRIS-COST-WDK72-PCB         PIC X.                                   
014800 01  PRIS-COST-WDB6-PCB          PIC X.                                   
014900                                                                          
015000*01  -COPY W0008 -PRE  WDK6-.                                             
015100     05  FILLER        PIC X.                                             
015200     EJECT                                                                
015300                                                                          
015400 PROCEDURE DIVISION  USING PRIS-ARTC-PCB                                  
015410                           PRIS-WDK7-PCB      PRIS-GMTA-PCB               
015500                           PRIS-BETA-PCB      PRIS-GPRIA-PCB              
015600                           PRIS-GPRIB-PCB     PRIS-COST-WDK6-PCB          
015700                           PRIS-COST-WDK7-PCB PRIS-COST-WDF1-PCB          
015800                           PRIS-COST-9305-PCB PRIS-COST-WDK72-PCB         
015900                           PRIS-COST-WDB6-PCB                             
015910                           WDK6-PCB.                                      
016000 MAIN SECTION.                                                            
016100     ENTRY 'DLITCBL' USING PRIS-ARTC-PCB                                  
016110                           PRIS-WDK7-PCB      PRIS-GMTA-PCB               
016200                           PRIS-BETA-PCB      PRIS-GPRIA-PCB              
016300                           PRIS-GPRIB-PCB     PRIS-COST-WDK6-PCB          
016400                           PRIS-COST-WDK7-PCB PRIS-COST-WDF1-PCB          
016500                           PRIS-COST-9305-PCB PRIS-COST-WDK72-PCB         
016600                           PRIS-COST-WDB6-PCB                             
016610                           WDK6-PCB.                                      
016700                                                                          
016800     PERFORM A-INIT                                                       
016900                                                                          
017000     PERFORM S01-LAES-W01184                                              
017100     PERFORM S02-NOLLA-UTPOST                                             
017200                                                                          
017300     PERFORM UNTIL END-OF-W01184                                          
017400       IF IN-SLAG-IDARTNR NOT = WS-IDARTNR                                
017500         IF LEV-ANT-1441 = LEV-ANT-DC-ART AND                             
017600            LEV-ANT-1441 > ZERO                                           
017700           MOVE ZERO                    TO UT-PRARTNTO                    
017800                                           UT-KDPSLLOC                    
017900           PERFORM IMS-GET-WDK611                                         
018000           IF SEGMENT-FINNS                                               
018100             MOVE CLAG-KDPSLLOC         TO UT-KDPSLLOC                    
018200             IF CLAG-PRARTSTD NOT = +0 AND CLAG-PRARTSJK NOT = +0         
018300               PERFORM S03-PRISTILLAMPA                                   
018400             END-IF                                                       
018500           END-IF                                                         
018600           IF UT-KDPSLLOC NOT = 98                                        
018700             PERFORM S11-SKRIV-W56081                                     
018800           END-IF                                                         
018900         END-IF                                                           
019000         PERFORM S02-NOLLA-UTPOST                                         
019100         MOVE ZERO                      TO LEV-ANT-1441                   
019200                                           LEV-ANT-DC-ART                 
019300         MOVE IN-SLAG-IDARTNR           TO WS-IDARTNR                     
019400                                           UT-IDARTNR                     
019500                                           W-IDARTNR                      
019600       END-IF                                                             
019700                                                                          
019800       ADD 1                            TO LEV-ANT-DC-ART                 
019900       MOVE IN-SLAG-IDDC                TO WS-IDDC                        
020000       IF IN-SLAG-IDLEVNR = '1441 '                                       
020100         ADD 1                          TO LEV-ANT-1441                   
020200         EVALUATE  TRUE                                                   
020300           WHEN NDC-US-RU                                                 
020400             MOVE '41'                  TO UT-IDDC(1)                     
020500             MOVE IN-SLAG-PRAVCOST      TO UT-PRAVCOST(1)                 
020600           WHEN NDC-US-BAT                                                
020700             MOVE '92'                  TO UT-IDDC(2)                     
020800             MOVE IN-SLAG-PRAVCOST      TO UT-PRAVCOST(2)                 
020900           WHEN NDC-US-LA                                                 
021000             MOVE '43'                  TO UT-IDDC(3)                     
021100             MOVE IN-SLAG-PRAVCOST      TO UT-PRAVCOST(3)                 
021110           WHEN NDC-US-SE                                                 
021120             MOVE '44'                  TO UT-IDDC(4)                     
021130             MOVE IN-SLAG-PRAVCOST      TO UT-PRAVCOST(4)                 
021140           WHEN NDC-US-CH                                                 
021150             MOVE '45'                  TO UT-IDDC(5)                     
021160             MOVE IN-SLAG-PRAVCOST      TO UT-PRAVCOST(5)                 
021170           WHEN NDC-US-JA                                                 
021180             MOVE '46'                  TO UT-IDDC(6)                     
021181             MOVE IN-SLAG-PRAVCOST      TO UT-PRAVCOST(6)                 
021182           WHEN NDC-US-DA                                                 
021183             MOVE '47'                  TO UT-IDDC(7)                     
021190             MOVE IN-SLAG-PRAVCOST      TO UT-PRAVCOST(7)                 
021200         END-EVALUATE                                                     
021300       END-IF                                                             
021400       PERFORM S01-LAES-W01184                                            
021500     END-PERFORM                                                          
021600                                                                          
021700** SISTA POST                                                             
021800     IF LEV-ANT-1441 = LEV-ANT-DC-ART AND                                 
021900        LEV-ANT-1441 > ZERO                                               
022000       MOVE ZERO                        TO UT-PRARTNTO                    
022100                                           UT-KDPSLLOC                    
022200       IF SEGMENT-FINNS                                                   
022300         MOVE CLAG-KDPSLLOC             TO UT-KDPSLLOC                    
022400         IF CLAG-PRARTSTD NOT = +0 AND CLAG-PRARTSJK NOT = +0             
022500           PERFORM S03-PRISTILLAMPA                                       
022600         END-IF                                                           
022700       END-IF                                                             
022800       IF UT-KDPSLLOC NOT = 98                                            
022900         PERFORM S11-SKRIV-W56081                                         
023000       END-IF                                                             
023100     END-IF                                                               
023200                                                                          
023300     PERFORM Z-FINIT                                                      
023400                                                                          
023500     MOVE ZERO TO RETURN-CODE                                             
023600     GOBACK                                                               
023700     .                                                                    
023800     EJECT                                                                
023900 A-INIT SECTION.                                                          
024000                                                                          
024100     OPEN INPUT  W01184                                                   
024200          OUTPUT W56081                                                   
024300                                                                          
024400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
024500     .                                                                    
024600     EJECT                                                                
024700 Z-FINIT SECTION.                                                         
024800                                                                          
024900     CLOSE W01184                                                         
025000           W56081                                                         
025100                                                                          
025200     MOVE 'S' TO POSTSUM-OPKOD                                            
025300     CALL POSTSUM USING POSTSUM-PARM                                      
025400     .                                                                    
025500     EJECT                                                                
025600 S01-LAES-W01184  SECTION.                                                
025700                                                                          
025800     READ W01184 INTO IN-AREA                                             
025900     AT END                                                               
026000        SET END-OF-W01184 TO TRUE                                         
026100     NOT AT END                                                           
026200        MOVE 'W01184' TO POSTSUM-FDNAMN                                   
026300        MOVE 'W56081D1' TO POSTSUM-DDNAMN2                                
026400        MOVE IN-SLAG-IDDC   TO POSTSUM-TRANSTYP                           
026500        CALL POSTSUM USING POSTSUM-PARM                                   
026600     END-READ                                                             
026700     .                                                                    
026800     EJECT                                                                
026900 S02-NOLLA-UTPOST SECTION.                                                
027000                                                                          
027100     MOVE '  '                TO UT-IDDC(1)                               
027200                                 UT-IDDC(2)                               
027300                                 UT-IDDC(3)                               
027310                                 UT-IDDC(4)                               
027320                                 UT-IDDC(5)                               
027330                                 UT-IDDC(6)                               
027400                                                                          
027500     MOVE ZERO                TO UT-PRAVCOST(1)                           
027600                                 UT-PRAVCOST(2)                           
027700                                 UT-PRAVCOST(3)                           
027710                                 UT-PRAVCOST(4)                           
027720                                 UT-PRAVCOST(5)                           
027730                                 UT-PRAVCOST(6)                           
027800     .                                                                    
027900     EJECT                                                                
028000 S03-PRISTILLAMPA SECTION.                                                
028100                                                                          
028200     MOVE 1                   TO PRIS-KDCALL                              
028210     MOVE 'W5608100'          TO PRIS-IDPGM                               
028300     MOVE WS-IDARTNR          TO PRIS-IDARTNR                             
028400     MOVE 08141               TO PRIS-IDDISTR                             
028500     MOVE 1                   TO PRIS-IDKUNDNR                            
028600     MOVE '11'                TO PRIS-IDDC                                
028700     MOVE +4                  TO PRIS-KDORDKL                             
028800     MOVE +1                  TO PRIS-KVBEART                             
028900     MOVE SPACE               TO PRIS-FLINVEST                            
029000                                                                          
029100     CALL W335PRIS USING PRIS-AREA                                        
029200                         PRIS-ARTC-PCB                                    
029210                         PRIS-WDK7-PCB                                    
029300                         PRIS-GMTA-PCB                                    
029400                         PRIS-BETA-PCB                                    
029500                         PRIS-GPRIA-PCB                                   
029600                         PRIS-GPRIB-PCB                                   
029700                         PRIS-COST-WDK6-PCB                               
029800                         PRIS-COST-WDK7-PCB                               
029900                         PRIS-COST-WDF1-PCB                               
030000                         PRIS-COST-9305-PCB                               
030100                         PRIS-COST-WDK72-PCB                              
030110                         PRIS-COST-WDB6-PCB                               
030200                                                                          
030300     IF PRIS-PRARTNTO > ZERO                                              
030400        MOVE PRIS-PRARTNTO    TO UT-PRARTNTO                              
030500     END-IF                                                               
030600     .                                                                    
030700     EJECT                                                                
030800 S11-SKRIV-W56081 SECTION.                                                
030900                                                                          
031000     WRITE UT-POST FROM UT-AREA                                           
031100                                                                          
031200     MOVE 'UTFIL'       TO POSTSUM-TRANSTYP                               
031300     MOVE 'W56081 '     TO POSTSUM-FDNAMN                                 
031400     MOVE 'W56081D2'    TO POSTSUM-DDNAMN2                                
031500     CALL POSTSUM USING POSTSUM-PARM                                      
031600                                                                          
031700     IF WS-ANT > 10000                                                    
031800       DISPLAY ' 10 000 ARTIKLAR TILL SKRIVNA '                           
031900       MOVE ZERO       TO WS-ANT                                          
032000     ELSE                                                                 
032100       ADD +1          TO WS-ANT                                          
032200     END-IF                                                               
032300     .                                                                    
032400     EJECT                                                                
032500 IMS-GET-WDK611 SECTION.                                                  
032600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
032700          DELIMITED BY SIZE INTO SSA1                                     
032800     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
032900          DELIMITED BY SIZE INTO SSA2                                     
033000     MOVE '  GE'           TO GODK-STATUSKODER                            
033100     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
033200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
033300     PERFORM IMS-STATUSKONTROLL                                           
033400     .                                                                    
033500     EJECT                                                                
033600                                                                          
033700 IMS-STATUSKONTROLL SECTION.                                              
033800                                                                          
033900     SET STATUS-IX TO 1                                                   
034000     SEARCH GODK-STATUS                                                   
034100       AT END                                                             
034200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
034300           DELIMITED BY SIZE INTO FELTEXT                                 
034400         DISPLAY FELTEXT                                                  
034500         CALL FELLOG                                                      
034600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
034700         CONTINUE                                                         
034800     END-SEARCH                                                           
034900     .                                                                    
