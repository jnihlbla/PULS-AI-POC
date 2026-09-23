000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5606000.                                                
000300 AUTHOR.         INGVAR SKJELBRED.                                        
000400 DATE-WRITTEN.   97/07/30.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        SKAPAR PRISREGISTER PÅ ALLA ARTIKLAR I SVENSKA KRONOR            
000900*        OCH I US-DOLLAR OCH VÄXELKURSEN VID KÖRNINGSTILLFÄLLET           
001000*                                                                         
001100*        PROGRAMMET LÄSER      WLARTC (WDK7)                              
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
002500*          --- LAGERBANDET                                                
002600     SELECT W01160                     ASSIGN TO W56060D1.                
002700     SKIP2                                                                
002800*          --- PRISREGISTER PER ARTIKEL OCH ÅR                            
002900     SELECT W56061                     ASSIGN TO W56060D2.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP2                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W01160                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  -COPY W01160      -L.                                                
004000     SKIP3                                                                
004100 FD  W56061                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  POST -COPY W56061 -PRE  UT-  -L.                                     
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900                                                                          
005000*    -- CHECKED BY WY2000                                                 
005100 77  IDPGM                       PIC X(8)    VALUE 'W5606000'.            
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005310 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
005320 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
005400                                                                          
005500 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
005600     88  END-OF-W01160                       VALUE 'J'.                   
005700     EJECT                                                                
005800 01  W-PRKURS                PIC S9(6)V9(5) VALUE +0     COMP-3.          
005900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006000 01  FILLER REDEFINES DAGENS-DATUM.                                       
006100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006400     EJECT                                                                
006500 01  DYNAMISKA-SUBPROGRAM.                                                
006600*                                                                         
006700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007100     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
007200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
007210     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
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
008400*    --- VALID IDDC CODES                                                 
008500*                                                                         
008600*01  -COPY WWDCKONS                                                       
008700     EJECT                                                                
008701*                                                                         
008710*01  -COPY W510CURR                                                       
008720     EJECT                                                                
008800*    --- PARAMETRAR TILL POSTSUM                                          
008900*                                                                         
009000*01  -COPY W0005   -PRE  POSTSUM-                                         
009100     EJECT                                                                
009200 01  FILLER                      PIC X(24) VALUE                          
009300                                 'PRISTILL-AREA'.                         
009400*01  PRIS-AREA   -COPY W335PRIS                                           
009500                                                                          
009600     EJECT                                                                
009700                                                                          
009800*    --- PARAMETRAR TILL DATKORT                                          
009900 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
010000                                                                          
010100*01  -COPY WDATKORT                                                       
010200     EJECT                                                                
010300                                                                          
010400*01  AREA -COPY W01160     -PRE IN-                                       
010500     EJECT                                                                
010600 01  UT-AREA-START               PIC X(24)   VALUE                        
010700                                 'UT-AREA-START  '.                       
010800     SKIP2                                                                
010900                                                                          
011000*01  AREA -COPY W56061     -PRE UT-                                       
011100     EJECT                                                                
014800 LINKAGE SECTION.                                                         
014900                                                                          
015000     EJECT                                                                
015100*01  -COPY W0008  -PRE WDG2-                                              
015200     05  FILLER                  PIC X.                                   
015300*01  -COPY W0008 -PRE  PRIS-ARTC-.                                        
015400     05  FILLER        PIC X.                                             
015500     EJECT                                                                
015600                                                                          
015610*01  -COPY W0008 -PRE  PRIS-WDK7-.                                        
015620     05  FILLER        PIC X.                                             
015630     EJECT                                                                
015640                                                                          
015700*01  -COPY W0008 -PRE  PRIS-GMTA-.                                        
015800     05  FILLER        PIC X.                                             
015900     EJECT                                                                
016000                                                                          
016100*01  -COPY W0008 -PRE  PRIS-BETA-.                                        
016200     05  FILLER        PIC X.                                             
016300                                                                          
016400*01  -COPY W0008 -PRE  PRIS-GPRIA-.                                       
016500     05  FILLER        PIC X.                                             
016600     EJECT                                                                
016700*01  -COPY W0008 -PRE  PRIS-GPRIB-.                                       
016800     05  FILLER        PIC X.                                             
016900     EJECT                                                                
017000 01  PRIS-COST-WDK6-PCB          PIC X.                                   
017100 01  PRIS-COST-WDK7-PCB          PIC X.                                   
017200 01  PRIS-COST-WDF1-PCB          PIC X.                                   
017300 01  PRIS-COST-9305-PCB          PIC X.                                   
017400 01  PRIS-COST-WDK72-PCB         PIC X.                                   
017500 01  PRIS-COST-WDB6-PCB          PIC X.                                   
017600 PROCEDURE DIVISION  USING WDG2-PCB PRIS-ARTC-PCB                         
017610                     PRIS-WDK7-PCB PRIS-GMTA-PCB                          
017700                     PRIS-BETA-PCB PRIS-GPRIA-PCB PRIS-GPRIB-PCB          
017800                     PRIS-COST-WDK6-PCB                                   
017900                     PRIS-COST-WDK7-PCB                                   
018000                     PRIS-COST-WDF1-PCB                                   
018100                     PRIS-COST-9305-PCB                                   
018200                     PRIS-COST-WDK72-PCB                                  
018210                     PRIS-COST-WDB6-PCB.                                  
018300 MAIN SECTION.                                                            
018400     ENTRY 'DLITCBL' USING WDG2-PCB PRIS-ARTC-PCB                         
018410                     PRIS-WDK7-PCB PRIS-GMTA-PCB                          
018500                     PRIS-BETA-PCB PRIS-GPRIA-PCB PRIS-GPRIB-PCB          
018600                     PRIS-COST-WDK6-PCB                                   
018700                     PRIS-COST-WDK7-PCB                                   
018800                     PRIS-COST-WDF1-PCB                                   
018900                     PRIS-COST-9305-PCB                                   
019000                     PRIS-COST-WDK72-PCB                                  
019010                     PRIS-COST-WDB6-PCB.                                  
019100                                                                          
019200                                                                          
019300     PERFORM A-INIT                                                       
019400                                                                          
019500     PERFORM S01-LAES-W01160                                              
019600     PERFORM UNTIL END-OF-W01160                                          
019700       IF IN-CLAG-IDARTNR NOT = 5929479                                   
019800         PERFORM B-BEARBETA                                               
019900       END-IF                                                             
020000                                                                          
020100       PERFORM S01-LAES-W01160                                            
020200     END-PERFORM                                                          
020300                                                                          
020400                                                                          
020500     PERFORM Z-FINIT                                                      
020600                                                                          
020700     MOVE ZERO TO RETURN-CODE                                             
020800     GOBACK                                                               
020900     .                                                                    
021000     EJECT                                                                
021100 A-INIT SECTION.                                                          
021200                                                                          
021300     OPEN INPUT  W01160                                                   
021400     OPEN OUTPUT W56061                                                   
021500                                                                          
021600     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
021700     MOVE D-AAR                 TO  W-DATE-AAMM(1:2)                      
021710     MOVE D-MAANAD              TO  W-DATE-AAMM(3:2)                      
021800                                                                          
021900     ACCEPT DAGENS-DATUM  FROM DATE                                       
022000     MOVE IDPGM                 TO POSTSUM-PROGNAMN                       
022100     MOVE 'USD'                 TO CURR-KDVALISO-ROW                      
022110     MOVE W-DATE-AAMM           TO CURR-TIAAMM                            
022120     MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                      
022140     MOVE 'M'                   TO CURR-KDVALTYP                          
022150                                                                          
022200     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
022300     IF CURR-KDSVAR = ' '                                                 
022400        MOVE CURR-PRKURS-NEW    TO W-PRKURS                               
022500     ELSE                                                                 
022600        MOVE ZERO               TO W-PRKURS                               
022700     END-IF                                                               
022800     .                                                                    
022900     EJECT                                                                
023000 B-BEARBETA SECTION.                                                      
023100                                                                          
023200     IF (IN-CLAG-PRARTSJK > ZERO                                          
023300     AND IN-CLAG-PRARTSTD > ZERO)                                         
023400       MOVE 1                   TO PRIS-KDCALL                            
023410       MOVE 'W5606000'          TO PRIS-IDPGM                             
023500       MOVE IN-CLAG-IDARTNR     TO PRIS-IDARTNR                           
023600       MOVE 08141               TO PRIS-IDDISTR                           
023700       MOVE 1                   TO PRIS-IDKUNDNR                          
023800       MOVE WC-CDC-SE           TO PRIS-IDDC                              
023900       MOVE +4                  TO PRIS-KDORDKL                           
024000       MOVE +1                  TO PRIS-KVBEART                           
024100       MOVE SPACE               TO PRIS-FLINVEST                          
024200                                                                          
024300       CALL W335PRIS USING PRIS-AREA                                      
024400                         PRIS-ARTC-PCB                                    
024410                         PRIS-WDK7-PCB                                    
024500                         PRIS-GMTA-PCB                                    
024600                         PRIS-BETA-PCB                                    
024700                         PRIS-GPRIA-PCB                                   
024800                         PRIS-GPRIB-PCB                                   
024900                         PRIS-COST-WDK6-PCB                               
025000                         PRIS-COST-WDK7-PCB                               
025100                         PRIS-COST-WDF1-PCB                               
025200                         PRIS-COST-9305-PCB                               
025300                         PRIS-COST-WDK72-PCB                              
025310                         PRIS-COST-WDB6-PCB                               
025400                                                                          
025500       IF PRIS-PRARTBTO-MARK > ZERO                                       
025600          MOVE PRIS-PRARTNTO       TO UT-PRARTBTO-SEK                     
025700          COMPUTE UT-PRARTBTO-USD = UT-PRARTBTO-SEK / W-PRKURS            
025800          MOVE W-PRKURS            TO UT-PRKURS                           
025900          IF PRIS-KDVALISO NOT = 'SEK' AND NOT = SPACE                    
026000            MOVE PRIS-KDVALISO     TO CURR-KDVALISO-ROW                   
026100            CALL W510CURR USING CURR-W510CURR WDG2-PCB                    
026110            IF CURR-KDSVAR = ' '                                          
026120              CONTINUE                                                    
026130            ELSE                                                          
026140              MOVE 1               TO CURR-PRKURS-NEW                     
026150            END-IF                                                        
026200            COMPUTE UT-PRARTBTO-USD ROUNDED =                             
026300                       PRIS-PRARTNTO * W-PRKURS / CURR-PRKURS-NEW         
026400            COMPUTE UT-PRKURS ROUNDED = W-PRKURS / CURR-PRKURS-NEW        
026500          END-IF                                                          
026600          MOVE IN-CLAG-IDARTNR     TO UT-IDARTNR                          
026700          PERFORM S11-SKRIV-W56061                                        
026800       END-IF                                                             
026900     END-IF                                                               
027000                                                                          
027100     .                                                                    
027200     EJECT                                                                
027300 Z-FINIT SECTION.                                                         
027400     CLOSE W01160                                                         
027500           W56061                                                         
027600     SKIP2                                                                
027700     MOVE 'S' TO POSTSUM-OPKOD                                            
027800     CALL POSTSUM USING POSTSUM-PARM                                      
027900     .                                                                    
028000     EJECT                                                                
028100 S01-LAES-W01160  SECTION.                                                
028200     READ W01160 INTO IN-AREA                                             
028300     AT END                                                               
028400        MOVE HIGH-VALUE TO IN-AREA                                        
028500        SET END-OF-W01160 TO TRUE                                         
028600                                                                          
028700     NOT AT END                                                           
028800        MOVE 'W01160' TO POSTSUM-FDNAMN                                   
028900        MOVE 'W56060D1' TO POSTSUM-DDNAMN2                                
029000        CALL POSTSUM USING POSTSUM-PARM                                   
029100     END-READ                                                             
029200     .                                                                    
029300     EJECT                                                                
029400 S11-SKRIV-W56061 SECTION.                                                
029500                                                                          
029600     WRITE UT-POST FROM UT-AREA                                           
029700                                                                          
029800     MOVE 'W56061' TO POSTSUM-FDNAMN                                      
029900     MOVE 'W56060D2' TO POSTSUM-DDNAMN2                                   
030000     CALL POSTSUM USING POSTSUM-PARM                                      
030100     .                                                                    
030200     EJECT                                                                
