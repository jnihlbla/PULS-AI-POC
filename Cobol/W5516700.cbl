000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5516700.                                                
000300 AUTHOR.         STINA MOGREN.                                            
000400 DATE-WRITTEN.   04/01/12.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*      - KOPIERAR PRISFIL INNEHÅLLANDE STANDARDPRISER TILL                
000900*        ÅRSKURS                                                          
001000*        PRISERNA RÄKNAS OM TILL MARKNADSBOLAGSVALUTA                     
001100*                                                                         
001200*    ABENDKODER:                                                          
001300*        U0016 -  . . . .                                                 
001400                                                                          
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*       --- UPPGIFT OM MARKNADSBOLAG                                      
002300     SELECT W551TYP                    ASSIGN TO W55167D1.                
002400*       --- INFIL FRÅN W55160                                             
002500     SELECT W55161                     ASSIGN TO W55167D2.                
002600     SKIP2                                                                
002700*       --- SAMMA SOM IN, MED ÄNDRAD SJÄLVKOST                            
002800     SELECT W55167                     ASSIGN TO W55167D3.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP2                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W551TYP                                                              
003500     LABEL RECORD STANDARD                                                
003600     RECORDING F                                                          
003700     BLOCK CONTAINS 0.                                                    
003800                                                                          
003900 01  FILLER                  PIC X(80).                                   
004000                                                                          
004100 FD  W55161                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  -COPY W551611       -L.                                              
004600     SKIP3                                                                
004700 FD  W55167                                                               
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000                                                                          
005100*01  POST -COPY W551611  -PRE  UT-  -L.                                   
005200     EJECT                                                                
005300 WORKING-STORAGE SECTION.                                                 
005400                                                                          
005500 77  IDPGM                       PIC X(8)      VALUE 'W5516700'.          
005600 77  JA                          PIC X         VALUE 'J'.                 
005700 77  NEJ                         PIC X         VALUE 'N'.                 
005800                                                                          
005900 77  WS-KDARTURS                 PIC 9(2)      VALUE ZERO.                
006000 77  WS-IDLEVNR                  PIC 9(5)      VALUE ZERO.                
006100 77  WS-IDLEVNR-LOC              PIC 9(5)      VALUE ZERO.                
006200                                                                          
006300 01  WS-PRARTSJK                 PIC 9(7)V9(2) VALUE ZERO.                
006400                                                                          
006600 77  WS-IDMARKBO                 PIC X      VALUE SPACE.                  
006700                                                                          
006800 77  W55161-EOF-SW               PIC X         VALUE 'N'.                 
006900     88  END-OF-W55161                         VALUE 'J'.                 
007000                                                                          
007100 01  DAGENS-DATUM                PIC 9(8)      VALUE ZERO.                
007200     EJECT                                                                
007300 01  WS-AAAAMMDD.                                                         
007400     03  WS-SEKEL                       PIC 9(2).                         
007500     03  WS-AAMMDD                      PIC 9(6).                         
007600 01  WS-TIFINLV REDEFINES WS-AAAAMMDD   PIC 9(8).                         
007700*                                                                         
007800 01  DYNAMISKA-SUBPROGRAM.                                                
007900*                                                                         
008000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008400     03  W335CURR                PIC X(8)    VALUE 'W335CURR'.            
008500     SKIP2                                                                
008900                                                                          
009000* INFO OM KÖRTYP(M5) FRÅN CONSTANTMEDLEM VALD AV JCL'EN                   
009100* INFON KOMMER SOM FIL D1                                                 
009200                                                                          
009300 01  W551TYP-POST.                                                        
009400     03  TYP-PARAMETER        PIC X(2).                                   
009500         88 EJ-MARKNAD        VALUE 'M '.                                 
009600         88 ME-MARKNAD        VALUE 'ME'.                                 
009700     03  FILLER               REDEFINES TYP-PARAMETER.                    
009800       05  TYP-M              PIC X.                                      
009900       05  TYP-BOLAG          PIC X.                                      
010000     03  FILLER               PIC X(78).                                  
010100                                                                          
010200 01  FELTEXT.                                                             
010300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010500     EJECT                                                                
010600*    --- PARAMETRAR TILL SUBPROGRAM W335CURR                              
010700*01 -COPY W335CURR                                                        
010800     EJECT                                                                
010900*    --- PARAMETRAR TILL POSTSUM                                          
011000*                                                                         
011100*01  -COPY W0005   -PRE  POSTSUM-                                         
011200     EJECT                                                                
011300 01  FILLER                      PIC X(16) VALUE 'WDATAREA'.              
011400*01 -COPY WDATAREA                                                        
011500     EJECT                                                                
011600 01  FILLER                      PIC X(24)   VALUE 'IN-AREA'.             
011700                                                                          
011800*01  AREA -COPY W551611      -PRE IN-                                     
011900     EJECT                                                                
012000 01  FILLER                      PIC X(24)   VALUE 'UT-AREA'.             
012100                                                                          
012200*01  AREA -COPY W551611      -PRE UT-                                     
012300     EJECT                                                                
012400*    --- ARBETS-AREOR TILL  IMS-SEKTIONERNA                               
012500*                                                                         
012600 01  W-KDVALISO                  PIC X(3)    VALUE SPACE.                 
012700                                                                          
012800 01  NYCKLAR-TILL-DLI.                                                    
012900     03  W-WDGX5117-X.                                                    
013000         05  W-WDGX5117          PIC X(4)    VALUE '5117'.                
013100         05  W-WDGXTIAA          PIC S9(3)   VALUE ZERO  COMP-3.          
013200         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
013300     03  W-WDGX5118-X.                                                    
013400         05  W-WDGX5118          PIC X(3)    VALUE SPACE.                 
013500         05  FILLER              PIC X(2)    VALUE LOW-VALUE.             
013600                                                                          
013700                                                                          
013800*    --- STATUS-KOD FRÅN IMS                                              
013900 01  STATUS-WS                   PIC XX.                                  
014000     88  SEGMENT-FINNS                       VALUE '  '.                  
014100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014300     88  BASEN-SLUT                          VALUE 'GB'.                  
014400     SKIP2                                                                
014500 01  GODK-STATUSKODER.                                                    
014600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014700     SKIP3                                                                
014800 01  SSA1                        PIC X(64).                               
014900 01  SSA2                        PIC X(64).                               
015000     EJECT                                                                
015100*    --- IMS FUNKTIONSKODER                                               
015200*01  -COPY W0003                                                          
015300     EJECT                                                                
015400 01  DLI-IO-WDGX5118.                                                     
015500*    03  WDR220   -COPY WDGX5118                                          
015600     EJECT                                                                
015700 LINKAGE SECTION.                                                         
015800*01  -COPY W0008      -PRE WDR2-                                          
015900     05  FILLER                  PIC X.                                   
016000     EJECT                                                                
016100 PROCEDURE DIVISION   USING  WDR2-PCB.                                    
016200                                                                          
016300 MAIN SECTION.                                                            
016400     ENTRY 'DLITCBL'  USING  WDR2-PCB.                                    
016500                                                                          
016600     PERFORM A-INIT                                                       
016700                                                                          
016800     PERFORM S01-LAES-W55161                                              
016900     PERFORM UNTIL END-OF-W55161                                          
017000         PERFORM B-FLYTTA-SKRIV-UTPOST                                    
017100         PERFORM S01-LAES-W55161                                          
017200     END-PERFORM                                                          
017300                                                                          
017400     PERFORM Z-FINIT                                                      
017500                                                                          
017600     MOVE ZERO TO RETURN-CODE                                             
017700     GOBACK                                                               
017800     .                                                                    
017900     EJECT                                                                
017910                                                                          
018000 A-INIT SECTION.                                                          
018200     OPEN INPUT  W55161                                                   
018300          INPUT  W551TYP                                                  
018400          OUTPUT W55167                                                   
018500                                                                          
018600     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
018700                                                                          
018800     PERFORM S04-LAES-W551TYP                                             
018900     .                                                                    
019000     EJECT                                                                
019010                                                                          
019100 B-FLYTTA-SKRIV-UTPOST SECTION.                                           
019300     MOVE IN-W551611         TO UT-W551611                                
019400     PERFORM S05-MARKNADSBOLAGS-VALUTA                                    
019500                                                                          
019600     WRITE UT-POST FROM UT-AREA                                           
019700                                                                          
019800     MOVE 'W55167'   TO POSTSUM-FDNAMN                                    
019900     MOVE 'W55167D3' TO POSTSUM-DDNAMN2                                   
020000     CALL POSTSUM USING POSTSUM-PARM                                      
020100     .                                                                    
020110                                                                          
020200 Z-FINIT SECTION.                                                         
020400     CLOSE W55161                                                         
020500           W55167                                                         
020600           W551TYP                                                        
020700                                                                          
020800     MOVE 'S'        TO POSTSUM-OPKOD                                     
020900     CALL POSTSUM USING POSTSUM-PARM                                      
021000     .                                                                    
021100     EJECT                                                                
021110                                                                          
021200 S01-LAES-W55161  SECTION.                                                
021400     READ W55161 INTO IN-AREA                                             
021500     AT END                                                               
021600        SET END-OF-W55161 TO TRUE                                         
021700     NOT AT END                                                           
021800        MOVE 'W55161'   TO POSTSUM-FDNAMN                                 
021900        MOVE 'W55167D2' TO POSTSUM-DDNAMN2                                
022000        CALL POSTSUM USING POSTSUM-PARM                                   
022100     END-READ                                                             
022200     .                                                                    
022300     EJECT                                                                
022310                                                                          
022400 S04-LAES-W551TYP   SECTION.                                              
022600     READ W551TYP             INTO W551TYP-POST                           
022700     MOVE TYP-BOLAG                   TO WS-IDMARKBO                      
022800     IF WS-IDMARKBO NOT = SPACE                                           
022900       PERFORM S06-HAMTA-MARKNADSVALUTA                                   
023000     END-IF                                                               
023100     .                                                                    
023200     SKIP3                                                                
023210                                                                          
023300 S05-MARKNADSBOLAGS-VALUTA  SECTION.                                      
023500     IF WS-IDMARKBO NOT = SPACE                                           
023600                                                                          
023700       MOVE ZERO                      TO CURR-SUORDV-IN                   
023800                                         CURR-PRARTVNA-IN                 
023900                                         CURR-PRARTSTD-IN                 
024000                                         CURR-PRKURS-02                   
024100*                                                                         
024200*      ÅRS-KURSEN                                                         
024300       COMPUTE CURR-PRKURS = 5118-PRKURS / 100                            
024400       MOVE ZERO                      TO CURR-PRARTSJK-IN                 
024500       MOVE UT-PRARTSTD-KOM           TO CURR-PRARTSTD-IN                 
024600       MOVE W-KDVALISO                TO CURR-KDVALISO-01                 
024700       MOVE +2                        TO CURR-KDCALL                      
024800       CALL W335CURR            USING CURR-W335CURR                       
024900       MOVE CURR-PRARTSTD-UT          TO UT-PRARTSTD-KOM                  
025000     END-IF                                                               
025100     .                                                                    
025200     SKIP2                                                                
025300                                                                          
025400 S06-HAMTA-MARKNADSVALUTA  SECTION.                                       
025600     MOVE 'SEK'                     TO W-KDVALISO                         
025700     MOVE DAGENS-DATUM(3:2)         TO W-WDGXTIAA                         
025800     MOVE W-KDVALISO                TO W-WDGX5118                         
025900     PERFORM IMS-GU-WDGX5118                                              
026000     .                                                                    
026100     EJECT                                                                
026110                                                                          
026200 IMS-GU-WDGX5118 SECTION.                                                 
026400     STRING 'WDR201  (WDGXKEY  =' W-WDGX5117-X ')'                        
026500          DELIMITED BY SIZE INTO SSA1                                     
026600     STRING 'WDR220  (WDGXKEY  =' W-WDGX5118-X ')'                        
026700          DELIMITED BY SIZE INTO SSA2                                     
026800     MOVE '  GE' TO GODK-STATUSKODER                                      
026900     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX5118                       
027000                        SSA1 SSA2                                         
027100     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
027200     PERFORM IMS-STATUSKONTROLL                                           
027300     .                                                                    
027400     EJECT                                                                
027410                                                                          
027500 IMS-STATUSKONTROLL SECTION.                                              
027700     SET STATUS-IX TO 1                                                   
027800     SEARCH GODK-STATUS                                                   
027900       AT END CALL FELLOG                                                 
028000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
028100     END-SEARCH                                                           
028200     .                                                                    
028300     EJECT                                                                
