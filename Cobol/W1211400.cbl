000100 ID DIVISION.                                                             
000200 PROGRAM-ID.    W1211400.                                                 
000300 AUTHOR.        BODIL LINDAHL.                                            
000400 DATE-WRITTEN.  JUNI 1986.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*       WDD3 UPPDATERAS AV FIL W12113 SOM INNEHÅLLER                      
000900*       BENÄMNINGAR UTAN ARTIKELSEGMENT OCH TAS BORT.                     
001000*                                                                         
001100*    ÄNDRING: 2003-01-16                                                  
001200*       EN UTFIL W12114 SKRIVS TILL W159D2 MED UPPGIFT                    
001300*       OM DE DEFINITIVT BORTTAGNA BENÄMNINGARNA.                         
001400*       FILEN TAS IN I RUTIN W159D2 (INFO TILL NEVIS)                     
001500                                                                          
001600                                                                          
001700 ENVIRONMENT DIVISION.                                                    
001800                                                                          
001900 INPUT-OUTPUT SECTION.                                                    
002000*                                                                         
002100 FILE-CONTROL.                                                            
002200                                                                          
002300*--- INFILER:                                                             
002400*                                                                         
002500     SELECT W12113       ASSIGN TO W12114D1.                              
002600     EJECT                                                                
002700*--- UTFILER:                                                             
002800*                                                                         
002900     SELECT W12114       ASSIGN TO W12114D2.                              
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP2                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W12113                                                               
003600     RECORDING      V                                                     
003700     BLOCK CONTAINS 0.                                                    
003800*01  FILLER    -COPY W12113 -L.                                           
003900     EJECT                                                                
004000 FD  W12114                                                               
004100     RECORDING      F                                                     
004200     BLOCK CONTAINS 0.                                                    
004300*01  UT-POST   -COPY W12114 -L.                                           
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004700*    -- CHECKED BY WY2000                                                 
004800 77  IDPGM                       PIC X(8)    VALUE 'W1211400'.            
004900 77  W12113-EOF                  PIC X(1)    VALUE 'N'.                   
005000 77  JA                          PIC X(1)    VALUE 'J'.                   
005100 77  NEJ                         PIC X(1)    VALUE 'N'.                   
005200 77  MAX-CHKP                PIC S9(5)   VALUE +99   COMP-3.              
005300 77  CHKP-ID                 PIC X(8)    VALUE 'W12114  '.                
005400 77  MSG-IO-AREA-LENGTH          PIC S9(9)   VALUE +32 COMP  SYNC.        
005500 77  MSG-IO-AREA                 PIC X(32)   VALUE SPACE.                 
005600 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32 COMP  SYNC.        
005700 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
005800     SKIP3                                                                
005900*                            *** ARBETSAREA                               
006000 01  W-ARBETS-AREA.                                                       
006100   03  W-CHKP                    PIC S9(5)   VALUE ZERO   COMP-3.         
006200     SKIP3                                                                
006300*                            *** NYCKLAR TILL DLI                         
006400 01  NYCKLAR-TILL-DLI.                                                    
006500   03  W-IDBENNR-X.                                                       
006600     05  W-IDBENNR               PIC S9(7)                COMP-3.         
006700*                            *** GENERELLA SUBRUTINER                     
006800 01  SUBPROGRAM.                                                          
006900   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
007000   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
007100   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
007200     EJECT                                                                
007300*01  -COPY W0005            -PRE POSTSUM-.                                
007400     EJECT                                                                
007500 01  FILLER                      PIC X(24) VALUE                          
007600                                    'IN-AREA-START'.                      
007700*01  AREA    -COPY W12113       -PRE IN-.                                 
007800     EJECT                                                                
007900 01  FILLER                      PIC X(24) VALUE                          
008000                                    'UT-AREA-START'.                      
008100*01  AREA    -COPY W12114       -PRE UT-.                                 
008200     EJECT                                                                
008300*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
008400*                                                                         
008500 01  IMS-WS.                                                              
008600   03  FILLER                    PIC X(8)    VALUE 'IMS-WS  '.            
008700     SKIP3                                                                
008800*                            *** STATUSKOD FRÅN IMS                       
008900   03  STATUS-WS                 PIC XX.                                  
009000     88  SEGMENT-FINNS                       VALUE '  '.                  
009100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009200     88  IMS-EJ-OK                           VALUE 'XD'.                  
009300     SKIP3                                                                
009400   03  GODK-STATUSKODER.                                                  
009500     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009600     SKIP3                                                                
009700   03  SSA1                      PIC X(64).                               
009800     EJECT                                                                
009900*01  -COPY W0003                                                          
010000     EJECT                                                                
010100 01  DLI-IO-AREA.                                                         
010200   03  IO-AREA                  PIC X(100)  VALUE SPACE.                  
010300*  03  WLBENA01  -COPY WDD301                 -RED IO-AREA.               
010400     EJECT                                                                
010500*  03  WLBENA12  -COPY WDD312                 -RED IO-AREA.               
010600     EJECT                                                                
010700 LINKAGE SECTION.                                                         
010800*01    -COPY W0009      -PRE MSG-                                         
010900                                                                          
011000*01    -COPY W0008      -PRE BENA-                                        
011100     05  FILLER        PIC X.                                             
011200     EJECT                                                                
011300 PROCEDURE DIVISION USING MSG-PCB BENA-PCB.                               
011400 MAIN SECTION.                                                            
011500     PERFORM A-INIT                                                       
011600     PERFORM S01-LAS-W12113                                               
011700     PERFORM UNTIL W12113-EOF = JA                                        
011800                                                                          
011900       MOVE IN-BENA-IDBENNR    TO W-IDBENNR                               
012000       PERFORM IMS-GET-BENA01                                             
012100       IF SEGMENT-FINNS                                                   
012200         PERFORM IMS-GET-BENA12                                           
012300         IF SEGMENT-FINNS                                                 
012400           DISPLAY 'IDBENNR' W-IDBENNR 'HAR ARTIKLAR'                     
012500         ELSE                                                             
012600           PERFORM IMS-GET-BENA01                                         
012700           MOVE BEN-IDBENNR   TO UT-DLET-IDBENNR                          
012800           MOVE BEN-KDHOMONYM TO UT-DLET-KDHOMONYM                        
012900           PERFORM S11-SKRIV-W12114                                       
013000                                                                          
013100           PERFORM IMS-DLET-BENA01                                        
013200           ADD +1            TO W-CHKP                                    
013300         END-IF                                                           
013400       END-IF                                                             
013500       IF W-CHKP > MAX-CHKP                                               
013600         PERFORM B-TAG-CHECK-POINT                                        
013700       END-IF                                                             
013800       PERFORM S01-LAS-W12113                                             
013900     END-PERFORM                                                          
014000     PERFORM Z-FINIT                                                      
014100                                                                          
014200     MOVE ZERO TO RETURN-CODE                                             
014300     GOBACK                                                               
014400     .                                                                    
014500     EJECT                                                                
014600 A-INIT SECTION.                                                          
014700                                                                          
014800     OPEN INPUT W12113                                                    
014900     OPEN OUTPUT W12114                                                   
015000                                                                          
015100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015200     MOVE 'W12114' TO POSTSUM-FDNAMN                                      
015300                                                                          
015400     PERFORM IMS-RESTART                                                  
015500     .                                                                    
015600     EJECT                                                                
015700 B-TAG-CHECK-POINT SECTION.                                               
015800                                                                          
015900     PERFORM IMS-CHECK-POINT                                              
016000     MOVE ZERO               TO W-CHKP                                    
016100     .                                                                    
016200     EJECT                                                                
016300 Z-FINIT SECTION.                                                         
016400                                                                          
016500     CLOSE W12113                                                         
016600     CLOSE W12114                                                         
016700                                                                          
016800     MOVE 'S' TO POSTSUM-OPKOD                                            
016900     CALL POSTSUM USING POSTSUM-PARM                                      
017000     .                                                                    
017100     EJECT                                                                
017200 S01-LAS-W12113 SECTION.                                                  
017300                                                                          
017400     READ W12113 INTO IN-AREA                                             
017500     AT END                                                               
017600        MOVE JA TO W12113-EOF                                             
017700     END-READ                                                             
017800                                                                          
017900     IF W12113-EOF = NEJ                                                  
018000       MOVE 'W12110D1' TO POSTSUM-DDNAMN2                                 
018100       CALL POSTSUM USING POSTSUM-PARM                                    
018200     END-IF                                                               
018300     .                                                                    
018400     EJECT                                                                
018500 S11-SKRIV-W12114 SECTION.                                                
018600     SKIP2                                                                
018700     WRITE UT-POST FROM UT-AREA                                           
018800                                                                          
018900     MOVE 'UT-'     TO POSTSUM-TRANSTYP                                   
019000     MOVE 'W12114' TO POSTSUM-FDNAMN                                      
019100     MOVE 'W12114D2' TO POSTSUM-DDNAMN2                                   
019200     CALL POSTSUM USING POSTSUM-PARM                                      
019300     .                                                                    
019400     EJECT                                                                
019500* IMS SECTIONER                                                           
019600                                                                          
019700 IMS-RESTART SECTION.                                                     
019800     MOVE SPACE TO MSG-IO-AREA                                            
019900     MOVE '  ' TO  GODK-STATUSKODER                                       
020000     CALL CBLTDLI USING XRST MSG-PCB                                      
020100                             MSG-IO-AREA-LENGTH                           
020200                             MSG-IO-AREA                                  
020300                             CHKP-AREA-1-LENGTH                           
020400                             CHKP-AREA-1                                  
020500                                                                          
020600     MOVE MSG-STATUS-CODE    TO STATUS-WS                                 
020700     PERFORM IMS-STATUSKONTROLL                                           
020800     .                                                                    
020900                                                                          
021000                                                                          
021100 IMS-CHECK-POINT SECTION.                                                 
021200     MOVE CHKP-ID TO MSG-IO-AREA                                          
021300     MOVE '  XD' TO  GODK-STATUSKODER                                     
021400     CALL CBLTDLI USING CHKP MSG-PCB                                      
021500                             MSG-IO-AREA-LENGTH                           
021600                             MSG-IO-AREA                                  
021700                             CHKP-AREA-1-LENGTH                           
021800                             CHKP-AREA-1                                  
021900                                                                          
022000     MOVE MSG-STATUS-CODE    TO STATUS-WS                                 
022100     PERFORM IMS-STATUSKONTROLL                                           
022200     IF IMS-EJ-OK                                                         
022300       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
022400       CALL FELLOG                                                        
022500     END-IF                                                               
022600     .                                                                    
022700                                                                          
022800     EJECT                                                                
022900 IMS-GET-BENA01 SECTION.                                                  
023000     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
023100            DELIMITED BY SIZE INTO SSA1                                   
023200     MOVE 'GE  '  TO GODK-STATUSKODER                                     
023300     CALL CBLTDLI USING GHU BENA-PCB DLI-IO-AREA SSA1                     
023400     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
023500     PERFORM IMS-STATUSKONTROLL                                           
023600     .                                                                    
023700     SKIP3                                                                
023800 IMS-GET-BENA12 SECTION.                                                  
023900     MOVE 'WLBENA12 ' TO SSA1                                             
024000     MOVE 'GE  '   TO GODK-STATUSKODER                                    
024100     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
024200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
024300     PERFORM IMS-STATUSKONTROLL                                           
024400     .                                                                    
024500     SKIP3                                                                
024600 IMS-DLET-BENA01 SECTION.                                                 
024700     MOVE '  '   TO GODK-STATUSKODER                                      
024800     CALL CBLTDLI USING DLET BENA-PCB DLI-IO-AREA                         
024900     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
025000     PERFORM IMS-STATUSKONTROLL                                           
025100     .                                                                    
025200     EJECT                                                                
025300 IMS-STATUSKONTROLL SECTION.                                              
025400     SET STATUS-IX TO 1                                                   
025500     SEARCH GODK-STATUS AT END CALL FELLOG                                
025600     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
025700       CONTINUE                                                           
025800     END-SEARCH                                                           
025900     .                                                                    
