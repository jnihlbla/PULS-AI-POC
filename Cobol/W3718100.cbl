000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3718100.                                                
000300 AUTHOR.         BO HAMMARIN.                                             
000400 DATE-WRITTEN.   MAY 2000.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION: PGM UPPDATERAR SEGMENT PÅ WDR4 (WDGX3161/62)               
000800*                                                                         
000900*    INDATA:   W37180 BORTTAGS-POSTER REMAN CONFIRMATION                  
001000*                                                                         
001100*    RETURKODER:                                                          
001200*              999 (DUMPKOD VID FELLOG ).                                 
001300*                                                                         
001400*    CHANGE LOG:                                                          
001500*                                                                         
001600*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
001700*      -----------------------------------------------------              
001800*      14/11/19 - REDDY RAHUL     - CHANGES TO INPUT FILE.                
001900*                                   E'TRACKER 10193018                    
002000*                                                                         
002100     EJECT                                                                
002200                                                                          
002300 ENVIRONMENT DIVISION.                                                    
002400 INPUT-OUTPUT SECTION.                                                    
002500 FILE-CONTROL.                                                            
002600     SELECT W37180     ASSIGN  TO  UT-S-W37181D1.                         
002700                                                                          
002800 DATA DIVISION.                                                           
002900                                                                          
003000 FILE SECTION.                                                            
003100 FD  W37180                                                               
003200     RECORDING F                                                          
003300     BLOCK 0 RECORDS.                                                     
003400                                                                          
003500*01  POST -COPY W37180 -PRE  IN-  -L.                                     
003600     EJECT                                                                
003700                                                                          
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                   PIC X(8)            VALUE 'W3718100'.        
004200 77  JA                      PIC X               VALUE 'J'.               
004300 77  NEJ                     PIC X               VALUE 'N'.               
004400                                                                          
004500 01  CHKP-VAR.                                                            
004600   03  CHKP-MSG-IO-AREA-LENGTH   PIC S9(9)   VALUE +32 COMP SYNC.         
004700   03  CHKP-MSG-IO-AREA          PIC X(32)   VALUE SPACE.                 
004800   03  CHKP-AREA-LENGTH          PIC S9(9)   VALUE +32 COMP SYNC.         
004900   03  CHKP-AREA                 PIC X(32)   VALUE SPACE.                 
005000   03  CHKP-ANT                  PIC S9(3)   VALUE +0.                    
005100   03  CHKP-MAX                  PIC S9(3)   VALUE +100.                  
005200                                                                          
005300 01  FELTEXT.                                                             
005400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005600                                                                          
005700 01  DIVERSE.                                                             
005800     03  W37180-EOF              PIC X                 VALUE 'N'.         
005900     03  RETURKOD                PIC S9(4)   COMP SYNC VALUE +0.          
006000   EJECT                                                                  
006100                                                                          
006200 01  FILLER                      PIC X(8)    VALUE 'NYCKLAR*'.            
006300 01  NYCKLAR-TILL-DLI.                                                    
006400     03  W-WDGX3161-X.                                                    
006500         05  W-IDHTYP-3161       PIC  X(4)   VALUE '3161'.                
006600         05  W-IDDISTR-3161      PIC S9(5) COMP-3 VALUE ZERO.             
006700         05  FILLER              PIC  X(23)  VALUE LOW-VALUE.             
006800     03  W-WDGX3162-MIN-X.                                                
006900         05  W-DAORDREG-3162-MIN PIC  9(8) VALUE ZERO.                    
007000         05  W-IDORDER-3162-MIN  PIC S9(7) COMP-3 VALUE ZERO.             
007100         05  W-IDARTNR-3162-MIN  PIC  X(5) VALUE LOW-VALUES.              
007200         05  W-IDRADNR-3162-MIN  PIC  X(3) VALUE LOW-VALUES.              
007300     03  W-WDGX3162-MAX-X.                                                
007400         05  W-DAORDREG-3162-MAX PIC  9(8) VALUE ZERO.                    
007500         05  W-IDORDER-3162-MAX  PIC S9(7) COMP-3 VALUE ZERO.             
007600         05  W-IDARTNR-3162-MAX  PIC  X(5) VALUE HIGH-VALUES.             
007700         05  W-IDRADNR-3162-MAX  PIC  X(3) VALUE HIGH-VALUES.             
007800                                                                          
007900                                                                          
008000 01  GENERELLA-SUBPROGRAM.                                                
008100     03  FELLOG                  PIC X(8)      VALUE 'FELLOG  '.          
008200     03  CBLTDLI                 PIC X(8)      VALUE 'CBLTDLI '.          
008300     03  POSTSUM                 PIC X(8)      VALUE 'POSTSUM '.          
008400     03  ABEND                   PIC X(8)      VALUE 'ABEND   '.          
008500                                                                          
008600*---- PARAMETRAR TILL ABEND                                               
008700 01  RETURKODER.                                                          
008800     03  RKOD                    PIC S9(4) COMP SYNC VALUE ZERO.          
008900     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4) COMP SYNC VALUE +16.           
009000     03  RKOD-ABEND-MED-DUMP     PIC S9(4) COMP SYNC VALUE +1000.         
009100     EJECT                                                                
009200                                                                          
009300 01  FILLER                      PIC X(16) VALUE '*****W37180'.           
009400*01  -COPY W37180  -PRE IN-                                               
009500     EJECT                                                                
009600*-----------------------------------------PARAMETRAR TILL                 
009700*                                         SUBPROGRAM POSTSUM              
009800 01  FILLER             PIC X(7)   VALUE 'POSTSUM'.                       
009900*01  -COPY W0005   -PRE POSTSUM-.                                         
010000     EJECT                                                                
010100*-----------------------------------------PARAMETRAR TILL                 
010200*                                         SUBPROGRAM WDATKONV             
010300 01  FILLER             PIC X(8)   VALUE 'WDATKONV'.                      
010400*01  -COPY WDATAREA.                                                      
010500     EJECT                                                                
010600                                                                          
010700*    SPARAREOR FÖR DATABASSEGMENT                                         
010800*                                                                         
010900 01  FILLER         PIC X(16) VALUE 'DLI-IO-3161'.                        
011000 01  DLI-IO-WDGX3161.                                                     
011100*    03  -COPY WDGX3161                                                   
011200     EJECT                                                                
011300                                                                          
011400 01  FILLER         PIC X(16) VALUE 'DLI-IO-3162'.                        
011500 01  DLI-IO-WDGX3162.                                                     
011600*    03  -COPY WDGX3162                                                   
011700     EJECT                                                                
011800*****                                                                     
011900*****    DIV-AREOR TILL IMS-SEKTIONERNA                                   
012000*****                                                                     
012100 01  IMS-WORKAREOR.                                                       
012200     03  FILLER          PIC X(16)   VALUE '*-*-*IMS-WS*-*-*'.            
012300     03  STATUS-WS       PIC XX.                                          
012400         88  SEGMENT-FINNS       VALUE '  '.                              
012500         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
012600         88  BASEN-SLUT          VALUE 'GB'.                              
012700         88  IMS-EJ-OK           VALUE 'XD'.                              
012800     03  GODK-STATUSKODER.                                                
012900         05  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.            
013000                                                                          
013100 01      SSA1            PIC X(128).                                      
013200 01      SSA2            PIC X(128).                                      
013300 01      SSA3            PIC X(128).                                      
013400     EJECT                                                                
013500*                                                                         
013600*        IMS FUNKTIONSKODER                                               
013700*                                                                         
013800*01      -COPY W0003                                                      
013900     EJECT                                                                
014000                                                                          
014100 LINKAGE SECTION.                                                         
014200*  MSG                                                                    
014300*01  -COPY W0009     -PRE MSG-                                            
014400   EJECT                                                                  
014500*01  -COPY W0008     -PRE 3161-                                           
014600     05  FILLER      PIC X(1).                                            
014700   EJECT                                                                  
014800                                                                          
014900 PROCEDURE DIVISION  USING MSG-PCB 3161-PCB.                              
015000 MAIN SECTION.                                                            
015100     ENTRY 'DLITCBL' USING MSG-PCB 3161-PCB.                              
015200                                                                          
015300     PERFORM A-INITIERING                                                 
015400                                                                          
015500     PERFORM B-BEARBETNING                                                
015600                                                                          
015700     PERFORM C-AVSLUTNING                                                 
015800                                                                          
015900     MOVE +0 TO RETURN-CODE                                               
016000     GOBACK                                                               
016100     .                                                                    
016200     EJECT                                                                
016300                                                                          
016400 A-INITIERING SECTION.                                                    
016500     PERFORM IMS-RESTART                                                  
016600                                                                          
016700     MOVE 'W37181' TO POSTSUM-PROGNAMN                                    
016800                                                                          
016900     OPEN INPUT W37180                                                    
017000     .                                                                    
017100     EJECT                                                                
017200                                                                          
017300 B-BEARBETNING SECTION.                                                   
017400     PERFORM BA-READ-W37180                                               
017500                                                                          
017600     PERFORM UNTIL W37180-EOF = JA                                        
017700       MOVE IN-IDDISTR        TO W-IDDISTR-3161                           
017800       MOVE IN-DAORDREG       TO W-DAORDREG-3162-MIN                      
017900                                 W-DAORDREG-3162-MAX                      
018000       MOVE IN-IDORDER        TO W-IDORDER-3162-MIN                       
018100                                 W-IDORDER-3162-MAX                       
018200       PERFORM IMS-GU-WDGX3161                                            
018300       PERFORM IMS-GHN-WDGX3162                                           
018400                                                                          
018500       PERFORM                                                            
018600         UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                               
018700         PERFORM IMS-DLET-WDGX3162                                        
018800         ADD +1               TO CHKP-ANT                                 
018900         PERFORM IMS-GHN-WDGX3162                                         
019000       END-PERFORM                                                        
019100                                                                          
019200       IF CHKP-ANT > CHKP-MAX                                             
019300         PERFORM X-TAG-CHECKPOINT                                         
019400       END-IF                                                             
019500                                                                          
019600       PERFORM BA-READ-W37180                                             
019700     END-PERFORM                                                          
019800     .                                                                    
019900     EJECT                                                                
020000                                                                          
020100 BA-READ-W37180 SECTION.                                                  
020200     READ W37180 INTO IN-W37180                                           
020300     AT END                                                               
020400        MOVE JA         TO W37180-EOF                                     
020500     NOT AT END                                                           
020600        MOVE 'W37180'   TO POSTSUM-FDNAMN                                 
020700        MOVE 'W37181D1' TO POSTSUM-DDNAMN2                                
020800        CALL POSTSUM USING POSTSUM-PARM                                   
020900     END-READ                                                             
021000     .                                                                    
021100     EJECT                                                                
021200                                                                          
021300 C-AVSLUTNING SECTION.                                                    
021400     CLOSE W37180                                                         
021500                                                                          
021600     MOVE 'S' TO POSTSUM-OPKOD                                            
021700     CALL POSTSUM USING POSTSUM-PARM                                      
021800     .                                                                    
021900     EJECT                                                                
022000                                                                          
022100 X-TAG-CHECKPOINT   SECTION.                                              
022200     PERFORM IMS-CHECKPOINT                                               
022300     MOVE ZERO TO CHKP-ANT                                                
022400     .                                                                    
022500     EJECT                                                                
022600                                                                          
022700 IMS-RESTART SECTION.                                                     
022800     MOVE SPACE           TO CHKP-MSG-IO-AREA                             
022900     MOVE '  '            TO GODK-STATUSKODER                             
023000     CALL CBLTDLI USING XRST MSG-PCB                                      
023100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
023200                        CHKP-AREA-LENGTH CHKP-AREA                        
023300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
023400     PERFORM IMS-STATUSKONTROLL                                           
023500     .                                                                    
023600                                                                          
023700 IMS-CHECKPOINT SECTION.                                                  
023800     MOVE SPACE           TO CHKP-MSG-IO-AREA                             
023900     MOVE '  XD'          TO GODK-STATUSKODER                             
024000     CALL CBLTDLI USING CHKP MSG-PCB                                      
024100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
024200                        CHKP-AREA-LENGTH CHKP-AREA                        
024300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024400     PERFORM IMS-STATUSKONTROLL                                           
024500                                                                          
024600     IF IMS-EJ-OK                                                         
024700       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
024800       DISPLAY FELTEXT                                                    
024900       CALL FELLOG                                                        
025000     END-IF                                                               
025100     .                                                                    
025110     EJECT                                                                
025111                                                                          
025120 IMS-GU-WDGX3161 SECTION.                                                 
025140     STRING 'WDR401  (WDGXKEY  =' W-WDGX3161-X ')'                        
025150             DELIMITED BY SIZE INTO SSA1                                  
025160     MOVE '  '                 TO GODK-STATUSKODER                        
025170     CALL CBLTDLI USING GU 3161-PCB DLI-IO-WDGX3161 SSA1                  
025180     MOVE 3161-STATUS-CODE       TO STATUS-WS                             
025190     PERFORM IMS-STATUSKONTROLL                                           
025191     .                                                                    
025200     EJECT                                                                
025300                                                                          
025400 IMS-GHN-WDGX3162 SECTION.                                                
025700     STRING 'WDGX3162(KY3162  >=' W-WDGX3162-MIN-X                        
025800                    '&KY3162  <=' W-WDGX3162-MAX-X ')'                    
025900          DELIMITED BY SIZE INTO SSA1                                     
026000     MOVE '  GEGB'         TO GODK-STATUSKODER                            
026100     CALL CBLTDLI USING GHN 3161-PCB DLI-IO-WDGX3162 SSA1                 
026200     MOVE 3161-STATUS-CODE TO STATUS-WS                                   
026300     PERFORM IMS-STATUSKONTROLL                                           
026400     .                                                                    
026500                                                                          
026600 IMS-DLET-WDGX3162 SECTION.                                               
026700     MOVE '  '             TO GODK-STATUSKODER                            
026800     CALL CBLTDLI USING DLET 3161-PCB DLI-IO-WDGX3162                     
026900     MOVE 3161-STATUS-CODE TO STATUS-WS                                   
027000     PERFORM IMS-STATUSKONTROLL                                           
027100     .                                                                    
027200     EJECT                                                                
027300                                                                          
027400 IMS-STATUSKONTROLL SECTION.                                              
027500     SET STATUS-IX TO 1                                                   
027600     SEARCH GODK-STATUS AT END CALL FELLOG                                
027700     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
027800     CONTINUE                                                             
027900     END-SEARCH                                                           
028000     .                                                                    
