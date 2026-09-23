000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6124400.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   99/10/05.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        NEDLÄSNING HTR WDR301                                            
001000*        SDC REFILL  AVVIKELSE VID INLEVERANS                             
001100*                                                                         
001200*        PROGRAMMET LÄSER WDR301                                          
001300*                         WDK6                                            
001400*        SKAPAR FIL W61244 LISTFIL AVVIKELSE                              
001500*                   W61245 FÖR BORTTAG HTR                                
001600*                                                                         
001700                                                                          
001800 ENVIRONMENT DIVISION.                                                    
001900                                                                          
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300                                                                          
002400*          --- LISTFIL AVVIKELSE                                          
002500     SELECT W61244                     ASSIGN TO W61244D1.                
002600*          --- BORTTAG HTR                                                
002700     SELECT W61245                     ASSIGN TO W61244D2.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200                                                                          
003300 FD  W61244                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600*01  POST -COPY W61244  -PRE  UT-     -L.                                 
003700     EJECT                                                                
003800 FD  W61245                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100*01  POST -COPY WDR301  -PRE  BORT-   -L.                                 
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400*    -- CHECKED BY WY2000                                                 
004500                                                                          
004600 77  IDPGM                       PIC X(8)    VALUE 'W6124400'.            
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
005000 77  TRAEFF-SW                   PIC X       VALUE 'N'.                   
005100                                                                          
005200 01  FELTEXT.                                                             
005300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005500                                                                          
005600 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
005700     EJECT                                                                
005800 01  DYNAMISKA-SUBPROGRAM.                                                
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006200     EJECT                                                                
006300*    --- PARAMETRAR TILL POSTSUM                                          
006400*01  -COPY W0005   -PRE  POSTSUM-                                         
006500*                                                                         
006600     EJECT                                                                
006700 01  UT-AREA-START               PIC X(16)   VALUE                        
006800                                             'UT-AREA-START'.             
006900*01  AREA -COPY W61244      -PRE UT-                                      
007000     EJECT                                                                
007100 01  LIST-AREA-START             PIC X(16)   VALUE                        
007200                                             'LIST-AREA-START'.           
007300 01  BORT-AREA-START             PIC X(16)   VALUE                        
007400                                             'BORT-AREA-START'.           
007500*01  AREA -COPY WDR301      -PRE BORT-                                    
007600     EJECT                                                                
007700 01  NYCKLAR-TILL-DLI.                                                    
007800     03  W-WDR301KY-X.                                                    
007900         05  W-WDR301KY          PIC X(27)    VALUE SPACE.                
008000     03  W-IDCPYTXT-X.                                                    
008100         05  W-IDCPYTXT          PIC X(08)    VALUE 'W61244  '.           
008200     03  W-IDARTNR-X.                                                     
008300         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
008400     03  W-KDSEGKEY-X.                                                    
008500         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
008600     03  W-WDK621KY-X.                                                    
008700         05  W-DAPRLIST-9KOMPL   PIC 9(8)    VALUE ZERO.                  
008800         05  W-IDLEVNR-21        PIC X(5)    VALUE LOW-VALUE.             
008900                                                                          
009000*    --- STATUS-KOD FRÅN IMS                                              
009100 01  STATUS-WS                   PIC XX.                                  
009200     88  SEGMENT-FINNS                       VALUE '  '.                  
009300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009500     88  IMS-EJ-OK                           VALUE 'XD'.                  
009600                                                                          
009700 01  GODK-STATUSKODER.                                                    
009800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009900                                                                          
010000                                                                          
010100 01  SSA1                        PIC X(96).                               
010200 01  SSA2                        PIC X(64).                               
010300     EJECT                                                                
010400*    --- IMS FUNKTIONSKODER                                               
010500*01  -COPY W0003                                                          
010600     EJECT                                                                
010700*    ---  DLI INPUT-OUTPUT AREA                                           
010800                                                                          
010900 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDR301'.                    
011000 01  DLI-IO-WDR301.                                                       
011100*  03  -COPY WDR301                                                       
011200                                                                          
011300     EJECT                                                                
011400 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDK611'.                    
011500 01  DLI-IO-WDK611.                                                       
011600*  03  -COPY WDK611                                                       
011700     EJECT                                                                
011800 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDK621'.                    
011900 01  DLI-IO-WDK621.                                                       
012000*  03  -COPY WDK621                                                       
012100     EJECT                                                                
012200 LINKAGE SECTION.                                                         
012300*01  -COPY W0008  -PRE WDR3-                                              
012400     05  FILLER                  PIC X.                                   
012500     EJECT                                                                
012600*01  -COPY W0008  -PRE WDK6-                                              
012700     05  FILLER                  PIC X.                                   
012800     EJECT                                                                
012900 PROCEDURE DIVISION  USING WDR3-PCB WDK6-PCB.                             
013000 MAIN SECTION.                                                            
013100     ENTRY 'DLITCBL' USING WDR3-PCB WDK6-PCB.                             
013200                                                                          
013300     PERFORM A-INIT                                                       
013400                                                                          
013500     PERFORM IMS-GET-FILC-ROT                                             
013600     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
013700                                                                          
013800        MOVE FIL-WDR301-DATA TO UT-AREA                                   
013900        PERFORM B-SKAPA-SKRIV-UTPOST                                      
014000        PERFORM C-SKAPA-SKRIV-BORTPOST                                    
014100        PERFORM S10-NOLLSTALL                                             
014200                                                                          
014300        PERFORM IMS-GET-FILC-ROT                                          
014400     END-PERFORM                                                          
014500                                                                          
014600     PERFORM Z-FINIT                                                      
014700     MOVE ZERO TO RETURN-CODE                                             
014800     GOBACK                                                               
014900     .                                                                    
015000     EJECT                                                                
015100 A-INIT SECTION.                                                          
015200                                                                          
015300     OPEN OUTPUT W61244                                                   
015400                 W61245                                                   
015500                                                                          
015600     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
015700                                                                          
015800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015900     PERFORM S10-NOLLSTALL                                                
016000     .                                                                    
016100     EJECT                                                                
016200 B-SKAPA-SKRIV-UTPOST SECTION.                                            
016400     MOVE UT-IDARTNR TO W-IDARTNR                                         
016500                                                                          
016600     PERFORM IMS-GET-WDK611                                               
016700     MOVE NEJ TO TRAEFF-SW                                                
016800     IF SEGMENT-FINNS                                                     
016900       COMPUTE W-DAPRLIST-9KOMPL = 99999999 - DAGENS-DATUM                
017000       PERFORM IMS-GET-WDK621                                             
017100       PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA                     
017200         IF PRL-KDSTATUS-PR = +1                                          
017700           MOVE JA TO TRAEFF-SW                                           
017800           MOVE PRL-PRARTBES-PR TO UT-PRARTBES-PR                         
018000         END-IF                                                           
018100         PERFORM IMS-GET-WDK621                                           
018200       END-PERFORM                                                        
018300     END-IF                                                               
018400     PERFORM S01-SKRIV-W61244                                             
018500     .                                                                    
018600     EJECT                                                                
018700 C-SKAPA-SKRIV-BORTPOST SECTION.                                          
018800                                                                          
018900     MOVE FIL-WDR301 TO BORT-FIL-WDR301                                   
019000     PERFORM S02-SKRIV-W61245                                             
019100     .                                                                    
019200     EJECT                                                                
019300 Z-FINIT SECTION.                                                         
019400                                                                          
019500     CLOSE W61244                                                         
019600           W61245                                                         
019700                                                                          
019800     MOVE 'S' TO POSTSUM-OPKOD                                            
019900     CALL POSTSUM USING POSTSUM-PARM                                      
020000     .                                                                    
020100     EJECT                                                                
020200 S01-SKRIV-W61244 SECTION.                                                
020300                                                                          
020400     WRITE UT-POST FROM UT-AREA                                           
020500                                                                          
020600     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
020700     MOVE 'W61244 '  TO POSTSUM-FDNAMN                                    
020800     MOVE 'W61244D1' TO POSTSUM-DDNAMN2                                   
020900     CALL POSTSUM USING POSTSUM-PARM                                      
021000     .                                                                    
021100     SKIP3                                                                
021200 S02-SKRIV-W61245 SECTION.                                                
021300                                                                          
021400     WRITE BORT-POST FROM BORT-AREA                                       
021500                                                                          
021600     MOVE SPACE       TO POSTSUM-TRANSTYP                                 
021700     MOVE 'W61245 '   TO POSTSUM-FDNAMN                                   
021800     MOVE 'W61244D2'  TO POSTSUM-DDNAMN2                                  
021900     CALL POSTSUM USING POSTSUM-PARM                                      
022000     .                                                                    
022100     EJECT                                                                
022200 S10-NOLLSTALL SECTION.                                                   
022300                                                                          
022400     MOVE SPACE TO UT-IDDC-REC                                            
022500                   UT-IDDC-SEND                                           
022600                   UT-IDKUNDRF                                            
022700                   UT-AVVIKELSETYP                                        
022800                   UT-IDLEVNR                                             
022900     MOVE ZERO  TO UT-IDFAKT                                              
023000                   UT-DAFAKT                                              
023100                   UT-IDARTNR                                             
023200                   UT-KVANTAL                                             
023300                   UT-PRARTBES-PR                                         
023400     .                                                                    
023500     EJECT                                                                
023600* --- IMS SEKTIONER ---                                                   
023700                                                                          
023800 IMS-GET-FILC-ROT SECTION.                                                
023900     STRING 'WDR301  (IDCPYTXT =' W-IDCPYTXT-X ')'                        
024000          DELIMITED BY SIZE INTO SSA1                                     
024100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
024200     CALL CBLTDLI USING GN WDR3-PCB DLI-IO-WDR301 SSA1                    
024300     MOVE WDR3-STATUS-CODE TO STATUS-WS                                   
024400     PERFORM IMS-STATUSKONTROLL                                           
024500     .                                                                    
024600     SKIP3                                                                
024700 IMS-GET-WDK611 SECTION.                                                  
024800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
024900          DELIMITED BY SIZE INTO SSA1                                     
025000     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
025100          DELIMITED BY SIZE INTO SSA2                                     
025200     MOVE '  GE' TO GODK-STATUSKODER                                      
025300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
025400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
025500     PERFORM IMS-STATUSKONTROLL                                           
025600     .                                                                    
025700     SKIP3                                                                
025800 IMS-GET-WDK621 SECTION.                                                  
025900                                                                          
026000     STRING 'WDK621  (WDK621KY>=' W-WDK621KY-X ')'                        
026100          DELIMITED BY SIZE INTO SSA1                                     
026200     MOVE '  GE' TO GODK-STATUSKODER                                      
026300     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK621 SSA1                   
026400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
026500     PERFORM IMS-STATUSKONTROLL                                           
026600     .                                                                    
026700     EJECT                                                                
026800 IMS-STATUSKONTROLL SECTION.                                              
026900     SET STATUS-IX TO 1                                                   
027000     SEARCH GODK-STATUS                                                   
027100       AT END                                                             
027200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
027300           DELIMITED BY SIZE INTO FELTEXT                                 
027400         DISPLAY FELTEXT                                                  
027500         CALL FELLOG                                                      
027600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
027700         CONTINUE                                                         
027800     END-SEARCH                                                           
027900     .                                                                    
