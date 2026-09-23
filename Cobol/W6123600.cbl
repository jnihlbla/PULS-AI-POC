000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6123600.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   97/03/12.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        NEDLÄSNING HTR WDR301                                            
001000*        NDC  NYA ARTIKLAR I FAKTURA                                      
001100*                                                                         
001200*        PROGRAMMET LÄSER WDR301                                          
001300*                   SKAPAR FIL W61238 LISTFIL REFILL-GRUPPEN              
001400*                              W61237 FÖR BORTTAG HTR                     
001500*                                                                         
001600*                   LÄSER WDK611 OCH WDK711                               
001700                                                                          
001800 ENVIRONMENT DIVISION.                                                    
001900                                                                          
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300                                                                          
002400*          --- LISTFIL REFILL-GRUPPEN                                     
002500     SELECT W61238                     ASSIGN TO W61236D1.                
002600                                                                          
002700*          --- BORTTAG HTR                                                
002800     SELECT W61237                     ASSIGN TO W61236D2.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300                                                                          
003400 FD  W61238                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700*01  POST -COPY W61238  -PRE  UT-     -L.                                 
003800     EJECT                                                                
003900                                                                          
004000 FD  W61237                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300*01  POST -COPY WDR301  -PRE  BORT-   -L.                                 
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600*    -- CHECKED BY WY2000                                                 
004700                                                                          
004800 77  IDPGM                       PIC X(8)    VALUE 'W6123600'.            
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100                                                                          
005200 01  FELTEXT.                                                             
005300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005500                                                                          
005600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005700 01  FILLER REDEFINES DAGENS-DATUM.                                       
005800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006100     EJECT                                                                
006200*      --- VALID IDDC CODES                                               
006300*                                                                         
006400*01    -COPY WWDC99                                                       
006500       EJECT                                                              
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006700*                                                                         
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007100     EJECT                                                                
007200*    --- PARAMETRAR TILL POSTSUM                                          
007300*                                                                         
007400*01  -COPY W0005   -PRE  POSTSUM-                                         
007500     EJECT                                                                
007600 01  UT-AREA-START               PIC X(16)   VALUE                        
007700                                             'UT-AREA-START'.             
007800*01  AREA -COPY W61238      -PRE UT-                                      
007900     EJECT                                                                
008000 01  LIST-AREA-START             PIC X(16)   VALUE                        
008100                                             'LIST-AREA-START'.           
008200 01  BORT-AREA-START             PIC X(16)   VALUE                        
008300                                             'BORT-AREA-START'.           
008400*01  AREA -COPY WDR301      -PRE BORT-                                    
008500     EJECT                                                                
008600 01  NYCKLAR-TILL-DLI.                                                    
008700     03  W-WDR301KY-X.                                                    
008800         05  W-WDR301KY          PIC X(27)    VALUE SPACE.                
008900     03  W-IDCPYTXT-X.                                                    
009000         05  W-IDCPYTXT          PIC X(08)    VALUE 'W61236  '.           
009100     03  W-IDARTNR-X.                                                     
009200         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
009300     03  W-IDDC-X.                                                        
009400         05  W-IDDC              PIC X(2)     VALUE SPACE.                
009500                                                                          
009600                                                                          
009700*    --- STATUS-KOD FRÅN IMS                                              
009800 01  STATUS-WS                   PIC XX.                                  
009900     88  SEGMENT-FINNS                       VALUE '  '.                  
010000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010200     88  IMS-EJ-OK                           VALUE 'XD'.                  
010300                                                                          
010400                                                                          
010500 01  GODK-STATUSKODER.                                                    
010600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010700                                                                          
010800                                                                          
010900 01  SSA1                        PIC X(64).                               
011000 01  SSA2                        PIC X(64).                               
011100     EJECT                                                                
011200*    --- IMS FUNKTIONSKODER                                               
011300*01  -COPY W0003                                                          
011400     EJECT                                                                
011500*    ---  DLI INPUT-OUTPUT AREA                                           
011600                                                                          
011700 01  FILLER           PIC X(16) VALUE 'DLI-IO-FILC01'.                    
011800 01  DLI-IO-FILC01.                                                       
011900*  03  -COPY WDR301                                                       
012000                                                                          
012100     EJECT                                                                
012200 01  FILLER           PIC X(16) VALUE 'DLI-IO-ARTC11'.                    
012300 01  DLI-IO-ARTC11.                                                       
012400*  03  -COPY WDK611                                                       
012500                                                                          
012600     EJECT                                                                
012700 01  FILLER           PIC X(16) VALUE 'DLI-IO-ARTS11'.                    
012800 01  DLI-IO-ARTS11.                                                       
012900*  03  -COPY WDK711                                                       
013000                                                                          
013100     EJECT                                                                
013200 LINKAGE SECTION.                                                         
013300*01  -COPY W0008  -PRE FILC-                                              
013400     05  FILLER                  PIC X.                                   
013500                                                                          
013600*01  -COPY W0008  -PRE ARTC-                                              
013700     05  FILLER                  PIC X.                                   
013800     EJECT                                                                
013900*01  -COPY W0008  -PRE ARTS-                                              
014000     05  FILLER                  PIC X.                                   
014100     EJECT                                                                
014200 PROCEDURE DIVISION  USING FILC-PCB ARTC-PCB ARTS-PCB.                    
014300 MAIN SECTION.                                                            
014400     ENTRY 'DLITCBL' USING FILC-PCB ARTC-PCB ARTS-PCB.                    
014500                                                                          
014600     PERFORM A-INIT                                                       
014700                                                                          
014800     PERFORM IMS-GET-FILC-ROT                                             
014900     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
015000                                                                          
015100        MOVE FIL-WDR301-DATA TO UT-AREA                                   
015200        MOVE UT-IDARTNR  TO W-IDARTNR                                     
015300        MOVE UT-IDDC-REC TO W-IDDC                                        
015400                            WS-IDDC                                       
015500                                                                          
015600*       LÄSNING WDK7                                                      
015700        PERFORM IMS-GET-WLARTS11                                          
015800        IF SEGMENT-FINNS                                                  
015900           MOVE SLAG-IDPERSON-BUY TO UT-IDPERSON-BUY                      
016000        ELSE                                                              
016100           MOVE ZERO              TO UT-IDPERSON-BUY                      
016200        END-IF                                                            
016300                                                                          
016400        PERFORM S01-SKRIV-W61238                                          
016500        PERFORM B-SKAPA-BORTPOST                                          
016600                                                                          
016700        PERFORM IMS-GET-FILC-ROT                                          
016800     END-PERFORM                                                          
016900                                                                          
017000     PERFORM Z-FINIT                                                      
017100                                                                          
017200     MOVE ZERO TO RETURN-CODE                                             
017300     GOBACK                                                               
017400     .                                                                    
017500     EJECT                                                                
017600 A-INIT SECTION.                                                          
017700                                                                          
017800     OPEN OUTPUT W61238                                                   
017900                 W61237                                                   
018000                                                                          
018100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018200     .                                                                    
018300     EJECT                                                                
018400 B-SKAPA-BORTPOST SECTION.                                                
018500                                                                          
018600     MOVE FIL-WDR301 TO BORT-FIL-WDR301                                   
018700                                                                          
018800     PERFORM S02-SKRIV-W61237                                             
018900     .                                                                    
019000     EJECT                                                                
019100 Z-FINIT SECTION.                                                         
019200                                                                          
019300     CLOSE W61238                                                         
019400           W61237                                                         
019500                                                                          
019600     MOVE 'S' TO POSTSUM-OPKOD                                            
019700     CALL POSTSUM USING POSTSUM-PARM                                      
019800     .                                                                    
019900     EJECT                                                                
020000 S01-SKRIV-W61238 SECTION.                                                
020100                                                                          
020200     WRITE UT-POST FROM UT-AREA                                           
020300                                                                          
020400     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
020500     MOVE 'W61236 '  TO POSTSUM-FDNAMN                                    
020600     MOVE 'W61236D1' TO POSTSUM-DDNAMN2                                   
020700     CALL POSTSUM USING POSTSUM-PARM                                      
020800     .                                                                    
020900     SKIP3                                                                
021000 S02-SKRIV-W61237 SECTION.                                                
021100                                                                          
021200     WRITE BORT-POST FROM BORT-AREA                                       
021300                                                                          
021400     MOVE SPACE       TO POSTSUM-TRANSTYP                                 
021500     MOVE 'W61237 '   TO POSTSUM-FDNAMN                                   
021600     MOVE 'W61236D2'  TO POSTSUM-DDNAMN2                                  
021700     CALL POSTSUM USING POSTSUM-PARM                                      
021800     .                                                                    
021900     EJECT                                                                
022000* --- IMS SEKTIONER ---                                                   
022100                                                                          
022200 IMS-GET-FILC-ROT SECTION.                                                
022300     STRING 'WLFILC01(IDCPYTXT =' W-IDCPYTXT-X ')'                        
022400          DELIMITED BY SIZE INTO SSA1                                     
022500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
022600     CALL CBLTDLI USING GN FILC-PCB DLI-IO-FILC01 SSA1                    
022700     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
022800     PERFORM IMS-STATUSKONTROLL                                           
022900     .                                                                    
023000     SKIP3                                                                
023100 IMS-GET-WLARTC11 SECTION.                                                
023200     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
023300          DELIMITED BY SIZE INTO SSA1                                     
023400     STRING 'WLARTC11(KDSEGKEY= 1)'                                       
023500          DELIMITED BY SIZE INTO SSA2                                     
023600     MOVE '  GE' TO GODK-STATUSKODER                                      
023700     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-ARTC11 SSA1 SSA2               
023800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
023900     PERFORM IMS-STATUSKONTROLL                                           
024000     .                                                                    
024100     SKIP3                                                                
024200 IMS-GET-WLARTS11 SECTION.                                                
024300     STRING 'WLARTS01(IDARTNR = ' W-IDARTNR-X ')'                         
024400          DELIMITED BY SIZE INTO SSA1                                     
024500     STRING 'WLARTS11(IDDC    = ' W-IDDC-X ')'                            
024600          DELIMITED BY SIZE INTO SSA2                                     
024700     MOVE '  GE' TO GODK-STATUSKODER                                      
024800     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-ARTS11 SSA1 SSA2               
024900     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
025000     PERFORM IMS-STATUSKONTROLL                                           
025100     .                                                                    
025200     EJECT                                                                
025300 IMS-STATUSKONTROLL SECTION.                                              
025400                                                                          
025500     SET STATUS-IX TO 1                                                   
025600     SEARCH GODK-STATUS                                                   
025700       AT END                                                             
025800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
025900           DELIMITED BY SIZE INTO FELTEXT                                 
026000         DISPLAY FELTEXT                                                  
026100         CALL FELLOG                                                      
026200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
026300         CONTINUE                                                         
026400     END-SEARCH                                                           
026500     .                                                                    
