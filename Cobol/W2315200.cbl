000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2315200.                                                
000300 AUTHOR.         NIHLBLAD JOHAN.                                          
000400 DATE-WRITTEN.   07/04/05.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        AK-LISTA                                                         
001000*                                                                         
001100*        PROGRAMMET LÄSER      WDL2                                       
001200*                                                                         
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- INFIL                                                      
002200     SELECT W01177                     ASSIGN TO W23152D1.                
002300     SKIP2                                                                
002400*          --- UTFIL                                                      
002500     SELECT W23152                     ASSIGN TO W23152D2.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W01177                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  -COPY W011100      -L.                                               
003600     SKIP3                                                                
003700 FD  W23152                                                               
003800     RECORDING       V                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100 01  UT-POST                     PIC X(84).                               
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500 77  IDPGM                       PIC X(8)    VALUE 'W2315200'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  IX                          PIC 9(2)    VALUE ZERO.                  
004810 77  ANTAL                       PIC 9(8)    VALUE ZERO.                  
004900     SKIP2                                                                
005000 01  ARBETSAREA.                                                          
005100     03  WS-KVAKS           PIC S9(7)   COMP-3 VALUE ZERO.                
005300     03  WS-ANTAL           PIC S9(9)V999   VALUE ZERO.                   
005301     03  WS-VARDE           PIC S9(9)V999   VALUE ZERO.                   
005302     03  WS-VARDE2          PIC S9(9)V999   VALUE ZERO.                   
005310     03  WS-TOTVARDE        PIC S9(9)V999   VALUE ZERO.                   
005400     03  WS-VARDE-910       PIC S9(9)    VALUE ZERO.                      
005500     03  WS-VARDE-911       PIC S9(9)    VALUE ZERO.                      
005600     03  WS-VARDE-912       PIC S9(9)    VALUE ZERO.                      
005700     03  WS-VARDE-913       PIC S9(9)    VALUE ZERO.                      
005800     03  WS-VARDE-914       PIC S9(9)    VALUE ZERO.                      
005900     03  WS-VARDE-915       PIC S9(9)    VALUE ZERO.                      
006000     03  WS-VARDE-916       PIC S9(9)    VALUE ZERO.                      
006100     03  WS-VARDE-917       PIC S9(9)    VALUE ZERO.                      
006200     03  WS-VARDE-918       PIC S9(9)    VALUE ZERO.                      
006300     03  WS-VARDE-919       PIC S9(9)    VALUE ZERO.                      
006310     03  WS-TOTVARDE-910       PIC S9(9)    VALUE ZERO.                   
006320     03  WS-TOTVARDE-911       PIC S9(9)    VALUE ZERO.                   
006330     03  WS-TOTVARDE-912       PIC S9(9)    VALUE ZERO.                   
006340     03  WS-TOTVARDE-913       PIC S9(9)    VALUE ZERO.                   
006350     03  WS-TOTVARDE-914       PIC S9(9)    VALUE ZERO.                   
006360     03  WS-TOTVARDE-915       PIC S9(9)    VALUE ZERO.                   
006370     03  WS-TOTVARDE-916       PIC S9(9)    VALUE ZERO.                   
006380     03  WS-TOTVARDE-917       PIC S9(9)    VALUE ZERO.                   
006390     03  WS-TOTVARDE-918       PIC S9(9)    VALUE ZERO.                   
006391     03  WS-TOTVARDE-919       PIC S9(9)    VALUE ZERO.                   
006400 01  FELTEXT.                                                             
006500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006700                                                                          
006800 77  W01177-EOF-SW               PIC X       VALUE 'N'.                   
006900     88  END-OF-W01177                       VALUE 'J'.                   
007000     EJECT                                                                
007100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007200 01  FILLER REDEFINES DAGENS-DATUM.                                       
007300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007600     EJECT                                                                
007700 01  DYNAMISKA-SUBPROGRAM.                                                
007800*                                                                         
007900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008200     EJECT                                                                
008300*    --- PARAMETRAR TILL POSTSUM                                          
008400*                                                                         
008500*01  -COPY W0005   -PRE  POSTSUM-                                         
008600     EJECT                                                                
008700 01  IN-AREA-START               PIC X(24)   VALUE                        
008800                                             'IN-AREA-START'.             
008900     SKIP2                                                                
009000                                                                          
009100*01  AREA -COPY W011100     -PRE IN-                                      
009200     EJECT                                                                
009300 01  UT-AREA-START               PIC X(24)   VALUE                        
009400                                             'UT-AREA-START'.             
009500 01  UT-AREA.                                                             
009600     03  UT-IDANSK            PIC 9(3)       VALUE ZERO.                  
009700     03  FILLER               PIC X(3)       VALUE SPACE.                 
009800     03  UT-VARDE             PIC Z(8)9      VALUE ZERO.                  
009810     03  FILLER               PIC X(1)       VALUE SPACE.                 
009820     03  UT-TOTVARDE          PIC Z(8)9      VALUE ZERO.                  
009900     03  FILLER               PIC X(55)      VALUE SPACE.                 
010000     SKIP2                                                                
010100 01  UT-RUBRIKRAD1.                                                       
010200     03  FILLER               PIC X(24)      VALUE SPACE.                 
010300     03  FILLER               PIC X(32)      VALUE                        
010400                  'AK VÄRDE INLEVERANSER LEVERANTÖR'.                     
010500     03  FILLER               PIC X(24)      VALUE SPACE.                 
010600 01  UT-RUBRIKRAD2.                                                       
010700     03  FILLER               PIC X(25)      VALUE                        
010800                  'ANSK      VÄRDE  TOTVÄRDE'.                            
010900     03  FILLER               PIC X(55)      VALUE SPACE.                 
011000                                                                          
011100     EJECT                                                                
011200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011300     SKIP3                                                                
011400 01  NYCKLAR-TILL-DLI.                                                    
011500     03  W-IDPTYP-X.                                                      
011600         05  W-IDPTYP            PIC X(3)    VALUE SPACE.                 
011700     03  W-IDARTNR-X.                                                     
011800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011900     SKIP2                                                                
012000*    --- STATUS-KOD FRÅN IMS                                              
012100 01  STATUS-WS                   PIC XX.                                  
012200     88  SEGMENT-FINNS                       VALUE '  '.                  
012300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012600     88  IMS-EJ-OK                           VALUE 'XD'.                  
012700     SKIP2                                                                
012800 01  GODK-STATUSKODER.                                                    
012900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013000     SKIP3                                                                
013100 01  SSA1                        PIC X(64).                               
013200 01  SSA2                        PIC X(64).                               
013300     EJECT                                                                
013400*    --- IMS FUNKTIONSKODER                                               
013500*01  -COPY W0003                                                          
013600     EJECT                                                                
013700*    ---  DLI INPUT-OUTPUT AREA                                           
013800                                                                          
013900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL221'.                      
014000 01  DLI-IO-WDL221.                                                       
014100*    03  -COPY WDL221                                                     
014200     EJECT                                                                
014300 LINKAGE SECTION.                                                         
014400                                                                          
014500*01  -COPY W0009   -PRE MSG-                                              
014600                                                                          
014700*01  -COPY W0008  -PRE WDL2-                                              
014800     05  FILLER                  PIC X.                                   
014900     EJECT                                                                
015000 PROCEDURE DIVISION  USING MSG-PCB WDL2-PCB.                              
015100 MAIN SECTION.                                                            
015200     ENTRY 'DLITCBL' USING MSG-PCB WDL2-PCB.                              
015300                                                                          
015400     SKIP2                                                                
015500     PERFORM A-INIT                                                       
015600     PERFORM S01-LAES-W01177                                              
015700     MOVE IN-IDARTNR TO W-IDARTNR                                         
015800     PERFORM UNTIL END-OF-W01177                                          
015900                                                                          
016000       MOVE ZERO TO WS-KVAKS                                              
016100       COMPUTE WS-KVAKS = IN-KVAKS-CDC +                                  
016200                          IN-KVAKS-T +                                    
016300                          IN-KVAKS-PAV                                    
016400*      IF WS-KVAKS > 0                                                    
016500          IF IN-IDANSK = 910 OR 911 OR 912 OR 913 OR 914 OR               
016600                         915 OR 916 OR 917 OR 918 OR 919                  
016700            PERFORM IMS-GET-WDL201                                        
016800            IF SEGMENT-FINNS                                              
016900               MOVE 'R31' TO W-IDPTYP                                     
017000               PERFORM IMS-GET-WDL221                                     
017100               PERFORM UNTIL SEGMENT-SAKNAS                               
017200                  IF MOT-KDRT = ZERO                                      
017310                     IF MOT-KVANTMOT > 0                                  
017400                       COMPUTE WS-ANTAL =                                 
017500                            MOT-KVAVIS - MOT-KVANTMOT                     
017510                     ELSE                                                 
017520                       MOVE MOT-KVAVIS TO WS-ANTAL                        
017530                     END-IF                                               
017600                     COMPUTE WS-VARDE2 ROUNDED =                          
017610                                         WS-ANTAL * IN-PRARTSTD           
017700                     ADD WS-VARDE2 TO WS-VARDE                            
020700                  END-IF                                                  
020800                  PERFORM IMS-GET-WDL221                                  
020900               END-PERFORM                                                
020904               COMPUTE WS-TOTVARDE ROUNDED =                              
020905                                     WS-KVAKS * IN-PRARTSTD               
020910               IF IN-IDANSK = 910                                         
020920                 ADD WS-VARDE TO WS-VARDE-910                             
020930                 ADD WS-TOTVARDE TO WS-TOTVARDE-910                       
020940               END-IF                                                     
020950               IF IN-IDANSK = 911                                         
020960                 ADD WS-VARDE TO WS-VARDE-911                             
020970                 ADD WS-TOTVARDE TO WS-TOTVARDE-911                       
020980               END-IF                                                     
020990               IF IN-IDANSK = 912                                         
020991                 ADD WS-VARDE TO WS-VARDE-912                             
020992                 ADD WS-TOTVARDE TO WS-TOTVARDE-912                       
020993               END-IF                                                     
020994               IF IN-IDANSK = 913                                         
020995                 ADD WS-VARDE TO WS-VARDE-913                             
020996                 ADD WS-TOTVARDE TO WS-TOTVARDE-913                       
020997               END-IF                                                     
020998               IF IN-IDANSK = 914                                         
020999                 ADD WS-VARDE TO WS-VARDE-914                             
021000                 ADD WS-TOTVARDE TO WS-TOTVARDE-914                       
021001               END-IF                                                     
021002               IF IN-IDANSK = 915                                         
021003                 ADD WS-VARDE TO WS-VARDE-915                             
021004                 ADD WS-TOTVARDE TO WS-TOTVARDE-915                       
021005               END-IF                                                     
021006               IF IN-IDANSK = 916                                         
021007                 ADD WS-VARDE TO WS-VARDE-916                             
021008                 ADD WS-TOTVARDE TO WS-TOTVARDE-916                       
021009               END-IF                                                     
021010               IF IN-IDANSK = 917                                         
021011                 ADD WS-VARDE TO WS-VARDE-917                             
021012                 ADD WS-TOTVARDE TO WS-TOTVARDE-917                       
021013               END-IF                                                     
021014               IF IN-IDANSK = 918                                         
021015                 ADD WS-VARDE TO WS-VARDE-918                             
021016                 ADD WS-TOTVARDE TO WS-TOTVARDE-918                       
021017               END-IF                                                     
021018               IF IN-IDANSK = 919                                         
021019                 ADD WS-VARDE TO WS-VARDE-919                             
021020                 ADD WS-TOTVARDE TO WS-TOTVARDE-919                       
021021               END-IF                                                     
021022               MOVE ZERO TO WS-VARDE                                      
021030            END-IF                                                        
021100          END-IF                                                          
021200*      END-IF                                                             
021300       PERFORM S01-LAES-W01177                                            
021310       MOVE IN-IDARTNR TO W-IDARTNR                                       
021400     END-PERFORM                                                          
021410                                                                          
021500     ADD 1 TO IX                                                          
021600     MOVE 910           TO UT-IDANSK                                      
021700     MOVE WS-VARDE-910  TO UT-VARDE                                       
021710     MOVE WS-TOTVARDE-910  TO UT-TOTVARDE                                 
021800     PERFORM S11-SKRIV-W23152                                             
021900                                                                          
022000     ADD 1 TO IX                                                          
022100     MOVE 911           TO UT-IDANSK                                      
022200     MOVE WS-VARDE-911  TO UT-VARDE                                       
022210     MOVE WS-TOTVARDE-911  TO UT-TOTVARDE                                 
022300     PERFORM S11-SKRIV-W23152                                             
022400                                                                          
022500     ADD 1 TO IX                                                          
022600     MOVE 912           TO UT-IDANSK                                      
022700     MOVE WS-VARDE-912  TO UT-VARDE                                       
022710     MOVE WS-TOTVARDE-912  TO UT-TOTVARDE                                 
022800     PERFORM S11-SKRIV-W23152                                             
022900                                                                          
023000     ADD 1 TO IX                                                          
023100     MOVE 913           TO UT-IDANSK                                      
023200     MOVE WS-VARDE-913  TO UT-VARDE                                       
023210     MOVE WS-TOTVARDE-913  TO UT-TOTVARDE                                 
023300     PERFORM S11-SKRIV-W23152                                             
023400                                                                          
023500     ADD 1 TO IX                                                          
023600     MOVE 914           TO UT-IDANSK                                      
023700     MOVE WS-VARDE-914  TO UT-VARDE                                       
023710     MOVE WS-TOTVARDE-914  TO UT-TOTVARDE                                 
023800     PERFORM S11-SKRIV-W23152                                             
023900                                                                          
024000     ADD 1 TO IX                                                          
024100     MOVE 915           TO UT-IDANSK                                      
024200     MOVE WS-VARDE-915  TO UT-VARDE                                       
024210     MOVE WS-TOTVARDE-915  TO UT-TOTVARDE                                 
024300     PERFORM S11-SKRIV-W23152                                             
024400                                                                          
024500     ADD 1 TO IX                                                          
024600     MOVE 916           TO UT-IDANSK                                      
024700     MOVE WS-VARDE-916  TO UT-VARDE                                       
024710     MOVE WS-TOTVARDE-916  TO UT-TOTVARDE                                 
024800     PERFORM S11-SKRIV-W23152                                             
024900                                                                          
025000     ADD 1 TO IX                                                          
025100     MOVE 917           TO UT-IDANSK                                      
025200     MOVE WS-VARDE-917  TO UT-VARDE                                       
025210     MOVE WS-TOTVARDE-917  TO UT-TOTVARDE                                 
025300     PERFORM S11-SKRIV-W23152                                             
025400                                                                          
025500     ADD 1 TO IX                                                          
025600     MOVE 918           TO UT-IDANSK                                      
025700     MOVE WS-VARDE-918  TO UT-VARDE                                       
025710     MOVE WS-TOTVARDE-918  TO UT-TOTVARDE                                 
025800     PERFORM S11-SKRIV-W23152                                             
025900                                                                          
026000     ADD 1 TO IX                                                          
026100     MOVE 919           TO UT-IDANSK                                      
026200     MOVE WS-VARDE-919  TO UT-VARDE                                       
026210     MOVE WS-TOTVARDE-919  TO UT-TOTVARDE                                 
026300     PERFORM S11-SKRIV-W23152                                             
026400                                                                          
026500     PERFORM Z-FINIT                                                      
026600                                                                          
026700     MOVE ZERO TO RETURN-CODE                                             
026800     GOBACK                                                               
026900     .                                                                    
027000     EJECT                                                                
027100 A-INIT SECTION.                                                          
027200     SKIP2                                                                
027300                                                                          
027400     OPEN INPUT W01177                                                    
027500                                                                          
027600     OPEN OUTPUT W23152                                                   
027700                                                                          
027800     MOVE 1 TO IX                                                         
027900     PERFORM S11-SKRIV-W23152                                             
028000     MOVE 2 TO IX                                                         
028100     PERFORM S11-SKRIV-W23152                                             
028200                                                                          
028300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
028400     .                                                                    
028500     EJECT                                                                
028600 Z-FINIT SECTION.                                                         
028700                                                                          
028800                                                                          
028900     CLOSE W01177                                                         
029000                                                                          
029100           W23152                                                         
029200     SKIP2                                                                
029300     MOVE 'S' TO POSTSUM-OPKOD                                            
029400     CALL POSTSUM USING POSTSUM-PARM                                      
029500     .                                                                    
029600     EJECT                                                                
029700 S01-LAES-W01177  SECTION.                                                
029800     SKIP2                                                                
029900     READ W01177 INTO IN-AREA                                             
030000     AT END                                                               
030100        SET END-OF-W01177 TO TRUE                                         
030200                                                                          
030300     NOT AT END                                                           
030400        MOVE 'W01177' TO POSTSUM-FDNAMN                                   
030500        MOVE 'W23152D1' TO POSTSUM-DDNAMN2                                
030600        MOVE SPACE     TO POSTSUM-TRANSTYP                                
030700        CALL POSTSUM USING POSTSUM-PARM                                   
030800     END-READ                                                             
030900     .                                                                    
031000     EJECT                                                                
031100 S11-SKRIV-W23152 SECTION.                                                
031200     SKIP2                                                                
031300     IF IX = 1                                                            
031400       WRITE UT-POST FROM UT-RUBRIKRAD1                                   
031500     END-IF                                                               
031600     IF IX = 2                                                            
031700       WRITE UT-POST FROM UT-RUBRIKRAD2                                   
031800     END-IF                                                               
031900     IF IX > 2                                                            
032000       WRITE UT-POST FROM UT-AREA                                         
032100     END-IF                                                               
032200                                                                          
032300     MOVE SPACE     TO POSTSUM-TRANSTYP                                   
032400     MOVE 'W23152 ' TO POSTSUM-FDNAMN                                     
032500     MOVE 'W23152D2' TO POSTSUM-DDNAMN2                                   
032600     CALL POSTSUM USING POSTSUM-PARM                                      
032700     .                                                                    
032800     EJECT                                                                
032900* --- IMS SEKTIONER ---                                                   
033000                                                                          
033100     EJECT                                                                
033200 IMS-GET-WDL201 SECTION.                                                  
033300*                                                                         
033400     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
033500             DELIMITED BY SIZE INTO SSA1                                  
033600     MOVE '  GE' TO GODK-STATUSKODER                                      
033700     CALL CBLTDLI USING GU WDL2-PCB DLI-IO-WDL221 SSA1                    
033800     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
033900     PERFORM IMS-STATUSKONTROLL                                           
034000     .                                                                    
034100     EJECT                                                                
034200 IMS-GET-WDL221 SECTION.                                                  
034300*                                                                         
034400     MOVE 'WDL211   ' TO SSA1                                             
034500     STRING 'WDL221  (IDPTYP   =' W-IDPTYP-X ')'                          
034600             DELIMITED BY SIZE INTO SSA2                                  
034700     MOVE '  GE' TO GODK-STATUSKODER                                      
034800     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL221 SSA1 SSA2              
034900     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
035000     PERFORM IMS-STATUSKONTROLL                                           
035100     .                                                                    
035200     SKIP3                                                                
035300 IMS-STATUSKONTROLL SECTION.                                              
035400     SKIP2                                                                
035500     SET STATUS-IX TO 1                                                   
035600     SEARCH GODK-STATUS                                                   
035700       AT END                                                             
035800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
035900           DELIMITED BY SIZE INTO FELTEXT                                 
036000         DISPLAY FELTEXT                                                  
036100         CALL FELLOG                                                      
036200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
036300         CONTINUE                                                         
036400     END-SEARCH                                                           
036500     .                                                                    
