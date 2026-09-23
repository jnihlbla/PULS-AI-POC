000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2365200.                                                
000400*AUTHOR.         STEFAN KIHLBERG.                                         
000500*DATE-WRITTEN.   92/09/10.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SKAPAR LISTFIL FÖR INLEVERANS R31 UTAN MOTSVARANDE               
001100*        LEVERANSPLAN.                                                    
001200*        LÄSER HÄNDELSE 2240 PÅ WDG3                                      
001300*        SKRIVER LISTFIL W23653                                           
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR WLXXCS (WDG3)                              
001600*                                                                         
001700*        960826 (FRONTEC) OMGJORT TILL BMP UTAN CHECKPOINT                
001800*               SOM BRYTER EFTER 500 UPPDATERINGAR                        
001900*                                                                         
002000*    ABENDKODER:                                                          
002100*        U0016 -  . . . .                                                 
002200*        U1000 -  . . . .                                                 
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     SKIP2                                                                
003200*          --- LISTPOSTER, INLEVERANS UTAN LEVERANSPLAN                   
003300     SELECT W23653                     ASSIGN TO W23652D1.                
003400                                                                          
003500*          --- POST OM HANTERINGEN HAR AVRUTITS                           
003600     SELECT W23652                     ASSIGN TO W23652D2.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004100     SKIP3                                                                
004200 FD  W23653                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500     SKIP2                                                                
004600*01  POST -COPY W23653 -PRE  W23653-  -L.                                 
004700                                                                          
004800                                                                          
004900 FD  W23652                                                               
005000     RECORDING F                                                          
005100     BLOCK 0                                                              
005200     LABEL RECORD STANDARD.                                               
005300                                                                          
005400 01  W23652-POST.                                                         
005500     03 FILLER     PIC X(1).                                              
005600     EJECT                                                                
005700 WORKING-STORAGE SECTION.                                                 
005800     SKIP2                                                                
005801                                                                          
005810*    -- CHECKED BY WY2000                                                 
005900 77  IDPGM                       PIC X(8)    VALUE 'W2365200'.            
006000 77  JA                          PIC X       VALUE 'J'.                   
006100 77  NEJ                         PIC X       VALUE 'N'.                   
006200     EJECT                                                                
006300                                                                          
006400 01  ARBETSFALT.                                                          
006500     03  WS-VAERDE               PIC S9(7)V9(2)  VALUE ZERO.              
006600     03  WS-PRARTSTD             PIC S9(7)V9(2)  VALUE ZERO.              
006700     03  WS-IDANSK               PIC S9(3)       VALUE ZERO.              
006710     03  WS-KDEFFMAN             PIC  X(1)       VALUE SPACE.             
006800                                                                          
006900 01  W-ANT-BORTTAG               PIC 9(4)    VALUE ZERO.                  
007000 01  W-MAX-BORTTAG               PIC 9(3)    VALUE 500.                   
007100     SKIP2                                                                
007200                                                                          
007300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007400 01  FILLER REDEFINES DAGENS-DATUM.                                       
007500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007800     EJECT                                                                
007900 01  DYNAMISKA-SUBPROGRAM.                                                
008000*                                                                         
008100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008500     SKIP2                                                                
008600*    --- PARAMETRAR TILL ABEND                                            
008700                                                                          
008800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009000     SKIP2                                                                
009100 01  FELTEXT.                                                             
009200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009400     EJECT                                                                
009500*    --- PARAMETRAR TILL POSTSUM                                          
009600*                                                                         
009700*01  -COPY W0005   -PRE  POSTSUM-                                         
009800     EJECT                                                                
009900 01  W23653-AREA-START           PIC X(24)   VALUE                        
010000                                 'W23653-AREA-START  '.                   
010100     SKIP2                                                                
010200                                                                          
010300*01  AREA -COPY W23653     -PRE W23653-                                   
010400     EJECT                                                                
010500*--------------------------------------- AREA FÖR W23652-POST             
010600*                                                                         
010700 01  W23652-AREA.                                                         
010800   03  FILLER            PIC X(1)    VALUE 'J'.                           
010900     EJECT                                                                
011000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011100*                                                                         
011200     EJECT                                                                
011300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011400     SKIP3                                                                
011500 01  NYCKLAR-TILL-DLI.                                                    
011600                                                                          
011700     03  W-IDARTNR-X.                                                     
011800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011900     03  W-KDSEGKEY-X.                                                    
012000         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
012100                                                                          
012200     03  W-2239KEY-X.                                                     
012300         05  W-IDHTYP            PIC X(4)     VALUE '2239'.               
012400         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
012500                                                                          
012600     SKIP2                                                                
012700*    --- STATUS-KOD FRÅN IMS                                              
012800 01  STATUS-WS                   PIC XX.                                  
012900     88  SEGMENT-FINNS                       VALUE '  '.                  
013000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013200     SKIP2                                                                
013300 01  GODK-STATUSKODER.                                                    
013400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013500     SKIP3                                                                
013600 01  SSA1                        PIC X(64).                               
013700 01  SSA2                        PIC X(64).                               
013800     EJECT                                                                
013900*    --- IMS FUNKTIONSKODER                                               
014000*01  -COPY W0003                                                          
014100     EJECT                                                                
014200*    ---  DLI INPUT-OUTPUT AREA                                           
014300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
014400     SKIP3                                                                
014500 01  DLI-IO-AREA.                                                         
014600     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
014700     SKIP3                                                                
014800     03  WLARTC01 REDEFINES IO-AREA.                                      
014900*        05  -COPY WDK601                                                 
015000     SKIP3                                                                
015100     03  WLARTC11 REDEFINES IO-AREA.                                      
015200*        05  -COPY WDK611                                                 
015300     EJECT                                                                
015400                                                                          
015500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-2'.         
015600     SKIP3                                                                
015700 01  DLI-IO-AREA-2.                                                       
015800     03  IO-AREA-2               PIC X(150)  VALUE SPACE.                 
015900     SKIP3                                                                
016000     SKIP3                                                                
016100     03  WDGX2240 REDEFINES IO-AREA-2.                                    
016200*        05  -COPY WDGX2240                                               
016300     EJECT                                                                
016400 LINKAGE SECTION.                                                         
016500                                                                          
016600*01  -COPY W0009  -PRE MSG-                                               
016700     EJECT                                                                
016800*01  -COPY W0008  -PRE ARTC-                                              
016900     05  FILLER                  PIC X.                                   
017000     EJECT                                                                
017100*01  -COPY W0008  -PRE XXCS-                                              
017200     05  FILLER                  PIC X.                                   
017300     EJECT                                                                
017400 PROCEDURE DIVISION  USING MSG-PCB ARTC-PCB XXCS-PCB.                     
017500     ENTRY 'DLITCBL' USING MSG-PCB ARTC-PCB XXCS-PCB.                     
017600                                                                          
017700     SKIP2                                                                
017800     PERFORM A-INIT                                                       
017900     PERFORM IMS-GET-2239                                                 
018000     IF SEGMENT-FINNS                                                     
018100        PERFORM IMS-GET-2240                                              
018300        PERFORM UNTIL ( SEGMENT-SAKNAS                                    
018400                       OR W-ANT-BORTTAG >= W-MAX-BORTTAG )                
018500           MOVE 2240-IDARTNR TO W-IDARTNR                                 
018600           PERFORM IMS-DLET-2240                                          
018700           ADD 1             TO W-ANT-BORTTAG                             
018800           PERFORM AA-NOLLSTALL                                           
018900           PERFORM B-SAMLA-DATA                                           
019000           PERFORM C-BERAKNA-VARDE                                        
019100           PERFORM D-FLYTTA-TILL-UTAREA                                   
019200           PERFORM S11-SKRIV-W23653                                       
019300           PERFORM IMS-GET-2240                                           
019400        END-PERFORM                                                       
019600     END-IF                                                               
019700                                                                          
019800     PERFORM E-AVSLUTA                                                    
019900     PERFORM Z-FINIT                                                      
020000                                                                          
020100     MOVE ZERO TO RETURN-CODE                                             
020200     GOBACK                                                               
020300     .                                                                    
020400     EJECT                                                                
020500                                                                          
020600 A-INIT SECTION.                                                          
020700                                                                          
020800     OPEN OUTPUT W23653 W23652                                            
020900     SKIP2                                                                
021000     ACCEPT DAGENS-DATUM  FROM DATE                                       
021100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021200     .                                                                    
021300     EJECT                                                                
021400                                                                          
021500 AA-NOLLSTALL SECTION.                                                    
021600                                                                          
021700     MOVE ZERO               TO WS-VAERDE                                 
021800                                WS-IDANSK                                 
021810     MOVE SPACE              TO WS-KDEFFMAN                               
021900     .                                                                    
022000     EJECT                                                                
022100                                                                          
022200 B-SAMLA-DATA SECTION.                                                    
022300                                                                          
022400     PERFORM IMS-GET-WDK601                                               
022500     IF SEGMENT-FINNS                                                     
022600        IF ART-KDERS-UTG = 0                                              
022700           PERFORM IMS-GET-WDK611                                         
022800           MOVE CLAG-IDANSK     TO WS-IDANSK                              
022810           MOVE CLAG-KDEFFMAN   TO WS-KDEFFMAN                            
022900           MOVE CLAG-PRARTSTD   TO WS-PRARTSTD                            
023000        END-IF                                                            
023100     END-IF                                                               
023200     .                                                                    
023300     EJECT                                                                
023400 C-BERAKNA-VARDE SECTION.                                                 
023500                                                                          
023600     COMPUTE WS-VAERDE = 2240-KVAVIS * WS-PRARTSTD                        
023700     .                                                                    
023800     EJECT                                                                
023900                                                                          
024000 D-FLYTTA-TILL-UTAREA SECTION.                                            
024100                                                                          
024200     MOVE 2240-IDARTNR         TO W23653-IDARTNR                          
024300     MOVE 2240-IDLEVNR         TO W23653-IDLEVNR                          
024400     MOVE 2240-TIANKDAG        TO W23653-TIANKDAG                         
024500     MOVE 2240-KVAVIS          TO W23653-KVAVIS                           
024600     MOVE WS-IDANSK            TO W23653-IDANSK                           
024700     MOVE WS-VAERDE            TO W23653-VAERDE                           
024710     MOVE WS-KDEFFMAN          TO W23653-KDEFFMAN                         
024800     .                                                                    
024900     EJECT                                                                
025000                                                                          
025100 E-AVSLUTA SECTION.                                                       
025200                                                                          
025300*****************************************************************         
025400*    SKRIVER POST PÅ FIL W23652 OM BEHANDLINGEN HAR AVBRUTITS             
025500*    DETTA FÖR ATT SE ATT OMSTART SKA GÖRAS.                              
025600*****************************************************************         
025700                                                                          
025800     IF SEGMENT-FINNS                                                     
025900        PERFORM S12-SKRIV-W23652                                          
026000     END-IF                                                               
026100     .                                                                    
026200     EJECT                                                                
026300 Z-FINIT SECTION.                                                         
026400     CLOSE W23653 W23652                                                  
026500     SKIP2                                                                
026600     MOVE 'S' TO POSTSUM-OPKOD                                            
026700     CALL POSTSUM USING POSTSUM-PARM                                      
026800     .                                                                    
026900     EJECT                                                                
027000 S11-SKRIV-W23653 SECTION.                                                
027100     SKIP2                                                                
027200     WRITE W23653-POST FROM W23653-AREA                                   
027300                                                                          
027400     MOVE 'W23653' TO POSTSUM-FDNAMN                                      
027500     MOVE 'W23652D1' TO POSTSUM-DDNAMN2                                   
027600     CALL POSTSUM USING POSTSUM-PARM                                      
027700     .                                                                    
027800     EJECT                                                                
027900 S12-SKRIV-W23652 SECTION.                                                
028000******************************************************************        
028100*    SKRIV W23652-POST                                           *        
028200******************************************************************        
028300                                                                          
028400     WRITE W23652-POST FROM W23652-AREA                                   
028500                                                                          
028600     MOVE 'W23652'     TO POSTSUM-FDNAMN                                  
028700     MOVE 'W23652D2'   TO POSTSUM-DDNAMN2                                 
028800     CALL POSTSUM USING POSTSUM-PARM                                      
028900     .                                                                    
029000     EJECT                                                                
029100* --- IMS SEKTIONER ---                                                   
029200     SKIP3                                                                
029300 IMS-GET-WDK601 SECTION.                                                  
029400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
029500          DELIMITED BY SIZE INTO SSA1                                     
029600     MOVE '  GE' TO GODK-STATUSKODER                                      
029700     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
029800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
029900     PERFORM IMS-STATUSKONTROLL                                           
030000     .                                                                    
030100     EJECT                                                                
030200 IMS-GET-WDK611 SECTION.                                                  
030300     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
030400          DELIMITED BY SIZE INTO SSA1                                     
030500     MOVE '  GE' TO GODK-STATUSKODER                                      
030600     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
030700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
030800     PERFORM IMS-STATUSKONTROLL                                           
030900     .                                                                    
031000     EJECT                                                                
031100 IMS-GET-2239 SECTION.                                                    
031200     STRING 'WLXXCS01(WDG3KEY  =' W-2239KEY-X ')'                         
031300          DELIMITED BY SIZE INTO SSA1                                     
031400     MOVE '  GE' TO GODK-STATUSKODER                                      
031500     CALL CBLTDLI USING GU XXCS-PCB DLI-IO-AREA-2 SSA1                    
031600     MOVE XXCS-STATUS-CODE TO STATUS-WS                                   
031700     PERFORM IMS-STATUSKONTROLL                                           
031800     .                                                                    
031900     EJECT                                                                
032000 IMS-GET-2240 SECTION.                                                    
032100     MOVE 'WLXXCS11  '      TO SSA1                                       
032200     MOVE '  GE' TO GODK-STATUSKODER                                      
032300     CALL CBLTDLI USING GHNP XXCS-PCB DLI-IO-AREA-2 SSA1                  
032400     MOVE XXCS-STATUS-CODE TO STATUS-WS                                   
032500     PERFORM IMS-STATUSKONTROLL                                           
032600     .                                                                    
032700     SKIP3                                                                
032800 IMS-DLET-2240 SECTION.                                                   
032900                                                                          
033000     MOVE '  ' TO GODK-STATUSKODER                                        
033100     CALL CBLTDLI USING DLET XXCS-PCB DLI-IO-AREA-2                       
033200     MOVE XXCS-STATUS-CODE TO STATUS-WS                                   
033300     PERFORM IMS-STATUSKONTROLL                                           
033400     .                                                                    
033500 IMS-STATUSKONTROLL SECTION.                                              
033600     SKIP2                                                                
033700     SET STATUS-IX TO 1                                                   
033800     SEARCH GODK-STATUS                                                   
033900       AT END                                                             
034000         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
034100         DISPLAY FELTEXT                                                  
034200         CALL FELLOG                                                      
034300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
034400         CONTINUE                                                         
034500     END-SEARCH                                                           
034600     .                                                                    
