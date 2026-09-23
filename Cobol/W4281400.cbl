000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4281400.                                                
000300 AUTHOR.         ÖSTRÖM ELEONOR.                                          
000400 DATE-WRITTEN.   08/08/08.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET SUMMERAR ANTAL INLAGDA, SKROTADE OCH AVVIKELSE        
001000*        RAPPORTERADE RADER FÖR LDC RETURER KOD 72 PER RETUR-DC.          
001100*        PROGRAMMET INGÅR I RUTIN W428R1.PERIODENS RADER.                 
001200*        LÄSER IN ALLA W42811-FILER FRÅN VECKORUTIN W428V1 OCH            
001300*        SKICKAR RADER TILL DISTR. OCH PRINT VIA WZ01.                    
001400*                                                                         
001500*        PROGRAMMET LÄSER      WDB6                                       
001600*                                                                         
001700*        E'TRACKER. 6785206  DATED 2008-06-18                             
001800*        E-TRACKER 10143271 CHINA WAREHOUSE PROJECT-1 2011-12-08          
001810*                                                                         
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- PERIODENS INLAGDA RADER KOD 72 HOS LDC                     
002900     SELECT W42811                     ASSIGN TO W42814D1.                
003000     EJECT                                                                
003010*          --- UTFIL SUMMERADE RADER MED LANDSPREFIX                      
003020     SELECT W42814                     ASSIGN TO W42814D2.                
003030     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W42811                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  -COPY W42811      -L.                                                
004000     SKIP2                                                                
004010 FD  W42814                                                               
004020     RECORDING       F                                                    
004030     BLOCK CONTAINS  0.                                                   
004040                                                                          
004050*01  POST -COPY W42814 -PRE  UT-   -L.                                    
004060     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W4281400'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600 77  SPAR-IDDISTR                PIC 9(4)    VALUE ZERO.                  
004700 77  SPAR-IDKUNDNR               PIC 9(6)    VALUE ZERO.                  
004800 77  SPAR-IDRAPPNR               PIC 9(7)    VALUE ZERO.                  
004900 77  SPAR-IDDC-RET               PIC X(2)    VALUE SPACE.                 
005000 77  SPAR-DOC-TIAAPP             PIC 9(4)    VALUE ZERO.                  
005010 77  SPAR-IDLANDX2               PIC X(2)    VALUE SPACE.                 
005020 77  SPAR-IDFTG                  PIC 9(2)    VALUE ZERO.                  
005100 77  W-KVRETINL-TOT              PIC 9(7)    VALUE ZERO.                  
005200 77  W-KVRETINL-SKR-TOT          PIC 9(7)    VALUE ZERO.                  
005300 77  W-KVAVV-KVANT-TOT           PIC 9(7)    VALUE ZERO.                  
005400 77  W-KVDAGAR-RET-TOT           PIC 9(7)    VALUE ZERO.                  
005500 77  WS-YYMMDDHHMM               PIC 9(10)   VALUE ZERO.                  
005600 77  W-KVRETINL-SUM              PIC 9(7)    VALUE ZERO.                  
005700                                                                          
005800 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
005900 77  KDRC-DISPLAY                PIC Z(5).                                
006000     SKIP2                                                                
006100 01  FELTEXT.                                                             
006200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006400                                                                          
006500 77  W42811-EOF-SW               PIC X       VALUE 'N'.                   
006600     88  END-OF-W42811                       VALUE 'J'.                   
006700     EJECT                                                                
006800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006900 01  FILLER REDEFINES DAGENS-DATUM.                                       
007000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007300     EJECT                                                                
007400 01  DYNAMISKA-SUBPROGRAM.                                                
007500*                                                                         
007600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL POSTSUM                                          
008300*                                                                         
008400*01  -COPY W0005   -PRE  POSTSUM-                                         
008500     EJECT                                                                
008600*    --- PARAMETERS TO ABEND                                              
008700                                                                          
008800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009100                                                                          
009200     EJECT                                                                
010200*                                                                         
010900 01  IN-AREA-START               PIC X(24)   VALUE                        
011000                                             'IN-AREA-START'.             
011100     SKIP2                                                                
011200                                                                          
011300*01  AREA -COPY W42811     -PRE IN-                                       
011400*                                                                         
011420 01  UT-AREA-START               PIC X(24)   VALUE                        
011421                                             'UT-AREA-START'.             
011422     SKIP2                                                                
011423                                                                          
011430*01  AREA -COPY W42814     -PRE UT-                                       
011440*                                                                         
011450     EJECT                                                                
011500     EJECT                                                                
011600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011700     SKIP3                                                                
011800 01  NYCKLAR-TILL-DLI.                                                    
011900     03  W-IDDC-X.                                                        
012000         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
012100     SKIP2                                                                
012200*    --- STATUS-KOD FRÅN IMS                                              
012300 01  STATUS-WS                   PIC XX.                                  
012400     88  SEGMENT-FINNS                       VALUE '  '.                  
012500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012800     88  IMS-EJ-OK                           VALUE 'XD'.                  
012900     SKIP2                                                                
013000 01  GODK-STATUSKODER.                                                    
013100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013200     SKIP3                                                                
013300 01  SSA1                        PIC X(64).                               
013400 01  SSA2                        PIC X(64).                               
013500     EJECT                                                                
013600*    --- IMS FUNKTIONSKODER                                               
013700*01  -COPY W0003                                                          
013800     EJECT                                                                
013900*    ---  DLI INPUT-OUTPUT AREA                                           
014000                                                                          
014100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
014200 01  DLI-IO-WDB601.                                                       
014300*    03  -COPY WDB601                                                     
014400     EJECT                                                                
014500 LINKAGE SECTION.                                                         
014600                                                                          
014700*01  -COPY W0009   -PRE MSG-                                              
014800     EJECT                                                                
014900 01  DISTRDOC-PCB                PIC X.                                   
015000     EJECT                                                                
015100                                                                          
015200*01  -COPY W0008  -PRE WDB6-                                              
015300     05  FILLER                  PIC X.                                   
015400     EJECT                                                                
015500 PROCEDURE DIVISION  USING MSG-PCB WDB6-PCB.                              
015600 MAIN SECTION.                                                            
015700     ENTRY 'DLITCBL' USING MSG-PCB WDB6-PCB.                              
015800                                                                          
015900     SKIP2                                                                
016000     PERFORM A-INIT                                                       
016200                                                                          
016250     PERFORM S01-LAES-W42811                                              
016260                                                                          
016300     PERFORM UNTIL END-OF-W42811                                          
016400       MOVE IN-IDDC-RET   TO SPAR-IDDC-RET                                
016500                             W-IDDC                                       
016510       MOVE IN-IDFTG      TO SPAR-IDFTG                                   
016520                                                                          
016600       MOVE IN-TISAAPP-INLINL-TIAAPP TO SPAR-DOC-TIAAPP                   
016700       MOVE ZERO          TO W-KVRETINL-TOT                               
016800                             W-KVRETINL-SKR-TOT                           
016900                             W-KVAVV-KVANT-TOT                            
017000                             W-KVDAGAR-RET-TOT                            
017100                             W-KVRETINL-SUM                               
017200                                                                          
017700       PERFORM UNTIL END-OF-W42811 OR                                     
017800            IN-IDDC-RET NOT = SPAR-IDDC-RET                               
017900                                                                          
018000         PERFORM B-BEHANDLA                                               
018100                                                                          
018200       END-PERFORM                                                        
018300                                                                          
018400       PERFORM C-FLYTTA-DATA                                              
018500                                                                          
018600     END-PERFORM                                                          
018700                                                                          
018800     PERFORM Z-FINIT                                                      
018900                                                                          
019000     MOVE ZERO TO RETURN-CODE                                             
019100     GOBACK                                                               
019200     .                                                                    
019300     EJECT                                                                
019400 A-INIT SECTION.                                                          
019500     SKIP2                                                                
019600                                                                          
019700     OPEN INPUT  W42811                                                   
019701                                                                          
019710     OPEN OUTPUT W42814                                                   
019800                                                                          
019900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020000     .                                                                    
020100     EJECT                                                                
020200 B-BEHANDLA  SECTION.                                                     
020300                                                                          
020400     MOVE IN-IDDISTR    TO SPAR-IDDISTR                                   
020500     MOVE IN-IDKUNDNR   TO SPAR-IDKUNDNR                                  
020600     MOVE IN-IDRAPPNR   TO SPAR-IDRAPPNR                                  
020700                                                                          
020800     COMPUTE W-KVDAGAR-RET-TOT = W-KVDAGAR-RET-TOT +                      
020900                                 IN-KVDAGAR-RET                           
021000     END-COMPUTE                                                          
021100                                                                          
021200     PERFORM UNTIL END-OF-W42811 OR                                       
021300           IN-IDDISTR  NOT = SPAR-IDDISTR  OR                             
021400           IN-IDKUNDNR NOT = SPAR-IDKUNDNR OR                             
021500           IN-IDRAPPNR NOT = SPAR-IDRAPPNR                                
021600                                                                          
022610       IF IN-KVRETINL > 0                                                 
022620         ADD +1 TO W-KVRETINL-TOT                                         
022630       END-IF                                                             
022640                                                                          
022650       IF IN-KVRETINL-SKR > 0                                             
022660         ADD +1  TO W-KVRETINL-SKR-TOT                                    
022670       END-IF                                                             
022680                                                                          
022690       IF IN-KVAVV-KVANT > 0                                              
022691         ADD +1  TO W-KVAVV-KVANT-TOT                                     
022692       END-IF                                                             
022700                                                                          
022800       PERFORM S01-LAES-W42811                                            
022900     END-PERFORM                                                          
023000     .                                                                    
023100     EJECT                                                                
023200 C-FLYTTA-DATA  SECTION.                                                  
023300                                                                          
023500     MOVE SPAR-IDDC-RET       TO UT-IDDC-RET                              
023510     MOVE SPAR-IDFTG          TO UT-IDFTG                                 
023600     MOVE SPAR-DOC-TIAAPP     TO UT-TIAAPP                                
023700                                                                          
023800     IF SPAR-IDDC-RET NOT = DCS-IDDC                                      
023900       MOVE SPAR-IDDC-RET TO W-IDDC                                       
024000     END-IF                                                               
024100     PERFORM IMS-GET-WDB601                                               
024200                                                                          
024300     IF SEGMENT-FINNS                                                     
024400       MOVE DCS-ADGMT-PADR(11:20)  TO UT-ADCITY                           
024420       MOVE DCS-IDLANDX2           TO UT-IDLANDX2                         
024430                                      SPAR-IDLANDX2                       
024500     ELSE                                                                 
024600       MOVE SPACE                  TO UT-ADCITY                           
024610       MOVE SPACE                  TO UT-IDLANDX2                         
024700     END-IF                                                               
024800                                                                          
024900     MOVE W-KVRETINL-TOT      TO UT-KVRETINL                              
025000     MOVE W-KVRETINL-SKR-TOT  TO UT-KVRETINL-SKR                          
025100     MOVE W-KVAVV-KVANT-TOT   TO UT-KVAVV-KVANT                           
025200                                                                          
025700     COMPUTE W-KVRETINL-SUM = W-KVRETINL-TOT + W-KVRETINL-SKR-TOT         
025800     END-COMPUTE                                                          
025900                                                                          
026000     COMPUTE UT-KVDAGDEC ROUNDED =                                        
026100             W-KVDAGAR-RET-TOT / W-KVRETINL-SUM                           
026200     END-COMPUTE                                                          
026300                                                                          
026310     IF DCS-IDLANDX2 =  SPAR-IDLANDX2                                     
026400       PERFORM S10-SKRIV-UTFIL                                            
026410     ELSE                                                                 
026420       MOVE SPACE TO UT-AREA                                              
026430     END-IF                                                               
026440                                                                          
026500     .                                                                    
026600     EJECT                                                                
026700 Z-FINIT SECTION.                                                         
026800                                                                          
026900                                                                          
027000     CLOSE W42811                                                         
027010           W42814                                                         
027100     SKIP2                                                                
027200     MOVE 'S' TO POSTSUM-OPKOD                                            
027300     CALL POSTSUM USING POSTSUM-PARM                                      
027400     .                                                                    
027500     EJECT                                                                
027600 S01-LAES-W42811  SECTION.                                                
027700     SKIP2                                                                
027800     READ W42811 INTO IN-AREA                                             
027900     AT END                                                               
028000        MOVE HIGH-VALUE TO IN-W42811                                      
028100        SET END-OF-W42811 TO TRUE                                         
028200                                                                          
028300     NOT AT END                                                           
028400        MOVE 'W42811'   TO POSTSUM-FDNAMN                                 
028500        MOVE 'W42814D1' TO POSTSUM-DDNAMN2                                
028600        MOVE SPACE      TO POSTSUM-TRANSTYP                               
028700        CALL POSTSUM USING POSTSUM-PARM                                   
028800     END-READ                                                             
028900     .                                                                    
029000     EJECT                                                                
036810 S10-SKRIV-UTFIL SECTION.                                                 
036820     SKIP2                                                                
036830     WRITE UT-POST FROM UT-AREA                                           
036840     MOVE 'W42814'      TO POSTSUM-FDNAMN                                 
036850     MOVE 'W42814D2'    TO POSTSUM-DDNAMN2                                
036860     MOVE 'UTPOST'      TO POSTSUM-TRANSTYP                               
036870     CALL POSTSUM      USING POSTSUM-PARM                                 
036880     .                                                                    
036890     EJECT                                                                
036900* --- IMS SEKTIONER ---                                                   
037000                                                                          
037100     EJECT                                                                
037200 IMS-GET-WDB601 SECTION.                                                  
037300                                                                          
037400     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
037500          DELIMITED BY SIZE INTO SSA1                                     
037600     MOVE '  GE' TO GODK-STATUSKODER                                      
037700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
037800     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
037900     PERFORM IMS-STATUSKONTROLL                                           
038000     .                                                                    
038100     EJECT                                                                
038200 IMS-STATUSKONTROLL SECTION.                                              
038300     SKIP2                                                                
038400     SET STATUS-IX TO 1                                                   
038500     SEARCH GODK-STATUS                                                   
038600       AT END                                                             
038700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
038800           DELIMITED BY SIZE INTO FELTEXT                                 
038900         DISPLAY FELTEXT                                                  
039000         CALL FELLOG                                                      
039100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
039200         CONTINUE                                                         
039300     END-SEARCH                                                           
039400     .                                                                    
