001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W4791700.                                                
001300*AUTHOR.         LARS CALAIS.                                             
001400*DATE-WRITTEN.   91/06/12.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        SKRIVER RENSNINGSPOST OM LIGGTIDEN FÖR OBEKR ÄR OK               
002000*                                                                         
002110*        PROGRAMMET LÄSER     WLGMTA (WDB2)                               
002200*                                                                         
002300*    ABENDKODER:                                                          
002400*        U0016 - OM FEL I DATUMKONVERTERINGEN                             
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003401     SKIP2                                                                
003402*          --- ORDER PÅ Q2 SOM UPPFYLLER RENSN.REGELN                     
003403     SELECT W47911                     ASSIGN TO W47917D1.                
003404     SKIP2                                                                
003405*          --- ORDERBEKRÄFTELSER SOM SKALL RENSAS                         
003410     SELECT W47917                     ASSIGN TO W47917D2.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004001     SKIP3                                                                
004002 FD  W47911                                                               
004003     RECORDING       F                                                    
004004     BLOCK CONTAINS  0.                                                   
004005     SKIP2                                                                
004006*01  -COPY W479011      -PRE OBEKR-  -L.                                  
004007     SKIP3                                                                
004008 FD  W47917                                                               
004009     RECORDING       F                                                    
004010     BLOCK CONTAINS  0.                                                   
004011     SKIP2                                                                
004020*01  POST -COPY W479017 -PRE  RENS-  -L.                                  
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004301*    -COPY WY2000W3                                                       
004310     SKIP3                                                                
004400 77  IDPGM                       PIC X(8)    VALUE 'W4791700'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700 01  WS-LIGGTID                  PIC S9(3)   COMP-3.                      
004800 01  WS-TIREGDAT                 PIC S9(5)   COMP-3.                      
004801 01  WS-TIORDER                  PIC S9(5)   COMP-3.                      
004802 01  WS-TIIDAG                   PIC S9(5)   COMP-3.                      
004803                                                                          
004804 77  W47911-EOF-SW               PIC X       VALUE 'N'.                   
004810     88  END-OF-W47911                       VALUE 'J'.                   
004900     EJECT                                                                
005000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005100 01  FILLER REDEFINES DAGENS-DATUM.                                       
005200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005500     EJECT                                                                
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006100     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006201     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006210     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006220     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
006300     SKIP2                                                                
006400*    --- PARAMETRAR TILL ABEND                                            
006500                                                                          
006600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006800     SKIP2                                                                
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL DATKORT                                          
007400*                                                                         
007500 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W47917'.              
007600     SKIP2                                                                
007700 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
007800     SKIP2                                                                
007900*01  -COPY WDATKORT                                                       
008001     EJECT                                                                
008002*    --- PARAMETRAR TILL POSTSUM                                          
008003*                                                                         
008010*01  -COPY W0005   -PRE  POSTSUM-                                         
008101     EJECT                                                                
008110*01  -COPY WDATAREA                                                       
008201     EJECT                                                                
008202 01  OBEKR-AREA-START            PIC X(24)   VALUE                        
008203                                 'OBEKR-AREA-START  '.                    
008204     SKIP2                                                                
008205                                                                          
008206*01  AREA -COPY W479011    -PRE OBEKR-                                    
008207     EJECT                                                                
008208 01  RENS-AREA-START             PIC X(24)   VALUE                        
008209                                 'RENS-AREA-START  '.                     
008210     SKIP2                                                                
008211                                                                          
008220*01  AREA -COPY W479017     -PRE RENS-                                    
008300     EJECT                                                                
008400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008500*                                                                         
008600     EJECT                                                                
008700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008800     SKIP3                                                                
008900 01  NYCKLAR-TILL-DLI.                                                    
009020     03  W-IDGMT-X.                                                       
009030         05  W-IDDISTR           PIC S9(5)  VALUE +0 COMP-3.              
009040         05  W-IDKUNDNR          PIC S9(7)  VALUE +0 COMP-3.              
009050                                                                          
009100     SKIP2                                                                
009200*    --- STATUS-KOD FRÅN IMS                                              
009300 01  STATUS-WS                   PIC XX.                                  
009400     88  SEGMENT-FINNS                       VALUE '  '.                  
009600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009700     SKIP2                                                                
009800 01  GODK-STATUSKODER.                                                    
009900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010000     SKIP3                                                                
010100 01  SSA1                        PIC X(64).                               
010300     EJECT                                                                
010400*    --- IMS FUNKTIONSKODER                                               
010500*01  -COPY W0003                                                          
010600     EJECT                                                                
010800*    ---  DLI INPUT-OUTPUT AREA                                           
011302 01  FILLER                      PIC X(16)  VALUE 'WDB2-AREA'.            
011303*01  -COPY WDB201                                                         
011304     EJECT                                                                
011700 LINKAGE SECTION.                                                         
011800                                                                          
011901     EJECT                                                                
011902*01  -COPY W0008  -PRE GMTA-                                              
011910     05  FILLER                  PIC X.                                   
012000     EJECT                                                                
012101 PROCEDURE DIVISION  USING GMTA-PCB.                                      
012110     ENTRY 'DLITCBL' USING GMTA-PCB.                                      
012200                                                                          
012400     SKIP2                                                                
012500     PERFORM A-INIT                                                       
012610     PERFORM S01-LAES-W47911                                              
012700     PERFORM UNTIL END-OF-W47911                                          
012710       IF OBEKR-IDDISTR = W-IDDISTR                                       
012720          PERFORM B-KONTROLLERA-LIGGTID                                   
012730       ELSE                                                               
012800          MOVE OBEKR-IDDISTR   TO W-IDDISTR                               
012810          MOVE OBEKR-IDKUNDNR  TO W-IDKUNDNR                              
012811          PERFORM IMS-GU-GMTA-WDB201                                      
012812                                                                          
012813*****************************************************************         
012814*** OM DISTRIKTET SAKNAS PÅ KUNDREGISTRET SÄTTS RENSNINGS-    ***         
012815*** TIDEN DEFAULT TILL 1 VECKA.                               ***         
012816*****************************************************************         
012817                                                                          
012824          IF SEGMENT-FINNS                                                
012825            MOVE GMT-KVVECKOR-OB TO WS-LIGGTID                            
012826          ELSE                                                            
012827            MOVE 1                TO WS-LIGGTID                           
012830          END-IF                                                          
013100          PERFORM B-KONTROLLERA-LIGGTID                                   
013200       END-IF                                                             
013410       PERFORM S01-LAES-W47911                                            
013500     END-PERFORM                                                          
013700                                                                          
013800     PERFORM Z-FINIT                                                      
013900                                                                          
014000     MOVE ZERO TO RETURN-CODE                                             
014100     GOBACK                                                               
014200     .                                                                    
014300     EJECT                                                                
014400 A-INIT SECTION.                                                          
014501                                                                          
014510     OPEN INPUT  W47911                                                   
014601                                                                          
014610     OPEN OUTPUT W47917                                                   
014700     SKIP2                                                                
014800     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
014900     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
015000     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
015100     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
015210     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015220                                                                          
015230     MOVE 'AAMMDD'       TO DAT-KDDATFORM                                 
015240     MOVE DAGENS-DATUM   TO DAT-I-TIDATUM                                 
015250                                                                          
015260     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
015270                     DAT-O-TIDATUM DAT-KDSVAR                             
015280                                                                          
015290     IF DAT-KDSVAR-OK                                                     
015300       MOVE DAT-TIAAVV-GRP TO WS-TIIDAG                                   
015301       COMPUTE WS-TIIDAG    = (DAT-TIAA-VECKA * 100)                      
015302           ADD DAT-TIVV    TO WS-TIIDAG                                   
015310     ELSE                                                                 
015320       MOVE 'FEL I DATUMKONV AV DAGENS DATUM' TO FELTEXT-STR              
015321       DISPLAY FELTEXT                                                    
015322       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
015330     END-IF                                                               
015400     .                                                                    
015500     EJECT                                                                
015510 B-KONTROLLERA-LIGGTID SECTION.                                           
015520                                                                          
015521     MOVE 'AAMMDD'       TO DAT-KDDATFORM                                 
015530     MOVE OBEKR-TIREGDAT TO DAT-I-TIDATUM                                 
015540                                                                          
015550     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
015560                         DAT-O-TIDATUM DAT-KDSVAR                         
015570                                                                          
015580     IF DAT-KDSVAR-OK                                                     
015582       COMPUTE WS-TIREGDAT  = (DAT-TIAA-VECKA * 100)                      
015584           ADD DAT-TIVV    TO WS-TIREGDAT                                 
015591     ELSE                                                                 
015592       MOVE 'FEL I DATUMKONV AV TIREGDAT   ORDER=' TO FELTEXT-STR         
015593       DISPLAY FELTEXT OBEKR-IDORDER                                      
015594       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
015595     END-IF                                                               
015596*                                                                         
015601     CALL W009VADD USING WS-TIREGDAT WS-LIGGTID                           
015602                                                                          
015603     MOVE WS-TIREGDAT       TO WS-TIORDER                                 
015604     MOVE WS-TIORDER   TO TMP1-YYWW                                       
015605     MOVE WS-TIIDAG    TO TMP2-YYWW                                       
015606     PERFORM WY2000P3                                                     
015609     IF TMP1-YYWW > TMP2-YYWW                                             
015610        CONTINUE                                                          
015611     ELSE                                                                 
015612        MOVE '017'          TO RENS-IDPTYP                                
015613        MOVE OBEKR-IDORDER  TO RENS-IDORDER                               
015614        MOVE OBEKR-IDDISTR  TO RENS-IDDISTR                               
015615        MOVE OBEKR-IDKUNDNR TO RENS-IDKUNDNR                              
015616        PERFORM S11-SKRIV-W47917                                          
015617     END-IF                                                               
015618     .                                                                    
015619     SKIP2                                                                
015700 Z-FINIT SECTION.                                                         
015701     CLOSE W47911                                                         
015710           W47917                                                         
015801                                                                          
015802     MOVE 'S' TO POSTSUM-OPKOD                                            
015810     CALL POSTSUM USING POSTSUM-PARM                                      
015900     .                                                                    
016000     SKIP2                                                                
016002 S01-LAES-W47911  SECTION.                                                
016003                                                                          
016004     READ W47911 INTO OBEKR-AREA                                          
016005     AT END                                                               
016006        MOVE HIGH-VALUE TO OBEKR-AREA                                     
016007        SET END-OF-W47911 TO TRUE                                         
016008                                                                          
016009     NOT AT END                                                           
016010        MOVE 'W47911' TO POSTSUM-FDNAMN                                   
016011        MOVE 'W47917D1' TO POSTSUM-DDNAMN2                                
016012        MOVE '011'      TO POSTSUM-TRANSTYP                               
016013        CALL POSTSUM USING POSTSUM-PARM                                   
016014     END-READ                                                             
016020     .                                                                    
016030     SKIP2                                                                
016102 S11-SKRIV-W47917 SECTION.                                                
016103                                                                          
016104     WRITE RENS-POST FROM RENS-AREA                                       
016105                                                                          
016106     MOVE RENS-IDPTYP TO POSTSUM-TRANSTYP                                 
016107     MOVE 'W47917' TO POSTSUM-FDNAMN                                      
016108     MOVE 'W47917D2' TO POSTSUM-DDNAMN2                                   
016109     CALL POSTSUM USING POSTSUM-PARM                                      
016110     .                                                                    
016120     SKIP3                                                                
016900* --- IMS SEKTIONER ---                                                   
017000     SKIP3                                                                
017210 IMS-GU-GMTA-WDB201      SECTION.                                         
017220     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
017230            DELIMITED BY SIZE INTO SSA1                                   
017240     MOVE '  GE' TO GODK-STATUSKODER                                      
017250     CALL CBLTDLI USING GU GMTA-PCB GMT-WDB201 SSA1                       
017260     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
017270     PERFORM IMS-STATUSKONTROLL                                           
017280     SKIP3                                                                
017290     .                                                                    
017300 IMS-STATUSKONTROLL SECTION.                                              
017400     SKIP2                                                                
017500     SET STATUS-IX TO 1                                                   
017600     SEARCH GODK-STATUS                                                   
017700       AT END                                                             
017800         MOVE 'FEAKTIG STATUSKOD FRÅN IMS' TO FELTEXT-STR                 
017900         DISPLAY FELTEXT                                                  
018000         CALL FELLOG                                                      
018100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
018200     END-SEARCH                                                           
018300     .                                                                    
018301     SKIP3                                                                
018302     EJECT                                                                
018303*    -COPY WY2000P3                                                       
