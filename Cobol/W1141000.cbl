001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W1141000.                                                
001300 AUTHOR.         EGHOLT CONNY.                                            
001400 DATE-WRITTEN.   05/09/29.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNKTION:                                                            
001701*                                                                         
001710*        PROGRAMMET LADDAR WDF7                                           
001800*                  FRÅN DEN SENASTE FILEN FRÅN FORD SYSTEM MPNR           
001810*                                                                         
001811*        IN-FILEN ÄR SORTERAD PÅ                                          
001820*        * POSTTYP          (STIGANDE)  "IDPTYP"                          
001821*        * VOLVO PARTNO     (STIGANDE)  "IDARTNR"                         
001822*        * ACTIVE FLAG      (FALLANDE)  "FLGEMFMC"                        
001823*        * LAST CHANGE DATE (FALLANDE)  "DAREGFMC"                        
001824*        * LAST CHANGE TIME (FALLANDE)  "TIREGFMC"                        
001830*                                                                         
001840*        INFILENS COPYTEXT W11410 HETER PC3248F1 I KDP                    
001841*        INFILENS NAMN I KDP ÄR "PC32Q.IMFMC1.PC3248F1(+0)"               
001850*                                                                         
001900*        HEADER & TRAILER INFO FRÅN FILEN SKRIVS PÅ DISPLAY               
001901*        ANTAL POSTER PÅ INFILEN SKRIVS I POSTSUM                         
001910*        ANTAL LADDADE POSTER I WDF7 SKRIVS I POSTSUM                     
002000*                                                                         
002100*                                                                         
002200*    ABENDKODER:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003302*          --- RECORDS FROM DAILY MPNR EXTRACTION FROM FORD SYSTEM        
003303*          --- FILE IS SORTED AND FIXED IN A PREVIOUS JOB                 
003310     SELECT W11410                     ASSIGN TO W11410D1.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  W11410                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003905                                                                          
003910*01  -COPY W11410      -L.                                                
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W1141000'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004410 77  YES                         PIC X       VALUE 'Y'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004701                                                                          
004702 77  WS-FILDAT                   PIC X(10).                               
004703 77  WS-FILTID                   PIC X(08).                               
004704 77  WS-RECORDS                  PIC Z(08).                               
004705 77  WS-SPAR-IN-IDARTNR          PIC X(08)   VALUE '00000000'.            
004706                                                                          
004707 77  W11410-EOF-SW               PIC X       VALUE 'N'.                   
004710     88  END-OF-W11410                       VALUE 'J'.                   
004720                                                                          
004800     EJECT                                                                
004810 01  FILLER                      PIC X(16)   VALUE 'DATUM-FÄLT'.          
004900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005000 01  FILLER REDEFINES DAGENS-DATUM.                                       
005100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005310                                                                          
005320 01  WS-DATETIME-FMC.                                                     
005340     03 WS-DAREGFMC.                                                      
005341        05 WS-DAREG-YYYY         PIC X(4).                                
005342        05 FILLER                PIC X.                                   
005343        05 WS-DAREG-MM           PIC X(2).                                
005344        05 FILLER                PIC X.                                   
005345        05 WS-DAREG-DD           PIC X(2).                                
005346                                                                          
005350     03 WS-TIREGFMC.                                                      
005351        05 WS-TIREG-HH           PIC X(2).                                
005352        05 FILLER                PIC X.                                   
005353        05 WS-TIREG-MM           PIC X(2).                                
005354        05 FILLER                PIC X.                                   
005355        05 WS-TIREG-SS           PIC X(2).                                
005360                                                                          
005380     03 WS-TIDATETIME-WK-X.                                               
005390        05 WS-DAREG-YYYY-WK      PIC X(4).                                
005392        05 WS-DAREG-MM-WK        PIC X(2).                                
005394        05 WS-DAREG-DD-WK        PIC X(2).                                
005397        05 WS-TIREG-HH-WK        PIC X(2).                                
005399        05 WS-TIREG-MM-WK        PIC X(2).                                
005401        05 WS-TIREG-SS-WK        PIC X(2).                                
005402                                                                          
005403     03 WS-TIDATETIME-WK REDEFINES WS-TIDATETIME-WK-X.                    
005404        05 WS-TIDATETIME-WK-NUM  PIC 9(14).                               
005405                                                                          
005406     03 WS-TIDATETIME-9KOMPL     PIC 9(14).                               
005407                                                                          
005410     EJECT                                                                
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600*                                                                         
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006100     SKIP2                                                                
006200*    --- PARAMETRAR TILL ABEND                                            
006300                                                                          
006400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006700     SKIP2                                                                
006800 01  FELTEXT.                                                             
006900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007101     EJECT                                                                
007102*    --- PARAMETRAR TILL POSTSUM                                          
007103*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007301     EJECT                                                                
007302 01  IN-AREA-START               PIC X(24)   VALUE                        
007303                                 'IN-AREA-START  '.                       
007304     SKIP2                                                                
007305                                                                          
007310*01  AREA -COPY W11410     -PRE IN-                                       
007400     EJECT                                                                
007410                                                                          
007500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
007910                                                                          
008000 01  NYCKLAR-TILL-DLI.                                                    
008101     03  W-WDF701KY-X.                                                    
008102         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008103         05  W-IDPRTNER-FORD     PIC S9(5)   VALUE +1   COMP-3.           
008200     SKIP2                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FINNS                       VALUE '  '.                  
008600     88  SEGMENT-FINNS-REDAN                 VALUE 'LB'.                  
008800     SKIP2                                                                
008900 01  GODK-STATUSKODER.                                                    
009000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(64).                               
009400     EJECT                                                                
009410                                                                          
009500*    --- IMS FUNKTIONSKODER                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009800                                                                          
009900*    ---  DLI INPUT-OUTPUT AREA                                           
010000                                                                          
010001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF701'.                      
010002 01  DLI-IO-WDF701.                                                       
010010*    03  -COPY WDF701                                                     
010011     EJECT                                                                
010310                                                                          
010400 LINKAGE SECTION.                                                         
010601                                                                          
010602*01  -COPY W0008  -PRE WDF7-                                              
010610     05  FILLER                  PIC X.                                   
010700     EJECT                                                                
010800                                                                          
010801 PROCEDURE DIVISION  USING WDF7-PCB.                                      
010802 MAIN SECTION.                                                            
010810     ENTRY 'DLITCBL' USING WDF7-PCB.                                      
010900                                                                          
011100                                                                          
011200     PERFORM A-INIT                                                       
011300                                                                          
011410     PERFORM S01-LAES-W11410                                              
011500     PERFORM UNTIL END-OF-W11410                                          
011600       IF IN-IDPTYP = '2'                                                 
011700         IF  IN-IDPRTNER-1 = 1                                            
011701         AND IN-KDPRTNER-1 = 'FORD'                                       
011702         AND IN-IDPRTNER-2 = 5                                            
011703         AND IN-KDPRTNER-2 = 'VL00'                                       
011710*          -- LADDA FORD-VOLVO-ARTIKEL                                    
011800           PERFORM B-KOLLA-LADDA-EN-POST                                  
012000         END-IF                                                           
012010       ELSE                                                               
012020         IF IN-IDPTYP = '1'                                               
012021           MOVE IN-W11410(10:10) TO WS-FILDAT                             
012022           MOVE IN-W11410(20:8)  TO WS-FILTID                             
012030           DISPLAY '+----------------------------------------+'           
012031           DISPLAY '| I DENNA KÖRNING BEHANDLAS FIL SKAPAD '              
012040           DISPLAY '| DEN ' WS-FILDAT ' KL.' WS-FILTID                    
012100         END-IF                                                           
012110         IF IN-IDPTYP = '9'                                               
012120           MOVE IN-W11410(10:8)  TO WS-RECORDS                            
012150           DISPLAY '| INNEHÅLLANDE TOTALT ' WS-RECORDS 'POSTER'           
012170           DISPLAY '+----------------------------------------+'           
012180         END-IF                                                           
012200       END-IF                                                             
012210       PERFORM S01-LAES-W11410                                            
012300     END-PERFORM                                                          
012400                                                                          
012500                                                                          
012600     PERFORM Z-FINIT                                                      
012700                                                                          
012800     MOVE ZERO TO RETURN-CODE                                             
012900     GOBACK                                                               
013000     .                                                                    
013100     EJECT                                                                
013200 A-INIT SECTION.                                                          
013301                                                                          
013310     OPEN INPUT  W11410                                                   
013500                                                                          
013600     ACCEPT DAGENS-DATUM  FROM DATE                                       
013710     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013720                                                                          
013721     MOVE '00000000' TO WS-SPAR-IN-IDARTNR                                
013730                                                                          
013900     .                                                                    
014000     EJECT                                                                
014001                                                                          
014002 B-KOLLA-LADDA-EN-POST  SECTION.                                          
014003     SKIP2                                                                
014005     IF IN-IDARTNR NUMERIC                                                
014006       IF IN-IDARTNR = WS-SPAR-IN-IDARTNR                                 
014007         CONTINUE                                                         
014008*                 WITH NEXT UNPROCESSED PARTNO                            
014009       ELSE                                                               
014010*        INSERT EVERY FIRST UNIQUE PARTNO FROM INPUT                      
014012         MOVE IN-IDARTNR       TO MPNR-IDARTNR                            
014014         MOVE IN-KDARTUTF      TO MPNR-KDARTUTF                           
014015         MOVE IN-IDPRTNER-1    TO MPNR-IDPRTNER                           
014016         MOVE IN-KDPRTNER-1    TO MPNR-KDPRTNER                           
014017         MOVE IN-IDARTFMC      TO MPNR-IDARTMPNR                          
014018         MOVE IN-BEARTFMC-FORD TO MPNR-BEARTFMC-FORD                      
014019         MOVE IN-BEARTFMC-KDP  TO MPNR-BEARTFMC-KDP                       
014020         MOVE IN-IDPRTNER-OWNER TO MPNR-IDPRTNER-OWNER                    
014021         IF IN-FLGEMFMC = YES                                             
014022           MOVE JA             TO MPNR-FLGEMFMC                           
014025         ELSE                                                             
014027           MOVE NEJ            TO MPNR-FLGEMFMC                           
014032         END-IF                                                           
014033         MOVE IN-BETEXT-FMC    TO MPNR-BETEXT-FMC                         
014034         MOVE IN-BETEXT-KDP    TO MPNR-BETEXT-KDP                         
014035         MOVE IN-IDCDS         TO MPNR-IDCDS                              
014036         MOVE IN-DAREGFMC      TO MPNR-DAREGFMC                           
014037                                    WS-DAREGFMC                           
014038         MOVE IN-TIREGFMC      TO MPNR-TIREGFMC                           
014039                                    WS-TIREGFMC                           
014040         MOVE WS-DAREG-YYYY    TO WS-DAREG-YYYY-WK                        
014041         MOVE WS-DAREG-MM      TO WS-DAREG-MM-WK                          
014042         MOVE WS-DAREG-DD      TO WS-DAREG-DD-WK                          
014043         MOVE WS-TIREG-HH      TO WS-TIREG-HH-WK                          
014044         MOVE WS-TIREG-MM      TO WS-TIREG-MM-WK                          
014045         MOVE WS-TIREG-SS      TO WS-TIREG-SS-WK                          
014046                                                                          
014047         IF WS-TIDATETIME-WK-X NOT NUMERIC                                
014048           MOVE ZERO           TO WS-TIDATETIME-WK-NUM                    
014049         END-IF                                                           
014050                                                                          
014051         SUBTRACT WS-TIDATETIME-WK-NUM FROM 99999999999999                
014052                             GIVING WS-TIDATETIME-9KOMPL                  
014053                                                                          
014054         MOVE WS-TIDATETIME-9KOMPL TO MPNR-TIDATETIME-9KOMPL              
014055         MOVE SPACE              TO MPNR-FILLER                           
014056                                                                          
014057         PERFORM IMS-ISRT-WDF701                                          
014058                                                                          
014059         IF SEGMENT-FINNS-REDAN                                           
014060           DISPLAY 'DUBBLETT ?  OMÖJLIGT ! '                              
014061                       IN-IDARTNR                                         
014062                       IN-KDARTUTF                                        
014063                   ' ' IN-IDARTFMC                                        
014067         END-IF                                                           
014068                                                                          
014069         MOVE IN-IDARTNR     TO  WS-SPAR-IN-IDARTNR                       
014070       END-IF                                                             
014071     ELSE                                                                 
014072*      --- IN-IDARTNR INNEHÅLLER OTILLÅTNA TECKEN                         
014073       CONTINUE                                                           
014074     END-IF                                                               
014075     .                                                                    
014076     EJECT                                                                
014080                                                                          
014100 Z-FINIT SECTION.                                                         
014210     CLOSE W11410                                                         
014301     SKIP2                                                                
014302     MOVE 'S' TO POSTSUM-OPKOD                                            
014310     CALL POSTSUM USING POSTSUM-PARM                                      
014400     .                                                                    
014501     EJECT                                                                
014502 S01-LAES-W11410  SECTION.                                                
014503     READ W11410 INTO IN-AREA                                             
014504     AT END                                                               
014505        MOVE HIGH-VALUE TO IN-AREA                                        
014506        SET END-OF-W11410 TO TRUE                                         
014507                                                                          
014508     NOT AT END                                                           
014509        MOVE 'W11410' TO POSTSUM-FDNAMN                                   
014510        MOVE 'W11410D1' TO POSTSUM-DDNAMN2                                
014513        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
014514        CALL POSTSUM USING POSTSUM-PARM                                   
014515     END-READ                                                             
014520     .                                                                    
014800     EJECT                                                                
015500* --- IMS SEKTIONER ---                                                   
015600                                                                          
015701     EJECT                                                                
015712 IMS-ISRT-WDF701 SECTION.                                                 
015713                                                                          
015714     MOVE 'WDF701 ' TO SSA1                                               
015715     MOVE '  LB' TO GODK-STATUSKODER                                      
015716     CALL CBLTDLI USING ISRT WDF7-PCB DLI-IO-WDF701 SSA1                  
015717     MOVE WDF7-STATUS-CODE TO STATUS-WS                                   
015718     PERFORM IMS-STATUSKONTROLL                                           
015719                                                                          
015720     MOVE 'W11410' TO POSTSUM-FDNAMN                                      
015721     MOVE 'WDF7  ' TO POSTSUM-DDNAMN2                                     
015722     MOVE 'ISRT'   TO POSTSUM-TRANSTYP                                    
015723     CALL POSTSUM USING POSTSUM-PARM                                      
015724     .                                                                    
015730     SKIP3                                                                
015810                                                                          
015900 IMS-STATUSKONTROLL SECTION.                                              
016000                                                                          
016100     SET STATUS-IX TO 1                                                   
016200     SEARCH GODK-STATUS                                                   
016300       AT END                                                             
016400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016500           DELIMITED BY SIZE INTO FELTEXT                                 
016600         DISPLAY FELTEXT                                                  
016700         CALL FELLOG                                                      
016800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
016900         CONTINUE                                                         
017000     END-SEARCH                                                           
017100     .                                                                    
