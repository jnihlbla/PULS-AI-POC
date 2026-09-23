001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W6114800.                                                
001300 AUTHOR.         NIHLBLAD JOHAN.                                          
001400 DATE-WRITTEN.   02/09/13.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNKTION:                                                            
001800*        LISTAR ARTNR. SOM INTE HAR NÅGOT BEHOV INOM 10 DAGAR             
001900*                                                                         
002010*        PROGRAMMET LÄSER      WDK6                                       
002100*                                                                         
002200*    ABENDKODER:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003302*          --- UTFIL FRÅN W6114800                                        
003310     SELECT W61148                     ASSIGN TO W61148D1.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  W61148                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003905                                                                          
003910*01  POST -COPY W61148 -PRE  UT-  -L.                                     
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W6114800'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600 77  IX                          PIC S9(2)   VALUE ZERO.                  
004700 77  MAX-IX                      PIC S9(2)   VALUE +4.                    
004710 01  WS-AREA.                                                             
004720   03 WS-IDARTNR                 PIC S9(9)   VALUE ZERO.                  
004721   03 WS-FELTEXT                 PIC X(30)   VALUE SPACE.                 
004722 01  W-BEHOV.                                                             
004730   03 W-KVBEHOV-CD OCCURS 4      PIC S9(7)V9(2)  VALUE ZERO.              
004800     EJECT                                                                
004810*01  ARBETSFAELT.                                                         
004820*                                                                         
004830*    03 DAGENS-DATUM.                                                     
004840*        05 DAGENS-AAR           PIC 9(4).                                
004850*        05 DAGENS-MAANAD        PIC 9(2).                                
004860*        05 DAGENS-DAG           PIC 9(2).                                
004900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005000*01  FILLER REDEFINES DAGENS-DATUM.                                       
005100*    03  DAGENS-DATUM-AAR        PIC 9(2).                                
005200*    03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005300*    03  DAGENS-DATUM-DAG        PIC 9(2).                                
005400     EJECT                                                                
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600*                                                                         
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006001     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006002     03  W61158                  PIC X(8)    VALUE 'W61158'.              
006010     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
006020     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
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
007201     EJECT                                                                
007202*01  AREA   -COPY W61158 -PRE W61158-                                     
007203     EJECT                                                                
007204*    ---PARAMETRAR TILL WORKDAY                                           
007205*01  -COPY WORKAREA                                                       
007206     EJECT                                                                
007302*    ---PARAMETRAR TILL DATKONV                                           
007303*01  -COPY WDATAREA                                                       
007304     EJECT                                                                
007325     EJECT                                                                
007326 01  UT-AREA-START               PIC X(24)   VALUE                        
007327                                 'UT-AREA-START  '.                       
007328     SKIP2                                                                
007329                                                                          
007330*01  AREA -COPY W61148     -PRE UT-                                       
007400     EJECT                                                                
007500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  NYCKLAR-TILL-DLI.                                                    
008101     03  W-IDARTNR-X.                                                     
008102         05  W-IDARTNR           PIC S9(5)   VALUE ZERO COMP-3.           
008103     03  W-KDSEGKEY-X.                                                    
008110         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
008200     SKIP2                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FINNS                       VALUE '  '.                  
008600     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
008700     SKIP2                                                                
008800 01  GODK-STATUSKODER.                                                    
008900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009000     SKIP3                                                                
009100 01  SSA1                        PIC X(64).                               
009200 01  SSA2                        PIC X(64).                               
009300     EJECT                                                                
009400*    --- IMS FUNKTIONSKODER                                               
009500*01  -COPY W0003                                                          
009600     EJECT                                                                
009700*    ---  DLI INPUT-OUTPUT AREA                                           
009710 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK6'.                        
009720 01  DLI-IO-WDK6.                                                         
009730     03  IO-AREA-K6              PIC X(900) VALUE SPACE.                  
009740     03  WDK601 REDEFINES IO-AREA-K6.                                     
009750*        05  -COPY WDK601                                                 
009760     03  WDK611 REDEFINES IO-AREA-K6.                                     
009770*        05  -COPY WDK611                                                 
009780     EJECT                                                                
010300 LINKAGE SECTION.                                                         
010400                                                                          
010501                                                                          
010502*01  -COPY W0008  -PRE WDK6-                                              
010503     05  WDK6-KEY-AREA-IDARTNR   PIC S9(9) COMP-3.                        
010504*01  -COPY W0008  -PRE WDK6A-.                                            
010505     05  FILLER                  PIC X.                                   
010506     EJECT                                                                
010507*01  -COPY W0008  -PRE WDK7-.                                             
010508     05  FILLER                  PIC X.                                   
010509     EJECT                                                                
010510*01  -COPY W0008  -PRE 2501-                                              
010511     05  FILLER                  PIC X.                                   
010512*01  -COPY W0008  -PRE WDK6-2-.                                           
010513     05  FILLER              PIC X.                                       
010514     EJECT                                                                
010515*01  -COPY W0008  -PRE WDK7-2-.                                           
010516     05  FILLER              PIC X.                                       
010517     EJECT                                                                
010518*01  -COPY W0008  -PRE WDL6-                                              
010519     05  FILLER                  PIC X.                                   
010520     EJECT                                                                
010521*01  -COPY W0008  -PRE WDD7A-                                             
010522     05  FILLER                  PIC X.                                   
010523     EJECT                                                                
010524*01  -COPY W0008  -PRE WDB6-                                              
010525     05  FILLER                  PIC X.                                   
010526                                                                          
010530     EJECT                                                                
010701 PROCEDURE DIVISION  USING WDK6-PCB WDK6A-PCB                             
010702                           WDK7-PCB 2501-PCB                              
010703                           WDK6-2-PCB WDK7-2-PCB                          
010704                           WDL6-PCB WDD7A-PCB                             
010705                           WDB6-PCB.                                      
010706 MAIN SECTION.                                                            
010710     ENTRY 'DLITCBL' USING WDK6-PCB WDK6A-PCB                             
010720                           WDK7-PCB 2501-PCB                              
010800                           WDK6-2-PCB WDK7-2-PCB                          
011010                           WDL6-PCB WDD7A-PCB                             
011020                           WDB6-PCB.                                      
011100     PERFORM A-INIT                                                       
011200                                                                          
011301     PERFORM IMS-GET-WDK6                                                 
011302     PERFORM UNTIL SEGMENT-SAKNAS                                         
011303       EVALUATE WDK6-SEG-NAME-FB                                          
011304         WHEN 'WDK601'                                                    
011305           MOVE ART-IDARTNR       TO WS-IDARTNR                           
011306                                     W61158-IDARTNR                       
011307           IF CLAG-FLCDART = 'J'                                          
011308              CALL W61158 USING W61158-AREA WDK6A-PCB WDK7-PCB            
011309                                2501-PCB                                  
011310                                WDK6-2-PCB WDK7-2-PCB                     
011311                                WDL6-PCB WDD7A-PCB WDB6-PCB               
011312              MOVE +1 TO IX                                               
011313              PERFORM UNTIL IX > MAX-IX                                   
011314                MOVE W61158-KVBEHOV-CD (IX)  TO W-KVBEHOV-CD (IX)         
011315                ADD +1 TO IX                                              
011316              END-PERFORM                                                 
011317           END-IF                                                         
011318         WHEN 'WDK611'                                                    
011319           MOVE +1 TO IX                                                  
011320           PERFORM UNTIL IX > MAX-IX                                      
011321             IF W-KVBEHOV-CD (IX) = ZERO                                  
011322             AND CLAG-KVLS-CD (IX) > ZERO                                 
011323                MOVE WS-IDARTNR          TO UT-IDARTNR                    
011324                MOVE CLAG-ADART-CD (IX)  TO UT-ADART-CD                   
011325                MOVE CLAG-KVLS-CD (IX)   TO UT-KVLS-CD                    
011326                MOVE CLAG-ADART          TO UT-ADART                      
011327                MOVE CLAG-KVLS           TO UT-KVLS                       
011328                PERFORM S11-SKRIV-W61148                                  
011329             END-IF                                                       
011330             ADD +1 TO IX                                                 
011331           END-PERFORM                                                    
011332       END-EVALUATE                                                       
011333       PERFORM IMS-GET-WDK6                                               
011340     END-PERFORM                                                          
011400     PERFORM Z-FINIT                                                      
011500                                                                          
011600     MOVE ZERO TO RETURN-CODE                                             
011700     GOBACK                                                               
011800     .                                                                    
011900     EJECT                                                                
012000 A-INIT SECTION.                                                          
012201                                                                          
012210     OPEN OUTPUT W61148                                                   
012300                                                                          
012301     MOVE ZERO               TO WS-IDARTNR                                
012302     MOVE +1 TO IX                                                        
012303     PERFORM UNTIL IX > MAX-IX                                            
012304        MOVE ZEROES          TO W-KVBEHOV-CD (IX)                         
012305        ADD +1 TO IX                                                      
012306     END-PERFORM                                                          
012307     ACCEPT DAGENS-DATUM  FROM DATE                                       
012308     MOVE DAGENS-DATUM       TO WORK-TIAAMMDD-FOM                         
012309     MOVE 002                TO WORK-KDCALL                               
012310     MOVE '11'               TO WORK-IDDC                                 
012311*                                                                         
012312*    ANTALET DAGAR LIGGER HÅRDKODAT                                       
012313*                                                                         
012314     MOVE 10                 TO WORK-KVWORKD                              
012315     CALL WORKDAY            USING WORK-KDCALL                            
012316                                   WORK-DATE-AREA                         
012317                                   WORK-KDSVAR                            
012318     IF WORK-KDSVAR-OK                                                    
012319        MOVE WORK-TIAAMMDD-TOM TO DAT-I-TIDATUM                           
012320        MOVE 'AAMMDD'          TO DAT-KDDATFORM                           
012321                                                                          
012322        CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                   
012323                        DAT-O-TIDATUM DAT-KDSVAR                          
012324                                                                          
012325        IF DAT-KDSVAR-OK                                                  
012326           MOVE DAT-TIAAVVD TO W61158-TIAAVVD                             
012327        ELSE                                                              
012328           MOVE 'FEL I DATKONV' TO WS-FELTEXT                             
012329           CALL FELLOG                                                    
012330        END-IF                                                            
012331     ELSE                                                                 
012332        MOVE 'FEL I WORKDAY' TO WS-FELTEXT                                
012333        CALL FELLOG                                                       
012340     END-IF                                                               
012510     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012700     .                                                                    
012800     EJECT                                                                
012900 Z-FINIT SECTION.                                                         
013010     CLOSE W61148                                                         
013101     SKIP2                                                                
013102     MOVE 'S' TO POSTSUM-OPKOD                                            
013110     CALL POSTSUM USING POSTSUM-PARM                                      
013200     .                                                                    
013401     EJECT                                                                
013402 S11-SKRIV-W61148 SECTION.                                                
013403                                                                          
013404     WRITE UT-POST FROM UT-AREA                                           
013405                                                                          
013406*    MOVE UT-IDPTYP TO POSTSUM-TRANSTYP                                   
013407     MOVE 'W61148' TO POSTSUM-FDNAMN                                      
013408     MOVE 'W61148D1' TO POSTSUM-DDNAMN2                                   
013409     CALL POSTSUM USING POSTSUM-PARM                                      
013410     .                                                                    
013600     EJECT                                                                
013700*S99-ABEND SECTION.                                                       
013800*                                                                         
013901*    SKIP2                                                                
013902*    MOVE 'S' TO POSTSUM-OPKOD                                            
013910*    CALL POSTSUM USING POSTSUM-PARM                                      
014000*    CALL ABEND USING RKOD-ABEND                                          
014100*    .                                                                    
014200*    EJECT                                                                
014300* --- IMS SEKTIONER ---                                                   
014400                                                                          
014501                                                                          
014502 IMS-GET-WDK6   SECTION.                                                  
014503                                                                          
014504     CALL CBLTDLI USING GN WDK6-PCB DLI-IO-WDK6                           
014505     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
014506     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
014507     PERFORM IMS-STATUSKONTROLL                                           
014510     .                                                                    
014600     EJECT                                                                
014700 IMS-STATUSKONTROLL SECTION.                                              
014800                                                                          
014900     SET STATUS-IX TO 1                                                   
015000     SEARCH GODK-STATUS                                                   
015100       AT END                                                             
015200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
015300           DELIMITED BY SIZE INTO FELTEXT                                 
015400         DISPLAY FELTEXT                                                  
015500         CALL FELLOG                                                      
015600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
015700         CONTINUE                                                         
015800     END-SEARCH                                                           
015900     .                                                                    
