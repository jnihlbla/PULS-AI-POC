000100 ID  DIVISION.                                                            
000200                                                                          
000300 PROGRAM-ID.    W5137B00.                                                 
000400 AUTHOR.        CHRISTINA BRUHN.                                          
000500 DATE-WRITTEN.  FEBRUARI 1989.                                            
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET LÄSER WDK7 MED SB.                                    
001100*        SKAPAR EN URVALSFIL MED ARTIKLAR SOM SKALL INVENTERAS.           
001200*        ARTIKLAR SOM TIDIGARE INVENTERATS UNDER ÅRET TAS EJ MED,         
001300*        ARTIKLAR MED SALDO = 0 TAS EJ HELLER MED.                        
001400*        ÖVRIGA SKRIVS PÅ UTFIL W5137B.                                   
001500*                                                                         
001600*        OBS! VID ÅRETS SISTA KÖRNING SKAPAS URVAL FÖR FÖRSTA             
001700*        ARBETSDAG NYTT ÅR.                                               
001800*        DETTA STYRS AV ÅRTALSFILEN W5137D SOM ÄR NYSKAPAD DÅ.            
001900     EJECT                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*--- URVALSFIL :                                                          
002700                                                                          
002800     SELECT W5137B                       ASSIGN TO UT-S-W5137BD1.         
002900                                                                          
003000     SELECT W5137D                       ASSIGN TO UT-S-W5137BD2.         
003100                                                                          
003200     SELECT SORTFIL                      ASSIGN TO UT-S-W5137BDS.         
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP2                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W5137B                                                               
003900     LABEL RECORD    STANDARD                                             
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS 0.                                                    
004200                                                                          
004300*01  POST -COPY W51371 -PRE  SORTWS- -L.                                  
004400     SKIP3                                                                
004500 FD  W5137D                                                               
004600     LABEL RECORD    STANDARD                                             
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS 0.                                                    
004900                                                                          
005000*01  INPOST -COPY W5137D             -L.                                  
005100     EJECT                                                                
005200 SD  SORTFIL.                                                             
005300     SKIP2                                                                
005400 01  SORT-POST.                                                           
005500     03   -COPY W51371 -PRE  SORT-                                        
005600     EJECT                                                                
005700 WORKING-STORAGE SECTION.                                                 
005800                                                                          
005900 77  IDPGM                       PIC X(8)  VALUE 'W5137B00'.              
006000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(3) VALUE +16.                     
006100 77  JA                          PIC X       VALUE 'J'.                   
006200 77  NEJ                         PIC X       VALUE 'N'.                   
006300 77  EOF                         PIC X       VALUE 'N'.                   
006400                                                                          
006500 01  FELTEXT.                                                             
006600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006800                                                                          
006900 01  DYNAMISKA-SUBPROGRAM.                                                
007000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
007400     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
007900                                                                          
008000 01  W-URVALS-AA                 PIC 9(2).                                
008100                                                                          
008200 01  JUST-AAVVD                  PIC 9(5).                                
008300     EJECT                                                                
008400*01  -COPY W0005      -PRE POSTSUM-                                       
008500     EJECT                                                                
008600 01  DATUMKORT-ID                PIC X(6)  VALUE 'WDATUM'.                
008700*01  -COPY WDATKORT                                                       
008800     EJECT                                                                
008900*01  -COPY WDATAREA                                                       
009000     EJECT                                                                
009100 01  FILLER                      PIC X(8)    VALUE 'IN-AREA'.             
009200 01  INAREA.                                                              
009300*    03  -COPY W5137D   -PRE IN-.                                         
009400     EJECT                                                                
009500 01  FILLER                      PIC X(8)    VALUE 'UT-AREA'.             
009600                                                                          
009700*01  AREA -COPY W51371 -PRE SORTWS-                                       
009800                                                                          
009900 01  SORT-RETURN-X               PIC X(2)    VALUE SPACE.                 
010000     EJECT                                                                
010100 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
010200                                                                          
010300 01  IMS-WS.                                                              
010400                                                                          
010500     03  STATUS-WS               PIC X(2).                                
010600        88  SEGMENT-SLUT                     VALUE 'GB'.                  
010700        88  SEGMENT-FINNS                    VALUE '  ' 'GA' 'GK'.        
010800        88  SEGMENT-SAKNAS                   VALUE 'GE'.                  
010900                                                                          
011000     03  GODK-STATUSKODER.                                                
011100         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
011200                                                                          
011300*01  -COPY W0003                                                          
011400     EJECT                                                                
011500 01  DLI-IO-AREA.                                                         
011600     03  IO-AREA              PIC X(400).                                 
011700     SKIP3                                                                
011800*    03  FILLER -COPY WDK701 -RED IO-AREA                                 
011900     EJECT                                                                
012000*    03  FILLER -COPY WDK711 -RED IO-AREA                                 
012100     EJECT                                                                
012200 LINKAGE SECTION.                                                         
012300     SKIP3                                                                
012400*01  -COPY W0008   -PRE WDK7-                                             
012500         05  FILLER           PIC X(1).                                   
012600     EJECT                                                                
012700 PROCEDURE DIVISION USING  WDK7-PCB.                                      
012800     ENTRY 'DLITCBL' USING WDK7-PCB.                                      
012900                                                                          
013000     PERFORM A-INIT                                                       
013100                                                                          
013200     SORT SORTFIL ASCENDING KEY SORT-IDDC                                 
013300                                SORT-IDARTNR                              
013400                  INPUT PROCEDURE B-SORT-INPUT                            
013500                  GIVING W5137B                                           
013600                                                                          
013700     IF SORT-RETURN NOT = 0                                               
013800       MOVE SORT-RETURN TO SORT-RETURN-X                                  
013900       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT '                     
014000              DELIMITED BY SIZE                                           
014100              INTO FELTEXT-STR                                            
014200       DISPLAY FELTEXT                                                    
014300       PERFORM S99-ABEND                                                  
014400     ELSE                                                                 
014500       PERFORM Z-FINIT                                                    
014600       MOVE ZERO TO RETURN-CODE                                           
014700       GOBACK                                                             
014800     END-IF                                                               
014900     .                                                                    
015000     EJECT                                                                
015100 A-INIT SECTION.                                                          
015200                                                                          
015300     OPEN INPUT W5137D                                                    
015400     PERFORM S01-LAS-IN-POST                                              
015500     IF EOF = NEJ                                                         
015600       MOVE IN-TIAAAA(3:2)  TO W-URVALS-AA                                
015700     END-IF                                                               
015800                                                                          
015900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016000                                                                          
016100     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
016200                                                                          
016300     DISPLAY '****  DAGENS ÅR  :20' W-URVALS-AA                           
016400                                                                          
016500     MOVE SPACE        TO SORTWS-W51371                                   
016600     .                                                                    
016700     EJECT                                                                
016800 B-SORT-INPUT SECTION.                                                    
016900                                                                          
017000     PERFORM IMS-GET-WDK7                                                 
017100                                                                          
017200     PERFORM UNTIL SEGMENT-SLUT                                           
017300                                                                          
017400        EVALUATE WDK7-SEG-NAME-FB                                         
017500           WHEN 'WDK701  ' MOVE SART-IDARTNR  TO SORTWS-IDARTNR           
017600           WHEN 'WDK711  ' PERFORM B-SKRIV-POST                           
017700                                                                          
017800        END-EVALUATE                                                      
017900                                                                          
018000        PERFORM IMS-GET-WDK7                                              
018100                                                                          
018200     END-PERFORM                                                          
018300     .                                                                    
018400     EJECT                                                                
018500                                                                          
018600 B-SKRIV-POST SECTION.                                                    
018700                                                                          
018800     MOVE SLAG-ADLAGOMR    TO SORTWS-ADLAGOMR                             
018900     MOVE SLAG-ADGANG      TO SORTWS-ADGANG                               
019000     MOVE SLAG-ADPLATS     TO SORTWS-ADPLATS                              
019100     MOVE SLAG-IDDC        TO SORTWS-IDDC                                 
019200     MOVE SPACE            TO SORTWS-IDURVAL-INV                          
019300     MOVE ZERO             TO SORTWS-KDVVKL                               
019400     MOVE SLAG-TIINVDAT    TO JUST-AAVVD                                  
019500                                                                          
019600     IF (JUST-AAVVD(1:2) NOT = W-URVALS-AA) OR                            
019700       SLAG-TIINVDAT = 0                                                  
019800       IF (SLAG-KVLS NOT = +0) OR (SLAG-KVAKS-SDC NOT = +0)               
019900         OR (SLAG-KVEFRS NOT= +0)                                         
020000         RELEASE SORT-POST FROM SORTWS-AREA                               
020100                                                                          
020200         MOVE SLAG-IDDC    TO POSTSUM-TRANSTYP                            
020300         MOVE 'W5137B'     TO POSTSUM-FDNAMN                              
020400         MOVE 'W5137BD1'   TO POSTSUM-DDNAMN2                             
020500         CALL POSTSUM USING POSTSUM-PARM                                  
020600       END-IF                                                             
020700     END-IF                                                               
020800     .                                                                    
020900 Z-FINIT  SECTION.                                                        
021000                                                                          
021100     MOVE 'S' TO POSTSUM-OPKOD                                            
021200     CALL POSTSUM USING POSTSUM-PARM                                      
021300                                                                          
021400     CLOSE W5137D                                                         
021500     .                                                                    
021600     EJECT                                                                
021700 S01-LAS-IN-POST SECTION.                                                 
021800                                                                          
021900     READ W5137D INTO INAREA                                              
022000     AT END                                                               
022100         MOVE JA TO EOF                                                   
022200     END-READ                                                             
022300     .                                                                    
022400     EJECT                                                                
022500 S99-ABEND SECTION.                                                       
022600     MOVE 'S' TO POSTSUM-OPKOD                                            
022700     CALL POSTSUM USING POSTSUM-PARM                                      
022800                                                                          
022900     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
023000     .                                                                    
023100     EJECT                                                                
023200*         * I M S  S E C T I O N                                          
023300                                                                          
023400 IMS-GET-WDK7         SECTION.                                            
023500                                                                          
023600     MOVE '  GAGKGBGAGK' TO GODK-STATUSKODER                              
023700     CALL CBLTDLI USING GN WDK7-PCB IO-AREA                               
023800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
023900     PERFORM IMS-STATUSKONTROLL                                           
024000     .                                                                    
024100     SKIP3                                                                
024200 IMS-STATUSKONTROLL   SECTION.                                            
024300                                                                          
024400     SET STATUS-IX TO 1                                                   
024500     SEARCH GODK-STATUS                                                   
024600       AT END CALL FELLOG                                                 
024700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
024800     END-SEARCH                                                           
024900     .                                                                    
