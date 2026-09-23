000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W0118700.                                                
000400 AUTHOR.         BODIL LINDAHL.                                           
000500 DATE-WRITTEN.   DECEMBER 2002.                                           
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER LEV.REG. WDF1 MED SB.                                      
001000*        SKAPAR FIL W01187.                                               
001300*                                                                         
001400*        PROGRAMMET LÄSER WDF1.                                           
001500*                                                                         
002200*    ABENDKODER:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400*          --- IDLEVNR OCH IDLEVNR-MOTSV                                  
003500     SELECT W01187                     ASSIGN TO W01187D1.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP2                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W01187                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  POST -COPY W01187 -PRE  UT-  -L.                                     
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005700                                                                          
005800*    -- CHECKED BY WY2000                                                 
005900 77  IDPGM            PIC X(8)    VALUE 'W0118700'.                       
006000 77  JA               PIC X       VALUE 'J'.                              
006100 77  NEJ              PIC X       VALUE 'N'.                              
006200                                                                          
008400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008500 01  FILLER REDEFINES DAGENS-DATUM.                                       
008600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008900     EJECT                                                                
009000 01  DYNAMISKA-SUBPROGRAM.                                                
009200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009700     SKIP2                                                                
009800*    --- PARAMETRAR TILL ABEND                                            
010000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010200     SKIP2                                                                
010300 01  FELTEXT.                                                             
010400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010600     EJECT                                                                
010700*    --- PARAMETRAR TILL POSTSUM                                          
010900*01  -COPY W0005   -PRE  POSTSUM-                                         
011000     EJECT                                                                
011100 01  W01187-AREA-START           PIC X(24)   VALUE                        
011200                                 'W01187-AREA-START  '.                   
011400                                                                          
011500*01  AREA -COPY W01187     -PRE UT-                                       
011600     EJECT                                                                
012700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012800*                                                                         
012900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013900*    --- STATUS-KOD FRÅN IMS                                              
014000 01  STATUS-WS                   PIC XX.                                  
014100     88  SEGMENT-FINNS                       VALUE '  '.                  
014200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
014300     SKIP2                                                                
014400 01  GODK-STATUSKODER.                                                    
014500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014600     SKIP3                                                                
014700 01  SSA1                        PIC X(64).                               
014800 01  SSA2                        PIC X(64).                               
014900     EJECT                                                                
015000*    --- IMS FUNKTIONSKODER                                               
015100*01  -COPY W0003                                                          
015200     EJECT                                                                
015300*    ---  DLI INPUT-OUTPUT AREA                                           
015400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
015500*                                                                         
015600 01  DLI-IO-AREA.                                                         
015700     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
015800     SKIP3                                                                
015900     03  WDF101 REDEFINES IO-AREA.                                        
016000*        05  -COPY WDF101                                                 
016100     SKIP3                                                                
016700     EJECT                                                                
016800 LINKAGE SECTION.                                                         
016900*                                                                         
017000*01  -COPY W0008  -PRE WDF1-                                              
017100     05  FILLER                  PIC X.                                   
017200     EJECT                                                                
017300 PROCEDURE DIVISION  USING WDF1-PCB.                                      
017400     ENTRY 'DLITCBL' USING WDF1-PCB.                                      
017500                                                                          
017600     PERFORM A-INIT                                                       
017610                                                                          
017700     PERFORM IMS-GET-WDF1                                                 
017800     PERFORM UNTIL SEGMENT-SLUT                                           
017900        EVALUATE WDF1-SEG-NAME-FB                                         
018000           WHEN 'WDF101  '                                                
018100              PERFORM B-SKAPA-SKRIV-UTFIL                                 
018800        END-EVALUATE                                                      
018900        PERFORM IMS-GET-WDF1                                              
019000     END-PERFORM                                                          
019010                                                                          
019100     PERFORM Z-FINIT                                                      
019200     MOVE ZERO TO RETURN-CODE                                             
019300     GOBACK                                                               
019400     .                                                                    
019500     EJECT                                                                
019800 A-INIT SECTION.                                                          
020000     OPEN OUTPUT W01187                                                   
020200                                                                          
020300     ACCEPT DAGENS-DATUM  FROM DATE                                       
020400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020500     .                                                                    
020600     SKIP3                                                                
020900 B-SKAPA-SKRIV-UTFIL SECTION.                                             
021100     MOVE LEV-IDLEVNR       TO UT-IDLEVNR                                 
021200     MOVE LEV-IDLEVNR-MOTSV TO UT-IDLEVNR-MOTSV                           
021210                                                                          
021300     WRITE UT-POST FROM UT-AREA                                           
021500     MOVE 'W01187'   TO POSTSUM-FDNAMN                                    
021600     MOVE 'W01187D1' TO POSTSUM-DDNAMN2                                   
021700     CALL POSTSUM USING POSTSUM-PARM                                      
021800     .                                                                    
021900     SKIP3                                                                
026800 Z-FINIT SECTION.                                                         
026900     CLOSE W01187                                                         
027100                                                                          
027200     MOVE 'S' TO POSTSUM-OPKOD                                            
027300     CALL POSTSUM USING POSTSUM-PARM                                      
027400     .                                                                    
027500     EJECT                                                                
029800* --- IMS SEKTIONER ---                                                   
030200                                                                          
030300 IMS-GET-WDF1 SECTION.                                                    
030400                                                                          
030500     CALL CBLTDLI USING GN WDF1-PCB DLI-IO-AREA                           
030600     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
030700     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
030800     PERFORM IMS-STATUSKONTROLL                                           
030900     .                                                                    
031000     SKIP3                                                                
031300 IMS-STATUSKONTROLL SECTION.                                              
031400                                                                          
031500     SET STATUS-IX TO 1                                                   
031600     SEARCH GODK-STATUS                                                   
031700       AT END                                                             
031800         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
031900         DISPLAY FELTEXT                                                  
032000         CALL FELLOG                                                      
032100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032200         CONTINUE                                                         
032300     END-SEARCH                                                           
032400     .                                                                    
