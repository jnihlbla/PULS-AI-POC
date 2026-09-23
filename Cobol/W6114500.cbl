001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W6114500.                                                
001300 AUTHOR.         NIHLBLAD JOHAN.                                          
001400 DATE-WRITTEN.   02/09/05.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNKTION:                                                            
001800*        LÄSER WDK6 OCH SKAPAR FIL                                        
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
003302*          --- UTFIL                                                      
003310     SELECT W61145                     ASSIGN TO W61145D1.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  W61145                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003905                                                                          
003910*01  POST -COPY W61145   -PRE  UT-  -L.                                   
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W6114500'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004800     EJECT                                                                
004810 77  IX                          PIC S9(2)   VALUE ZERO.                  
004820 77  MAX-IX                      PIC S9(2)   VALUE +4.                    
004821 01  WS-AREA.                                                             
004830   03 WS-SLASK                   PIC 9(9)    VALUE ZERO.                  
004840   03 WS-REST                    PIC 9(9)    VALUE ZERO.                  
004841   03 WS-RESTSALDO OCCURS 4      PIC 9(9)    VALUE ZERO.                  
004850   03 WS-IDARTNR                 PIC S9(9)   VALUE ZERO.                  
004860    SKIP2                                                                 
004900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005000 01  FILLER REDEFINES DAGENS-DATUM.                                       
005100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005400     EJECT                                                                
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
007302 01  UT-AREA-START               PIC X(24)   VALUE                        
007303                                 'UT-AREA-START  '.                       
007304     SKIP2                                                                
007305                                                                          
007310*01  AREA -COPY W61145       -PRE UT-                                     
007400     EJECT                                                                
007500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FINNS                       VALUE '  '.                  
008600     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
008700     SKIP2                                                                
008800 01  GODK-STATUSKODER.                                                    
008900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009000     SKIP3                                                                
009300     EJECT                                                                
009400*    --- IMS FUNKTIONSKODER                                               
009500*01  -COPY W0003                                                          
009600     EJECT                                                                
009800*    ---  DLI INPUT-OUTPUT AREA                                           
009901 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK6'.                        
009902 01  DLI-IO-WDK6.                                                         
009903     03  IO-AREA-K6              PIC X(900) VALUE SPACE.                  
009910     03  WDK601 REDEFINES IO-AREA-K6.                                     
009920*        05  -COPY WDK601                                                 
009930     03  WDK611 REDEFINES IO-AREA-K6.                                     
009940*        05  -COPY WDK611                                                 
010200     EJECT                                                                
010300 LINKAGE SECTION.                                                         
010400                                                                          
010501                                                                          
010502*01  -COPY W0008  -PRE WDK6-                                              
010510     05  WDK6-KEY-AREA-IDARTNR   PIC S9(9) COMP-3.                        
010600     EJECT                                                                
010701 PROCEDURE DIVISION  USING WDK6-PCB.                                      
010702 MAIN SECTION.                                                            
010710     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
010800                                                                          
011000                                                                          
011100     PERFORM A-INIT                                                       
011200                                                                          
011301     PERFORM IMS-GET-WDK6                                                 
011302     PERFORM UNTIL SEGMENT-SAKNAS                                         
011303       EVALUATE WDK6-SEG-NAME-FB                                          
011304         WHEN 'WDK601'                                                    
011305           MOVE ART-IDARTNR          TO WS-IDARTNR                        
011306                                                                          
011307         WHEN 'WDK611'                                                    
011309           IF CLAG-FLCDART = 'J'                                          
011310             IF CLAG-KVQPACK-3 > 0                                        
011311                MOVE +1 TO IX                                             
011312                MOVE ZERO TO WS-REST                                      
011313                PERFORM UNTIL IX > MAX-IX                                 
011315                  COMPUTE WS-RESTSALDO (IX) =                             
011316                         (CLAG-KVLS-CD (IX) - CLAG-KVRESS-CD (IX))        
011317                  DIVIDE WS-RESTSALDO (IX)                                
011318                          BY CLAG-KVQPACK-3                               
011319                          GIVING WS-SLASK                                 
011320                          REMAINDER WS-REST                               
011327                  IF WS-REST NOT = 0                                      
011328                     MOVE WS-IDARTNR TO UT-IDARTNR                        
011329                     MOVE CLAG-KVQPACK-3 TO UT-KVQPACK-3                  
011330                     MOVE CLAG-ADART-CD(IX) TO UT-ADART-CD                
011331                     MOVE CLAG-KVLS-CD(IX)  TO UT-KVLS-CD                 
011332                     PERFORM S11-SKRIV-W61145                             
011333                  END-IF                                                  
011334                  ADD +1 TO IX                                            
011335                END-PERFORM                                               
011336             END-IF                                                       
011337           END-IF                                                         
011338       END-EVALUATE                                                       
011339       PERFORM IMS-GET-WDK6                                               
011340     END-PERFORM                                                          
011400     PERFORM Z-FINIT                                                      
011500                                                                          
011600     MOVE ZERO TO RETURN-CODE                                             
011700     GOBACK                                                               
011800     .                                                                    
011900     EJECT                                                                
012000 A-INIT SECTION.                                                          
012201                                                                          
012210     OPEN OUTPUT W61145                                                   
012300                                                                          
012400     ACCEPT DAGENS-DATUM  FROM DATE                                       
012510     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012700     .                                                                    
012800     EJECT                                                                
012900 Z-FINIT SECTION.                                                         
013010     CLOSE W61145                                                         
013101     SKIP2                                                                
013102     MOVE 'S' TO POSTSUM-OPKOD                                            
013110     CALL POSTSUM USING POSTSUM-PARM                                      
013200     .                                                                    
013401     EJECT                                                                
013402 S11-SKRIV-W61145 SECTION.                                                
013403                                                                          
013404     WRITE UT-POST FROM UT-AREA                                           
013405                                                                          
013407     MOVE 'W61145' TO POSTSUM-FDNAMN                                      
013408     MOVE 'W61145D1' TO POSTSUM-DDNAMN2                                   
013409     CALL POSTSUM USING POSTSUM-PARM                                      
013410     .                                                                    
013600     EJECT                                                                
014300* --- IMS SEKTIONER ---                                                   
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
