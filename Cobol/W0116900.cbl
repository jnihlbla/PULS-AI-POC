001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W0116900.                                                
001300 AUTHOR.         ELEONOR ÖSTRÖM.                                          
001400 DATE-WRITTEN.   06/02/09.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNKTION:                                                            
001800*        SB NEDLÄSNING WDF7                                               
001900*                                                                         
002010*        PROGRAMMET LÄSER      WDF7                                       
002100*                                                                         
002200*    ABENDKODER:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003302*          --- NEDLÄST WDF7                                               
003310     SELECT W01169                     ASSIGN TO W01169D1.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  W01169                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003905                                                                          
003910001  POST -COPY W01169 -PRE  UT-  -L.                                     
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W0116900'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004800                                                                          
004900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005000 01  FILLER REDEFINES DAGENS-DATUM.                                       
005100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005400                                                                          
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006100     SKIP2                                                                
006200*    --- PARAMETRAR TILL ABEND                                            
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
007310*01  AREA -COPY W01169    -PRE UT-                                        
007400     EJECT                                                                
007500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007600*                                                                         
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
009800*    ---  DLI INPUT-OUTPUT AREA                                           
009901 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF7'.                        
009902 01  DLI-IO-AREA     PIC X(200).                                          
009903     SKIP3                                                                
009904*01  WDF701      -COPY WDF701  -PRE WDF701- -RED DLI-IO-AREA.             
009905     EJECT                                                                
010300 LINKAGE SECTION.                                                         
010502*01  -COPY W0008  -PRE WDF7-                                              
010510     05  FILLER                  PIC X.                                   
010600     EJECT                                                                
010701 PROCEDURE DIVISION  USING WDF7-PCB.                                      
010702 MAIN SECTION.                                                            
010710     ENTRY 'DLITCBL' USING WDF7-PCB.                                      
010800                                                                          
011100     PERFORM A-INIT                                                       
011200                                                                          
011301     PERFORM IMS-GET-WDF7                                                 
011302     PERFORM UNTIL SEGMENT-SAKNAS                                         
011303       EVALUATE WDF7-SEG-NAME-FB                                          
011304         WHEN 'WDF701'                                                    
011305           IF WDF701-MPNR-IDPRTNER = 1                                    
011307             PERFORM B-FLYTTA-WDF701                                      
011308             PERFORM S11-SKRIV-W01169                                     
011309           END-IF                                                         
011310       END-EVALUATE                                                       
011311       PERFORM IMS-GET-WDF7                                               
011320     END-PERFORM                                                          
011400     PERFORM Z-FINIT                                                      
011500                                                                          
011600     MOVE ZERO TO RETURN-CODE                                             
011700     GOBACK                                                               
011800     .                                                                    
011900     EJECT                                                                
012000 A-INIT SECTION.                                                          
012201                                                                          
012210     OPEN OUTPUT W01169                                                   
012400     ACCEPT DAGENS-DATUM  FROM DATE                                       
012510     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012700     .                                                                    
012800     SKIP3                                                                
012810 B-FLYTTA-WDF701 SECTION.                                                 
012811                                                                          
012812     MOVE WDF701-MPNR-IDARTNR  TO UT-MPNR-IDARTNR                         
012813     MOVE WDF701-MPNR-IDPRTNER TO UT-MPNR-IDPRTNER                        
012814     MOVE WDF701-MPNR-FLGEMFMC TO UT-MPNR-FLGEMFMC                        
012820     .                                                                    
012830     SKIP3                                                                
012900 Z-FINIT SECTION.                                                         
013000                                                                          
013010     CLOSE W01169                                                         
013102     MOVE 'S' TO POSTSUM-OPKOD                                            
013110     CALL POSTSUM USING POSTSUM-PARM                                      
013200     .                                                                    
013401     SKIP3                                                                
013402 S11-SKRIV-W01169 SECTION.                                                
013403                                                                          
013404     WRITE UT-POST FROM UT-AREA                                           
013405                                                                          
013406     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
013407     MOVE 'W01169'   TO POSTSUM-FDNAMN                                    
013408     MOVE 'W01169D1' TO POSTSUM-DDNAMN2                                   
013409     CALL POSTSUM USING POSTSUM-PARM                                      
013410     .                                                                    
014200     EJECT                                                                
014300* --- IMS SEKTIONER ---                                                   
014501*                                                                         
014502 IMS-GET-WDF7   SECTION.                                                  
014504     CALL CBLTDLI USING GN WDF7-PCB DLI-IO-AREA                           
014505     MOVE WDF7-STATUS-CODE TO STATUS-WS                                   
014506     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
014507     PERFORM IMS-STATUSKONTROLL                                           
014510     .                                                                    
014600     SKIP3                                                                
014700 IMS-STATUSKONTROLL SECTION.                                              
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
