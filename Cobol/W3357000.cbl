001000 ID DIVISION.                                                             
001200 PROGRAM-ID.     W3357000.                                                
001300 AUTHOR.         INGVAR SKJELBRED.                                        
001400 DATE-WRITTEN.   95/10/06.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNKTION:                                                            
001800*                                                                         
001900*                                                                         
002010*        PROGRAMMET LÄSER      WLBETC (WDB1)                              
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
003302*          --- FIL FRÅN DATABASEN WDB1                                    
003310     SELECT W33570                     ASSIGN TO W33570D1.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  W33570                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003905                                                                          
003910*01  POST -COPY W33570 -PRE  UT-  -L.                                     
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004201                                                                          
004210*    -- CHECKED BY WY2000                                                 
004300 77  IDPGM                       PIC X(8)    VALUE 'W3357000'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004800     EJECT                                                                
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
006400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006600     SKIP2                                                                
006700 01  FELTEXT.                                                             
006800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007001     EJECT                                                                
007002*    --- PARAMETRAR TILL POSTSUM                                          
007003*                                                                         
007010*01  -COPY W0005   -PRE  POSTSUM-                                         
007201     EJECT                                                                
007202 01  UT-AREA-START               PIC X(24)   VALUE                        
007203                                 'UT-AREA-START  '.                       
007204     SKIP2                                                                
007205                                                                          
007210*01  AREA -COPY W33570     -PRE UT-                                       
007300     EJECT                                                                
007400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007500*                                                                         
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
008200*    --- STATUS-KOD FRÅN IMS                                              
008300 01  STATUS-WS                   PIC XX.                                  
008400     88  SEGMENT-FINNS                       VALUE '  '.                  
008500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008510     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008600     SKIP2                                                                
008700 01  GODK-STATUSKODER.                                                    
008800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008900     SKIP3                                                                
009000 01  SSA1                        PIC X(64).                               
009100 01  SSA2                        PIC X(64).                               
009200     EJECT                                                                
009300*    --- IMS FUNKTIONSKODER                                               
009400*01  -COPY W0003                                                          
009500     EJECT                                                                
009700*    ---  DLI INPUT-OUTPUT AREA                                           
009800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
009900                                                                          
010000 01  DLI-IO-AREA.                                                         
010203*  03  -COPY WDB101  -PRE WDB1-                                           
010500     EJECT                                                                
010600 LINKAGE SECTION.                                                         
010802*01  -COPY W0008  -PRE WDB1-                                              
010810     05  FILLER                  PIC X.                                   
010900     EJECT                                                                
011001 PROCEDURE DIVISION  USING WDB1-PCB.                                      
011002 MAIN SECTION.                                                            
011010     ENTRY 'DLITCBL' USING WDB1-PCB.                                      
011300                                                                          
011400     PERFORM A-INIT                                                       
011500     PERFORM IMS-GET-WDB1                                                 
011600     PERFORM UNTIL SEGMENT-SLUT                                           
011603       PERFORM B-FLYTTA-DISTRIKT                                          
011620       PERFORM IMS-GET-WDB1                                               
012400     END-PERFORM                                                          
012500                                                                          
012600                                                                          
012700     PERFORM Z-FINIT                                                      
012800                                                                          
012900     MOVE ZERO TO RETURN-CODE                                             
013000     GOBACK                                                               
013100     .                                                                    
013200     EJECT                                                                
013300 A-INIT SECTION.                                                          
013501                                                                          
013510     OPEN OUTPUT W33570                                                   
013600                                                                          
013700     ACCEPT DAGENS-DATUM  FROM DATE                                       
013810     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014000     .                                                                    
014100     EJECT                                                                
014101 B-FLYTTA-DISTRIKT SECTION.                                               
014120                                                                          
014130     MOVE WDB1-BET-IDPARTNR     TO UT-IDPARTNR                            
014160     MOVE WDB1-BET-IDPROMR      TO UT-IDPROMR                             
014170     PERFORM S11-SKRIV-W33570                                             
014195     .                                                                    
014196     EJECT                                                                
014300 Z-FINIT SECTION.                                                         
014401                                                                          
014402     CLOSE W33570                                                         
014403                                                                          
014404     MOVE 'S' TO POSTSUM-OPKOD                                            
014410     CALL POSTSUM USING POSTSUM-PARM                                      
014500     .                                                                    
014701     EJECT                                                                
014702 S11-SKRIV-W33570 SECTION.                                                
014703                                                                          
014704     WRITE UT-POST FROM UT-AREA                                           
014705     MOVE 'W335'    TO POSTSUM-TRANSTYP                                   
014707     MOVE 'W33570' TO POSTSUM-FDNAMN                                      
014708     MOVE 'W33570D1' TO POSTSUM-DDNAMN2                                   
014709     CALL POSTSUM USING POSTSUM-PARM                                      
014710     .                                                                    
015600* --- IMS SEKTIONER ---                                                   
015801     EJECT                                                                
015802 IMS-GET-WDB1   SECTION.                                                  
015803                                                                          
015804     CALL CBLTDLI USING GN WDB1-PCB DLI-IO-AREA                           
015805     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
015806     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
015807     PERFORM IMS-STATUSKONTROLL                                           
015810     .                                                                    
015900     EJECT                                                                
016000 IMS-STATUSKONTROLL SECTION.                                              
016100                                                                          
016200     SET STATUS-IX TO 1                                                   
016300     SEARCH GODK-STATUS                                                   
016400       AT END                                                             
016500         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
016600         DISPLAY FELTEXT                                                  
016700         CALL FELLOG                                                      
016800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
016900         CONTINUE                                                         
017000     END-SEARCH                                                           
017100     .                                                                    
