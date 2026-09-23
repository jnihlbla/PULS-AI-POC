001000 ID DIVISION.                                                             
001100                                                                          
001200 PROGRAM-ID.     W5532400.                                                
001300 AUTHOR.         GUN LÖFGREN.                                             
001400 DATE-WRITTEN.   96/10/01.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNKTION:                                                            
001800*        LÄSER NER WDH8 OCH SORTERAR UTFILERNA PÅ ARTIKELNR.              
001900*                                                                         
002010*        PROGRAMMET LÄSER      WLART  (WDH8)                              
002100*                                                                         
002200*    ABENDKODER:                                                          
002300*        U0016 -  SORTEN GICK SNETT                                       
002500*                                                                         
002600                                                                          
002700     EJECT                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003302*          --- FIL MED GJORDA UPPDATERINGAR                               
003310     SELECT W55324                     ASSIGN TO W55324D1.                
003400     SKIP2                                                                
003410*          --- FIL MED RENSNINGSPOSTER                                    
003420     SELECT W55326                     ASSIGN TO W55324D2.                
003430     SKIP2                                                                
003500*          --- SORTERINGSFIL                                              
003600     SELECT SORTFIL                    ASSIGN TO W55324DS.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004201     SKIP3                                                                
004202 FD  W55324                                                               
004203     RECORDING       F                                                    
004204     BLOCK CONTAINS  0.                                                   
004205     SKIP2                                                                
004210 01  UT-POST1.                                                            
004220*    03   -COPY WDH801 -PRE  UT1- -L.                                     
004240     SKIP3                                                                
004250 FD  W55326                                                               
004260     RECORDING       F                                                    
004270     BLOCK CONTAINS  0.                                                   
004280     SKIP2                                                                
004290 01  UT-POST2.                                                            
004291*    03   -COPY WDH801 -PRE  UT2- -L.                                     
004300     EJECT                                                                
004400 SD  SORTFIL.                                                             
004410                                                                          
004500 01  SORT-POST.                                                           
004510*    03   -COPY WDH801 -PRE  SORT-                                        
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004801                                                                          
004810*    -- CHECKED BY WY2000                                                 
004900 77  IDPGM                       PIC X(8)    VALUE 'W5532400'.            
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200 77  SORTPOST-SLUT-SW            PIC X       VALUE 'N'.                   
006000     EJECT                                                                
006100 01  DYNAMISKA-SUBPROGRAM.                                                
006200*                                                                         
006300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006610     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006700     SKIP2                                                                
006800*    --- PARAMETRAR TILL ABEND                                            
006900                                                                          
007000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007200     SKIP2                                                                
007300 01  FELTEXT.                                                             
007400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007601     EJECT                                                                
007602*    --- PARAMETRAR TILL POSTSUM                                          
007603*                                                                         
007610*01  -COPY W0005   -PRE  POSTSUM-                                         
007801     EJECT                                                                
007802 01  SORT-AREA-START             PIC X(24)   VALUE                        
007803                                 'SORT-AREA-START  '.                     
007804     SKIP2                                                                
007805                                                                          
007810 01  SORTWS-AREA.                                                         
007820*    03   -COPY WDH801     -PRE SORTWS-                                   
007840*                                                                         
008000 01  SORT-RETURN-X               PIC X(4)  VALUE SPACE.                   
008100     EJECT                                                                
008200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008300*                                                                         
008400     SKIP3                                                                
008500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008600     SKIP2                                                                
009000*    --- STATUS-KOD FRÅN IMS                                              
009100 01  STATUS-WS                   PIC XX.                                  
009200     88  SEGMENT-FINNS                       VALUE '  '.                  
009300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009400     SKIP2                                                                
009500 01  GODK-STATUSKODER.                                                    
009600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010000     EJECT                                                                
010100*    --- IMS FUNKTIONSKODER                                               
010200*01  -COPY W0003                                                          
010300     EJECT                                                                
010500*    ---  DLI INPUT-OUTPUT AREA                                           
010600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010700     SKIP3                                                                
010800 01  DLI-IO-AREA.                                                         
010900     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
011001     SKIP3                                                                
011002     03  WLPRIG01 REDEFINES IO-AREA.                                      
011003*        05  -COPY WDH801                                                 
011300     EJECT                                                                
011400 LINKAGE SECTION.                                                         
011500                                                                          
011601                                                                          
011602*01  -COPY W0008  -PRE PRIG-                                              
011610     05  FILLER                  PIC X.                                   
011700     EJECT                                                                
011801 PROCEDURE DIVISION  USING PRIG-PCB.                                      
011802 MAIN SECTION.                                                            
011810     ENTRY 'DLITCBL' USING PRIG-PCB.                                      
011900                                                                          
012100     SKIP2                                                                
012200     PERFORM A-INIT                                                       
012300                                                                          
012400     SORT SORTFIL ASCENDING KEY SORT-PRI-IDARTNR                          
012600                  INPUT PROCEDURE B-SORT-INPUT                            
012710                  OUTPUT PROCEDURE C-SKRIV-FILER                          
012800                                                                          
012900     IF SORT-RETURN NOT ZERO                                              
013000       MOVE SORT-RETURN TO SORT-RETURN-X                                  
013100       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
013200           DELIMITED BY SIZE                                              
013300           INTO FELTEXT-STR                                               
013400       DISPLAY FELTEXT                                                    
013500       PERFORM S99-ABEND                                                  
013600     END-IF                                                               
013610                                                                          
013700     PERFORM Z-FINIT                                                      
013800                                                                          
013900     MOVE ZERO TO RETURN-CODE                                             
014000     GOBACK                                                               
014300     .                                                                    
014400     EJECT                                                                
014500 A-INIT SECTION.                                                          
014800                                                                          
015010     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015011                                                                          
015012     MOVE ZERO TO SORTWS-PRI-WDH801                                       
015013                                                                          
015030     OPEN OUTPUT W55324                                                   
015040                 W55326                                                   
015200     .                                                                    
015408     SKIP3                                                                
015410 B-SORT-INPUT SECTION.                                                    
015420                                                                          
015430     PERFORM IMS-GET-WDH8                                                 
015440     PERFORM UNTIL SEGMENT-SLUT                                           
015490       IF PRI-FLKLAR = JA OR PRI-KDPRIBEH = 'B'                           
015491         MOVE PRI-WDH801     TO SORT-POST                                 
015493         PERFORM S31-RELEASE-W55324                                       
015495       END-IF                                                             
015497       PERFORM IMS-GET-WDH8                                               
015498     END-PERFORM                                                          
015499     .                                                                    
015500     SKIP3                                                                
015510 C-SKRIV-FILER  SECTION.                                                  
015520                                                                          
015521     PERFORM S32-RETURN-SORTPOST                                          
015522     PERFORM UNTIL SORTPOST-SLUT-SW = JA                                  
015523       PERFORM S01-SKRIV-W55324                                           
015524       PERFORM S02-SKRIV-W55326                                           
015525       PERFORM S32-RETURN-SORTPOST                                        
015527     END-PERFORM                                                          
015598     .                                                                    
015599     SKIP3                                                                
015600 Z-FINIT SECTION.                                                         
015701                                                                          
015703     CLOSE W55324                                                         
015704           W55326                                                         
015705                                                                          
015706     MOVE 'S' TO POSTSUM-OPKOD                                            
015710     CALL POSTSUM USING POSTSUM-PARM                                      
015800     .                                                                    
016200     EJECT                                                                
016300 S01-SKRIV-W55324   SECTION.                                              
016400     SKIP2                                                                
016500     WRITE UT-POST1 FROM SORTWS-AREA                                      
016510                                                                          
016520     MOVE 'WDH8'       TO POSTSUM-TRANSTYP                                
016530     MOVE 'W55324'     TO POSTSUM-FDNAMN                                  
016540     MOVE 'W55324D1'   TO POSTSUM-DDNAMN2                                 
016550     CALL POSTSUM USING POSTSUM-PARM                                      
016600     .                                                                    
016700     EJECT                                                                
016701 S02-SKRIV-W55326   SECTION.                                              
016702     SKIP2                                                                
016703     WRITE UT-POST2 FROM SORTWS-AREA                                      
016704                                                                          
016705     MOVE 'WDH8'       TO POSTSUM-TRANSTYP                                
016706     MOVE 'W55326'     TO POSTSUM-FDNAMN                                  
016707     MOVE 'W55324D2'   TO POSTSUM-DDNAMN2                                 
016708     CALL POSTSUM USING POSTSUM-PARM                                      
016709     .                                                                    
016710     EJECT                                                                
016711 S31-RELEASE-W55324 SECTION.                                              
016720     SKIP2                                                                
016730     RELEASE SORT-POST                                                    
016740                                                                          
016750     MOVE 'WDH8'       TO POSTSUM-TRANSTYP                                
016760     MOVE 'SORTIN'     TO POSTSUM-FDNAMN                                  
016770     MOVE 'W55324DS'   TO POSTSUM-DDNAMN2                                 
016780     CALL POSTSUM USING POSTSUM-PARM                                      
016781     .                                                                    
016782     EJECT                                                                
016783 S32-RETURN-SORTPOST SECTION.                                             
016784                                                                          
016785     RETURN SORTFIL    INTO  SORTWS-AREA                                  
016786     AT END                                                               
016787       MOVE JA         TO SORTPOST-SLUT-SW                                
016788     NOT END                                                              
016789       MOVE 'WDH8'     TO POSTSUM-TRANSTYP                                
016790       MOVE 'SORTUT'   TO POSTSUM-FDNAMN                                  
016791       MOVE 'W55324DS' TO POSTSUM-DDNAMN2                                 
016792       CALL POSTSUM USING POSTSUM-PARM                                    
016793     END-RETURN                                                           
016800     .                                                                    
016801     EJECT                                                                
016810 S99-ABEND SECTION.                                                       
016900     SKIP2                                                                
017002     MOVE 'S' TO POSTSUM-OPKOD                                            
017010     CALL POSTSUM USING POSTSUM-PARM                                      
017100     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
017200     .                                                                    
017300     EJECT                                                                
017400* --- IMS SEKTIONER ---                                                   
017500     SKIP3                                                                
017602 IMS-GET-WDH8   SECTION.                                                  
017603     SKIP2                                                                
017604     CALL CBLTDLI USING GN PRIG-PCB DLI-IO-AREA                           
017605     MOVE PRIG-STATUS-CODE TO STATUS-WS                                   
017606     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
017607     PERFORM IMS-STATUSKONTROLL                                           
017610     .                                                                    
017700     SKIP3                                                                
017800 IMS-STATUSKONTROLL SECTION.                                              
017900     SKIP2                                                                
018000     SET STATUS-IX TO 1                                                   
018100     SEARCH GODK-STATUS                                                   
018200       AT END                                                             
018300         MOVE 'STATUS EJ GODK' TO FELTEXT-STR                             
018400         DISPLAY FELTEXT                                                  
018500         CALL FELLOG                                                      
018600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
018700         CONTINUE                                                         
018800     END-SEARCH                                                           
018900     .                                                                    
