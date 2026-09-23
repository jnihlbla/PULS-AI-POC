001000 ID DIVISION.                                                             
001100                                                                          
001200 PROGRAM-ID.     W5514300.                                                
001300 AUTHOR.         THOMAS LARSSON.                                          
001400 DATE-WRITTEN.   94/05/03.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNKTION:                                                            
001800*        LÄSER NER WDD9 OCH SORTERAR UTFILEN PÅ ARTIKELNR.                
001900*                                                                         
002010*        PROGRAMMET LÄSER      WLINLB (WDD9)                              
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
003302*          --- FIL MED LEVERANTÖRSINFO                                    
003310     SELECT W55143                     ASSIGN TO W55143D1.                
003400     SKIP2                                                                
003500*          --- SORTERINGSFIL                                              
003600     SELECT SORTFIL                    ASSIGN TO W55143DS.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004201     SKIP3                                                                
004202 FD  W55143                                                               
004203     RECORDING       F                                                    
004204     BLOCK CONTAINS  0.                                                   
004205     SKIP2                                                                
004210*01  POST -COPY W55143 -PRE  UT-  -L.                                     
004300     SKIP3                                                                
004400 SD  SORTFIL                                                              
004410     RECORDING       F                                                    
004420     BLOCK CONTAINS  0.                                                   
004500*01  POST -COPY W55143 -PRE  SORT-                                        
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004801                                                                          
004810*    -- CHECKED BY WY2000                                                 
004900 77  IDPGM                       PIC X(8)    VALUE 'W5514300'.            
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005400     EJECT                                                                
005500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005600 01  FILLER REDEFINES DAGENS-DATUM.                                       
005700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
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
007810*01  AREA -COPY W55143     -PRE SORTWS-                                   
008000 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
008100     EJECT                                                                
008200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008300*                                                                         
008400     EJECT                                                                
008500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008600     SKIP3                                                                
008700 01  NYCKLAR-TILL-DLI.                                                    
008801     03  W-IDARTNR-X.                                                     
008802         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008803     03  W-IDLEVNR-X.                                                     
008810         05  W-IDLEVNR           PIC X(5)   VALUE SPACE.                  
008900     SKIP2                                                                
009000*    --- STATUS-KOD FRÅN IMS                                              
009100 01  STATUS-WS                   PIC XX.                                  
009200     88  SEGMENT-FINNS                       VALUE '  '.                  
009300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009400     SKIP2                                                                
009500 01  GODK-STATUSKODER.                                                    
009600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009700     SKIP3                                                                
009800 01  SSA1                        PIC X(64).                               
009900 01  SSA2                        PIC X(64).                               
010000     EJECT                                                                
010100*    --- IMS FUNKTIONSKODER                                               
010200*01  -COPY W0003                                                          
010300     EJECT                                                                
010500*    ---  DLI INPUT-OUTPUT AREA                                           
010600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010700     SKIP3                                                                
010800 01  DLI-IO-AREA.                                                         
010900     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
011001     SKIP3                                                                
011002     03  WLINLB01 REDEFINES IO-AREA.                                      
011003*        05  -COPY WDD901  -PRE INLB01-                                   
011004     SKIP3                                                                
011005     03  WLINLB02 REDEFINES IO-AREA.                                      
011010*        05  -COPY WDD902  -PRE INLB11-                                   
011300     EJECT                                                                
011400 LINKAGE SECTION.                                                         
011500                                                                          
011601     EJECT                                                                
011602*01  -COPY W0008  -PRE INLB-                                              
011610     05  FILLER                  PIC X.                                   
011700     EJECT                                                                
011801 PROCEDURE DIVISION  USING INLB-PCB.                                      
011810     ENTRY 'DLITCBL' USING INLB-PCB.                                      
011900                                                                          
012100     SKIP2                                                                
012200     PERFORM A-INIT                                                       
012300                                                                          
012400     SORT SORTFIL ASCENDING KEY SORT-IDARTNR                              
012500                                SORT-IDLEVNR                              
012600                  INPUT PROCEDURE B-SORT-INPUT                            
012710                  GIVING W55143                                           
012800                                                                          
012900     IF SORT-RETURN NOT = 0                                               
013000       MOVE SORT-RETURN TO SORT-RETURN-X                                  
013100       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
013200           DELIMITED BY SIZE                                              
013300           INTO FELTEXT-STR                                               
013400       DISPLAY FELTEXT                                                    
013500       PERFORM S99-ABEND                                                  
013600     ELSE                                                                 
013700       PERFORM Z-FINIT                                                    
013800                                                                          
013900       MOVE ZERO TO RETURN-CODE                                           
014000       GOBACK                                                             
014100     END-IF                                                               
014200                                                                          
014300     .                                                                    
014400     EJECT                                                                
014500 A-INIT SECTION.                                                          
014800     SKIP2                                                                
014900     ACCEPT DAGENS-DATUM  FROM DATE                                       
015010     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015020     PERFORM AA-NOLLSTAELL-UTPOST                                         
015200     .                                                                    
015400     EJECT                                                                
015401 AA-NOLLSTAELL-UTPOST SECTION.                                            
015402     SKIP2                                                                
015403     MOVE SPACE TO SORTWS-IDLEVNR                                         
015406     MOVE ZERO TO SORTWS-KVBR                                             
015407     .                                                                    
015408     EJECT                                                                
015410 B-SORT-INPUT SECTION.                                                    
015420     SKIP2                                                                
015430     PERFORM IMS-GET-WDD9                                                 
015440     PERFORM UNTIL SEGMENT-SLUT                                           
015450       EVALUATE INLB-SEG-NAME-FB                                          
015460         WHEN 'WDD901  '                                                  
015470           MOVE INLB01-IDARTNR   TO SORTWS-IDARTNR                        
015480         WHEN 'WDD902  '                                                  
015490           IF INLB11-KVBR > ZERO                                          
015491             MOVE INLB11-IDLEVNR TO SORTWS-IDLEVNR                        
015492             MOVE INLB11-KVBR    TO SORTWS-KVBR                           
015493             PERFORM S31-RELEASE-W55143                                   
015494             PERFORM AA-NOLLSTAELL-UTPOST                                 
015495           END-IF                                                         
015496       END-EVALUATE                                                       
015497       PERFORM IMS-GET-WDD9                                               
015498     END-PERFORM                                                          
015499     .                                                                    
015500     EJECT                                                                
015600 Z-FINIT SECTION.                                                         
015701     SKIP2                                                                
015702     MOVE 'S' TO POSTSUM-OPKOD                                            
015710     CALL POSTSUM USING POSTSUM-PARM                                      
015800     .                                                                    
016200     EJECT                                                                
016300 S31-RELEASE-W55143 SECTION.                                              
016400     SKIP2                                                                
016500     RELEASE SORT-POST FROM SORTWS-AREA                                   
016510                                                                          
016520     MOVE 'WDD9'  TO POSTSUM-TRANSTYP                                     
016530     MOVE 'W55143' TO POSTSUM-FDNAMN                                      
016540     MOVE 'W55143D1' TO POSTSUM-DDNAMN2                                   
016550     CALL POSTSUM USING POSTSUM-PARM                                      
016600     .                                                                    
016700     EJECT                                                                
016800 S99-ABEND SECTION.                                                       
016900     SKIP2                                                                
017001     SKIP2                                                                
017002     MOVE 'S' TO POSTSUM-OPKOD                                            
017010     CALL POSTSUM USING POSTSUM-PARM                                      
017100     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
017200     .                                                                    
017300     EJECT                                                                
017400* --- IMS SEKTIONER ---                                                   
017500     SKIP3                                                                
017601     EJECT                                                                
017602 IMS-GET-WDD9   SECTION.                                                  
017603     SKIP2                                                                
017604     CALL CBLTDLI USING GN INLB-PCB DLI-IO-AREA                           
017605     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
017606     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
017607     PERFORM IMS-STATUSKONTROLL                                           
017610     .                                                                    
017700     EJECT                                                                
017800 IMS-STATUSKONTROLL SECTION.                                              
017900     SKIP2                                                                
018000     SET STATUS-IX TO 1                                                   
018100     SEARCH GODK-STATUS                                                   
018200       AT END                                                             
018300         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
018400         DISPLAY FELTEXT                                                  
018500         CALL FELLOG                                                      
018600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
018700         CONTINUE                                                         
018800     END-SEARCH                                                           
018900     .                                                                    
