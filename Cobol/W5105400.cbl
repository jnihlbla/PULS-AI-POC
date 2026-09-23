000100 ID DIVISION.                                                             
000110                                                                          
000120 PROGRAM-ID.     W5105400.                                                
000130 AUTHOR.         KARL JOHAN HANSSON.                                      
000140 DATE-WRITTEN.   98/10/02.                                                
000150 DATE-COMPILED.                                                           
000160                                                                          
000170*    FUNKTION:                                                            
000180*        PROGRAMMET LÄSER NER EKONOMISALDON FRÅN WDK6 PÅ FIL              
000190*                                                                         
000200*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
000300*                                                                         
000400                                                                          
000500     SKIP3                                                                
000600 ENVIRONMENT DIVISION.                                                    
000700     SKIP2                                                                
000800 INPUT-OUTPUT SECTION.                                                    
000900                                                                          
001000 FILE-CONTROL.                                                            
001100     SKIP2                                                                
001200*    FIL MED AKTUELLA SALDON FRÅN WDK6 PER ART.                           
001300     SELECT W51054                     ASSIGN TO W51054D1.                
001400     SKIP2                                                                
001500*          --- SORTERINGSFIL                                              
001600     SELECT SORTFIL                    ASSIGN TO W51054DS.                
001700     EJECT                                                                
001800 DATA DIVISION.                                                           
001900     SKIP2                                                                
002000 FILE SECTION.                                                            
002100     SKIP3                                                                
002200 FD  W51054                                                               
002300     RECORDING       F                                                    
002400     BLOCK CONTAINS  0.                                                   
002500                                                                          
002600*01  POST -COPY W51054 -PRE  W510D2-  -L.                                 
002700     SKIP2                                                                
002800 SD  SORTFIL.                                                             
002900*01  POST -COPY W51054 -PRE  SORT-                                        
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(8)    VALUE 'W5105400'.            
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700                                                                          
003800 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
003900     88  END-OF-SORTFIL                      VALUE 'J'.                   
004000     EJECT                                                                
004100 01  DYNAMISKA-SUBPROGRAM.                                                
004200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
004600     SKIP2                                                                
004700*    --- VALID IDDC CODES                                                 
004701*01  -COPY WWDCKONS                                                       
004702                                                                          
004710*    --- PARAMETRAR TILL ABEND                                            
004800                                                                          
004900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005200     SKIP2                                                                
005300 01  FELTEXT.                                                             
005400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005600 01  SPARFAELT.                                                           
005700     03  SPAR-IDARTNR            PIC S9(9)   VALUE ZERO   COMP-3.         
005800     EJECT                                                                
005900*    --- PARAMETRAR TILL POSTSUM                                          
006000*                                                                         
006100*01  -COPY W0005   -PRE  POSTSUM-                                         
006200     EJECT                                                                
006300 01  FILLER                      PIC X(24)   VALUE 'SORTWS-AREA'.         
006400                                                                          
006500 01  AREA   -COPY W51054 -PRE SORTWS-                                     
006600 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
006700     EJECT                                                                
006800 01  FILLER                      PIC X(24)   VALUE 'UT-AREA'.             
006900                                                                          
007000*01  AREA -COPY W51054     -PRE UT-                                       
007200     EJECT                                                                
007300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007400*                                                                         
007500     EJECT                                                                
007600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007700     SKIP3                                                                
007800 01  NYCKLAR-TILL-DLI.                                                    
007900     03  W-IDARTNR-X.                                                     
008000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008100     SKIP2                                                                
008200*    --- STATUS-KOD FRÅN IMS                                              
008300 01  STATUS-WS                   PIC XX.                                  
008400     88  SEGMENT-FINNS                       VALUE '  '.                  
008500     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
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
009510 01  DLI-IO-AREA.                                                         
009520     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
009530     SKIP3                                                                
009540     03  WDK701   REDEFINES IO-AREA.                                      
009550*        05  -COPY WDK601                                                 
009560     SKIP3                                                                
009570     03  WDK711   REDEFINES IO-AREA.                                      
009580*        05  -COPY WDK611                                                 
010000     EJECT                                                                
010100 LINKAGE SECTION.                                                         
010200                                                                          
010300                                                                          
010400*01  -COPY W0008  -PRE ARTC-                                              
010500     05  FILLER                  PIC X.                                   
010600     EJECT                                                                
010700 PROCEDURE DIVISION  USING ARTC-PCB.                                      
010800 MAIN SECTION.                                                            
010900     ENTRY 'DLITCBL' USING ARTC-PCB.                                      
011000                                                                          
011100     OPEN OUTPUT W51054                                                   
011200                                                                          
011300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
011600                                                                          
011700     SORT SORTFIL ASCENDING KEY SORT-IDARTNR                              
011800                                SORT-IDDC                                 
011900                  INPUT PROCEDURE B-SORT-INPUT                            
012000                  OUTPUT PROCEDURE C-SORT-OUTPUT                          
012100                                                                          
012200     IF SORT-RETURN NOT = 0                                               
012300       MOVE SORT-RETURN TO SORT-RETURN-X                                  
012400       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
012500           DELIMITED BY SIZE                                              
012600           INTO FELTEXT-STR                                               
012700       DISPLAY FELTEXT                                                    
012800       MOVE RKOD-ABEND-UTAN-DUMP  TO RKOD-ABEND                           
012900       PERFORM S99-ABEND                                                  
013000     ELSE                                                                 
013100       PERFORM Z-FINIT                                                    
013200                                                                          
013300       MOVE ZERO TO RETURN-CODE                                           
013400       GOBACK                                                             
013500     END-IF                                                               
013600     .                                                                    
013700     EJECT                                                                
013800 B-SORT-INPUT  SECTION.                                                   
013900                                                                          
014000     PERFORM IMS-GET-ARTC                                                 
014100     PERFORM UNTIL SEGMENT-SAKNAS                                         
014200       EVALUATE ARTC-SEG-NAME-FB                                          
014300         WHEN 'WDK601  '                                                  
014400           MOVE ART-IDARTNR                 TO SPAR-IDARTNR               
014500         WHEN 'WDK611  '                                                  
014510           MOVE FUNCTION CURRENT-DATE(01:8) TO SORTWS-DAREGDAT            
014520           MOVE FUNCTION CURRENT-DATE(09:8) TO SORTWS-TIKLOCK             
014530           MOVE SPAR-IDARTNR                TO SORTWS-IDARTNR             
014540           MOVE WC-CDC-SE                   TO SORTWS-IDDC                
014550           MOVE CLAG-KVLS                   TO SORTWS-KVLS                
014560           MOVE CLAG-KVEFRS                 TO SORTWS-KVEFRS              
014570           MOVE CLAG-KVAKS-CDC              TO SORTWS-KVAKS               
014580           MOVE CLAG-KVAKS-PAV              TO SORTWS-KVAKS-PAV           
014590           MOVE CLAG-PRINK                  TO SORTWS-PRINK               
014600           MOVE CLAG-PRARTSTD               TO SORTWS-PRARTSTD            
014610           PERFORM S31-SORT-RELEASE                                       
014620                                                                          
014621           IF CLAG-KVAKS-T NOT = ZERO                                     
014622             MOVE WC-CDC-TR                 TO SORTWS-IDDC                
014623             MOVE CLAG-KVAKS-T              TO SORTWS-KVAKS               
014624             MOVE ZERO                      TO SORTWS-KVLS                
014625                                               SORTWS-KVEFRS              
014626                                               SORTWS-KVAKS-PAV           
014627             PERFORM S31-SORT-RELEASE                                     
014628           END-IF                                                         
014629                                                                          
014630       END-EVALUATE                                                       
014640       PERFORM IMS-GET-ARTC                                               
014650     END-PERFORM                                                          
014660     .                                                                    
014670     EJECT                                                                
014680 C-SORT-OUTPUT SECTION.                                                   
014690                                                                          
014700     PERFORM S32-SORT-RETURN                                              
014800     PERFORM UNTIL END-OF-SORTFIL                                         
015000       PERFORM S11-SKRIV-W51054                                           
015100       PERFORM S32-SORT-RETURN                                            
015200     END-PERFORM                                                          
015300     .                                                                    
015400     EJECT                                                                
015500 Z-FINIT SECTION.                                                         
015600                                                                          
015700     CLOSE W51054                                                         
015800                                                                          
015900     MOVE 'S'          TO POSTSUM-OPKOD                                   
016000     CALL POSTSUM USING POSTSUM-PARM                                      
016100     .                                                                    
016200     EJECT                                                                
016300 S11-SKRIV-W51054 SECTION.                                                
016400                                                                          
016500     WRITE W510D2-POST FROM SORTWS-AREA                                   
016600                                                                          
016700     MOVE 'ART '       TO POSTSUM-TRANSTYP                                
016800     MOVE 'W51054'     TO POSTSUM-FDNAMN                                  
016900     MOVE 'W51054D1'   TO POSTSUM-DDNAMN2                                 
017000     CALL POSTSUM USING POSTSUM-PARM                                      
017100     .                                                                    
017200     EJECT                                                                
017300 S31-SORT-RELEASE  SECTION.                                               
017400                                                                          
017500     RELEASE SORT-POST FROM SORTWS-AREA                                   
017600     .                                                                    
017700     EJECT                                                                
017800 S32-SORT-RETURN  SECTION.                                                
017900                                                                          
018000     RETURN SORTFIL INTO SORTWS-AREA                                      
018100     AT END                                                               
018200         SET END-OF-SORTFIL TO TRUE                                       
018300     .                                                                    
018400     EJECT                                                                
018500 S99-ABEND SECTION.                                                       
018600                                                                          
018700     SKIP2                                                                
018800     MOVE 'S' TO POSTSUM-OPKOD                                            
018900     CALL POSTSUM USING POSTSUM-PARM                                      
019000     CALL ABEND USING RKOD-ABEND                                          
019100     .                                                                    
019200     EJECT                                                                
019300* --- IMS SEKTIONER ---                                                   
019400 IMS-GET-ARTC   SECTION.                                                  
019500                                                                          
019600     CALL CBLTDLI USING GN ARTC-PCB DLI-IO-AREA                           
019700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
019800     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
019900     PERFORM IMS-STATUSKONTROLL                                           
020000     .                                                                    
020100     EJECT                                                                
020200 IMS-STATUSKONTROLL SECTION.                                              
020300                                                                          
020400     SET STATUS-IX TO 1                                                   
020500     SEARCH GODK-STATUS                                                   
020600       AT END                                                             
020700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
020800           DELIMITED BY SIZE INTO FELTEXT                                 
020900         DISPLAY FELTEXT                                                  
021000         CALL FELLOG                                                      
021100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
021200         CONTINUE                                                         
021300     END-SEARCH                                                           
021400     .                                                                    
