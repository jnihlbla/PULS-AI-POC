001000 ID DIVISION.                                                             
001100 PROGRAM-ID.     W3711400.                                                
001200 AUTHOR.         MARKUS ASPFJÄLL.                                         
001300 DATE-WRITTEN.   99/12/20.                                                
001400 DATE-COMPILED.                                                           
001500                                                                          
001600*    FUNKTION:                                                            
001700*        LÄSER IGENOM WDA7 BASEN OCH SKAPAR EN UTFIL W37114               
001800*                                                                         
001910*        PROGRAMMET LÄSER      WDA7                                       
002000*                                                                         
002100*    ABENDKODER:                                                          
002200*        U0016 -  . . . .                                                 
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003201     SKIP2                                                                
003202*          --- UTFIL MED WDA7 INFO                                        
003210     SELECT W37114                     ASSIGN TO W37114D1.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003801     SKIP3                                                                
003802 FD  W37114                                                               
003803     RECORDING       F                                                    
003804     BLOCK CONTAINS  0.                                                   
003805                                                                          
003810*01  POST -COPY W37109 -PRE  UT-  -L.                                     
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004101*    -COPY WY2000W9                                                       
004110     SKIP3                                                                
004200 77  IDPGM                       PIC X(8)    VALUE 'W3711400'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004700     EJECT                                                                
004800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004900 01  FILLER REDEFINES DAGENS-DATUM.                                       
005000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005201     EJECT                                                                
005210 01  W-DAAAVV                    PIC 9(6)    VALUE ZERO.                  
005220 01  FILLER REDEFINES W-DAAAVV.                                           
005230     03 W-SEKEL                  PIC 9(2).                                
005240     03 W-AAR                    PIC 9(2).                                
005250     03 W-VECKA                  PIC 9(2).                                
005260     EJECT                                                                
005400 01  DYNAMISKA-SUBPROGRAM.                                                
005500*                                                                         
005600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005920     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006000     SKIP2                                                                
006010*    --- PARAMETRAR TILL DATKORT                                          
006020*                                                                         
006030 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
006040     SKIP2                                                                
006050*01  -COPY WDATKORT                                                       
006060     EJECT                                                                
006100*    --- PARAMETRAR TILL ABEND                                            
006200                                                                          
006300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
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
007210*01  AREA -COPY W37109     -PRE UT-                                       
007300     EJECT                                                                
007400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007500*                                                                         
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
007900 01  NYCKLAR-TILL-DLI.                                                    
008001     03  W-IDPTYP-X.                                                      
008010         05  W-IDPTYP            PIC X(3)    VALUE SPACE.                 
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
009700*    ---  DLI INPUT-OUTPUT AREA                                           
009801 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA701'.                      
009802 01  DLI-IO-AREA.                                                         
009810*    03  -COPY WDA701                                                     
010100     EJECT                                                                
010200 LINKAGE SECTION.                                                         
010300                                                                          
010401                                                                          
010402*01  -COPY W0008  -PRE WDA7-                                              
010410     05  FILLER                  PIC X.                                   
010500     EJECT                                                                
010601 PROCEDURE DIVISION  USING WDA7-PCB.                                      
010602 MAIN SECTION.                                                            
010610     ENTRY 'DLITCBL' USING WDA7-PCB.                                      
010700                                                                          
010900                                                                          
011000     PERFORM A-INIT                                                       
011100                                                                          
011201     PERFORM IMS-GET-WDA7                                                 
011202     PERFORM UNTIL SEGMENT-SAKNAS                                         
011203       EVALUATE WDA7-SEG-NAME-FB                                          
011204         WHEN 'WDA701'                                                    
011205           PERFORM B-SKAPA-UTFIL                                          
011206           PERFORM S11-SKRIV-W37114                                       
011207       END-EVALUATE                                                       
011208       PERFORM IMS-GET-WDA7                                               
011210     END-PERFORM                                                          
011300     PERFORM Z-FINIT                                                      
011400                                                                          
011500     MOVE ZERO TO RETURN-CODE                                             
011600     GOBACK                                                               
011700     .                                                                    
011800     EJECT                                                                
011900 A-INIT SECTION.                                                          
012101                                                                          
012110     OPEN OUTPUT W37114                                                   
012200                                                                          
012300     ACCEPT DAGENS-DATUM  FROM DATE                                       
012400     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
012401     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
012402                        W-AAR                                             
012403     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
012404     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
012405     MOVE D-VECKA   TO W-VECKA                                            
012406*    MOVE DAGENS-DATUM-AAR   TO TMP1-YY                                   
012407*    MOVE 50                 TO TMP2-YY                                   
012409     IF DAGENS-DATUM-AAR > 50                                             
012411       MOVE 19      TO W-SEKEL                                            
012412     ELSE                                                                 
012413       MOVE 20      TO W-SEKEL                                            
012414     END-IF                                                               
012420     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012600     .                                                                    
012700     EJECT                                                                
012710 B-SKAPA-UTFIL SECTION.                                                   
012711     MOVE W-DAAAVV         TO UT-DAAAVV                                   
012720     MOVE ROT-DAFAKT       TO UT-DADATUM                                  
012721     MOVE ROT-IDPTYP       TO UT-IDPTYP                                   
012722     MOVE ROT-IDDISTR      TO UT-IDDISTR                                  
012723     MOVE ROT-IDKUNDNR     TO UT-IDKUNDNR                                 
012724     MOVE ROT-IDBYTRAD     TO UT-IDBYTRAD                                 
012725     MOVE ROT-IDORDER      TO UT-IDORDER                                  
012726     MOVE ROT-KDEXCHA      TO UT-KDEXCHA                                  
012727     MOVE ROT-KVANTAL      TO UT-KVANTAL                                  
012728     MOVE ROT-KVPOINT      TO UT-KVPOINT                                  
012729     MOVE ROT-TENOTE       TO UT-TENOTE                                   
012730     MOVE ROT-IDARTNR-BYT  TO UT-IDARTNR                                  
012731     MOVE SPACE            TO UT-KDBYTREF                                 
012732                              UT-KDBYTSTA-RAPP                            
012733                              UT-KDBYTSTA-OBJ                             
012734                              UT-FLBYTKND                                 
012735                              UT-FLINKLBS                                 
012736                              UT-BEART-ENG                                
012737     MOVE ZERO             TO UT-IDDISTR-BET                              
012738                              UT-IDFKNGRP                                 
012739                              UT-IDBYTRAP                                 
012740                              UT-KVRETUR-GODK                             
012741                              UT-KVVECKOR                                 
012742                              UT-SUPOINT                                  
012743     .                                                                    
012750     EJECT                                                                
012800 Z-FINIT SECTION.                                                         
012910     CLOSE W37114                                                         
013001     SKIP2                                                                
013002     MOVE 'S' TO POSTSUM-OPKOD                                            
013010     CALL POSTSUM USING POSTSUM-PARM                                      
013100     .                                                                    
013301     EJECT                                                                
013302 S11-SKRIV-W37114 SECTION.                                                
013303                                                                          
013304     WRITE UT-POST FROM UT-AREA                                           
013305                                                                          
013306     MOVE UT-IDPTYP TO POSTSUM-TRANSTYP                                   
013307     MOVE 'W37114' TO POSTSUM-FDNAMN                                      
013308     MOVE 'W37114D1' TO POSTSUM-DDNAMN2                                   
013309     CALL POSTSUM USING POSTSUM-PARM                                      
013310     .                                                                    
013500     EJECT                                                                
013600 S99-ABEND SECTION.                                                       
013700                                                                          
013801     SKIP2                                                                
013802     MOVE 'S' TO POSTSUM-OPKOD                                            
013810     CALL POSTSUM USING POSTSUM-PARM                                      
013900     CALL ABEND USING RKOD-ABEND                                          
014000     .                                                                    
014100     EJECT                                                                
014200* --- IMS SEKTIONER ---                                                   
014300                                                                          
014401                                                                          
014402 IMS-GET-WDA7   SECTION.                                                  
014403                                                                          
014404     CALL CBLTDLI USING GN WDA7-PCB DLI-IO-AREA                           
014405     MOVE WDA7-STATUS-CODE TO STATUS-WS                                   
014406     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
014407     PERFORM IMS-STATUSKONTROLL                                           
014410     .                                                                    
014500     EJECT                                                                
014600 IMS-STATUSKONTROLL SECTION.                                              
014700                                                                          
014800     SET STATUS-IX TO 1                                                   
014900     SEARCH GODK-STATUS                                                   
015000       AT END                                                             
015100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
015200           DELIMITED BY SIZE INTO FELTEXT                                 
015300         DISPLAY FELTEXT                                                  
015400         CALL FELLOG                                                      
015500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
015600         CONTINUE                                                         
015700     END-SEARCH                                                           
015800     .                                                                    
015810     EJECT                                                                
015900*    -COPY WY2000P9                                                       
