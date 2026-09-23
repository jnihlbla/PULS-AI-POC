001000 ID DIVISION.                                                             
001100 PROGRAM-ID.     W3711200.                                                
001200 AUTHOR.         MARKUS ASPFJÄLL.                                         
001300 DATE-WRITTEN.   99/12/15.                                                
001400 DATE-COMPILED.                                                           
001500                                                                          
001600*    FUNKTION:                                                            
001700*        LÄSER WDM6-BYTESREGISTER OCH SKAPAR EN UT FIL PÅ                 
001800*        VALD INFORMATION.                                                
001900*                                                                         
002010*        PROGRAMMET LÄSER      WDM6                                       
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
003302*          --- UTFIL WDM6 POSTER                                          
003310     SELECT W37112                     ASSIGN TO W37112D1.                
003500     EJECT                                                                
003510*          --- UTFIL WDM6 POSTER                                          
003520     SELECT W37112X                    ASSIGN TO W37112D2.                
003530     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  W37112                                                               
003903     RECORDING       F                                                    
003910     BLOCK CONTAINS  0.                                                   
003920*01  POST -COPY W37109 -PRE UT-  -L.                                      
004000     EJECT                                                                
004010 FD  W37112X                                                              
004020     RECORDING       F                                                    
004030     BLOCK CONTAINS  0.                                                   
004040*01  POST -COPY W37112X -PRE XT-  -L.                                     
004050     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004201                                                                          
004210*    -- CHECKED BY WY2000                                                 
004300 77  IDPGM                       PIC X(8)    VALUE 'W3711200'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004510 01  611-PRESENT                 PIC X.                                   
004520     88  611-NOK                             VALUE 'N'.                   
004600 01  W-KDBYTSTA-RAPP             PIC X       VALUE SPACE.                 
004610 01  W-KDBYTSTA-AVL              PIC X       VALUE SPACE.                 
004700                                                                          
004800     EJECT                                                                
004900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005000 01  FILLER REDEFINES DAGENS-DATUM.                                       
005100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005301     EJECT                                                                
005310 01  W-DAAAVV                    PIC 9(6)    VALUE ZERO.                  
005320 01  FILLER REDEFINES W-DAAAVV.                                           
005330     03 W-SEKEL                  PIC 9(2).                                
005340     03 W-AAR                    PIC 9(2).                                
005350     03 W-VECKA                  PIC 9(2).                                
005400     EJECT                                                                
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600*                                                                         
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006020     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006100     SKIP2                                                                
006110*    --- PARAMETRAR TILL DATKORT                                          
006120*                                                                         
006150 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
006160     SKIP2                                                                
006170*01  -COPY WDATKORT                                                       
006180     EJECT                                                                
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
007310     SKIP2                                                                
007400     EJECT                                                                
007410*01  AREA -COPY W37109          -PRE UT-                                  
007420     EJECT                                                                
007421 01  XT-AREA-START               PIC X(24)   VALUE                        
007422                                 'XT-AREA-START  '.                       
007423     SKIP2                                                                
007428     EJECT                                                                
007429*01  AREA -COPY W37112X         -PRE XT-                                  
007430     EJECT                                                                
007440                                                                          
007500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  NYCKLAR-TILL-DLI.                                                    
008101     03  W-IDDISTR-X.                                                     
008102         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
008103     03  W-IDBYTRAD-X.                                                    
008110         05  W-IDBYTRAD          PIC S9(5)   VALUE ZERO COMP-3.           
008200     SKIP2                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FINNS                       VALUE '  '.                  
008600     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
008610     88  SEGMENT-NEXT-PARENT                 VALUE 'GA'.                  
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
009901 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM6'.                        
009902 01  DLI-IO-AREA.                                                         
009903     03  IO-AREA                     PIC X(100) VALUE SPACE.              
009904     03  DLI-IO-WDM601 REDEFINES IO-AREA.                                 
009910*       05  -COPY WDM601                                                  
009911     EJECT                                                                
009912     03  DLI-IO-WDM611 REDEFINES IO-AREA.                                 
009920*       05  -COPY WDM611                                                  
010200     EJECT                                                                
010300 LINKAGE SECTION.                                                         
010400                                                                          
010501                                                                          
010502*01  -COPY W0008  -PRE WDM6-                                              
010510     05  FILLER                  PIC X.                                   
010600     EJECT                                                                
010701 PROCEDURE DIVISION  USING WDM6-PCB.                                      
010702 MAIN SECTION.                                                            
010710     ENTRY 'DLITCBL' USING WDM6-PCB.                                      
010800                                                                          
011000                                                                          
011100     PERFORM A-INIT                                                       
011200                                                                          
011301     PERFORM IMS-GET-WDM6                                                 
011302     PERFORM UNTIL SEGMENT-SAKNAS                                         
011303       EVALUATE WDM6-SEG-NAME-FB                                          
011304         WHEN 'WDM601'                                                    
011305           PERFORM B-FLYTTA-WDM601-DATA                                   
011306         WHEN 'WDM611'                                                    
011307           MOVE JA TO 611-PRESENT                                         
011308           PERFORM C-KONTROLLERA-STATUS                                   
011309           PERFORM D-MOVE-WDM611-DATA                                     
011310           PERFORM S12-SKRIV-W37112X                                      
011311       END-EVALUATE                                                       
011312       PERFORM IMS-GET-WDM6                                               
011313       IF SEGMENT-NEXT-PARENT OR SEGMENT-SAKNAS                           
011314        IF 611-NOK                                                        
011315         PERFORM S12-SKRIV-W37112X                                        
011316        END-IF                                                            
011317        MOVE NEJ TO 611-PRESENT                                           
011318       END-IF                                                             
011320     END-PERFORM                                                          
011400     PERFORM Z-FINIT                                                      
011500                                                                          
011600     MOVE ZERO TO RETURN-CODE                                             
011700     GOBACK                                                               
011800     .                                                                    
011900     EJECT                                                                
012000 A-INIT SECTION.                                                          
012201                                                                          
012210     OPEN OUTPUT W37112                                                   
012220     OPEN OUTPUT W37112X                                                  
012300                                                                          
012400*    ACCEPT DAGENS-DATUM  FROM DATE                                       
012501     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
012502     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
012503                        W-AAR                                             
012504     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
012505     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
012506     MOVE D-VECKA   TO W-VECKA                                            
012507     IF DAGENS-DATUM-AAR > 50                                             
012508       MOVE 19      TO W-SEKEL                                            
012509     ELSE                                                                 
012510       MOVE 20      TO W-SEKEL                                            
012511     END-IF                                                               
012513                                                                          
012520     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012530     MOVE NEJ TO 611-PRESENT                                              
012700     .                                                                    
012800     EJECT                                                                
012810 B-FLYTTA-WDM601-DATA SECTION.                                            
012811     MOVE RAPP-KDBYTSTA-AVL  TO W-KDBYTSTA-AVL                            
012812                                XT-RAPP-KDBYTSTA-AVL                      
012813     MOVE RAPP-KDBYTSTA-RAPP TO W-KDBYTSTA-RAPP                           
012814                                XT-RAPP-KDBYTSTA-RAPP                     
012815     MOVE W-DAAAVV           TO UT-DAAAVV                                 
012816     MOVE 'RET'              TO UT-IDPTYP                                 
012821     MOVE RAPP-IDDISTR       TO UT-IDDISTR                                
012822                                XT-RAPP-IDDISTR                           
012823     MOVE RAPP-IDBYTRAP      TO UT-IDBYTRAP                               
012824                                XT-RAPP-IDBYTRAP                          
012825     MOVE RAPP-IDKUNDNR      TO UT-IDKUNDNR                               
012826                                XT-RAPP-IDKUNDNR                          
012827     MOVE RAPP-DAREGDAT      TO UT-DADATUM                                
012828                                XT-RAPP-DAREGDAT                          
012829     MOVE RAPP-ADBYTANK      TO XT-RAPP-ADBYTANK                          
012830     MOVE RAPP-FLBYTGAR      TO XT-RAPP-FLBYTGAR                          
012831     MOVE RAPP-FLBYGODK      TO XT-RAPP-FLBYGODK                          
012832     MOVE RAPP-IDFAKT        TO XT-RAPP-IDFAKT                            
012833     MOVE RAPP-IDDC          TO XT-RAPP-IDDC                              
012836     MOVE RAPP-IDUSER        TO XT-RAPP-IDUSER                            
012839     MOVE RAPP-KDBYTBEK      TO XT-RAPP-KDBYTBEK                          
012840     MOVE RAPP-KVRETUR-TOT   TO XT-RAPP-KVRETUR-TOT                       
012843     MOVE RAPP-DAANKDAG      TO XT-RAPP-DAANKDAG                          
012849     MOVE RAPP-DAREGDAT-GODK TO XT-RAPP-DAREGDAT-GODK                     
012850     .                                                                    
012851     EJECT                                                                
012852 C-KONTROLLERA-STATUS SECTION.                                            
012853     IF W-KDBYTSTA-RAPP        = 2 OR 3                                   
012854       PERFORM CA-FLYTTA-WDM611-DATA                                      
012855     ELSE                                                                 
012856       IF W-KDBYTSTA-RAPP      > 3                                        
012857*CHANGED                                                                  
012858         IF W-KDBYTSTA-AVL NOT > 3                                        
012859*        IF OBJ-KDBYTSTA-AVL NOT = W-KDBYTSTA-RAPP                        
012860           PERFORM CA-FLYTTA-WDM611-DATA                                  
012861         END-IF                                                           
012862       END-IF                                                             
012863     END-IF                                                               
012870     .                                                                    
012880     EJECT                                                                
012890 CA-FLYTTA-WDM611-DATA SECTION.                                           
012891     IF OBJ-IDARTNR-OBJ > 0                                               
012892       MOVE OBJ-IDBYTRAD       TO UT-IDBYTRAD                             
012893       MOVE OBJ-KVRETUR-URSP   TO UT-KVANTAL                              
012894       MOVE OBJ-KVRETUR-GODK   TO UT-KVRETUR-GODK                         
012895       MOVE OBJ-IDARTNR-OBJ    TO UT-IDARTNR                              
012896       MOVE OBJ-KDBYTREF       TO UT-KDBYTREF                             
012897       MOVE OBJ-IDORDER        TO UT-IDORDER                              
012898       MOVE OBJ-KDBYTSTA-OBJ   TO UT-KDBYTSTA-OBJ                         
012899       MOVE W-KDBYTSTA-RAPP    TO UT-KDBYTSTA-RAPP                        
012900       MOVE ZERO               TO UT-KDEXCHA                              
012901                                  UT-IDDISTR-BET                          
012902                                  UT-KVPOINT                              
012903                                  UT-SUPOINT                              
012904                                  UT-KVVECKOR                             
012905                                  UT-IDFKNGRP                             
012906       MOVE SPACE              TO UT-FLBYTKND                             
012907                                  UT-FLINKLBS                             
012908                                  UT-BEART-ENG                            
012909                                  UT-TENOTE                               
012910                                                                          
012911       PERFORM S11-SKRIV-W37112                                           
012912     END-IF                                                               
012913     .                                                                    
012920     EJECT                                                                
013000 D-MOVE-WDM611-DATA SECTION.                                              
013100      MOVE OBJ-IDBYTRAD       TO XT-OBJ-IDBYTRAD                          
013101      MOVE OBJ-IDTABNR        TO XT-OBJ-IDTABNR                           
013102      MOVE OBJ-IDARTNR-OBJ    TO XT-OBJ-IDARTNR-OBJ                       
013103      MOVE OBJ-BERADREF       TO XT-OBJ-BERADREF                          
013104      MOVE OBJ-IDORDER        TO XT-OBJ-IDORDER                           
013105      MOVE OBJ-KDBYTREF       TO XT-OBJ-KDBYTREF                          
013106      MOVE OBJ-KDBYTSTA-AVL   TO XT-OBJ-KDBYTSTA-AVL                      
013107      MOVE OBJ-KDBYTSTA-OBJ   TO XT-OBJ-KDBYTSTA-OBJ                      
013110      MOVE OBJ-KVRETUR-URSP   TO XT-OBJ-KVRETUR-URSP                      
013120      MOVE OBJ-KVRETUR-GODK   TO XT-OBJ-KVRETUR-GODK                      
013130      MOVE OBJ-IDBYTRAP-9KOMPL TO XT-OBJ-IDBYTRAP-9KOMPL                  
013140      MOVE OBJ-FLSKROT        TO XT-OBJ-FLSKROT                           
013150      MOVE OBJ-FILLER         TO XT-OBJ-FILLERX10                         
013200     .                                                                    
013401     EJECT                                                                
013402 Z-FINIT SECTION.                                                         
013403     CLOSE W37112                                                         
013404     CLOSE W37112X                                                        
013405     SKIP2                                                                
013406     MOVE 'S' TO POSTSUM-OPKOD                                            
013407     CALL POSTSUM USING POSTSUM-PARM                                      
013408     .                                                                    
013409     EJECT                                                                
013410 S11-SKRIV-W37112 SECTION.                                                
013411                                                                          
013412     WRITE UT-POST FROM UT-AREA                                           
013413                                                                          
013414     MOVE UT-IDPTYP TO POSTSUM-TRANSTYP                                   
013415     MOVE 'W37112' TO POSTSUM-FDNAMN                                      
013416     MOVE 'W37112D1' TO POSTSUM-DDNAMN2                                   
013417     CALL POSTSUM USING POSTSUM-PARM                                      
013420     .                                                                    
013600     EJECT                                                                
013610 S12-SKRIV-W37112X SECTION.                                               
013620                                                                          
013630     WRITE XT-POST FROM XT-AREA                                           
013640                                                                          
013650     MOVE 'W371'    TO POSTSUM-TRANSTYP                                   
013660     MOVE 'W37112X' TO POSTSUM-FDNAMN                                     
013670     MOVE 'W37112D2' TO POSTSUM-DDNAMN2                                   
013680     CALL POSTSUM USING POSTSUM-PARM                                      
013690     .                                                                    
013691     EJECT                                                                
013700 S99-ABEND SECTION.                                                       
013800                                                                          
013901     SKIP2                                                                
013902     MOVE 'S' TO POSTSUM-OPKOD                                            
013910     CALL POSTSUM USING POSTSUM-PARM                                      
014000     CALL ABEND USING RKOD-ABEND                                          
014100     .                                                                    
014200     EJECT                                                                
014300* --- IMS SEKTIONER ---                                                   
014400                                                                          
014501                                                                          
014502 IMS-GET-WDM6   SECTION.                                                  
014503                                                                          
014504     CALL CBLTDLI USING GN WDM6-PCB DLI-IO-AREA                           
014505     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
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
