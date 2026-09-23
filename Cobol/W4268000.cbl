001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W4268000.                                                
001300*AUTHOR.         ANNELIE ENGLUND.                                         
001400*DATE-WRITTEN.   93/10/26.                                                
001500                                                                          
001600*    REMARKS                                                              
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        LÄSER NER W6L1 KVALITETSUPPFÖLJNING PÅ EN FIL                    
002000*        W020.ÅÅVV.WKVAHIST                                               
002100*                                                                         
002210*        PROGRAMMET LÄSER      W6UPFA (W6L1)    SB                        
002300*                                                                         
002400*    ABENDKODER:                                                          
002500*        U0016 -  . . . .                                                 
002600*        U1000 -  . . . .                                                 
002700*                                                                         
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003501     SKIP2                                                                
003502*          --- FIL MED KVALITETSHISTORIK                                  
003510     SELECT W42680                     ASSIGN TO W42680D1.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004101     SKIP3                                                                
004102 FD  W42680                                                               
004103     RECORDING       F                                                    
004104     BLOCK CONTAINS  0.                                                   
004105     SKIP2                                                                
004110*01  POST -COPY W4268001 -PRE  UT-  -L.                                   
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400     SKIP2                                                                
004401                                                                          
004410*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W4268000'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  SPAR-AAVV                   PIC 9(4)    VALUE ZERO.                  
004900 77  W-AAVV                      PIC 9(4)    VALUE ZERO.                  
005000     EJECT                                                                
005100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005200 01  FILLER REDEFINES DAGENS-DATUM.                                       
005300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005600     EJECT                                                                
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800*                                                                         
005900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006201     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006210     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006300     SKIP2                                                                
006400*    --- PARAMETRAR TILL ABEND                                            
006500                                                                          
006600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006800     SKIP2                                                                
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007201     EJECT                                                                
007202*    --- PARAMETRAR TILL POSTSUM                                          
007203*                                                                         
007210*01  -COPY W0005   -PRE  POSTSUM-                                         
007301     EJECT                                                                
007310*01  -COPY WDATAREA                                                       
007401     EJECT                                                                
007402 01  UT-AREA-START             PIC X(24)   VALUE                          
007403                                 'UT-AREA-START  '.                       
007404     SKIP2                                                                
007405                                                                          
007410*01  AREA -COPY W4268001   -PRE UT-                                       
007500     EJECT                                                                
007600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  NYCKLAR-TILL-DLI.                                                    
008201     03  W-IDLOPNRM-X.                                                    
008210         05  W-IDLOPNRM          PIC S9(9)   VALUE ZERO COMP-3.           
008300     SKIP2                                                                
008400*    --- STATUS-KOD FRÅN IMS                                              
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FINNS                       VALUE '  '.                  
008700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008710     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008800     SKIP2                                                                
008900 01  GODK-STATUSKODER.                                                    
009000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(64).                               
009300 01  SSA2                        PIC X(64).                               
009400     EJECT                                                                
009500*    --- IMS FUNKTIONSKODER                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009900*    ---  DLI INPUT-OUTPUT AREA                                           
010000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010100     SKIP3                                                                
010200 01  DLI-IO-AREA.                                                         
010300     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
010401     SKIP3                                                                
010402     03  W6UPFA01 REDEFINES IO-AREA.                                      
010410*        05  -COPY W6L101  -PRE UPFA-                                     
010700     EJECT                                                                
010800 LINKAGE SECTION.                                                         
010900                                                                          
011001     EJECT                                                                
011002*01  -COPY W0008  -PRE UPFA-                                              
011010     05  FILLER                  PIC X.                                   
011100     EJECT                                                                
011201 PROCEDURE DIVISION  USING UPFA-PCB.                                      
011210     ENTRY 'DLITCBL' USING UPFA-PCB.                                      
011300                                                                          
011500     SKIP2                                                                
011600     PERFORM A-INIT                                                       
011700     PERFORM IMS-GET-UPFA-W6L1                                            
011800     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
011810       MOVE UPFA-UPPF-TIREGDAT TO DAT-I-TIDATUM                           
011820       MOVE 'AAMMDD' TO DAT-KDDATFORM                                     
011840                                                                          
011850       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
011860                       DAT-O-TIDATUM DAT-KDSVAR                           
011870                                                                          
011880       IF DAT-KDSVAR-OK                                                   
011890         MOVE DAT-TIAAVV-GRP TO W-AAVV                                    
011891       ELSE                                                               
011892         PERFORM S99-ABEND                                                
011893       END-IF                                                             
011894                                                                          
011896       IF W-AAVV = SPAR-AAVV                                              
011900         MOVE UPFA-UPPF-W6L101 TO UT-AREA                                 
012000         PERFORM S11-SKRIV-W42680                                         
012100       END-IF                                                             
012200       PERFORM IMS-GET-UPFA-W6L1                                          
012600     END-PERFORM                                                          
012700                                                                          
012800                                                                          
012900     PERFORM Z-FINIT                                                      
013000                                                                          
013100     MOVE ZERO TO RETURN-CODE                                             
013200     GOBACK                                                               
013300     .                                                                    
013400     EJECT                                                                
013500 A-INIT SECTION.                                                          
013701                                                                          
013710     OPEN OUTPUT W42680                                                   
013800     SKIP2                                                                
013900     ACCEPT DAGENS-DATUM  FROM DATE                                       
014000*    MOVE 940702 TO DAGENS-DATUM                                          
014001     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
014002     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
014003                                                                          
014004     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
014005                     DAT-O-TIDATUM DAT-KDSVAR                             
014006                                                                          
014007     IF DAT-KDSVAR-OK                                                     
014008       MOVE DAT-TIAAVV-GRP TO SPAR-AAVV                                   
014009     ELSE                                                                 
014010       PERFORM S99-ABEND                                                  
014011     END-IF                                                               
014012                                                                          
014020     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014200     .                                                                    
014300     EJECT                                                                
014400 Z-FINIT SECTION.                                                         
014510     CLOSE W42680                                                         
014601     SKIP2                                                                
014602     MOVE 'S' TO POSTSUM-OPKOD                                            
014610     CALL POSTSUM USING POSTSUM-PARM                                      
014700     .                                                                    
014901     EJECT                                                                
014902 S11-SKRIV-W42680 SECTION.                                                
014903     SKIP2                                                                
014904     WRITE UT-POST FROM UT-AREA                                           
014905                                                                          
014907     MOVE 'W42680' TO POSTSUM-FDNAMN                                      
014908     MOVE 'W42680D1' TO POSTSUM-DDNAMN2                                   
014909     CALL POSTSUM USING POSTSUM-PARM                                      
014910     .                                                                    
015100     EJECT                                                                
015200 S99-ABEND SECTION.                                                       
015300     SKIP2                                                                
015401     SKIP2                                                                
015402     MOVE 'S' TO POSTSUM-OPKOD                                            
015410     CALL POSTSUM USING POSTSUM-PARM                                      
015500     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
015600     .                                                                    
015700     EJECT                                                                
015800* --- IMS SEKTIONER ---                                                   
015900     SKIP3                                                                
016001     EJECT                                                                
016002 IMS-GET-UPFA-W6L1   SECTION.                                             
016003     SKIP2                                                                
016004     CALL CBLTDLI USING GN UPFA-PCB DLI-IO-AREA                           
016005     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
016006     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
016007     PERFORM IMS-STATUSKONTROLL                                           
016010     .                                                                    
016100     EJECT                                                                
016200 IMS-STATUSKONTROLL SECTION.                                              
016300     SKIP2                                                                
016400     SET STATUS-IX TO 1                                                   
016500     SEARCH GODK-STATUS                                                   
016600       AT END                                                             
016700         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
016800         DISPLAY FELTEXT                                                  
016900         CALL FELLOG                                                      
017000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017100         CONTINUE                                                         
017200     END-SEARCH                                                           
017300     .                                                                    
